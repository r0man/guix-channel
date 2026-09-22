;;; VM-backed GitHub Actions runner ("the third way").
;;;
;;; Each runner executes inside a dedicated, disposable QEMU virtual
;;; machine whose entire operating system is thrown away after every
;;; job -- the faithful translation of GitHub's hosted `ubuntu-latest'
;;; model.  The inner OS (a normal Guix `operating-system') runs the
;;; runner agent, a rootful dockerd, and powers the whole VM off
;;; (via the shutdown watcher service) when the runner's job is done.
;;; The host's shepherd manages the QEMU process with resource limits;
;;; the host never grants the workflow anything except the QEMU process
;;; itself.
;;;
;;; The VM is the trust boundary: workflow code runs with docker-group
;;; privileges *inside* the VM, but the kernel, filesystem, and network
;;; are the VM's (user-mode/slirp networking, outbound only).
;;;
;;; Deviation from the design doc worth noting: instead of the upstream
;;; `docker-service-type', the guest runs a re-declared dockerd shepherd
;;; service (`github-actions-runner-vm-dockerd') whose requirement list
;;; includes the `scratch-disk' one-shot service.  Upstream's
;;; docker-service-type hardcodes its requirement list, so there is no
;;; way to make dockerd wait for the per-boot scratch disk (which holds
;;; /var/lib/docker) to be formatted and mounted.

(define-module (r0man guix services github-actions-vm)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services networking)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (r0man guix packages github-actions)
  #:use-module (r0man guix services github-actions)
  #:use-module (r0man guix services github-actions-vm-mint)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:re-export (%github-api-base
               %http-request
               github-api-url
               read-pat-file
               mint-registration-token
               mint-and-store-registration-token
               remove-github-actions-runner)
  #:export (github-actions-runner-vm-mint-program
            github-actions-vm-remove-runner-program
            github-actions-runner-vm-configuration
            github-actions-runner-vm-configuration?
            github-actions-runner-vm-configuration-os
            github-actions-runner-vm-configuration-qemu
            github-actions-runner-vm-configuration-memory-size
            github-actions-runner-vm-configuration-cpus
            github-actions-runner-vm-configuration-scratch-disk-size
            github-actions-runner-vm-configuration-arch
            github-actions-runner-vm-configuration-url
            github-actions-runner-vm-configuration-pat-file
            github-actions-runner-vm-configuration-labels
            github-actions-runner-vm-configuration-parallel-instances
            github-actions-runner-vm-configuration-ephemeral?
            github-actions-runner-vm-configuration-runner-package
            github-actions-runner-vm-configuration-use-host-daemon?
            github-actions-runner-vm-configuration-guix-daemon-port
            github-actions-runner-vm-configuration-state-directory
            github-actions-runner-vm-configuration-registry?
            github-actions-runner-vm-configuration-registry-port
            github-actions-runner-vm-configuration-registry-remote-url
            github-actions-runner-vm-configuration-registry-cache-directory
            github-actions-runner-vm-registry-service
            vm-guest-registry-config-file
            vm-guest-dockerd-config-file
            github-actions-runner-vm-operating-system
            github-actions-runner-vm-boot-script
            github-actions-runner-vm-shepherd-services
            github-actions-runner-vm-service-type
            github-actions-vm-workflow-program))

;;;
;;; Store programs for the host-side credential lifecycle.
;;;

(define (github-actions-runner-vm-mint-program)
  "Return a store program that mints a runner registration token: it
takes a PAT-FILE, a repository/organization URL, and a TOKEN-FILE as
arguments, minting a fresh token when needed and writing it to
TOKEN-FILE (mode 0600).  GITHUB_API_BASE overrides the API base URL for
testing."
  (program-file "github-actions-vm-mint-registration-token"
                (with-extensions (list guile-json-4)
                  (with-imported-modules '((r0man guix services github-actions-vm-mint))
                    #~(begin
                      (use-modules (ice-9 match)
                                   (ice-9 textual-ports)
                                   (r0man guix services github-actions-vm-mint))
                      (match (cdr (command-line))
                        ((pat-file url token-file)
                         (catch #t
                           (lambda ()
                             (mint-and-store-registration-token
                              (read-pat-file pat-file) url token-file))
                           (lambda args
                             (format (current-error-port)
                                     "github-actions-runner-vm: token minting failed: ~a~%"
                                     args)
                             (exit 1))))
                        (args
                         (format (current-error-port)
                                 "github-actions-runner-vm: usage: ~a PAT-FILE URL TOKEN-FILE~%"
                                 "github-actions-vm-mint-registration-token")
                         (exit 2))))))))

(define (github-actions-vm-remove-runner-program)
  "Return a store program that removes a registered runner: it takes a
PAT-FILE, a repository/organization URL, and a runner NAME as
arguments, and removes the runner (best effort)."
  (program-file "github-actions-vm-remove-runner"
                (with-extensions (list guile-json-4)
                  (with-imported-modules '((r0man guix services github-actions-vm-mint))
                    #~(begin
                      (use-modules (ice-9 match)
                                   (r0man guix services github-actions-vm-mint))
                      ;; Note: 'exit' must be called outside the
                      ;; catch; quit is itself catchable, so exiting
                      ;; from inside the protected thunk would run the
                      ;; handler instead of exiting.
                      (match (cdr (command-line))
                        ((pat-file url name)
                         (let ((removed
                                (catch #t
                                  (lambda ()
                                    (remove-github-actions-runner
                                     (read-pat-file pat-file) url name))
                                  (lambda args
                                    (format (current-error-port)
                                            "github-actions-runner-vm: runner removal failed: ~a~%"
                                            args)
                                    (exit 1)))))
                           (exit (if removed 0 1))))
                        (args
                         (format (current-error-port)
                                 "github-actions-runner-vm: usage: ~a PAT-FILE URL NAME~%"
                                 "github-actions-vm-remove-runner")
                         (exit 2))))))))

(define (github-actions-vm-workflow-program)
  "Return a store program for dispatching and cancelling GitHub
Actions workflow runs: it takes a subcommand and a PAT-FILE, a
repository/organization URL, and a workflow file path (or numeric ID)
as arguments.  Subcommands:

  dispatch PAT-FILE URL WORKFLOW REF   dispatch WORKFLOW for REF
  cancel-queued PAT-FILE URL WORKFLOW  cancel queued runs of WORKFLOW

GITHUB_API_BASE overrides the API base URL for testing."
  (program-file "github-actions-vm-workflow"
                (with-extensions (list guile-json-4)
                  (with-imported-modules '((r0man guix services github-actions-vm-mint))
                    #~(begin
                      (use-modules (ice-9 match)
                                   (r0man guix services github-actions-vm-mint))
                      (match (cdr (command-line))
                        (("dispatch" pat-file url workflow ref)
                         (let ((dispatched
                                (catch #t
                                  (lambda ()
                                    (dispatch-workflow-file
                                     (read-pat-file pat-file) url workflow ref))
                                  (lambda args
                                    (format (current-error-port)
                                            "github-actions-runner-vm: dispatching ~a failed: ~a~%"
                                            workflow args)
                                    (exit 1)))))
                           (exit (if dispatched 0 1))))
                        (("cancel-queued" pat-file url workflow)
                         (let ((count
                                (catch #t
                                  (lambda ()
                                    (cancel-queued-workflow-runs
                                     (read-pat-file pat-file) url workflow))
                                  (lambda args
                                    (format (current-error-port)
                                            "github-actions-runner-vm: cancelling queued runs of ~a failed: ~a~%"
                                            workflow args)
                                    (exit 1)))))
                           (exit 0)))
                        (args
                         (format (current-error-port)
                                 "github-actions-vm: usage: ~a dispatch|cancel-queued PAT-FILE URL WORKFLOW [REF]~%"
                                 "github-actions-vm-workflow")
                         (exit 2))))))))

;;;
;;; Guest OS.
;;;

(define %runner-user "github-actions-runner")
(define %runner-group "github-actions-runner")

(define %seed-mount-point "/run/github-actions-seed")
(define %feedback-mount-point "/run/github-actions-feedback")
(define %scratch-mount-point "/scratch")
(define %shutdown-directory "/run/github-actions-vm")
(define %shutdown-file (string-append %shutdown-directory "/poweroff"))

(define-record-type* <github-actions-runner-vm-configuration>
  github-actions-runner-vm-configuration
  make-github-actions-runner-vm-configuration
  github-actions-runner-vm-configuration?
  (os                  github-actions-runner-vm-configuration-os
                       (default #f))  ;operating-system or #f (default)
  (qemu                github-actions-runner-vm-configuration-qemu
                       (default qemu-minimal))
  (memory-size         github-actions-runner-vm-configuration-memory-size
                       (default 4096))    ;MiB, per VM
  (cpus                github-actions-runner-vm-configuration-cpus
                       (default 2))       ;per VM
  (scratch-disk-size   github-actions-runner-vm-configuration-scratch-disk-size
                       (default (* 32 (expt 2 30)))) ;bytes, per-boot qcow2
  (arch                github-actions-runner-vm-configuration-arch
                       (default (%current-system)))
  (url                 github-actions-runner-vm-configuration-url
                       (default #f))      ;repository or organization URL
  (pat-file            github-actions-runner-vm-configuration-pat-file
                       (default #f))      ;string path or #f
  (labels              github-actions-runner-vm-configuration-labels
                       (default '("docker" "vm")))
  (parallel-instances
   github-actions-runner-vm-configuration-parallel-instances
   (default 2))
  (ephemeral?          github-actions-runner-vm-configuration-ephemeral?
                       (default #t))
  (runner-package      github-actions-runner-vm-configuration-runner-package
                       (default github-actions-runner))
  (use-host-daemon?    github-actions-runner-vm-configuration-use-host-daemon?
                       (default #t))
  (guix-daemon-port    github-actions-runner-vm-configuration-guix-daemon-port
                       (default 17447))
  (state-directory
   github-actions-runner-vm-configuration-state-directory
   (default "/var/lib/github-actions-runner-vm"))
  (registry?          github-actions-runner-vm-configuration-registry?
                      (default #t))
  (registry-port      github-actions-runner-vm-configuration-registry-port
                      (default 5000))
  (registry-remote-url
   github-actions-runner-vm-configuration-registry-remote-url
   (default "https://registry-1.docker.io"))
  (registry-cache-directory
   github-actions-runner-vm-configuration-registry-cache-directory
   (default "/var/cache/github-actions-runner-vm/registry")))

(define* (vm-guest-dockerd-config-file #:key (registry-port #f))
  "Return a dockerd daemon.json that keeps all docker state on the
per-boot scratch disk.  When REGISTRY-PORT is set, the host's
pull-through registry cache (reachable from the guest at QEMU slirp's
gateway address 10.0.2.2) is configured as a registry mirror, so that
images pulled from Docker Hub are cached on the host and are not
re-downloaded for every job."
  (plain-file "dockerd-github-actions-vm.json"
              (string-append
               "{\"data-root\": \"/scratch/docker\""
               (if registry-port
                   (string-append
                    ",\"registry-mirrors\": [\"http://10.0.2.2:"
                    (number->string registry-port) "\"]"
                    ",\"insecure-registries\": [\"10.0.2.2:"
                    (number->string registry-port) "\"]")
                   "")
               "}\n")))

(define (vm-guest-registry-config-file config)
  "Return the configuration file of the host's pull-through registry
cache: a Docker registry v2 in proxy mode that caches images pulled
from Docker Hub under REGISTRY-CACHE-DIRECTORY."
  (plain-file "docker-registry-github-actions-vm.yml"
              (string-append
               "version: 0.1\n"
               "log:\n"
               "  level: info\n"
               "storage:\n"
               "  filesystem:\n"
               "    rootdirectory: "
               (github-actions-runner-vm-configuration-registry-cache-directory
                config)
               "\n"
               "  delete:\n"
               "    enabled: false\n"
               "http:\n"
               "  addr: 127.0.0.1:"
               (number->string
                (github-actions-runner-vm-configuration-registry-port
                 config))
               "\n"
               "proxy:\n"
               "  remoteurl: "
               (github-actions-runner-vm-configuration-registry-remote-url
                config)
               "\n")))

(define (github-actions-runner-vm-registry-service config)
  "Return the shepherd service running the host's pull-through
registry cache, or '() when it is disabled.  The registry stores
upstream images on the host's disk, keyed by manifest digest, so the
guest dockerd's per-job pulls are served from the local cache instead
of being re-downloaded from Docker Hub."
  (if (github-actions-runner-vm-configuration-registry? config)
      (list (shepherd-service
             (documentation
              "Docker registry v2 in pull-through proxy mode, caching
images pulled from Docker Hub for the runner VMs.")
             (provision '(github-actions-runner-vm-registry))
             (requirement '(user-processes networking))
             (respawn? #t)
             (start #~(make-forkexec-constructor
                       (list #$(file-append docker-registry "/bin/registry")
                             "serve"
                             #$(vm-guest-registry-config-file config))
                       #:log-file
                       "/var/log/github-actions-runner-vm-registry.log"))
             (stop #~(make-kill-destructor))))
      '()))

(define (vm-guest-dockerd-shepherd-service config-file)
  "Return a dockerd shepherd service whose requirements include the
per-boot `scratch-disk' service, so that dockerd never starts before
its data root is available.  This is the one deviation from upstream's
`docker-service-type', whose requirement list is hardcoded."
  (list (shepherd-service
         (documentation "Docker daemon for the GitHub Actions runner VM.")
         (provision '(dockerd))
         (requirement '(user-processes containerd dbus-system elogind
                                      file-system-/sys/fs/cgroup
                                      networking udev scratch-disk))
         (start #~(make-forkexec-constructor
                   (list #$(file-append docker "/bin/dockerd")
                         "-p" "/var/run/docker.pid"
                         (string-append "--config-file=" #$config-file)
                         "--iptables"
                         "--userland-proxy=false")
                   ;; dockerd needs to find iptables (for the container
                   ;; bridge's NAT) and modprobe (for the kernel's
                   ;; netfilter modules) in PATH.
                   #:environment-variables
                   (list "PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin")
                   #:log-file "/var/log/dockerd.log"))
         (stop #~(make-kill-destructor)))))

(define (vm-guest-scratch-disk-service runner-user runner-group)
  "Return a one-shot shepherd service that formats and mounts the
per-boot scratch disk (/dev/vdb) at /scratch, creating the docker data
root and the runner work directory.  The disk is empty at every boot,
so it is formatted unconditionally.  Tolerate a missing disk so that
the OS still boots in environments without one (e.g. the system test
marionette)."
  (shepherd-service
   (documentation "Format and mount the scratch disk.")
   (provision '(scratch-disk))
   (requirement '(file-systems udev))
   (one-shot? #t)
   (start
    #~(lambda ()
        (system* #$(file-append bash-minimal "/bin/bash") "-c"
                 (string-append
                  "set -u\n"
                  "mkdir -p " #$%scratch-mount-point "\n"
                  "if [ -b /dev/vdb ]; then\n"
                  "    if ! " #$(file-append e2fsprogs "/sbin/mkfs.ext4")
                  " -F /dev/vdb; then\n"
                  "        echo 'github-actions-runner-vm: failed to format /dev/vdb' >&2\n"
                  "    elif ! " #$(file-append util-linux "/bin/mount")
                  " /dev/vdb " #$%scratch-mount-point "; then\n"
                  "        echo 'github-actions-runner-vm: failed to mount /dev/vdb' >&2\n"
                  "    fi\n"
                  "else\n"
                  "    echo 'github-actions-runner-vm: no scratch disk (/dev/vdb), using root filesystem' >&2\n"
                  "fi\n"
                  "mkdir -p " #$%scratch-mount-point "/docker"
                  " " #$%scratch-mount-point "/github-actions-runner\n"
                  "chown -R " #$runner-user ":" #$runner-group
                  " " #$%scratch-mount-point "/github-actions-runner || true\n"
                  "exit 0\n"))
        #t))
   (stop #~(const #f))))

(define (vm-guest-shutdown-watcher-service)
  "Return a shepherd service that powers the VM off when the runner
start script touches the shutdown file.  The runner account cannot
invoke the power-off shepherd action itself, so it signals through the
file system instead.  `herd power-off root' stops all services and asks
the kernel to power the machine off, which makes QEMU exit."
  (shepherd-service
   (documentation "Power the VM off when the runner is done.")
   (provision '(github-actions-vm-shutdown))
   (requirement '(file-systems))
   (start #~(make-forkexec-constructor
             (list #$(file-append bash-minimal "/bin/bash") "-c"
                   (string-append
                    "set -u\n"
                    "mkdir -p " #$%shutdown-directory "\n"
                    "chmod 0777 " #$%shutdown-directory "\n"
                    "while [ ! -e " #$%shutdown-file " ]; do sleep 2; done\n"
                    "rm -f " #$%shutdown-file "\n"
                    "exec /run/current-system/profile/bin/herd power-off root\n"))
             #:log-file "/var/log/github-actions-vm-shutdown.log"))
   (stop #~(make-kill-destructor))))

(define* (github-actions-runner-vm-operating-system config
                                                    #:key (index 1))
  "Return the operating system running inside one of the runner VMs of
CONFIG (instance INDEX).  The OS expects the host's /gnu/store to be
9p-mounted read-only, a seed directory with the registration token at
/run/github-actions-seed, and a writable feedback directory at
/run/github-actions-feedback, all provided by the boot script of the
host service.  Docker and the runner agent write to the per-boot
scratch disk mounted at /scratch."
  (define arch (github-actions-runner-vm-configuration-arch config))
  (define aarch64? (string-prefix? "aarch64" arch))
  (define runner-name (format #f "github-actions-vm-~a" index))
  (define labels
    (github-actions-runner-vm-configuration-labels config))
  (define runner-config
    (github-actions-runner-configuration
     (package
      (github-actions-runner-vm-configuration-runner-package config))
     (user %runner-user)
     (group %runner-group)
     (work-dir (string-append %scratch-mount-point
                              "/github-actions-runner"))
     (url (github-actions-runner-vm-configuration-url config))
     (token (string-append %seed-mount-point "/token"))
     (name runner-name)
     (labels labels)
     ;; Ephemeral runners are removed by GitHub after their first job;
     ;; --replace cleans up runners left behind by a crashed VM.
     (replace? #t)
     (extra-registration-args '("--ephemeral"))
     (environment-variables
      (append
       (list "PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin:/usr/local/bin:/usr/bin:/bin")
       (if (github-actions-runner-vm-configuration-use-host-daemon? config)
           (list (string-append
                  "GUIX_DAEMON_SOCKET=guix://10.0.2.2:"
                  (number->string
                   (github-actions-runner-vm-configuration-guix-daemon-port
                    config))))
           '())))
     (supplementary-groups '("docker"))
     (ephemeral? #t)
     (requirements '(scratch-disk virtio-net-module dockerd networking
                    github-actions-vm-shutdown))
     (shutdown-file %shutdown-file)
     (registration-marker
      (string-append %feedback-mount-point "/registered"))
     ;; Log the runner's output to the serial console so that E2E tests
     ;; can see what the runner is doing.
     (log-file "/dev/console")))
  (operating-system
    (host-name runner-name)
    (timezone "UTC")
    (locale "en_US.utf8")
    ;; The kernel is booted directly (-kernel), so the bootloader is a
    ;; placeholder that builds nothing on any architecture.
    (bootloader (bootloader-configuration
                 (bootloader u-boot-bootloader)
                 (targets '("/dev/sdX"))))
    ;; net.ifnames=0 so the virtio NIC appears as eth0, as expected by
    ;; %qemu-static-networking.  The console is chosen per architecture;
    ;; note that the boot script prepends its own console=ttyS0, and the
    ;; kernel uses the LAST console= for /dev/console, so the guest
    ;; services' output reaches the serial console on both architectures.
    (kernel-arguments
     (append (list "net.ifnames=0")
             (if aarch64?
                 '("earlycon=pl011,0x9000000" "console=ttyAMA0")
                 '("console=ttyS0"))
             %default-kernel-arguments))
    (initrd-modules (cons "virtio_net" %base-initrd-modules))
    (file-systems %base-file-systems)
    (services
     (append
      ;; The guest gets no guix-daemon of its own: its guix client
      ;; reaches the host daemon over TCP.
      (remove (lambda (service)
                (eq? (service-kind service) guix-service-type))
              %base-services)
      (list
       (service static-networking-service-type (list %qemu-static-networking))
       (service dbus-root-service-type)
       (service polkit-service-type)
       (service elogind-service-type)
       (service containerd-service-type)
       (simple-service 'github-actions-runner-vm-dockerd
                       shepherd-root-service-type
                       (vm-guest-dockerd-shepherd-service
                        (vm-guest-dockerd-config-file
                         #:registry-port
                         (and (github-actions-runner-vm-configuration-registry?
                               config)
                              (github-actions-runner-vm-configuration-registry-port
                               config)))))
       (simple-service 'github-actions-runner-vm-profile
                       profile-service-type
                       (list docker
                             docker-cli
                             git
                             guix
                             iptables
                             kmod
                             nss-certs
                             e2fsprogs
                             util-linux))
       (simple-service 'github-actions-runner-vm-network
                       shepherd-root-service-type
                       (list (shepherd-service
                              (documentation "Load the virtio_net kernel module.")
                              (provision '(virtio-net-module))
                              (requirement '(file-systems))
                              (one-shot? #t)
                              (start #~(lambda ()
                                         (system* #$(file-append kmod "/bin/modprobe")
                                                  "virtio_net")
                                         #t))
                              (stop #~(const #f)))))
       (simple-service 'github-actions-runner-vm-scratch
                       shepherd-root-service-type
                       (list (vm-guest-scratch-disk-service
                              %runner-user %runner-group)))
       (simple-service 'github-actions-runner-vm-shutdown
                       shepherd-root-service-type
                       (list (vm-guest-shutdown-watcher-service)))
       (simple-service 'github-actions-runner-vm-log
                       shepherd-root-service-type
                       (list (shepherd-service
                              (documentation "Tail the runner log to the serial console.")
                              (provision '(runner-log-tail))
                              (requirement '(github-actions-runner))
                              (start #~(make-forkexec-constructor
                                        (list #$(file-append bash-minimal "/bin/bash") "-c"
                                              (string-append
                                               ;; -F, not -f: the log file
                                               ;; does not exist yet when
                                               ;; the service starts.
                                               "tail -F /var/log/github-actions-runner.log"
                                               " > /dev/console 2>&1"))
                                        #:log-file "/dev/null"))
                              (stop #~(make-kill-destructor)))))
       (service github-actions-runner-service-type runner-config))))))

;;;
;;; Boot script (one QEMU invocation per instance).
;;;

(define (vm-instance-state-directory config index)
  (string-append
   (github-actions-runner-vm-configuration-state-directory config)
   "/" (number->string index)))

;; A lowerable object standing for the QEMU boot script of one VM
;; instance, in the spirit of <virtual-machine> of (gnu system vm): it
;; is lowered by the gexp compiler below, so instances of it can be
;; spliced into gexps and built by the daemon.
(define-record-type* <github-actions-runner-vm-boot>
  github-actions-runner-vm-boot
  make-github-actions-runner-vm-boot
  github-actions-runner-vm-boot?
  (configuration github-actions-runner-vm-boot-configuration)
  (index         github-actions-runner-vm-boot-index (default 1)))

(define-gexp-compiler (github-actions-runner-vm-boot-compiler
                       (boot <github-actions-runner-vm-boot>)
                       system target)
  (match boot
    (($ <github-actions-runner-vm-boot> config index)
     (define arch (github-actions-runner-vm-configuration-arch config))
     (define state-dir (vm-instance-state-directory config index))
     (system-qemu-image/shared-store-script
      (github-actions-runner-vm-operating-system config #:index index)
      #:system (or system arch)
      #:qemu (github-actions-runner-vm-configuration-qemu config)
      #:graphic? #f
      #:volatile? #t
      #:memory-size (github-actions-runner-vm-configuration-memory-size
                     config)
      #:mappings
      (list (file-system-mapping
             (source (string-append state-dir "/seed"))
             (target %seed-mount-point))
            (file-system-mapping
             (source (string-append state-dir "/feedback"))
             (target %feedback-mount-point)
             (writable? #t)))
      ;; The scratch disk file is created fresh by the shepherd start
      ;; script before QEMU starts.
      #:options
      (append
       (list "-nic" "user,model=virtio-net-pci"
             "-smp" (number->string
                     (github-actions-runner-vm-configuration-cpus config))
             "-no-reboot"
             "-drive"
             (string-append
              "file=" (string-append state-dir "/scratch.qcow2")
              ",format=qcow2,if=virtio"))
       (if (string-prefix? "aarch64" (or system arch))
           '("-M" "virt" "-cpu" "max")
           '()))))))

(define* (github-actions-runner-vm-boot-script config
                                               #:key (index 1))
  "Return the lowerable boot script object of runner VM instance INDEX
of CONFIG.  The script boots the inner OS kernel directly, shares the
host's /gnu/store over 9p read-only, mounts the instance's seed and
feedback directories over 9p, and attaches a per-boot qcow2 scratch
disk (created by the shepherd start script)."
  (github-actions-runner-vm-boot
   (configuration config)
   (index index)))

;;;
;;; Host shepherd services.
;;;

(define (github-actions-runner-vm-alias-service instances)
  "Return a shepherd service that acts on the whole pool: starting it
starts every VM instance, and `herd status github-actions-runner-vm'
summarizes the pool."
  (shepherd-service
   (documentation "Alias for the GitHub Actions runner VM pool.")
   (provision '(github-actions-runner-vm))
   (requirement instances)
   (one-shot? #t)
   (respawn? #f)
   (start #~(lambda _ #t))))

(define (github-actions-runner-vm-guix-daemon-service port)
  "Return the shepherd service forwarding the host's guix-daemon socket
to a TCP port on 127.0.0.1, reachable from the VMs at 10.0.2.2 (QEMU
slirp's gateway address maps to host loopback)."
  (shepherd-service
   (documentation
    "Forward the guix-daemon socket to 127.0.0.1 for the runner VMs.")
   (provision '(github-actions-runner-vm-guix-daemon))
   (requirement '(networking))
   (respawn? #t)
   (start #~(make-forkexec-constructor
             (list #$(file-append socat "/bin/socat")
                   (string-append "TCP-LISTEN:"
                                  #$(number->string port)
                                  ",bind=127.0.0.1,fork,reuseaddr")
                   "UNIX-CONNECT:/var/guix/daemon-socket/socket")
             #:log-file "/var/log/github-actions-runner-vm-guix-daemon.log"))
   (stop #~(make-kill-destructor))))

(define (github-actions-runner-vm-instance-service config index)
  "Return the shepherd service running runner VM instance INDEX of
CONFIG: it mints (or reuses) the registration token, creates a fresh
per-boot scratch disk, starts QEMU, and removes the runner registration
when stopped."
  (define state-dir (vm-instance-state-directory config index))
  (define seed-dir (string-append state-dir "/seed"))
  (define feedback-dir (string-append state-dir "/feedback"))
  (define scratch-file (string-append state-dir "/scratch.qcow2"))
  (define log-file
    (string-append "/var/log/github-actions-runner-vm-"
                   (number->string index) ".log"))
  (define marker-file (string-append feedback-dir "/registered"))
  (define token-file (string-append seed-dir "/token"))
  (define scratch-disk-size
    (github-actions-runner-vm-configuration-scratch-disk-size config))
  (define boot (github-actions-runner-vm-boot-script config #:index index))
  (define mint-program (github-actions-runner-vm-mint-program))
  (define remove-program (github-actions-vm-remove-runner-program))
  (define url (github-actions-runner-vm-configuration-url config))
  (define pat-file (github-actions-runner-vm-configuration-pat-file config))
  (define name (format #f "github-actions-vm-~a" index))
  (shepherd-service
   (documentation
    "Run one GitHub Actions runner inside a disposable QEMU VM.")
   (provision (list (string->symbol
                     (format #f "github-actions-runner-vm-~a" index))))
   (requirement
    `(user-processes networking
                     ,@(if (github-actions-runner-vm-configuration-use-host-daemon?
                            config)
                           '(github-actions-runner-vm-guix-daemon)
                           '())
                     ,@(if (github-actions-runner-vm-configuration-registry?
                            config)
                           '(github-actions-runner-vm-registry)
                           '())))
   (respawn? #t)
   (start
    #~(lambda ()
        (use-modules (ice-9 format))
        (mkdir-p #$state-dir)
        (chmod #$state-dir #o700)
        ;; The seed directory is 9p-mounted into the VM; its mode bits
        ;; are visible in the guest, so it must be readable there.  The
        ;; 0700 state directory above keeps it private on the host.
        (mkdir-p #$seed-dir)
        (chmod #$seed-dir #o755)
        (mkdir-p #$feedback-dir)
        (chmod #$feedback-dir #o777)
        (let mint-loop ((attempt 0))
          (cond
           ((zero? (system* #$mint-program #$pat-file #$url #$token-file))
            ;; The guest runner reads the seed token through 9p
        ;; (security_model=none), so it must be readable on the
        ;; host.  The state directory restricts access.
        (chmod (string-append #$seed-dir "/token") #o644)
        ;; Fresh, empty scratch disk for this boot.
            (false-if-exception (delete-file #$scratch-file))
            (if (zero? (system* #$(file-append
                                 (github-actions-runner-vm-configuration-qemu
                                  config)
                                 "/bin/qemu-img")
                                "create" "-f" "qcow2"
                                #$scratch-file
                                (number->string #$scratch-disk-size)))
                (begin
                  ;; Delete the seed token as soon as the runner has
                  ;; registered (it is only needed during boot), or give
                  ;; up after 30 minutes.
                  (fork+exec-command
                   (list #$(file-append bash-minimal "/bin/bash") "-c"
                         (string-append
                          "for i in $(seq 1 900); do "
                          "[ -f " #$marker-file " ] && break; "
                          "sleep 2; done\n"
                          "sleep 5\n"
                          "rm -f " #$token-file "\n"))
                   #:log-file #$log-file)
                  ;; The actual VM.
                  ((make-forkexec-constructor
                    (list #$boot)
                    #:directory #$state-dir
                    #:log-file #$log-file
                    ;; Coarse guard only: with KVM the guest RAM is
                    ;; mmap'd, so RLIMIT_AS approximates a memory cap;
                    ;; the real caps are QEMU's -m and -smp flags.
                    #:resource-limits
                    '((RLIMIT_AS . #,(expt 2 34))))))
                (begin
                  (format (current-error-port)
                          "github-actions-runner-vm: failed to create scratch disk ~a~%"
                          #$scratch-file)
                  #f)))
           ((< attempt 4)
            (format (current-error-port)
                    "github-actions-runner-vm: token minting failed, retrying in 30 seconds (attempt ~a)~%"
                    (+ attempt 1))
            (sleep 30)
            (mint-loop (+ attempt 1)))
           (else
            (format (current-error-port)
                    "github-actions-runner-vm: token minting failed after ~a attempts, giving up~%"
                    (+ attempt 1))
            #f)))))
   (stop
    #~(lambda (running . args)
        (let ((result (apply (make-kill-destructor) running args)))
          ;; Best effort: remove the runner registration so that a
          ;; stopped VM does not leave an offline runner behind on
          ;; GitHub.
          #$(if pat-file
                #~(false-if-exception
                   (system* #$remove-program #$pat-file #$url #$name))
                #~#f)
          result)))))

(define (github-actions-runner-vm-shepherd-services config)
  "Return the shepherd services of CONFIG: the optional guix-daemon TCP
forwarder, one shepherd service per VM instance, and a pool alias."
  (define instances
    (iota (github-actions-runner-vm-configuration-parallel-instances
           config)
          1))
  (append
   (github-actions-runner-vm-registry-service config)
   (if (github-actions-runner-vm-configuration-use-host-daemon? config)
       (list (github-actions-runner-vm-guix-daemon-service
              (github-actions-runner-vm-configuration-guix-daemon-port
               config)))
       '())
   (map (cut github-actions-runner-vm-instance-service config <>)
        instances)
   (list (github-actions-runner-vm-alias-service
          (map (lambda (index)
                 (string->symbol
                  (format #f "github-actions-runner-vm-~a" index)))
               instances)))))

;;;
;;; Configuration record and service type.
;;;

(define github-actions-runner-vm-service-type
  (service-type
   (name 'github-actions-runner-vm)
   (extensions
    (list (service-extension shepherd-root-service-type
                             github-actions-runner-vm-shepherd-services)))
   (default-value (github-actions-runner-vm-configuration))
   (description
    "Run self-hosted GitHub Actions runners inside disposable QEMU
virtual machines.  Each runner executes in a dedicated Guix System VM
with a rootful Docker daemon, the host's /gnu/store mounted read-only,
and user-mode (slirp) networking; the VM powers off when the runner's
job is done and is respawned immediately.  Registration tokens are
minted on the host from a personal access token and never cross into
the VM.  The VM appears in GitHub as an ordinary runner with the
configured labels, so @code{runs-on: [self-hosted, docker]} routes
jobs to it unmodified.")))
