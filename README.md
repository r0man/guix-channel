# Guix Channel: Cortex XDR Agent

A GNU Guix channel providing the Palo Alto Networks Cortex XDR endpoint
security agent package and system service for Guix System.

## Overview

This channel provides:

- **`cortex-agent`** - The Cortex XDR Agent package (version 8.6.1)
- **`cortex-agent-service-type`** - A Guix System service for running the agent

Cortex XDR provides comprehensive endpoint security including:
- Anti-Virus/Anti-Malware protection
- EDR (Endpoint Detection and Response)
- Ransomware Protection
- Exploit Prevention
- Behavioral Threat Protection

## Installation

### Step 1: Add the Channel

Add this channel to your `~/.config/guix/channels.scm`:

```scheme
(cons* (channel
        (name 'guix-channel-cortex)
        (url "https://github.com/r0man/guix-channel-cortex")
        (branch "main"))
       %default-channels)
```

Then update your Guix:

```bash
guix pull
```

### Step 2: Add the Service to Your System Configuration

In your `/etc/config.scm` (or system configuration file):

```scheme
(use-modules (gnu)
             (r0man guix services cortex))

(operating-system
  ;; ... your existing configuration ...

  (services
   (cons* (service cortex-agent-service-type
                   (cortex-agent-configuration
                    (distribution-id "your-distribution-id")
                    (distribution-server "https://your-cortex-server.com")))
          %base-services)))
```

### Step 3: Reconfigure Your System

```bash
sudo guix system reconfigure /etc/config.scm
```

## Configuration Options

The `cortex-agent-configuration` record supports the following options:

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `package` | package | cortex-agent | Cortex XDR Agent package |
| `config-file` | file-like | #f | Custom cortex.conf file |
| `distribution-id` | string | "" | Distribution ID from Cortex XDR console |
| `distribution-server` | string | "" | Cortex XDR management server URL |
| `endpoint-tags` | list | '() | Tags for endpoint organization |
| `proxy-list` | list | '() | HTTP proxy servers |
| `unprivileged-user` | string | "cortexuser" | User for unprivileged operations |
| `kernel-module?` | boolean | #t | Enable kernel module for behavioral protection |
| `temporary-session?` | boolean | #f | Temporary session mode (VDI) |
| `vm-template?` | boolean | #f | VM template mode |
| `restrictions` | list | '() | Feature restrictions |
| `extra-options` | list | '() | Additional installer options |

### Example: Full Configuration

```scheme
(service cortex-agent-service-type
         (cortex-agent-configuration
          ;; Required: Your Cortex XDR credentials
          (distribution-id "prod-dist-001")
          (distribution-server "https://cortex.example.com")

          ;; Optional: Endpoint tagging
          (endpoint-tags '("production" "web-server" "zone-dmz"))

          ;; Optional: Proxy configuration
          (proxy-list '("http://proxy1:8080" "http://proxy2:8080"))

          ;; Optional: Disable kernel module (reduces protection)
          ;; (kernel-module? #f)

          ;; Optional: Feature restrictions
          (restrictions '("live_terminal" "script_execution"))))
```

### Available Restrictions

- `live_terminal` - Disable Live Terminal feature
- `script_execution` - Disable remote script execution
- `file_retrieval` - Disable file retrieval capability
- `all` - Apply all restrictions

## Service Management

After installation, manage the service with Shepherd:

```bash
# Check service status
sudo herd status cortex-agent

# Start the service
sudo herd start cortex-agent

# Stop the service
sudo herd stop cortex-agent

# Restart the service
sudo herd restart cortex-agent
```

### Viewing Logs

```bash
# Real-time log viewing
sudo tail -f /var/log/traps/pmd.log

# View recent logs
sudo tail -100 /var/log/traps/pmd.log
```

### Using cytool

The `cytool` utility provides agent status and management:

```bash
# Check agent status
sudo /run/current-system/profile/opt/traps/bin/cytool status

# View help
/run/current-system/profile/opt/traps/bin/cytool --help
```

## File Locations

| Path | Description |
|------|-------------|
| `/etc/panw/cortex.conf` | Generated configuration file |
| `/var/log/traps/pmd.log` | Agent log file |
| `/opt/traps/` | Agent installation directory |
| `/opt/traps/config/` | Agent XML configuration |
| `/opt/traps/bin/` | Agent binaries (pmd, cytool, initd) |

## System Requirements

- **Architecture**: x86_64 only
- **Kernel**: Linux 2.6.32+ (3.4+ recommended for full functionality)
- **RAM**: 4GB minimum (8GB recommended)
- **Disk**: 10GB available space
- **Network**: HTTPS access to Cortex XDR management server
- **License**: Valid Palo Alto Networks Cortex XDR license

## Known Limitations

### Kernel Version Compatibility

The kernel module (`kernel-module? #t`) requires a compatible kernel version.
If you experience issues, try disabling the kernel module:

```scheme
(cortex-agent-configuration
 (kernel-module? #f)
 ;; ... other options ...
 )
```

This disables behavioral threat protection but allows the agent to run.

### nscd Dependency

The service requires `nscd` (Name Service Cache Daemon) for user lookups.
This is included in `%base-services` by default. If you're using a minimal
service configuration, ensure nscd is included:

```scheme
(services
 (cons* (service nscd-service-type)
        (service cortex-agent-service-type)
        ;; ... other services ...
        ))
```

### Proprietary Binary

The Cortex XDR Agent is proprietary software from Palo Alto Networks.
This package repackages the official RPM distribution for Guix System.
You must have a valid license to use the agent.

## Security Considerations

1. **Credentials**: Store `distribution-id` and `distribution-server` securely.
   Consider using Guix secrets management for production deployments.

2. **Config Permissions**: The `/etc/panw/cortex.conf` file is created with
   mode 0600 (root-only readable) automatically.

3. **Unprivileged User**: The agent creates a dedicated `cortexuser` account
   for unprivileged operations, minimizing root exposure.

4. **Feature Restrictions**: Use the `restrictions` option to limit agent
   capabilities based on your security policy.

5. **Network Access**: The agent requires outbound HTTPS access to your
   Cortex XDR server. Configure firewalls accordingly.

## Troubleshooting

### Service Won't Start

**Check logs first:**
```bash
sudo tail -100 /var/log/traps/pmd.log
```

**Common causes:**
- Missing or invalid `distribution-id`/`distribution-server`
- Network connectivity issues to Cortex XDR server
- License validation failures
- Kernel module compatibility issues

**Try without kernel module:**
```scheme
(cortex-agent-configuration
 (kernel-module? #f)
 ;; ... other options ...
 )
```

### Binary Execution Errors

If you see "cannot execute binary file" errors:

```bash
# Check library paths
ldd /run/current-system/profile/opt/traps/bin/pmd

# Verify interpreter
file /run/current-system/profile/opt/traps/bin/pmd
```

### Network Issues

Verify connectivity to your Cortex XDR server:

```bash
curl -v https://your-cortex-server.com
```

Check SSL certificate configuration:
```bash
ls -la /etc/ssl/certs/ca-certificates.crt
```

## Testing in a VM

For testing without affecting your production system, use the provided VM
configuration:

```bash
# Build VM image
guix system image -L . --image-type=qcow2 \
  examples/cortex-xdr-vm-config.scm \
  -r /tmp/test-vm.qcow2

# Run the VM
qemu-system-x86_64 \
  -m 2048 \
  -enable-kvm \
  -drive file=/tmp/test-vm.qcow2,format=qcow2,snapshot=on \
  -net nic,model=virtio \
  -net user,hostfwd=tcp::2222-:22 \
  -nographic &
```

See `examples/VM-TESTING-GUIDE.md` for detailed instructions.

## Examples

The `examples/` directory contains:

- **`cortex-agent-system-config.scm`** - Configuration examples from basic
  to advanced
- **`cortex-xdr-vm-config.scm`** - Complete VM configuration for testing
- **`README.md`** - Detailed examples documentation
- **`VM-TESTING-GUIDE.md`** - Step-by-step VM testing guide

## License

The Cortex XDR Agent is proprietary software from Palo Alto Networks.
The Guix package definition and service configuration in this channel are
provided for convenience under the same terms as GNU Guix.

## Support

- **Issues**: Report issues via the project's issue tracker
- **Cortex XDR Documentation**: https://docs.paloaltonetworks.com/cortex
- **Guix Manual**: https://guix.gnu.org/manual/

## Contributing

Contributions are welcome. Please ensure:

1. Package builds successfully: `guix build -L . cortex-agent`
2. Service module loads: `guix repl -L . -e '(use-modules (r0man guix services security))'`
3. VM configuration builds: `guix system build -L . examples/cortex-xdr-vm-config.scm`
