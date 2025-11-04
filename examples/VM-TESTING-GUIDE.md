# Cortex XDR Agent VM Testing Guide

This guide explains how to test the Cortex XDR Agent package and service
in a Guix System virtual machine.

## Prerequisites

Before you begin, ensure you have:

1. **Guix installed** with daemon running
2. **QEMU** available (installed via Guix or system package manager)
3. **At least 4GB RAM** available for the VM
4. **10GB disk space** for the VM image
5. **This guix-channel** checked out locally

## Quick Start

```bash
# 1. Navigate to your guix-channel directory
cd /path/to/guix-channel

# 2. Build the VM image (this takes 10-30 minutes first time)
guix system vm -L . examples/cortex-xdr-vm-config.scm

# 3. Run the VM (the build command outputs the run script path)
$(guix system vm -L . examples/cortex-xdr-vm-config.scm)
```

## Detailed Instructions

### Step 1: Build the VM Image

The VM image is built from the system configuration:

```bash
cd /path/to/guix-channel

guix system vm -L . examples/cortex-xdr-vm-config.scm
```

**What this does:**
- Builds all required packages (including cortex-agent)
- Creates a system closure with all services
- Generates a QEMU startup script
- **Time**: 10-30 minutes on first build, ~2 minutes on subsequent builds

**Expected output:**
```
/gnu/store/xxxxx-run-vm.sh
```

**Save this path** - you'll need it to run the VM.

### Step 2: Run the VM

Execute the script from the build output:

```bash
/gnu/store/xxxxx-run-vm.sh
```

**VM Specifications:**
- **RAM**: 2GB (default, can be adjusted)
- **Disk**: 10GB virtual disk
- **Network**: User-mode networking (NAT)
- **Display**: QEMU window (can run headless with -nographic)

**Startup time**: 30-60 seconds

### Step 3: Access the VM

#### Option A: Console Login (Default)

The VM opens with a graphical console. Log in at the prompt:

```
Username: test
Password: (press Enter - no password set)
```

Then switch to root:
```bash
sudo -i
```

#### Option B: SSH Access

If you enabled port forwarding, SSH into the VM:

```bash
# Run VM with SSH port forwarding
/gnu/store/xxxxx-run-vm.sh -nic user,hostfwd=tcp::10022-:22

# In another terminal, SSH to the VM
ssh -p 10022 test@localhost
```

### Step 4: Verify Cortex XDR Agent Installation

Once logged into the VM, check the installation:

```bash
# 1. Check if the package is installed
ls -la /run/current-system/profile/opt/traps/

# 2. Check the cytool utility
/run/current-system/profile/opt/traps/bin/cytool --help

# 3. Check agent version
cat /run/current-system/profile/opt/traps/version.txt
```

**Expected output:**
```
cortex-agent-8.6.1.129181
0f2740b27a4e5621269d8021025078072b1d04a7
129181
```

### Step 5: Check Service Status

Verify the Cortex XDR service:

```bash
# Check if service is running
sudo herd status cortex-agent

# View service logs
sudo tail -f /var/log/traps/pmd.log

# Check service process
ps aux | grep pmd
```

**Expected states:**

**If service is running:**
```
Status of cortex-agent:
  It is started.
  Running value is #t.
```

**If service failed to start:**
```
Status of cortex-agent:
  It is stopped.
```
(This is expected if no valid Cortex XDR server is configured)

### Step 6: Check Configuration

Inspect the generated configuration:

```bash
# Check cortex.conf
sudo cat /etc/panw/cortex.conf

# Check directory structure
ls -la /etc/panw/
ls -la /var/log/traps/
ls -la /opt/traps/
```

### Step 7: Test Service Management

Try controlling the service:

```bash
# Stop the service
sudo herd stop cortex-agent

# Start the service
sudo herd start cortex-agent

# Restart the service
sudo herd restart cortex-agent

# Check status after each command
sudo herd status cortex-agent
```

### Step 8: Test Configuration Changes

Modify the VM config to test different settings:

```scheme
;; Edit examples/cortex-xdr-vm-config.scm
(service cortex-agent-service-type
         (cortex-agent-configuration
          (endpoint-tags '("test" "modified" "new-tag"))
          (kernel-module? #f)  ; Disable kernel module
          (restrictions '("live_terminal"))))  ; Add restrictions
```

Then rebuild and restart the VM:
```bash
# Rebuild
guix system vm -L . examples/cortex-xdr-vm-config.scm

# Run the new image
/gnu/store/NEW-xxxxx-run-vm.sh
```

## Advanced Testing

### Running Headless

For automated testing without GUI:

```bash
/gnu/store/xxxxx-run-vm.sh -nographic
```

Access via serial console. Use `Ctrl-A X` to exit.

### Increasing VM Resources

Edit the run script or use QEMU options:

```bash
# More RAM
/gnu/store/xxxxx-run-vm.sh -m 4096

# More CPUs
/gnu/store/xxxxx-run-vm.sh -smp 2
```

### Persistent Disk Image

By default, changes are not saved. To create a persistent disk:

```bash
# Create a disk image
qemu-img create -f qcow2 cortex-test.img 20G

# Use it with the VM
/gnu/store/xxxxx-run-vm.sh -hda cortex-test.img
```

### Network Port Forwarding

Forward additional ports:

```bash
/gnu/store/xxxxx-run-vm.sh \
  -nic user,hostfwd=tcp::10022-:22,hostfwd=tcp::8080-:80
```

### Taking Snapshots

Save VM state for quick testing:

```bash
# In QEMU monitor (Ctrl-Alt-2)
savevm test-state-1

# Later, restore
loadvm test-state-1
```

## Testing Checklist

Use this checklist to verify all functionality:

- [ ] VM builds successfully
- [ ] VM boots without errors
- [ ] Can log in as test user
- [ ] Can switch to root with sudo
- [ ] Package is installed in /run/current-system/profile
- [ ] cytool utility is accessible
- [ ] Version file shows correct version
- [ ] Service is defined (herd status shows it)
- [ ] Configuration file exists at /etc/panw/cortex.conf
- [ ] Directories created: /var/log/traps, /opt/traps/*
- [ ] Can start/stop service with herd
- [ ] Service attempts to start pmd binary
- [ ] Logs are written to /var/log/traps/pmd.log
- [ ] Configuration changes can be made and applied

## Troubleshooting

### VM Won't Build

**Error: "guix system: error: failed to load"**

```bash
# Make sure you're in the right directory
cd /path/to/guix-channel

# Check module syntax
guix repl -L . examples/cortex-xdr-vm-config.scm
```

**Error: "no code for module (r0man guix services security)"**

```bash
# Verify the -L flag is used
guix system vm -L . examples/cortex-xdr-vm-config.scm
```

### VM Won't Start

**Error: "Could not initialize SDL"**

```bash
# Run headless instead
/gnu/store/xxxxx-run-vm.sh -nographic
```

**Error: "Cannot set up guest memory 'pc.ram'"**

```bash
# Reduce RAM allocation
/gnu/store/xxxxx-run-vm.sh -m 1024
```

### Service Won't Start

**Service shows "It is stopped"**

This is **expected behavior** if:
- No distribution-id/distribution-server configured
- Cannot reach Cortex XDR server
- No valid license

**Check logs for details:**
```bash
sudo tail -100 /var/log/traps/pmd.log
```

**Common log messages:**
```
# Cannot connect to server (expected in test)
Failed to connect to distribution server

# Missing configuration
No distribution ID configured

# License issues
License validation failed
```

### Binary Execution Issues

**Error: "cannot execute binary file"**

The binaries may need additional RPATH patching. This is a known
limitation noted in the test report.

**Workaround:**
```bash
# Try with explicit library path
sudo LD_LIBRARY_PATH=/run/current-system/profile/opt/traps/lib:\
/run/current-system/profile/opt/traps/glibc/lib \
/run/current-system/profile/opt/traps/bin/pmd
```

### Network Issues

**Cannot SSH to VM**

```bash
# Ensure port forwarding is set up
/gnu/store/xxxxx-run-vm.sh -nic user,hostfwd=tcp::10022-:22

# Wait for SSH to start (check with herd)
sudo herd status ssh-daemon
```

**VM cannot reach internet**

User-mode networking should work by default. If not:
```bash
# Inside VM, check network
ip addr show
ping -c 3 8.8.8.8
```

## Production Deployment Notes

This VM configuration is for **testing only**. For production:

1. **Never use password authentication** for SSH
2. **Configure real Cortex XDR credentials**:
   ```scheme
   (distribution-id "your-real-distribution-id")
   (distribution-server "https://your-cortex-server.com")
   ```
3. **Use proper secrets management** for sensitive data
4. **Configure appropriate restrictions** based on security policy
5. **Enable kernel module** if your kernel supports it
6. **Test thoroughly** in staging before production
7. **Monitor resource usage** and tune accordingly
8. **Set up proper logging** and alerting
9. **Configure backup** and disaster recovery
10. **Keep packages updated** with security patches

## Next Steps

After successful VM testing:

1. **Review logs** in `/var/log/traps/pmd.log` for any errors
2. **Monitor resource usage** with `htop` or `top`
3. **Test with real Cortex XDR server** if available
4. **Adjust configuration** based on your requirements
5. **Create production deployment** plan
6. **Set up monitoring** and alerting
7. **Document** your specific deployment procedures

## Useful Commands Reference

```bash
# System status
sudo herd status
sudo systemctl status  # Won't work - Guix uses Shepherd

# Service management
sudo herd start cortex-agent
sudo herd stop cortex-agent
sudo herd restart cortex-agent
sudo herd status cortex-agent

# Logs
sudo tail -f /var/log/traps/pmd.log
sudo journalctl -xe  # Won't work - use Shepherd logs

# Package info
guix package --show=cortex-agent
ls -la /run/current-system/profile/opt/traps/

# Cytool utility
sudo /run/current-system/profile/opt/traps/bin/cytool status
sudo /run/current-system/profile/opt/traps/bin/cytool --help

# Configuration
sudo cat /etc/panw/cortex.conf
sudo ls -la /opt/traps/config/

# System reconfiguration (from live system)
sudo guix system reconfigure /etc/config.scm

# Shutdown VM
sudo shutdown now
# or from QEMU: Ctrl-A X (in -nographic mode)
```

## Further Information

- **Guix System Manual**: https://guix.gnu.org/manual/en/html_node/System-Configuration.html
- **Guix VM Documentation**: https://guix.gnu.org/manual/en/html_node/Running-Guix-in-a-VM.html
- **Cortex XDR Documentation**: https://docs.paloaltonetworks.com/cortex
- **Test Report**: See `history/CORTEX-XDR-TEST-REPORT.md` for package test results

## Reporting Issues

If you encounter issues with this VM configuration:

1. Check the troubleshooting section above
2. Review logs in `/var/log/traps/`
3. Check service status with `sudo herd status cortex-agent`
4. Report issues via the project's issue tracker

Include in your report:
- Guix version: `guix --version`
- QEMU version: `qemu-system-x86_64 --version`
- Full error messages
- Relevant log excerpts
- Steps to reproduce
