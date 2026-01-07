# Cortex XDR Agent Examples

This directory contains example configurations and guides for using the
Cortex XDR Agent package and service on Guix System.

## Files

### Configuration Examples

- **`cortex-agent-system-config.scm`** - Comprehensive system configuration
  examples showing different ways to configure the Cortex XDR Agent service.
  Includes 4 example configurations from basic to advanced, with detailed
  documentation of all available options.

- **`cortex-xdr-vm-config.scm`** - A complete, bootable Guix System
  configuration for testing the Cortex XDR Agent in a virtual machine.
  Includes SSH access, debugging tools, and a pre-configured service.

### Guides

- **`VM-TESTING-GUIDE.md`** - Complete guide for building and testing the
  Cortex XDR Agent in a VM. Includes step-by-step instructions, testing
  procedures, troubleshooting tips, and reference commands.

## Quick Start

### For Production Use

1. Review `cortex-agent-system-config.scm` for configuration examples
2. Choose the configuration style that fits your needs
3. Add the service to your system configuration
4. Reconfigure your system: `sudo guix system reconfigure /etc/config.scm`

### For Testing in VM

1. Follow the instructions in `VM-TESTING-GUIDE.md`
2. Build the VM image:
   ```bash
   guix system vm -L /path/to/guix-channel cortex-xdr-vm-config.scm
   ```
3. Run the VM and test the service

## Service Configuration Options

The Cortex XDR Agent service supports the following configuration options:

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `package` | package | cortex-agent | Cortex XDR Agent package to use |
| `config-file` | file-like | #f | Custom cortex.conf file |
| `distribution-id` | string | "" | Distribution ID from Cortex XDR |
| `distribution-server` | string | "" | Cortex XDR server URL |
| `endpoint-tags` | list | '() | Tags for endpoint organization |
| `proxy-list` | list | '() | HTTP proxy servers |
| `unprivileged-user` | string | "cortexuser" | User for agent processes |
| `kernel-module?` | boolean | #t | Enable kernel module |
| `temporary-session?` | boolean | #f | Temporary session mode |
| `vm-template?` | boolean | #f | VM template mode |
| `restrictions` | list | '() | Capability restrictions |
| `extra-options` | list | '() | Additional installer options |

## Example: Basic Configuration

```scheme
(service cortex-agent-service-type
         (cortex-agent-configuration
          (distribution-id "your-dist-id")
          (distribution-server "https://cortex.example.com")))
```

## Example: Full Configuration

```scheme
(service cortex-agent-service-type
         (cortex-agent-configuration
          (distribution-id "prod-dist-001")
          (distribution-server "https://cortex.example.com")
          (endpoint-tags '("production" "web-server" "zone-dmz"))
          (proxy-list '("http://proxy1:8080" "http://proxy2:8080"))
          (kernel-module? #t)
          (restrictions '("live_terminal" "script_execution"))))
```

## Service Management

Once installed, manage the service with Shepherd:

```bash
# Check status
sudo herd status cortex-agent

# Start/stop/restart
sudo herd start cortex-agent
sudo herd stop cortex-agent
sudo herd restart cortex-agent

# View logs
sudo tail -f /var/log/traps/pmd.log

# Use cytool utility
sudo /run/current-system/profile/opt/traps/bin/cytool status
```

## Module Import

Add to your system configuration:

```scheme
(use-modules (r0man guix services cortex))
```

For package access (e.g., adding to system packages):

```scheme
(use-modules (r0man guix packages cortex))
```

## Documentation

For more detailed information, see:

- **Package documentation**: `(r0man guix packages cortex)` module commentary
- **Service documentation**: `(r0man guix services cortex)` module commentary
- **VM testing guide**: `VM-TESTING-GUIDE.md`
- **Configuration examples**: `cortex-agent-system-config.scm`
- **Test report**: `../history/CORTEX-XDR-TEST-REPORT.md`

## Requirements

- **Guix System** or Guix package manager
- **x86_64 architecture**
- **Linux kernel 2.6.32+** (3.4+ recommended)
- **4GB RAM** (8GB recommended)
- **10GB disk space**
- **Network access** to Cortex XDR server

## Notes

- The agent requires a valid Cortex XDR license and server configuration
- For VM testing, the service may not fully start without valid credentials
- See `VM-TESTING-GUIDE.md` for troubleshooting tips
- Production deployments should use proper secrets management

## License

The Cortex XDR Agent itself is proprietary software from Palo Alto Networks.
These configuration examples and documentation are provided for convenience.
