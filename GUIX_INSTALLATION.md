# Guix Installation

## Installation Summary

Guix has been successfully installed on this system.

### Version Installed
- Guix version 1.4.0 (binary installation)

### Installation Steps Completed

1. **Downloaded Guix Binary**: Downloaded the official Guix 1.4.0 binary tarball from GNU FTP
2. **Extracted Files**: Extracted to `/gnu` and `/var/guix`
3. **Created Build Users**: Created guixbuild group and 10 build users (guixbuilder01-10)
4. **Configured Symlinks**:
   - Created `/usr/local/bin/guix` and `/usr/local/bin/guix-daemon` symlinks
   - Configured user profiles in `~/.config/guix/current`
5. **Started Daemon**: Started guix-daemon with build users group
6. **Authorized Substitutes**: Authorized ci.guix.gnu.org for binary substitutes

### Current Status

- Guix binary: `/usr/local/bin/guix`
- Daemon: Running (PID 9632)
- Build users: 10 users in guixbuild group
- Version: 1.4.0

### Update Limitations

Attempted to update to the latest version using `guix pull`, but encountered network connectivity issues:
- DNS resolution failures when running as root
- Proxy authentication errors (401) when running as user
- TLS handshake failures to git.savannah.gnu.org

These appear to be environment-specific network/proxy configuration issues that prevent Git operations from completing successfully.

### Next Steps

To update Guix when network connectivity is available:
```bash
guix pull
hash guix  # Refresh the cached path
guix --version  # Verify new version
```

To install packages:
```bash
guix install <package-name>
```

To search for packages:
```bash
guix search <term>
```
