# Dotfiles

Portable development environment bootstrap for Ubuntu/Debian.

This repo manages:

- `~/.bashrc`
- `~/.gitconfig`
- `~/.tmux.conf`
- `~/.config/kitty/kitty.conf`
- `~/.emacs.d` -> repo `emacs/`

The bootstrap installs a core development stack including Docker, Git, tmux,
kitty, Emacs, C/C++ tooling, `pyenv`, `uv`, and `rustup`. Kubernetes
tooling is available as an opt-in install.

Secrets and machine-specific settings stay out of git. Use the example local
override files to add personal identity, CUDA paths, aliases, or host-specific
environment changes.

## Usage

Run the bootstrap from the repo root:

```bash
./setup.sh
```

Useful modes:

```bash
./setup.sh --dry-run
./setup.sh --packages-only
./setup.sh --dotfiles-only
./setup.sh --force
./setup.sh --with-k8s
./setup.sh --packages-only --with-k8s
```

Optional browser installs:

```bash
./setup.sh --with-brave
./setup.sh --with-chrome
./setup.sh --packages-only --with-brave
```

## Local Overrides

The bootstrap creates these files if they do not already exist:

- `~/.bashrc.local`
- `~/.gitconfig.local`

Edit them for values that should not be committed.

## Docker

Docker is installed from Docker's official apt repository. The script also adds
your user to the `docker` group and enables the Docker service.

After a fresh install, log out and back in before using `docker` without `sudo`.

## Kubernetes

Kubernetes tooling is opt-in with `--with-k8s`. It installs `kubectl` from the
current stable Kubernetes apt repository, Helm from the Helm Debian/Ubuntu apt
repository, and K9s from the latest GitHub release `.deb`. It also installs
`ansible`, `kubectx`, and `python3-kubernetes` for common cluster operations.

Bash completion is configured for `kubectl`, the `k` alias, and Helm. Set
`KUBERNETES_MINOR` to a minor such as `v1.34` before running the script to pin
the Kubernetes apt repository instead of using the latest stable minor.

## Browsers

Browser installs are opt-in.

- `--with-brave` installs Brave from Brave's official apt repository.
- `--with-chrome` installs Google Chrome from Google's official apt repository.

Chrome support in this script is limited to `amd64`, matching Google's Debian/
Ubuntu package availability.
