# Dotfiles

Portable development environment bootstrap for Ubuntu/Debian.

This repo manages:

- `~/.bashrc`
- `~/.gitconfig`
- `~/.tmux.conf`
- `~/.config/kitty/kitty.conf`
- `~/.emacs.d` -> repo `emacs/`

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
```

## Local Overrides

The bootstrap creates these files if they do not already exist:

- `~/.bashrc.local`
- `~/.gitconfig.local`

Edit them for values that should not be committed.
