#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DRY_RUN=0
PACKAGES_ONLY=0
DOTFILES_ONLY=0
FORCE=0
OS_ID=""
OS_CODENAME=""
ARCH=""

APT_PACKAGES=(
  bash-completion
  build-essential
  ca-certificates
  clang
  clang-format
  clangd
  cmake
  curl
  emacs-nox
  fd-find
  git
  gh
  gpg
  jq
  kitty
  libbz2-dev
  libffi-dev
  liblzma-dev
  libncursesw5-dev
  libreadline-dev
  libsqlite3-dev
  libssl-dev
  llvm
  make
  ninja-build
  npm
  pkg-config
  ripgrep
  software-properties-common
  tk-dev
  tmux
  unzip
  wl-clipboard
  xclip
  xz-utils
  zlib1g-dev
)

DOCKER_PACKAGES=(
  containerd.io
  docker-buildx-plugin
  docker-ce
  docker-ce-cli
  docker-compose-plugin
)

usage() {
  cat <<'USAGE'
Usage: ./setup.sh [options]

Options:
  --dry-run        Print actions without making changes
  --packages-only  Install packages and toolchains only
  --dotfiles-only  Create local examples and symlink dotfiles only
  --force          Replace conflicting files without keeping a backup
  -h, --help       Show this help
USAGE
}

log() {
  printf '[setup] %s\n' "$*"
}

run() {
  if [ "$DRY_RUN" -eq 1 ]; then
    printf '[dry-run]'
    printf ' %q' "$@"
    printf '\n'
    return 0
  fi
  "$@"
}

run_shell() {
  if [ "$DRY_RUN" -eq 1 ]; then
    printf '[dry-run] bash -lc %q\n' "$*"
    return 0
  fi
  bash -lc "$*"
}

run_sudo_shell() {
  if [ "$DRY_RUN" -eq 1 ]; then
    printf '[dry-run] sudo bash -lc %q\n' "$*"
    return 0
  fi
  sudo bash -lc "$*"
}

require_ubuntu_or_debian() {
  if [ ! -r /etc/os-release ]; then
    printf 'Unsupported system: missing /etc/os-release\n' >&2
    exit 1
  fi

  # shellcheck disable=SC1091
  . /etc/os-release
  case "${ID:-}" in
    ubuntu|debian)
      OS_ID="$ID"
      OS_CODENAME="${VERSION_CODENAME:-}"
      ;;
    *)
      printf 'Unsupported distribution: %s\n' "${ID:-unknown}" >&2
      exit 1
      ;;
  esac

  if [ -z "$OS_CODENAME" ]; then
    printf 'Unable to determine distribution codename\n' >&2
    exit 1
  fi

  ARCH="$(dpkg --print-architecture)"
}

need_sudo() {
  if [ "$DRY_RUN" -eq 1 ]; then
    return 0
  fi
  sudo -v
}

install_apt_packages() {
  local missing=()
  local pkg

  for pkg in "${APT_PACKAGES[@]}"; do
    if ! dpkg -s "$pkg" >/dev/null 2>&1; then
      missing+=("$pkg")
    fi
  done

  if [ "${#missing[@]}" -eq 0 ]; then
    log "apt packages already installed"
    return 0
  fi

  log "installing apt packages: ${missing[*]}"
  if [ "$DRY_RUN" -eq 1 ]; then
    printf '[dry-run] sudo apt-get update\n'
    printf '[dry-run] sudo apt-get install -y'
    printf ' %q' "${missing[@]}"
    printf '\n'
    return 0
  fi

  sudo apt-get update
  sudo apt-get install -y "${missing[@]}"
}

ensure_docker_repo() {
  local keyring='/etc/apt/keyrings/docker.asc'
  local repo_file='/etc/apt/sources.list.d/docker.list'
  local repo_line="deb [arch=${ARCH} signed-by=${keyring}] https://download.docker.com/linux/${OS_ID} ${OS_CODENAME} stable"

  if [ "$DRY_RUN" -eq 1 ]; then
    printf '[dry-run] sudo install -m 0755 -d /etc/apt/keyrings\n'
    printf '[dry-run] curl -fsSL https://download.docker.com/linux/%s/gpg | sudo gpg --dearmor? no, using ascii keyring at %s\n' "$OS_ID" "$keyring"
    printf '[dry-run] sudo tee %s >/dev/null <<< %q\n' "$repo_file" "$repo_line"
    return 0
  fi

  sudo install -m 0755 -d /etc/apt/keyrings
  curl -fsSL "https://download.docker.com/linux/${OS_ID}/gpg" | sudo tee "$keyring" >/dev/null
  sudo chmod a+r "$keyring"
  printf '%s\n' "$repo_line" | sudo tee "$repo_file" >/dev/null
}

install_docker() {
  local missing=()
  local pkg

  for pkg in "${DOCKER_PACKAGES[@]}"; do
    if ! dpkg -s "$pkg" >/dev/null 2>&1; then
      missing+=("$pkg")
    fi
  done

  ensure_docker_repo

  if [ "${#missing[@]}" -eq 0 ]; then
    log "docker packages already installed"
  else
    log "installing docker packages: ${missing[*]}"
    if [ "$DRY_RUN" -eq 1 ]; then
      printf '[dry-run] sudo apt-get update\n'
      printf '[dry-run] sudo apt-get install -y'
      printf ' %q' "${missing[@]}"
      printf '\n'
    else
      sudo apt-get update
      sudo apt-get install -y "${missing[@]}"
    fi
  fi

  if getent group docker >/dev/null 2>&1; then
    log "docker group already exists"
  else
    log "creating docker group"
    run sudo groupadd docker
  fi

  if id -nG "$USER" | tr ' ' '\n' | grep -Fxq docker; then
    log "user $USER already in docker group"
  else
    log "adding $USER to docker group"
    run sudo usermod -aG docker "$USER"
    log "log out and back in to use docker without sudo"
  fi

  if [ "$DRY_RUN" -eq 0 ]; then
    run sudo systemctl enable --now docker
  else
    printf '[dry-run] sudo systemctl enable --now docker\n'
  fi
}

install_rustup() {
  if command -v rustup >/dev/null 2>&1; then
    log "rustup already installed"
    return 0
  fi

  log "installing rustup"
  run_shell 'curl https://sh.rustup.rs -sSf | sh -s -- -y'
}

install_pyenv() {
  if [ -d "$HOME/.pyenv" ]; then
    log "pyenv already installed"
    return 0
  fi

  log "installing pyenv"
  run_shell 'curl https://pyenv.run | bash'
}

install_uv() {
  if command -v uv >/dev/null 2>&1; then
    log "uv already installed"
    return 0
  fi

  log "installing uv"
  run_shell 'curl -LsSf https://astral.sh/uv/install.sh | sh'
}

ensure_local_file() {
  local source_file="$1"
  local target_file="$2"

  if [ -e "$target_file" ]; then
    return 0
  fi

  log "creating local override $target_file"
  run mkdir -p "$(dirname "$target_file")"
  run cp "$source_file" "$target_file"
}

backup_path() {
  local path="$1"
  printf '%s.backup-%s' "$path" "$(date +%Y%m%d%H%M%S)"
}

symlink_target() {
  local source_path="$1"
  local target_path="$2"
  local target_dir
  local current_link
  local backup

  target_dir="$(dirname "$target_path")"
  run mkdir -p "$target_dir"

  if [ -L "$target_path" ]; then
    current_link="$(readlink "$target_path")"
    if [ "$current_link" = "$source_path" ]; then
      log "link already correct: $target_path"
      return 0
    fi
  fi

  if [ -e "$target_path" ] || [ -L "$target_path" ]; then
    if [ "$FORCE" -eq 1 ]; then
      log "removing conflicting target $target_path"
      run rm -rf "$target_path"
    else
      backup="$(backup_path "$target_path")"
      log "backing up $target_path to $backup"
      run mv "$target_path" "$backup"
    fi
  fi

  log "linking $target_path -> $source_path"
  run ln -s "$source_path" "$target_path"
}

link_dotfiles() {
  ensure_local_file "$ROOT_DIR/local/bashrc.local.example" "$HOME/.bashrc.local"
  ensure_local_file "$ROOT_DIR/local/gitconfig.local.example" "$HOME/.gitconfig.local"

  symlink_target "$ROOT_DIR/bashrc" "$HOME/.bashrc"
  symlink_target "$ROOT_DIR/gitconfig" "$HOME/.gitconfig"
  symlink_target "$ROOT_DIR/tmux.conf" "$HOME/.tmux.conf"
  symlink_target "$ROOT_DIR/emacs" "$HOME/.emacs.d"
  symlink_target "$ROOT_DIR/.config/kitty" "$HOME/.config/kitty"
}

parse_args() {
  while [ "$#" -gt 0 ]; do
    case "$1" in
      --dry-run)
        DRY_RUN=1
        ;;
      --packages-only)
        PACKAGES_ONLY=1
        ;;
      --dotfiles-only)
        DOTFILES_ONLY=1
        ;;
      --force)
        FORCE=1
        ;;
      -h|--help)
        usage
        exit 0
        ;;
      *)
        printf 'Unknown argument: %s\n' "$1" >&2
        usage
        exit 1
        ;;
    esac
    shift
  done

  if [ "$PACKAGES_ONLY" -eq 1 ] && [ "$DOTFILES_ONLY" -eq 1 ]; then
    printf 'Choose either --packages-only or --dotfiles-only, not both\n' >&2
    exit 1
  fi
}

main() {
  parse_args "$@"
  require_ubuntu_or_debian

  if [ "$DOTFILES_ONLY" -ne 1 ]; then
    need_sudo
    install_apt_packages
    install_docker
    install_rustup
    install_pyenv
    install_uv
  fi

  if [ "$PACKAGES_ONLY" -ne 1 ]; then
    link_dotfiles
  fi

  log "done"
}

main "$@"
