#!/bin/bash

set -e

colors=(51 45 39 33 27 21)

echo -e "\033[38;5;${colors[0]}m   ____           _    \033[0m"
echo -e "\033[38;5;${colors[1]}m  / ___|__ _ _ __| |_  \033[0m"
echo -e "\033[38;5;${colors[2]}m | |   / _\` | '__| __| \033[0m"
echo -e "\033[38;5;${colors[3]}m | |__| (_| | |  | |_  \033[0m"
echo -e "\033[38;5;${colors[4]}m  \\____\\__,_|_|   \\__| \033[0m"
echo -e "\033[38;5;${colors[5]}m                      \033[0m"

INSTALL_DIR="$HOME/.cart"
BIN_DIR="$INSTALL_DIR/bin"
LIB_DIR="$INSTALL_DIR/lib"

OS=$(uname -s | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m)

case "$ARCH" in
  arm64)
    if [[ "$OS" == "linux" ]]; then
      echo "Error: ARM Linux is not supported. Exiting."
      exit 1
    fi
    ARCH="arm64"
    ;;
  x86_64)
    ARCH="x86_64"
    ;;
  *)
    echo "Error: Unsupported architecture: $ARCH"
    exit 1
    ;;
esac

RELEASE_FILE=""
case "$OS" in
  darwin)
    RELEASE_FILE="cartlang-macos-${ARCH}.tar.gz"
    ;;
  linux)
    if [[ "$ARCH" == "x86_64" ]]; then
      RELEASE_FILE="cartlang-ubuntu-${ARCH}.tar.gz"
    else
      echo "Error: Unsupported platform: $OS on $ARCH"
      exit 1
    fi
    ;;
  *)
    echo "Error: Unsupported operating system: $OS"
    exit 1
    ;;
esac

REPO="bbayazit16/cart"
URL="https://github.com/$REPO/releases/latest/download/$RELEASE_FILE"

if [[ -f "$BIN_DIR/cart" || -f "$LIB_DIR/libcartstd.a" ]]; then
  echo "An existing installation was found:"

  LATEST_VERSION=$(curl -s https://api.github.com/repos/$REPO/releases/latest | grep '"tag_name":' | sed -E 's/.*"tag_name": "([^"]+)".*/\1/')

  if [[ -f "$LIB_DIR/libcartstd.a" ]]; then
    echo "- Standard library at $LIB_DIR/libcartstd.a"
  fi

  if [[ -f "$BIN_DIR/cart" ]]; then
    echo "- Compiler at $BIN_DIR/cart"
    EXISTING_VERSION=$("$BIN_DIR/cart" -V 2>/dev/null || echo "unknown")
    echo "  Existing version: $EXISTING_VERSION"
    echo "  Latest version: $LATEST_VERSION"
  fi

  echo ""

  NORMALIZED_EXISTING_VERSION="${EXISTING_VERSION#cart }"
  NORMALIZED_LATEST_VERSION="${LATEST_VERSION#v}"
  if [[ "$NORMALIZED_EXISTING_VERSION" == "$NORMALIZED_LATEST_VERSION" ]]; then
    echo "Your existing version is up to date. This will overwrite the installation."
  elif [[ "$EXISTING_VERSION" == "unknown" ]]; then
    echo "Your existing version is unknown. This will overwrite the installation."
  else
    echo "This will overwrite and update the existing version."
  fi

  echo -e "\033[31mThe installation will continue in 15 seconds. Press Ctrl+C to cancel.\033[0m"
  sleep 15

  echo "Proceeding with installation..."
fi

echo "Downloading $RELEASE_FILE..."
curl -L "$URL" -o "$RELEASE_FILE"

echo "Extracting $RELEASE_FILE..."
tar -xzf "$RELEASE_FILE"

echo "Installing files..."
mkdir -p "$BIN_DIR" "$LIB_DIR"
cp bin/cart "$BIN_DIR/"
cp lib/libcartstd.a "$LIB_DIR/"

echo "Cleaning up..."
rm -rf "$RELEASE_FILE" bin lib

EXPORT_CMD="export PATH=\"$BIN_DIR:\$PATH\""
USER_SHELL=$(basename "$SHELL")

update_shell_config() {
    local shell_config="$1"

    if [[ ! -f "$shell_config" ]]; then
        touch "$shell_config"
        echo "Created $shell_config."
    fi

    if grep -q "$EXPORT_CMD" "$shell_config"; then
        echo "PATH is already set in $shell_config. Skipping."
    else
        echo "$EXPORT_CMD" >> "$shell_config"
        echo "Added PATH to $shell_config."
    fi
}

case "$USER_SHELL" in
    bash)
        if [[ "$OSTYPE" == "darwin"* ]]; then
            [[ -f ~/.bashrc ]] && update_shell_config ~/.bashrc || update_shell_config ~/.bash_profile
        else
            update_shell_config ~/.bashrc
        fi
        ;;
    zsh)
        update_shell_config ~/.zshrc
        ;;
    fish)
        echo "Fish shell detected. Please add the following to your Fish configuration manually:"
        echo "set -x PATH $BIN_DIR \$PATH"
        ;;
    *)
        echo "Unsupported shell: $USER_SHELL. Please set PATH manually in your shell configuration:"
        echo "$EXPORT_CMD"
        ;;
esac

export PATH="$BIN_DIR:$PATH"

echo ""

echo -e "\033[38;5;46mInstallation complete! \033[0m"
echo -e "\033[38;5;82mCompiler installed to \033[38;5;154m$BIN_DIR/cart\033[0m"
echo -e "\033[38;5;82mStandard library installed to \033[38;5;154m$LIB_DIR/libcartstd.a\033[0m"

echo ""

echo -e "\033[31mTo start using CART, you must do one of the following:\033[0m"
echo -e "\033[31m  - EITHER restart your terminal or create a new terminal window/session\033[0m"
echo -e "\033[31m  - OR run one of the following commands, depending on your shell:\033[0m"
echo -e "\033[31m      source ~/.bashrc\033[0m"
echo -e "\033[31m      source ~/.bash_profile\033[0m"
echo -e "\033[31m      source ~/.zshrc\033[0m"
echo -e "\033[31m  - This applies to all of your current terminal sessions.\033[0m"
