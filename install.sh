#!/bin/bash

set -e

colors=(51 45 39 33 27 21)

echo -e "\033[38;5;${colors[0]}m   ____           _    \033[0m"
echo -e "\033[38;5;${colors[1]}m  / ___|__ _ _ __| |_  \033[0m"
echo -e "\033[38;5;${colors[2]}m | |   / _\` | '__| __| \033[0m"
echo -e "\033[38;5;${colors[3]}m | |__| (_| | |  | |_  \033[0m"
echo -e "\033[38;5;${colors[4]}m  \\____\\__,_|_|   \\__| \033[0m"
echo -e "\033[38;5;${colors[5]}m                      \033[0m"

BIN_DIR="/usr/local/bin"
LIB_DIR="/usr/local/lib/cartlang"

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

  if [[ -f "$BIN_DIR/cart" ]]; then
    echo "- Compiler at $BIN_DIR/cart"
    EXISTING_VERSION=$("$BIN_DIR/cart" -v 2>/dev/null || echo "unknown")
    echo "  Existing version: $EXISTING_VERSION"
  fi

  if [[ -f "$LIB_DIR/libcartstd.a" ]]; then
    echo "- Standard library at $LIB_DIR/libcartstd.a"
  fi

  LATEST_VERSION=$(curl -s https://api.github.com/repos/$REPO/releases/latest | grep '"tag_name":' | sed -E 's/.*"tag_name": "([^"]+)".*/\1/')

  echo "  Latest version: $LATEST_VERSION"

  if [[ "$EXISTING_VERSION" == "$LATEST_VERSION" ]]; then
    echo "The existing version is up to date. This will overwrite the installation."
  elif [[ "$EXISTING_VERSION" == "unknown" ]]; then
    echo "The existing version is unknown. This will overwrite the installation."
  else
    echo "This will overwrite and update the existing version."
  fi

  read -p "Do you want to continue? (y/n): " CONFIRM
  if [[ "$CONFIRM" != "y" && "$CONFIRM" != "Y" && "$CONFIRM" != "yes" && "$CONFIRM" != "YES" ]]; then
    echo "Installation canceled."
    exit 0
  fi
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

echo "Installation complete!"
echo "Compiler installed to $BIN_DIR/cart"
echo "Standard library installed to $LIB_DIR/libcartstd.a"
