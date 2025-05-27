#!/bin/sh
set -e

# Arguments
GEN_DIR="$1"
BRANCH="$2"
TAG_PREFIX="$3"
VERSION="$4"

# Debug output
echo "Current working directory: $(pwd)"
echo "SSH_PRIVATE_KEY path: $SSH_PRIVATE_KEY"
echo "GIT_REPOSITORY: $GIT_REPOSITORY"

# SSH setup
if [ ! -f "$SSH_PRIVATE_KEY" ]; then
  echo "SSH_PRIVATE_KEY file not found at: $SSH_PRIVATE_KEY"
  exit 1
fi

echo "Reading SSH key from file: $SSH_PRIVATE_KEY"
eval $(ssh-agent -s)
SSH_DIR="$HOME/.ssh"
mkdir -p "$SSH_DIR"
chmod 700 "$SSH_DIR"
base64 -d "$SSH_PRIVATE_KEY" > "$SSH_DIR/id_rsa"
chmod 600 "$SSH_DIR/id_rsa"
ssh-add "$SSH_DIR/id_rsa" 2>&1 || { echo "Failed to add SSH key"; exit 1; }
ssh-add -l || echo "No keys found in ssh-agent"
ssh-keyscan gitlab.com >> "$SSH_DIR/known_hosts"
chmod 644 "$SSH_DIR/known_hosts"
git remote set-url origin "${GIT_REPOSITORY}"

# Git push logic
git checkout "$BRANCH" || git checkout -b "$BRANCH"
mkdir -p temp_backup
cp -rv "$GEN_DIR"/* temp_backup/
find . -mindepth 1 -maxdepth 1 ! -name '.git' ! -name 'temp_backup' -exec rm -rf {} +
mv temp_backup/* .
rm -rf temp_backup
git add .
git commit -m "Update generated ${TAG_PREFIX} code from OpenAPI spec ${TAG_PREFIX}-${VERSION}" || echo "No changes to commit"
git tag -a "${TAG_PREFIX}-${VERSION}" -m "Release ${TAG_PREFIX}-${VERSION}" || echo "Tag already exists"
git push origin "$BRANCH" || echo "Branch is up to date"
git push origin "${TAG_PREFIX}-${VERSION}" || echo "Tag already pushed"
