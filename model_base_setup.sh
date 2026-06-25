#!/bin/sh
# Safe versioned core setup for:
#   BFM
#   GOTM
#   GETM
#   FABM
#
# This script:
#   - mimics the old layout under $HOME/version_2
#   - never modifies anything outside $HOME/version_2
#   - only creates symlinks inside $HOME/version_2
#   - does NOT compile a specific domain setup yet

set -eu

###############################################################################
# USER SETTINGS
###############################################################################

VERSION_NAME="${VERSION_NAME:-version_6node}"
VERSION_ROOT="${HOME}/${VERSION_NAME}"

# Mimic old layout under version root
HOME_ROOT="${VERSION_ROOT}/home"
TOOLS_ROOT="${VERSION_ROOT}/tools"
LOCAL_ROOT="${VERSION_ROOT}/local"
BUILD_ROOT="${HOME_ROOT}/build"

# Source trees
BFM_PARENT="${HOME_ROOT}/BFM_SOURCES"
GOTM_PARENT="${HOME_ROOT}/GOTM_SOURCES"
GETM_PARENT="${HOME_ROOT}/GETM_SOURCES"
FABM_PARENT="${HOME_ROOT}/fabm-git"

BFM_DIR="${BFM_PARENT}/bfm_2016"
GOTM_DIR="${GOTM_PARENT}/gotm_coupled_bfm_2016"
GETM_DIR="${GETM_PARENT}/getm_coupled_bfm_2016"
FABM_DIR="${FABM_PARENT}/fabm"

# Private helper links inside the version root only
BFM_LINK="${HOME_ROOT}/bfm-git"
GOTM_LINK="${HOME_ROOT}/gotm-git"
GETM_LINK="${HOME_ROOT}/getm-git"

# Optional helper tool
BBPY_SOURCE="/export/lv1/user/jvandermolen/tools/bbpy"

# Optional local copy of getm_configure.sh if you choose to place one here later
CONTAINER_DIR="${HOME_ROOT}/GETM_ERSEM_SETUPS/Container"

# Repositories
BFM_REPO="git@github.com:jvdmolen/bfm_2016.git"
GOTM_REPO="git@github.com:jvdmolen/gotm_coupled_bfm_2016.git"
GETM_REPO="git@github.com:jvdmolen/getm_coupled_bfm_2016.git"
FABM_REPO="git@github.com:fabm-model/fabm.git"

# Branches / commits
BFM_BRANCH="bfm2016_production_20250827"
GOTM_BRANCH="master_20210107_couplingGETM_bfm2016_20241126"

GETM_BRANCH="iow_20200609_bfm2016_20250116"
GETM_COMMIT="969dceb73ca9d801c03eb7f218da1d45d5748db3"

FABM_BRANCH="master_20200610"
FABM_COMMIT="e1f1f08e42d84f8324f5114924b67ad567719334"

###############################################################################
# HELPERS
###############################################################################

say() {
    printf '%s\n' "$*"
}

die() {
    printf 'ERROR: %s\n' "$*" >&2
    exit 1
}

require_cmd() {
    command -v "$1" >/dev/null 2>&1 || die "Missing command: $1"
}

require_dir() {
    [ -d "$1" ] || die "Missing directory: $1"
}

require_file() {
    [ -f "$1" ] || die "Missing file: $1"
}

# Refuse to modify anything outside VERSION_ROOT
assert_inside_version_root() {
    case "$1" in
        "${VERSION_ROOT}"|"${VERSION_ROOT}"/*) ;;
        *)
            die "Refusing to modify path outside VERSION_ROOT: $1"
            ;;
    esac
}

safe_mkdir() {
    assert_inside_version_root "$1"
    mkdir -p "$1"
}

safe_remove() {
    target=$1
    assert_inside_version_root "$target"
    if [ -L "$target" ] || [ -e "$target" ]; then
        rm -rf "$target"
    fi
}

safe_link() {
    target=$1
    linkname=$2
    assert_inside_version_root "$linkname"

    if [ -L "$linkname" ] || [ -e "$linkname" ]; then
        rm -rf "$linkname"
    fi
    ln -s "$target" "$linkname"
}

clone_if_missing() {
    repo_url=$1
    parent_dir=$2
    repo_name=$3

    safe_mkdir "$parent_dir"
    cd "$parent_dir" || exit 1

    if [ -d "${repo_name}/.git" ]; then
        say "Repo already exists: ${parent_dir}/${repo_name}"
    else
        say "Cloning ${repo_name}..."
        git clone "$repo_url"
    fi
}

checkout_remote_branch() {
    repo_dir=$1
    branch=$2

    cd "$repo_dir" || exit 1
    git fetch --all --prune
    git checkout -B "$branch" "origin/$branch"
}

checkout_commit_branch() {
    repo_dir=$1
    branch=$2
    commit=$3

    cd "$repo_dir" || exit 1
    git fetch --all --prune
    git checkout -B "$branch" "$commit"
}

print_layout() {
    say "==========================================="
    say "Versioned isolated build layout"
    say "-------------------------------------------"
    say "VERSION_ROOT = $VERSION_ROOT"
    say "HOME_ROOT    = $HOME_ROOT"
    say "TOOLS_ROOT   = $TOOLS_ROOT"
    say "LOCAL_ROOT   = $LOCAL_ROOT"
    say "BUILD_ROOT   = $BUILD_ROOT"
    say "BFM_DIR      = $BFM_DIR"
    say "GOTM_DIR     = $GOTM_DIR"
    say "GETM_DIR     = $GETM_DIR"
    say "FABM_DIR     = $FABM_DIR"
    say "==========================================="
}

###############################################################################
# CHECKS
###############################################################################

check_requirements() {
    require_cmd git
    require_cmd ssh
    require_cmd cmake
    require_cmd make
}

check_ssh() {
    say "Checking SSH access to GitHub..."
    if ! ssh -T git@github.com 2>&1 | grep -q "successfully authenticated"; then
        die "SSH authentication to GitHub failed. Check your SSH key and ssh-agent."
    fi
    say "SSH authentication OK."
}

###############################################################################
# LAYOUT
###############################################################################

create_version_layout() {
    say "Creating versioned directory layout under ${VERSION_ROOT} ..."
    safe_mkdir "$VERSION_ROOT"
    safe_mkdir "$HOME_ROOT"
    safe_mkdir "$TOOLS_ROOT"
    safe_mkdir "$LOCAL_ROOT"
    safe_mkdir "$BUILD_ROOT"

    safe_mkdir "$BFM_PARENT"
    safe_mkdir "$GOTM_PARENT"
    safe_mkdir "$GETM_PARENT"
    safe_mkdir "$FABM_PARENT"
}

cleanup_version_build() {
    say "Cleaning only inside ${VERSION_ROOT} ..."
    safe_remove "$BFM_PARENT"
    safe_remove "$GOTM_PARENT"
    safe_remove "$GETM_PARENT"
    safe_remove "$FABM_PARENT"
    safe_remove "$BUILD_ROOT"
    safe_remove "$TOOLS_ROOT"
    safe_remove "$LOCAL_ROOT"
    safe_remove "$BFM_LINK"
    safe_remove "$GOTM_LINK"
    safe_remove "$GETM_LINK"

    # recreate root layout after cleanup
    create_version_layout
}

###############################################################################
# CLONING
###############################################################################

clone_repos() {
    create_version_layout

    clone_if_missing "$BFM_REPO"  "$BFM_PARENT"  "bfm_2016"
    clone_if_missing "$GOTM_REPO" "$GOTM_PARENT" "gotm_coupled_bfm_2016"
    clone_if_missing "$GETM_REPO" "$GETM_PARENT" "getm_coupled_bfm_2016"
    clone_if_missing "$FABM_REPO" "$FABM_PARENT" "fabm"

    say "Checking out BFM..."
    checkout_remote_branch "$BFM_DIR" "$BFM_BRANCH"

    say "Checking out GOTM..."
    checkout_remote_branch "$GOTM_DIR" "$GOTM_BRANCH"
    cd "$GOTM_DIR" || exit 1
    git submodule update --init --recursive

    say "Checking out GETM..."
    checkout_commit_branch "$GETM_DIR" "$GETM_BRANCH" "$GETM_COMMIT"

    say "Checking out FABM..."
    checkout_commit_branch "$FABM_DIR" "$FABM_BRANCH" "$FABM_COMMIT"

    say "Creating private helper links inside version root..."
    safe_link "$BFM_DIR"  "$BFM_LINK"
    safe_link "$GOTM_DIR" "$GOTM_LINK"
    safe_link "$GETM_DIR" "$GETM_LINK"

    say "Clone stage completed."
}

###############################################################################
# BUILD
###############################################################################

prepare_tools() {
    safe_mkdir "$TOOLS_ROOT"

    if [ -e "$BBPY_SOURCE" ]; then
        say "Copying bbpy into versioned tools directory..."
        cp -a "$BBPY_SOURCE" "$TOOLS_ROOT/"
    else
        say "bbpy not found at $BBPY_SOURCE, skipping."
    fi
}

compile_gotm() {
    say "Compiling GOTM..."
    require_dir "$GOTM_DIR"
    require_dir "$FABM_DIR"

    safe_remove "${BUILD_ROOT}/gotm"
    safe_mkdir "${BUILD_ROOT}/gotm"
    safe_mkdir "${LOCAL_ROOT}/gotm"

    cd "${BUILD_ROOT}/gotm" || exit 1
    cmake "$GOTM_DIR" \
        -DFABM_BASE="$FABM_DIR" \
        -DCMAKE_INSTALL_PREFIX="${LOCAL_ROOT}/gotm"

    make install
    require_file "${LOCAL_ROOT}/gotm/bin/gotm"

    say "Installed GOTM at ${LOCAL_ROOT}/gotm/bin/gotm"
}

configure_getm_toolchain() {
    say "Preparing GETM toolchain area..."
    safe_mkdir "${TOOLS_ROOT}/getm"
    safe_mkdir "${TOOLS_ROOT}/getm/build"

    if [ -f "${CONTAINER_DIR}/getm_configure.sh" ]; then
        cd "${TOOLS_ROOT}/getm/build" || exit 1
        cp "${CONTAINER_DIR}/getm_configure.sh" .
        chmod +x ./getm_configure.sh

	export GETM_BASE="${GETM_DIR}"
	export GOTM_BASE="${GOTM_DIR}"
	export FABM_BASE="${FABM_DIR}"
	export BFM_BASE="${BFM_LINK}"
	export BFMDIR="${BFM_LINK}"
	export GOTM_BUILD_DIR="${BUILD_ROOT}/gotm"
	export install_prefix="${LOCAL_ROOT}/getm"
	export CMAKE_BIN=cmake
	export COMPILATION_MODE=production

        ./getm_configure.sh
        say "GETM configure script completed."
    else
        say "No version-local getm_configure.sh found at:"
        say "  ${CONTAINER_DIR}/getm_configure.sh"
        say "Skipping GETM configure step for now."
        say "That is safe and expected unless you place a copy there inside version_2."
    fi
}

build_core_models() {
    create_version_layout
    prepare_tools
    compile_gotm
    configure_getm_toolchain
    say "Core model build completed inside ${VERSION_ROOT}"
}

###############################################################################
# MAIN
###############################################################################

say "==========================================="
say " Safe isolated BFM / GOTM / GETM / FABM setup"
say "==========================================="
say "1) Clean version_2 and clone + build"
say "2) Build only inside version_2"
say "3) Clone only inside version_2"
say "-------------------------------------------"
printf "Enter choice [1-3]: "
read ACTION
[ -z "${ACTION:-}" ] && ACTION=0

check_requirements
print_layout

case "$ACTION" in
    1)
        check_ssh
        cleanup_version_build
        clone_repos
        build_core_models
        ;;
    2)
        build_core_models
        ;;
    3)
        check_ssh
        clone_repos
        ;;
    *)
        die "Invalid choice: $ACTION"
        ;;
esac
