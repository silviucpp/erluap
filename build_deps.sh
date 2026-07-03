#!/usr/bin/env bash

CPUS=$(getconf _NPROCESSORS_ONLN 2>/dev/null || sysctl -n hw.ncpu || nproc)
OS=$(uname -s)
DEPS_LOCATION="_build/deps"

UAP_CPP_REPO="https://github.com/ua-parser/uap-cpp.git"
UAP_CPP_BRANCH="master"
UAP_CPP_REV="bc4494ccd1a7ff474d13b5c3e3cf37a6d2c72f52"
UAP_CPP_DESTINATION="uap-cpp"

UAP_CORE_REPO="https://github.com/ua-parser/uap-core.git"
UAP_CORE_BRANCH="master"
UAP_CORE_REV="f87f3a9aea3916399d70e5cb2c2aedea5008d881"
UAP_CORE_DESTINATION="uap-core"

if [[ -f "$DEPS_LOCATION/$UAP_CPP_DESTINATION/build/libuaparser_cpp.a" && -f "$DEPS_LOCATION/$UAP_CORE_DESTINATION/regexes.yaml" ]]; then
    echo "uap-cpp and uap-core are already present. Delete them for a fresh checkout."
    exit 0
fi

fail_check() {
    "$@"
    local status=$?
    if [[ $status -ne 0 ]]; then
        echo "Error executing: $*" >&2
        exit 1
    fi
}

DownloadLibs() {
    echo "Cloning uap-cpp repo=$UAP_CPP_REPO rev=$UAP_CPP_REV branch=$UAP_CPP_BRANCH"

    mkdir -p "$DEPS_LOCATION"
    pushd "$DEPS_LOCATION" > /dev/null

    # Download and compile uap-cpp
    if [[ ! -d "$UAP_CPP_DESTINATION" ]]; then
        fail_check git clone --depth=1 -b "$UAP_CPP_BRANCH" "$UAP_CPP_REPO" "$UAP_CPP_DESTINATION"
    fi

    pushd "$UAP_CPP_DESTINATION"
    fail_check git fetch --depth=1 origin "$UAP_CPP_REV"
    fail_check git checkout "$UAP_CPP_REV"

    mkdir -p build
    pushd build

    case "$OS" in
        Darwin)
            export HOMEBREW_NO_INSTALL_UPGRADE=true
            export HOMEBREW_NO_INSTALL_CLEANUP=true
            export HOMEBREW_NO_AUTO_UPDATE=1
            brew install re2 yaml-cpp || true
            YAML_DIR=$(brew --prefix yaml-cpp)
            RE2_DIR=$(brew --prefix re2)
            fail_check cmake -DBUILD_SHARED=OFF -DBUILD_STATIC=ON -DBUILD_BENCHMARKS=OFF -DBUILD_TESTS=OFF -DCMAKE_CXX_FLAGS="-I$YAML_DIR/include -I$RE2_DIR/include" ..
            ;;
        *)
            fail_check cmake -DBUILD_SHARED=OFF -DBUILD_STATIC=ON -DBUILD_BENCHMARKS=OFF -DBUILD_TESTS=OFF ..
            ;;
    esac

    fail_check make -j "$CPUS" uap-cpp-static
    popd
    popd

    # Download uap-core

    if [[ ! -d "$UAP_CORE_DESTINATION" ]]; then
        fail_check git clone --depth=1 -b "$UAP_CORE_BRANCH" "$UAP_CORE_REPO" "$UAP_CORE_DESTINATION"
    fi

    pushd "$UAP_CORE_DESTINATION"
    fail_check git fetch --depth=1 origin "$UAP_CORE_REV"
    fail_check git checkout "$UAP_CORE_REV"
    popd

    popd
}

CopyFiles() {
    # Copy regex file
    mkdir -p priv
    pushd priv
    rm -f regexes.yaml
    cp "../$DEPS_LOCATION/$UAP_CORE_DESTINATION/regexes.yaml" regexes.yaml
    popd
}

DownloadLibs
CopyFiles
