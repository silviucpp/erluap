#!/usr/bin/env bash

DEPS_LOCATION=deps

UAP_CPP_REPO=https://github.com/ua-parser/uap-cpp.git
UAP_CPP_BRANCH=master
UAP_CPP_REV=652edf2f5e285363d0a4391df922e91660be711c
UAP_CPP_DESTINATION=uap-cpp

UAP_CORE_REPO=https://github.com/ua-parser/uap-core.git
UAP_CORE_BRANCH=master
UAP_CORE_REV=fc570f378e41063bad3bdf0532967743efc75b4b
UAP_CORE_DESTINATION=uap-core

if [ -f "$DEPS_LOCATION/$UAP_CPP_DESTINATION/UaParser.cpp" ] && [ -f "$DEPS_LOCATION/$UAP_CORE_DESTINATION/regexes.yaml" ]; then
    echo "uap-cpp and uap-core fork already exist. delete them for a fresh checkout."
    exit 0
fi

function fail_check
{
    "$@"
    local status=$?
    if [ $status -ne 0 ]; then
        echo "error with $1" >&2
        exit 1
    fi
}

function DownloadLibs()
{
    echo "uap-cpp repo=$UAP_CPP_REPO rev=$UAP_CPP_REV branch=$UAP_CPP_BRANCH"

    mkdir -p $DEPS_LOCATION
    pushd $DEPS_LOCATION

    #download uap-cpp

    if [ ! -d "$UAP_CPP_DESTINATION" ]; then
        fail_check git clone -b $UAP_CPP_BRANCH $UAP_CPP_REPO $UAP_CPP_DESTINATION
    fi

    pushd $UAP_CPP_DESTINATION
    fail_check git checkout $UAP_CPP_REV
    popd

    #download uap-core

    if [ ! -d "$UAP_CORE_DESTINATION" ]; then
        fail_check git clone -b $UAP_CORE_BRANCH $UAP_CORE_REPO $UAP_CORE_DESTINATION
    fi

    pushd $UAP_CORE_DESTINATION
    fail_check git checkout $UAP_CORE_REV
    popd

    popd
}

function CopyFiles()
{
    #links to ua-cpp files

    pushd c_src
    rm UaParser.*
    ln -s ../$DEPS_LOCATION/$UAP_CPP_DESTINATION/UaParser.cpp UaParser.cc
    ln -s ../$DEPS_LOCATION/$UAP_CPP_DESTINATION/UaParser.h UaParser.h
    popd

    #copy regex file

    mkdir -p priv
    pushd priv
    rm regexes.yaml
    cp ../$DEPS_LOCATION/$UAP_CORE_DESTINATION/regexes.yaml regexes.yaml
    popd
}

DownloadLibs
CopyFiles
