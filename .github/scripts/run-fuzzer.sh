#!/bin/bash

FUZZER_DIR=${1}
OTP_DIR=${2}


set -euxo pipefail

# Install Rust non-interactively
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain nightly
PATH=$HOME/.cargo/bin:$PATH
sudo apt-get install parallel

mkdir "../${FUZZER_DIR}"
cd "../${FUZZER_DIR}"
git clone https://github.com/WhatsApp/erlfuzz.git
cd erlfuzz

# Be permissive when building erlfuzz - we don't need it to we warning-free for
# erlfuzz to be useful
RUSTFLAGS=-Awarnings $HOME/.cargo/bin/cargo build --release

mkdir -p out
mkdir -p interesting
mkdir -p ../../minimized

#N=100000
N=5 # TODO Remove after testing

echo "Fuzzing erl"
echo "Generating ${N} test cases"

seq ${N} | parallel --line-buffer "./target/release/erlfuzz fuzz-and-reduce -c ./run_erl_once.sh --tmp-directory out --interesting-directory interesting --minimized-directory ../../minimized test{}"

pwd
ls -laH
ls -laH ../
ls -laH ../../

echo "Fuzzing complete"
