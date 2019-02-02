#!/usr/bin/env bash

set -ex

main() {
    # quick check
    cargo check

    # check that we can source import libm into compiler-builtins
    #cargo check --package cb

    # generate tests
    cargo run -p input-generator --target x86_64-unknown-linux-musl
    cargo run -p musl-generator --target x86_64-unknown-linux-musl
    #cargo run -p newlib-generator

    # run unit tests
    cargo test --lib --features checked --target $TARGET --release

    # run generated tests
    cargo test --tests --features checked --target $TARGET --release
}

main
