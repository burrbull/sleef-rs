set -euxo pipefail

main() {
    #if ! hash cross >/dev/null 2>&1; then
    #    cargo install cross
    #fi

    rustup target add x86_64-unknown-linux-musl

    rustup target add $TARGET
}

main
