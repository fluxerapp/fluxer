// SPDX-License-Identifier: AGPL-3.0-or-later

fn main() {
    if std::env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("linux") {
        println!("cargo:rustc-link-arg=-Wl,--unresolved-symbols=ignore-all");
    }
}
