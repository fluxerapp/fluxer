// SPDX-License-Identifier: AGPL-3.0-or-later

use std::{path::PathBuf, process::Command};

fn homebrew_prefix() -> PathBuf {
    if let Ok(prefix) = std::env::var("HOMEBREW_PREFIX") {
        return PathBuf::from(prefix);
    }
    let output = Command::new("brew")
        .arg("--prefix")
        .output()
        .expect("libyuv needs a Homebrew prefix: set HOMEBREW_PREFIX or install Homebrew");
    assert!(output.status.success(), "`brew --prefix` failed");
    PathBuf::from(
        String::from_utf8(output.stdout)
            .expect("`brew --prefix` printed invalid UTF-8")
            .trim(),
    )
}

fn main() {
    println!("cargo:rerun-if-env-changed=HOMEBREW_PREFIX");
    if std::env::var("CARGO_CFG_TARGET_OS").unwrap_or_default() != "macos" {
        return;
    }
    println!(
        "cargo:rustc-link-arg=-Wl,-rpath,{}",
        homebrew_prefix().join("lib").display()
    );
}
