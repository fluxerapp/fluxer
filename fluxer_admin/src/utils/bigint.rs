// SPDX-License-Identifier: AGPL-3.0-or-later

pub fn format_discriminator(discriminator: &str) -> String {
    let num: u16 = discriminator.parse().unwrap_or(0);
    format!("{num:04}")
}
