// SPDX-License-Identifier: AGPL-3.0-or-later

pub fn format_discriminator(discriminator: &str) -> String {
    let num: u16 = discriminator.parse().unwrap_or(0);
    format!("{num:04}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn format_discriminator_pads_short() {
        assert_eq!(format_discriminator("1"), "0001");
        assert_eq!(format_discriminator("42"), "0042");
        assert_eq!(format_discriminator("0"), "0000");
    }

    #[test]
    fn format_discriminator_four_digits() {
        assert_eq!(format_discriminator("1234"), "1234");
        assert_eq!(format_discriminator("9999"), "9999");
    }

    #[test]
    fn format_discriminator_invalid_input() {
        assert_eq!(format_discriminator(""), "0000");
        assert_eq!(format_discriminator("abc"), "0000");
    }
}
