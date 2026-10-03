// SPDX-License-Identifier: AGPL-3.0-or-later

pub fn browser_button(x11_button: u32) -> Option<u8> {
    match x11_button {
        1 => Some(0),
        2 => Some(1),
        3 => Some(2),
        8 => Some(3),
        9 => Some(4),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn primary_buttons_map_to_browser_indices() {
        assert_eq!(browser_button(1), Some(0));
        assert_eq!(browser_button(2), Some(1));
        assert_eq!(browser_button(3), Some(2));
    }

    #[test]
    fn back_forward_buttons_map_to_3_and_4() {
        assert_eq!(browser_button(8), Some(3));
        assert_eq!(browser_button(9), Some(4));
    }

    #[test]
    fn wheel_buttons_are_ignored() {
        for button in 4..=7 {
            assert_eq!(browser_button(button), None);
        }
    }

    #[test]
    fn unknown_buttons_are_ignored() {
        assert_eq!(browser_button(0), None);
        assert_eq!(browser_button(15), None);
        assert_eq!(browser_button(255), None);
    }
}
