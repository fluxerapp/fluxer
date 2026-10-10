// SPDX-License-Identifier: AGPL-3.0-or-later

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Modifiers {
    pub ctrl: bool,
    pub alt: bool,
    pub shift: bool,
    pub meta: bool,
}

pub mod windows {
    use super::Modifiers;

    pub const HIGH_BIT: u16 = 0x8000;

    pub fn from_sampled(
        shift_state: u16,
        ctrl_state: u16,
        alt_state: u16,
        lwin_state: u16,
        rwin_state: u16,
    ) -> Modifiers {
        Modifiers {
            shift: (shift_state & HIGH_BIT) != 0,
            ctrl: (ctrl_state & HIGH_BIT) != 0,
            alt: (alt_state & HIGH_BIT) != 0,
            meta: ((lwin_state | rwin_state) & HIGH_BIT) != 0,
        }
    }

    pub fn with_pressed_key(modifiers: Modifiers, vk: u16) -> Modifiers {
        match vk {
            0x10 | 0xa0 | 0xa1 => Modifiers {
                shift: true,
                ..modifiers
            },
            0x11 | 0xa2 | 0xa3 => Modifiers {
                ctrl: true,
                ..modifiers
            },
            0x12 | 0xa4 | 0xa5 => Modifiers {
                alt: true,
                ..modifiers
            },
            0x5b | 0x5c => Modifiers {
                meta: true,
                ..modifiers
            },
            _ => modifiers,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn windows_no_high_bits_all_modifiers_false() {
        assert_eq!(Modifiers::default(), windows::from_sampled(0, 0, 0, 0, 0));
    }

    #[test]
    fn windows_low_bit_only_state_ignored() {
        assert_eq!(Modifiers::default(), windows::from_sampled(1, 1, 1, 1, 0));
    }

    #[test]
    fn windows_shift_only() {
        assert_eq!(
            Modifiers {
                shift: true,
                ..Modifiers::default()
            },
            windows::from_sampled(windows::HIGH_BIT, 0, 0, 0, 0)
        );
    }

    #[test]
    fn windows_pressed_modifier_adds_its_own_flag() {
        let ctrl = Modifiers {
            ctrl: true,
            ..Modifiers::default()
        };
        assert_eq!(
            Modifiers {
                ctrl: true,
                shift: true,
                ..Modifiers::default()
            },
            windows::with_pressed_key(ctrl, 0xa0)
        );
        assert_eq!(
            Modifiers {
                shift: true,
                ..Modifiers::default()
            },
            windows::with_pressed_key(Modifiers::default(), 0xa1)
        );
        assert_eq!(ctrl, windows::with_pressed_key(Modifiers::default(), 0xa3));
        assert_eq!(
            Modifiers {
                alt: true,
                ..Modifiers::default()
            },
            windows::with_pressed_key(Modifiers::default(), 0xa5)
        );
        assert_eq!(
            Modifiers {
                meta: true,
                ..Modifiers::default()
            },
            windows::with_pressed_key(Modifiers::default(), 0x5c)
        );
        assert_eq!(
            Modifiers {
                ctrl: true,
                alt: true,
                shift: true,
                meta: false,
            },
            windows::with_pressed_key(
                windows::with_pressed_key(
                    windows::with_pressed_key(Modifiers::default(), 0x10),
                    0x11
                ),
                0x12
            )
        );
    }

    #[test]
    fn windows_pressed_non_modifier_keeps_sampled_flags() {
        let ctrl = Modifiers {
            ctrl: true,
            ..Modifiers::default()
        };
        assert_eq!(ctrl, windows::with_pressed_key(ctrl, 0x41));
        assert_eq!(
            Modifiers::default(),
            windows::with_pressed_key(Modifiers::default(), 0x14)
        );
    }

    #[test]
    fn windows_either_win_key_sets_meta() {
        assert!(windows::from_sampled(0, 0, 0, windows::HIGH_BIT, 0).meta);
        assert!(windows::from_sampled(0, 0, 0, 0, windows::HIGH_BIT).meta);
    }

    #[test]
    fn windows_all_four_modifiers_held() {
        let m = windows::from_sampled(
            windows::HIGH_BIT,
            windows::HIGH_BIT,
            windows::HIGH_BIT,
            windows::HIGH_BIT,
            0,
        );
        assert!(m.ctrl && m.alt && m.shift && m.meta);
    }
}
