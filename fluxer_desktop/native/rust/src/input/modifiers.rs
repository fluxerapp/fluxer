// SPDX-License-Identifier: AGPL-3.0-or-later

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Modifiers {
    pub ctrl: bool,
    pub alt: bool,
    pub shift: bool,
    pub meta: bool,
}

pub mod linux {
    use super::Modifiers;

    pub const SHIFT_MASK: u32 = 1 << 0;
    pub const CONTROL_MASK: u32 = 1 << 2;
    pub const MOD1_MASK: u32 = 1 << 3;
    pub const MOD4_MASK: u32 = 1 << 6;

    pub fn from_state(state: u32) -> Modifiers {
        Modifiers {
            ctrl: (state & CONTROL_MASK) != 0,
            alt: (state & MOD1_MASK) != 0,
            shift: (state & SHIFT_MASK) != 0,
            meta: (state & MOD4_MASK) != 0,
        }
    }
}

pub mod macos {
    use super::Modifiers;

    pub const SHIFT_MASK: u64 = 1 << 17;
    pub const CONTROL_MASK: u64 = 1 << 18;
    pub const ALTERNATE_MASK: u64 = 1 << 19;
    pub const COMMAND_MASK: u64 = 1 << 20;

    pub fn from_flags(flags: u64) -> Modifiers {
        Modifiers {
            ctrl: (flags & CONTROL_MASK) != 0,
            alt: (flags & ALTERNATE_MASK) != 0,
            shift: (flags & SHIFT_MASK) != 0,
            meta: (flags & COMMAND_MASK) != 0,
        }
    }
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
    fn linux_no_bits_all_modifiers_false() {
        assert_eq!(Modifiers::default(), linux::from_state(0));
    }

    #[test]
    fn linux_shift_mask_alone_sets_only_shift() {
        assert_eq!(
            Modifiers {
                shift: true,
                ..Modifiers::default()
            },
            linux::from_state(linux::SHIFT_MASK)
        );
    }

    #[test]
    fn linux_ctrl_alt_shift_meta_combo_all_true() {
        assert_eq!(
            Modifiers {
                ctrl: true,
                alt: true,
                shift: true,
                meta: true,
            },
            linux::from_state(
                linux::SHIFT_MASK | linux::CONTROL_MASK | linux::MOD1_MASK | linux::MOD4_MASK
            )
        );
    }

    #[test]
    fn linux_lockmask_and_numlock_are_ignored() {
        assert_eq!(Modifiers::default(), linux::from_state((1 << 1) | (1 << 4)));
    }

    #[test]
    fn linux_mod1_mapped_to_alt_mod4_mapped_to_meta() {
        let a = linux::from_state(linux::MOD1_MASK);
        assert!(a.alt && !a.meta);
        let b = linux::from_state(linux::MOD4_MASK);
        assert!(b.meta && !b.alt);
    }

    #[test]
    fn macos_no_flags_all_false() {
        assert_eq!(Modifiers::default(), macos::from_flags(0));
    }

    #[test]
    fn macos_command_alone_sets_only_meta() {
        assert_eq!(
            Modifiers {
                meta: true,
                ..Modifiers::default()
            },
            macos::from_flags(macos::COMMAND_MASK)
        );
    }

    #[test]
    fn macos_option_alone_sets_only_alt() {
        assert_eq!(
            Modifiers {
                alt: true,
                ..Modifiers::default()
            },
            macos::from_flags(macos::ALTERNATE_MASK)
        );
    }

    #[test]
    fn macos_cmd_shift_combo() {
        assert_eq!(
            Modifiers {
                shift: true,
                meta: true,
                ..Modifiers::default()
            },
            macos::from_flags(macos::COMMAND_MASK | macos::SHIFT_MASK)
        );
    }

    #[test]
    fn macos_all_four_modifiers_together() {
        assert_eq!(
            Modifiers {
                ctrl: true,
                alt: true,
                shift: true,
                meta: true,
            },
            macos::from_flags(
                macos::SHIFT_MASK
                    | macos::CONTROL_MASK
                    | macos::ALTERNATE_MASK
                    | macos::COMMAND_MASK
            )
        );
    }

    #[test]
    fn macos_unrelated_high_bits_ignored() {
        assert_eq!(Modifiers::default(), macos::from_flags(0xff << 32));
    }

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
