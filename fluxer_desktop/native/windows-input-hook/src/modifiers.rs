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
