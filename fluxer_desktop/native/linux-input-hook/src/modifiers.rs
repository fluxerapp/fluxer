// SPDX-License-Identifier: AGPL-3.0-or-later

pub const SHIFT_MASK: u32 = 1 << 0;
pub const CONTROL_MASK: u32 = 1 << 2;
pub const MOD1_MASK: u32 = 1 << 3;
pub const MOD4_MASK: u32 = 1 << 6;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Modifiers {
    pub ctrl: bool,
    pub alt: bool,
    pub shift: bool,
    pub meta: bool,
}

pub fn from_state(state: u32) -> Modifiers {
    Modifiers {
        ctrl: (state & CONTROL_MASK) != 0,
        alt: (state & MOD1_MASK) != 0,
        shift: (state & SHIFT_MASK) != 0,
        meta: (state & MOD4_MASK) != 0,
    }
}
