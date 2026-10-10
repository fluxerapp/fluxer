// SPDX-License-Identifier: AGPL-3.0-or-later

pub const SHIFT_MASK: u64 = 1 << 17;
pub const CONTROL_MASK: u64 = 1 << 18;
pub const ALTERNATE_MASK: u64 = 1 << 19;
pub const COMMAND_MASK: u64 = 1 << 20;

const DEVICE_LEFT_CONTROL_MASK: u64 = 0x0000_0001;
const DEVICE_LEFT_SHIFT_MASK: u64 = 0x0000_0002;
const DEVICE_RIGHT_SHIFT_MASK: u64 = 0x0000_0004;
const DEVICE_LEFT_COMMAND_MASK: u64 = 0x0000_0008;
const DEVICE_RIGHT_COMMAND_MASK: u64 = 0x0000_0010;
const DEVICE_LEFT_ALTERNATE_MASK: u64 = 0x0000_0020;
const DEVICE_RIGHT_ALTERNATE_MASK: u64 = 0x0000_0040;
const DEVICE_RIGHT_CONTROL_MASK: u64 = 0x0000_2000;

const LEFT_SHIFT_KEYCODE: u16 = 0x38;
const RIGHT_SHIFT_KEYCODE: u16 = 0x3c;
const LEFT_CONTROL_KEYCODE: u16 = 0x3b;
const RIGHT_CONTROL_KEYCODE: u16 = 0x3e;
const LEFT_OPTION_KEYCODE: u16 = 0x3a;
const RIGHT_OPTION_KEYCODE: u16 = 0x3d;
const LEFT_COMMAND_KEYCODE: u16 = 0x37;
const RIGHT_COMMAND_KEYCODE: u16 = 0x36;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct Modifiers {
    pub ctrl: bool,
    pub alt: bool,
    pub shift: bool,
    pub meta: bool,
}

pub fn from_flags(flags: u64) -> Modifiers {
    Modifiers {
        ctrl: (flags & CONTROL_MASK) != 0,
        alt: (flags & ALTERNATE_MASK) != 0,
        shift: (flags & SHIFT_MASK) != 0,
        meta: (flags & COMMAND_MASK) != 0,
    }
}

struct ModifierSide {
    group_mask: u64,
    side_mask: u64,
    other_side_mask: u64,
}

fn modifier_side(keycode: u16) -> Option<ModifierSide> {
    let (group_mask, side_mask, other_side_mask) = match keycode {
        LEFT_SHIFT_KEYCODE => (SHIFT_MASK, DEVICE_LEFT_SHIFT_MASK, DEVICE_RIGHT_SHIFT_MASK),
        RIGHT_SHIFT_KEYCODE => (SHIFT_MASK, DEVICE_RIGHT_SHIFT_MASK, DEVICE_LEFT_SHIFT_MASK),
        LEFT_CONTROL_KEYCODE => (
            CONTROL_MASK,
            DEVICE_LEFT_CONTROL_MASK,
            DEVICE_RIGHT_CONTROL_MASK,
        ),
        RIGHT_CONTROL_KEYCODE => (
            CONTROL_MASK,
            DEVICE_RIGHT_CONTROL_MASK,
            DEVICE_LEFT_CONTROL_MASK,
        ),
        LEFT_OPTION_KEYCODE => (
            ALTERNATE_MASK,
            DEVICE_LEFT_ALTERNATE_MASK,
            DEVICE_RIGHT_ALTERNATE_MASK,
        ),
        RIGHT_OPTION_KEYCODE => (
            ALTERNATE_MASK,
            DEVICE_RIGHT_ALTERNATE_MASK,
            DEVICE_LEFT_ALTERNATE_MASK,
        ),
        LEFT_COMMAND_KEYCODE => (
            COMMAND_MASK,
            DEVICE_LEFT_COMMAND_MASK,
            DEVICE_RIGHT_COMMAND_MASK,
        ),
        RIGHT_COMMAND_KEYCODE => (
            COMMAND_MASK,
            DEVICE_RIGHT_COMMAND_MASK,
            DEVICE_LEFT_COMMAND_MASK,
        ),
        _ => return None,
    };
    Some(ModifierSide {
        group_mask,
        side_mask,
        other_side_mask,
    })
}

pub fn modifier_key_down_from_flags(keycode: u16, flags: u64, was_held: bool) -> Option<bool> {
    let side = modifier_side(keycode)?;
    if flags & side.group_mask == 0 {
        return Some(false);
    }
    if flags & (side.side_mask | side.other_side_mask) != 0 {
        return Some(flags & side.side_mask != 0);
    }
    Some(!was_held)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn modifier_key_down_uses_the_device_side_bits() {
        let both_shifts = SHIFT_MASK | DEVICE_LEFT_SHIFT_MASK | DEVICE_RIGHT_SHIFT_MASK;
        assert_eq!(
            modifier_key_down_from_flags(LEFT_SHIFT_KEYCODE, both_shifts, false),
            Some(true)
        );
        let right_only = SHIFT_MASK | DEVICE_RIGHT_SHIFT_MASK;
        assert_eq!(
            modifier_key_down_from_flags(LEFT_SHIFT_KEYCODE, right_only, true),
            Some(false)
        );
        assert_eq!(
            modifier_key_down_from_flags(RIGHT_SHIFT_KEYCODE, right_only, false),
            Some(true)
        );
        assert_eq!(
            modifier_key_down_from_flags(RIGHT_SHIFT_KEYCODE, 0, true),
            Some(false)
        );
    }

    #[test]
    fn releasing_one_side_while_the_other_is_held_is_a_keyup_for_every_group() {
        let cases = [
            (
                LEFT_CONTROL_KEYCODE,
                RIGHT_CONTROL_KEYCODE,
                CONTROL_MASK,
                DEVICE_LEFT_CONTROL_MASK,
                DEVICE_RIGHT_CONTROL_MASK,
            ),
            (
                LEFT_OPTION_KEYCODE,
                RIGHT_OPTION_KEYCODE,
                ALTERNATE_MASK,
                DEVICE_LEFT_ALTERNATE_MASK,
                DEVICE_RIGHT_ALTERNATE_MASK,
            ),
            (
                LEFT_COMMAND_KEYCODE,
                RIGHT_COMMAND_KEYCODE,
                COMMAND_MASK,
                DEVICE_LEFT_COMMAND_MASK,
                DEVICE_RIGHT_COMMAND_MASK,
            ),
        ];
        for (left, right, group, left_bit, right_bit) in cases {
            assert_eq!(
                modifier_key_down_from_flags(left, group | left_bit, false),
                Some(true)
            );
            assert_eq!(
                modifier_key_down_from_flags(right, group | left_bit | right_bit, false),
                Some(true)
            );
            assert_eq!(
                modifier_key_down_from_flags(left, group | right_bit, true),
                Some(false)
            );
            assert_eq!(modifier_key_down_from_flags(right, 0, true), Some(false));
        }
    }

    #[test]
    fn missing_device_bits_fall_back_to_toggling_the_held_state() {
        assert_eq!(
            modifier_key_down_from_flags(RIGHT_OPTION_KEYCODE, ALTERNATE_MASK, false),
            Some(true)
        );
        assert_eq!(
            modifier_key_down_from_flags(RIGHT_OPTION_KEYCODE, ALTERNATE_MASK, true),
            Some(false)
        );
    }

    #[test]
    fn non_modifier_keycodes_are_not_decided() {
        assert_eq!(modifier_key_down_from_flags(0x39, 0, false), None);
        assert_eq!(modifier_key_down_from_flags(0x00, SHIFT_MASK, true), None);
    }
}
