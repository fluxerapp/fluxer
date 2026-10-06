// SPDX-License-Identifier: AGPL-3.0-or-later

#[repr(u32)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CgEventType {
    LeftMouseDown = 1,
    LeftMouseUp = 2,
    RightMouseDown = 3,
    RightMouseUp = 4,
    KeyDown = 10,
    KeyUp = 11,
    FlagsChanged = 12,
    OtherMouseDown = 25,
    OtherMouseUp = 26,
}

impl CgEventType {
    pub fn from_u32(value: u32) -> Option<Self> {
        Some(match value {
            1 => Self::LeftMouseDown,
            2 => Self::LeftMouseUp,
            3 => Self::RightMouseDown,
            4 => Self::RightMouseUp,
            10 => Self::KeyDown,
            11 => Self::KeyUp,
            12 => Self::FlagsChanged,
            25 => Self::OtherMouseDown,
            26 => Self::OtherMouseUp,
            _ => return None,
        })
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Classification {
    Button(u8),
    Ignored,
}

pub fn classify(event_type: CgEventType, other_button: u32) -> Classification {
    match event_type {
        CgEventType::LeftMouseDown | CgEventType::LeftMouseUp => Classification::Button(0),
        CgEventType::RightMouseDown | CgEventType::RightMouseUp => Classification::Button(2),
        CgEventType::OtherMouseDown | CgEventType::OtherMouseUp => match other_button {
            2 => Classification::Button(1),
            3 => Classification::Button(3),
            4 => Classification::Button(4),
            _ => Classification::Ignored,
        },
        _ => Classification::Ignored,
    }
}

pub fn is_down(event_type: CgEventType) -> bool {
    matches!(
        event_type,
        CgEventType::LeftMouseDown | CgEventType::RightMouseDown | CgEventType::OtherMouseDown
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn left_and_right_buttons_map_to_0_and_2() {
        assert_eq!(
            Classification::Button(0),
            classify(CgEventType::LeftMouseDown, 0)
        );
        assert_eq!(
            Classification::Button(2),
            classify(CgEventType::RightMouseUp, 0)
        );
    }

    #[test]
    fn middle_button_other_2_maps_to_1() {
        assert_eq!(
            Classification::Button(1),
            classify(CgEventType::OtherMouseDown, 2)
        );
    }

    #[test]
    fn back_forward_other_3_4_map_to_3_and_4() {
        assert_eq!(
            Classification::Button(3),
            classify(CgEventType::OtherMouseDown, 3)
        );
        assert_eq!(
            Classification::Button(4),
            classify(CgEventType::OtherMouseUp, 4)
        );
    }

    #[test]
    fn unknown_other_button_is_ignored() {
        assert_eq!(
            Classification::Ignored,
            classify(CgEventType::OtherMouseDown, 99)
        );
    }

    #[test]
    fn is_down_distinguishes_press_from_release() {
        assert!(is_down(CgEventType::LeftMouseDown));
        assert!(!is_down(CgEventType::LeftMouseUp));
        assert!(is_down(CgEventType::OtherMouseDown));
        assert!(!is_down(CgEventType::KeyDown));
    }

    #[test]
    fn from_u32_maps_known_event_types() {
        assert_eq!(Some(CgEventType::LeftMouseDown), CgEventType::from_u32(1));
        assert_eq!(Some(CgEventType::OtherMouseUp), CgEventType::from_u32(26));
        assert_eq!(None, CgEventType::from_u32(5));
        assert_eq!(None, CgEventType::from_u32(22));
        assert_eq!(None, CgEventType::from_u32(999));
    }
}
