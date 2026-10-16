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
