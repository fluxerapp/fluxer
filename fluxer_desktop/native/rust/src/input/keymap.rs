// SPDX-License-Identifier: AGPL-3.0-or-later

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct KeyMapU16 {
    pub code: u16,
    pub name: &'static str,
}

pub mod windows {
    use super::KeyMapU16;

    pub const VK_TABLE: &[KeyMapU16] = &[
        KeyMapU16 {
            code: 0x1b,
            name: "Escape",
        },
        KeyMapU16 {
            code: 0x70,
            name: "F1",
        },
        KeyMapU16 {
            code: 0x71,
            name: "F2",
        },
        KeyMapU16 {
            code: 0x72,
            name: "F3",
        },
        KeyMapU16 {
            code: 0x73,
            name: "F4",
        },
        KeyMapU16 {
            code: 0x74,
            name: "F5",
        },
        KeyMapU16 {
            code: 0x75,
            name: "F6",
        },
        KeyMapU16 {
            code: 0x76,
            name: "F7",
        },
        KeyMapU16 {
            code: 0x77,
            name: "F8",
        },
        KeyMapU16 {
            code: 0x78,
            name: "F9",
        },
        KeyMapU16 {
            code: 0x79,
            name: "F10",
        },
        KeyMapU16 {
            code: 0x7a,
            name: "F11",
        },
        KeyMapU16 {
            code: 0x7b,
            name: "F12",
        },
        KeyMapU16 {
            code: 0x7c,
            name: "F13",
        },
        KeyMapU16 {
            code: 0x7d,
            name: "F14",
        },
        KeyMapU16 {
            code: 0x7e,
            name: "F15",
        },
        KeyMapU16 {
            code: 0x7f,
            name: "F16",
        },
        KeyMapU16 {
            code: 0x80,
            name: "F17",
        },
        KeyMapU16 {
            code: 0x81,
            name: "F18",
        },
        KeyMapU16 {
            code: 0x82,
            name: "F19",
        },
        KeyMapU16 {
            code: 0x83,
            name: "F20",
        },
        KeyMapU16 {
            code: 0x84,
            name: "F21",
        },
        KeyMapU16 {
            code: 0x85,
            name: "F22",
        },
        KeyMapU16 {
            code: 0x86,
            name: "F23",
        },
        KeyMapU16 {
            code: 0x87,
            name: "F24",
        },
        KeyMapU16 {
            code: 0x13,
            name: "Pause",
        },
        KeyMapU16 {
            code: 0x2c,
            name: "PrintScreen",
        },
        KeyMapU16 {
            code: 0x91,
            name: "ScrollLock",
        },
        KeyMapU16 {
            code: 0x90,
            name: "NumLock",
        },
        KeyMapU16 {
            code: 0x5d,
            name: "ContextMenu",
        },
        KeyMapU16 {
            code: 0xc0,
            name: "Backquote",
        },
        KeyMapU16 {
            code: 0x31,
            name: "1",
        },
        KeyMapU16 {
            code: 0x32,
            name: "2",
        },
        KeyMapU16 {
            code: 0x33,
            name: "3",
        },
        KeyMapU16 {
            code: 0x34,
            name: "4",
        },
        KeyMapU16 {
            code: 0x35,
            name: "5",
        },
        KeyMapU16 {
            code: 0x36,
            name: "6",
        },
        KeyMapU16 {
            code: 0x37,
            name: "7",
        },
        KeyMapU16 {
            code: 0x38,
            name: "8",
        },
        KeyMapU16 {
            code: 0x39,
            name: "9",
        },
        KeyMapU16 {
            code: 0x30,
            name: "0",
        },
        KeyMapU16 {
            code: 0xbd,
            name: "Minus",
        },
        KeyMapU16 {
            code: 0xbb,
            name: "Equal",
        },
        KeyMapU16 {
            code: 0x08,
            name: "Backspace",
        },
        KeyMapU16 {
            code: 0x09,
            name: "Tab",
        },
        KeyMapU16 {
            code: 0x51,
            name: "Q",
        },
        KeyMapU16 {
            code: 0x57,
            name: "W",
        },
        KeyMapU16 {
            code: 0x45,
            name: "E",
        },
        KeyMapU16 {
            code: 0x52,
            name: "R",
        },
        KeyMapU16 {
            code: 0x54,
            name: "T",
        },
        KeyMapU16 {
            code: 0x59,
            name: "Y",
        },
        KeyMapU16 {
            code: 0x55,
            name: "U",
        },
        KeyMapU16 {
            code: 0x49,
            name: "I",
        },
        KeyMapU16 {
            code: 0x4f,
            name: "O",
        },
        KeyMapU16 {
            code: 0x50,
            name: "P",
        },
        KeyMapU16 {
            code: 0xdb,
            name: "BracketLeft",
        },
        KeyMapU16 {
            code: 0xdd,
            name: "BracketRight",
        },
        KeyMapU16 {
            code: 0xdc,
            name: "Backslash",
        },
        KeyMapU16 {
            code: 0x14,
            name: "CapsLock",
        },
        KeyMapU16 {
            code: 0x41,
            name: "A",
        },
        KeyMapU16 {
            code: 0x53,
            name: "S",
        },
        KeyMapU16 {
            code: 0x44,
            name: "D",
        },
        KeyMapU16 {
            code: 0x46,
            name: "F",
        },
        KeyMapU16 {
            code: 0x47,
            name: "G",
        },
        KeyMapU16 {
            code: 0x48,
            name: "H",
        },
        KeyMapU16 {
            code: 0x4a,
            name: "J",
        },
        KeyMapU16 {
            code: 0x4b,
            name: "K",
        },
        KeyMapU16 {
            code: 0x4c,
            name: "L",
        },
        KeyMapU16 {
            code: 0xba,
            name: "Semicolon",
        },
        KeyMapU16 {
            code: 0xde,
            name: "Quote",
        },
        KeyMapU16 {
            code: 0x0d,
            name: "Enter",
        },
        KeyMapU16 {
            code: 0xa0,
            name: "ShiftLeft",
        },
        KeyMapU16 {
            code: 0x5a,
            name: "Z",
        },
        KeyMapU16 {
            code: 0x58,
            name: "X",
        },
        KeyMapU16 {
            code: 0x43,
            name: "C",
        },
        KeyMapU16 {
            code: 0x56,
            name: "V",
        },
        KeyMapU16 {
            code: 0x42,
            name: "B",
        },
        KeyMapU16 {
            code: 0x4e,
            name: "N",
        },
        KeyMapU16 {
            code: 0x4d,
            name: "M",
        },
        KeyMapU16 {
            code: 0xbc,
            name: "Comma",
        },
        KeyMapU16 {
            code: 0xbe,
            name: "Period",
        },
        KeyMapU16 {
            code: 0xbf,
            name: "Slash",
        },
        KeyMapU16 {
            code: 0xa1,
            name: "ShiftRight",
        },
        KeyMapU16 {
            code: 0xa2,
            name: "ControlLeft",
        },
        KeyMapU16 {
            code: 0x5b,
            name: "MetaLeft",
        },
        KeyMapU16 {
            code: 0xa4,
            name: "AltLeft",
        },
        KeyMapU16 {
            code: 0x20,
            name: "Space",
        },
        KeyMapU16 {
            code: 0xa5,
            name: "AltRight",
        },
        KeyMapU16 {
            code: 0x5c,
            name: "MetaRight",
        },
        KeyMapU16 {
            code: 0xa3,
            name: "ControlRight",
        },
        KeyMapU16 {
            code: 0x60,
            name: "Numpad0",
        },
        KeyMapU16 {
            code: 0x61,
            name: "Numpad1",
        },
        KeyMapU16 {
            code: 0x62,
            name: "Numpad2",
        },
        KeyMapU16 {
            code: 0x63,
            name: "Numpad3",
        },
        KeyMapU16 {
            code: 0x64,
            name: "Numpad4",
        },
        KeyMapU16 {
            code: 0x65,
            name: "Numpad5",
        },
        KeyMapU16 {
            code: 0x66,
            name: "Numpad6",
        },
        KeyMapU16 {
            code: 0x67,
            name: "Numpad7",
        },
        KeyMapU16 {
            code: 0x68,
            name: "Numpad8",
        },
        KeyMapU16 {
            code: 0x69,
            name: "Numpad9",
        },
        KeyMapU16 {
            code: 0x6a,
            name: "NumpadMultiply",
        },
        KeyMapU16 {
            code: 0x6b,
            name: "NumpadAdd",
        },
        KeyMapU16 {
            code: 0x6c,
            name: "NumpadComma",
        },
        KeyMapU16 {
            code: 0x6d,
            name: "NumpadSubtract",
        },
        KeyMapU16 {
            code: 0x6e,
            name: "NumpadDecimal",
        },
        KeyMapU16 {
            code: 0x6f,
            name: "NumpadDivide",
        },
        KeyMapU16 {
            code: 0x92,
            name: "NumpadEqual",
        },
        KeyMapU16 {
            code: 0x0c,
            name: "Numpad5",
        },
        KeyMapU16 {
            code: 0x25,
            name: "ArrowLeft",
        },
        KeyMapU16 {
            code: 0x26,
            name: "ArrowUp",
        },
        KeyMapU16 {
            code: 0x27,
            name: "ArrowRight",
        },
        KeyMapU16 {
            code: 0x28,
            name: "ArrowDown",
        },
        KeyMapU16 {
            code: 0x2d,
            name: "Insert",
        },
        KeyMapU16 {
            code: 0x2e,
            name: "Delete",
        },
        KeyMapU16 {
            code: 0x24,
            name: "Home",
        },
        KeyMapU16 {
            code: 0x23,
            name: "End",
        },
        KeyMapU16 {
            code: 0x21,
            name: "PageUp",
        },
        KeyMapU16 {
            code: 0x22,
            name: "PageDown",
        },
        KeyMapU16 {
            code: 0xad,
            name: "AudioVolumeMute",
        },
        KeyMapU16 {
            code: 0xae,
            name: "AudioVolumeDown",
        },
        KeyMapU16 {
            code: 0xaf,
            name: "AudioVolumeUp",
        },
        KeyMapU16 {
            code: 0xb0,
            name: "MediaTrackNext",
        },
        KeyMapU16 {
            code: 0xb1,
            name: "MediaTrackPrevious",
        },
        KeyMapU16 {
            code: 0xb2,
            name: "MediaStop",
        },
        KeyMapU16 {
            code: 0xb3,
            name: "MediaPlayPause",
        },
        KeyMapU16 {
            code: 0xa6,
            name: "BrowserBack",
        },
        KeyMapU16 {
            code: 0xa7,
            name: "BrowserForward",
        },
        KeyMapU16 {
            code: 0xa8,
            name: "BrowserRefresh",
        },
        KeyMapU16 {
            code: 0xa9,
            name: "BrowserStop",
        },
        KeyMapU16 {
            code: 0xaa,
            name: "BrowserSearch",
        },
        KeyMapU16 {
            code: 0xab,
            name: "BrowserFavorites",
        },
        KeyMapU16 {
            code: 0xac,
            name: "BrowserHome",
        },
        KeyMapU16 {
            code: 0xb4,
            name: "LaunchMail",
        },
        KeyMapU16 {
            code: 0xb5,
            name: "LaunchMediaPlayer",
        },
        KeyMapU16 {
            code: 0xb6,
            name: "LaunchApp1",
        },
        KeyMapU16 {
            code: 0xb7,
            name: "LaunchApp2",
        },
        KeyMapU16 {
            code: 0x1c,
            name: "Convert",
        },
        KeyMapU16 {
            code: 0x1d,
            name: "NonConvert",
        },
        KeyMapU16 {
            code: 0x15,
            name: "KanaMode",
        },
        KeyMapU16 {
            code: 0x5f,
            name: "Sleep",
        },
    ];

    pub fn vk_to_name(vk: u16) -> Option<&'static str> {
        VK_TABLE
            .iter()
            .find(|entry| entry.code == vk)
            .map(|entry| entry.name)
    }
}

pub fn fallback_name(prefix_value: impl std::fmt::Display) -> String {
    format!("Key{prefix_value}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn windows_ascii_letters_use_vk_mapping() {
        assert_eq!(Some("A"), windows::vk_to_name(0x41));
        assert_eq!(Some("Z"), windows::vk_to_name(0x5a));
        assert_eq!(Some("M"), windows::vk_to_name(0x4d));
    }

    #[test]
    fn windows_modifiers_map_to_side_distinguished_names() {
        assert_eq!(Some("ShiftLeft"), windows::vk_to_name(0xa0));
        assert_eq!(Some("ShiftRight"), windows::vk_to_name(0xa1));
        assert_eq!(Some("ControlLeft"), windows::vk_to_name(0xa2));
        assert_eq!(Some("MetaLeft"), windows::vk_to_name(0x5b));
        assert_eq!(Some("AltLeft"), windows::vk_to_name(0xa4));
    }

    #[test]
    fn windows_function_keys_cover_f1_to_f12() {
        assert_eq!(Some("F1"), windows::vk_to_name(0x70));
        assert_eq!(Some("F12"), windows::vk_to_name(0x7b));
        assert_eq!(Some("F13"), windows::vk_to_name(0x7c));
        assert_eq!(Some("Pause"), windows::vk_to_name(0x13));
    }

    #[test]
    fn windows_special_numpad_and_media_keys_map() {
        assert_eq!(Some("PrintScreen"), windows::vk_to_name(0x2c));
        assert_eq!(Some("Numpad0"), windows::vk_to_name(0x60));
        assert_eq!(Some("NumpadDivide"), windows::vk_to_name(0x6f));
        assert_eq!(Some("NumpadEqual"), windows::vk_to_name(0x92));
        assert_eq!(Some("AudioVolumeMute"), windows::vk_to_name(0xad));
        assert_eq!(Some("BrowserBack"), windows::vk_to_name(0xa6));
        assert_eq!(Some("LaunchMail"), windows::vk_to_name(0xb4));
        assert_eq!(Some("KanaMode"), windows::vk_to_name(0x15));
    }

    #[test]
    fn windows_arrows_and_editing_keys_map() {
        assert_eq!(Some("ArrowLeft"), windows::vk_to_name(0x25));
        assert_eq!(Some("PageDown"), windows::vk_to_name(0x22));
        assert_eq!(Some("Delete"), windows::vk_to_name(0x2e));
    }

    #[test]
    fn windows_unknown_vk_falls_back_to_key_number() {
        assert_eq!(None, windows::vk_to_name(0x0fff));
        assert_eq!("Key291", fallback_name(0x123_u16));
    }
}
