// SPDX-License-Identifier: AGPL-3.0-or-later

pub const KEY_MAP: &[(u16, &str)] = &[
    (1, "Escape"),
    (2, "1"),
    (3, "2"),
    (4, "3"),
    (5, "4"),
    (6, "5"),
    (7, "6"),
    (8, "7"),
    (9, "8"),
    (10, "9"),
    (11, "0"),
    (12, "Minus"),
    (13, "Equal"),
    (14, "Backspace"),
    (15, "Tab"),
    (16, "Q"),
    (17, "W"),
    (18, "E"),
    (19, "R"),
    (20, "T"),
    (21, "Y"),
    (22, "U"),
    (23, "I"),
    (24, "O"),
    (25, "P"),
    (26, "BracketLeft"),
    (27, "BracketRight"),
    (28, "Enter"),
    (29, "ControlLeft"),
    (30, "A"),
    (31, "S"),
    (32, "D"),
    (33, "F"),
    (34, "G"),
    (35, "H"),
    (36, "J"),
    (37, "K"),
    (38, "L"),
    (39, "Semicolon"),
    (40, "Quote"),
    (41, "Backquote"),
    (42, "ShiftLeft"),
    (43, "Backslash"),
    (44, "Z"),
    (45, "X"),
    (46, "C"),
    (47, "V"),
    (48, "B"),
    (49, "N"),
    (50, "M"),
    (51, "Comma"),
    (52, "Period"),
    (53, "Slash"),
    (54, "ShiftRight"),
    (55, "NumpadMultiply"),
    (56, "AltLeft"),
    (57, "Space"),
    (58, "CapsLock"),
    (59, "F1"),
    (60, "F2"),
    (61, "F3"),
    (62, "F4"),
    (63, "F5"),
    (64, "F6"),
    (65, "F7"),
    (66, "F8"),
    (67, "F9"),
    (68, "F10"),
    (69, "NumLock"),
    (70, "ScrollLock"),
    (71, "Numpad7"),
    (72, "Numpad8"),
    (73, "Numpad9"),
    (74, "NumpadSubtract"),
    (75, "Numpad4"),
    (76, "Numpad5"),
    (77, "Numpad6"),
    (78, "NumpadAdd"),
    (79, "Numpad1"),
    (80, "Numpad2"),
    (81, "Numpad3"),
    (82, "Numpad0"),
    (83, "NumpadDecimal"),
    (86, "IntlBackslash"),
    (119, "Pause"),
    (87, "F11"),
    (88, "F12"),
    (89, "IntlRo"),
    (90, "Lang3"),
    (92, "Convert"),
    (93, "KanaMode"),
    (94, "NonConvert"),
    (96, "NumpadEnter"),
    (97, "ControlRight"),
    (98, "NumpadDivide"),
    (99, "PrintScreen"),
    (100, "AltRight"),
    (102, "Home"),
    (103, "ArrowUp"),
    (104, "PageUp"),
    (105, "ArrowLeft"),
    (106, "ArrowRight"),
    (107, "End"),
    (108, "ArrowDown"),
    (109, "PageDown"),
    (110, "Insert"),
    (111, "Delete"),
    (113, "AudioVolumeMute"),
    (114, "AudioVolumeDown"),
    (115, "AudioVolumeUp"),
    (116, "Power"),
    (117, "NumpadEqual"),
    (121, "NumpadComma"),
    (122, "Lang1"),
    (123, "Lang2"),
    (124, "IntlYen"),
    (125, "MetaLeft"),
    (126, "MetaRight"),
    (127, "ContextMenu"),
    (142, "Sleep"),
    (143, "WakeUp"),
    (148, "LaunchApp1"),
    (149, "LaunchApp2"),
    (155, "LaunchMail"),
    (158, "BrowserBack"),
    (159, "BrowserForward"),
    (163, "MediaTrackNext"),
    (164, "MediaPlayPause"),
    (165, "MediaTrackPrevious"),
    (166, "MediaStop"),
    (172, "BrowserHome"),
    (173, "BrowserRefresh"),
    (183, "F13"),
    (184, "F14"),
    (185, "F15"),
    (186, "F16"),
    (187, "F17"),
    (188, "F18"),
    (189, "F19"),
    (190, "F20"),
    (191, "F21"),
    (192, "F22"),
    (193, "F23"),
    (194, "F24"),
    (217, "BrowserSearch"),
    (226, "LaunchMediaPlayer"),
    (364, "BrowserFavorites"),
];

pub const LEFT_CTRL: u16 = 29;
pub const RIGHT_CTRL: u16 = 97;
pub const LEFT_SHIFT: u16 = 42;
pub const RIGHT_SHIFT: u16 = 54;
pub const LEFT_ALT: u16 = 56;
pub const RIGHT_ALT: u16 = 100;
pub const LEFT_META: u16 = 125;
pub const RIGHT_META: u16 = 126;

pub const BTN_LEFT: u16 = 0x110;
pub const BTN_RIGHT: u16 = 0x111;
pub const BTN_MIDDLE: u16 = 0x112;
pub const BTN_SIDE: u16 = 0x113;
pub const BTN_EXTRA: u16 = 0x114;
pub const BTN_FORWARD: u16 = 0x115;
pub const BTN_BACK: u16 = 0x116;

pub fn evdev_button_to_browser_button(code: u16) -> Option<u8> {
    match code {
        BTN_LEFT => Some(0),
        BTN_MIDDLE => Some(1),
        BTN_RIGHT => Some(2),
        BTN_SIDE | BTN_BACK => Some(3),
        BTN_EXTRA | BTN_FORWARD => Some(4),
        _ => None,
    }
}

pub fn keycode_to_name(code: u16) -> Option<&'static str> {
    KEY_MAP
        .iter()
        .find(|(entry_code, _)| *entry_code == code)
        .map(|(_, name)| *name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keycode_to_name_covers_canonical_letters_and_arrows() {
        assert_eq!(keycode_to_name(30), Some("A"));
        assert_eq!(keycode_to_name(44), Some("Z"));
        assert_eq!(keycode_to_name(119), Some("Pause"));
        assert_eq!(keycode_to_name(183), Some("F13"));
        assert_eq!(keycode_to_name(96), Some("NumpadEnter"));
        assert_eq!(keycode_to_name(113), Some("AudioVolumeMute"));
        assert_eq!(keycode_to_name(103), Some("ArrowUp"));
        assert_eq!(keycode_to_name(125), Some("MetaLeft"));
        assert_eq!(keycode_to_name(0), None);
        assert_eq!(keycode_to_name(0xffff), None);
    }

    #[test]
    fn evdev_button_matches_dom_convention() {
        assert_eq!(evdev_button_to_browser_button(BTN_LEFT), Some(0));
        assert_eq!(evdev_button_to_browser_button(BTN_MIDDLE), Some(1));
        assert_eq!(evdev_button_to_browser_button(BTN_RIGHT), Some(2));
        assert_eq!(evdev_button_to_browser_button(BTN_SIDE), Some(3));
        assert_eq!(evdev_button_to_browser_button(BTN_BACK), Some(3));
        assert_eq!(evdev_button_to_browser_button(BTN_EXTRA), Some(4));
        assert_eq!(evdev_button_to_browser_button(BTN_FORWARD), Some(4));
        assert_eq!(evdev_button_to_browser_button(0x100), None);
        assert_eq!(evdev_button_to_browser_button(0xffff), None);
    }

    #[test]
    fn modifier_constants_match_linux_input_event_codes() {
        assert_eq!(LEFT_CTRL, 29);
        assert_eq!(RIGHT_CTRL, 97);
        assert_eq!(LEFT_SHIFT, 42);
        assert_eq!(RIGHT_SHIFT, 54);
        assert_eq!(LEFT_ALT, 56);
        assert_eq!(RIGHT_ALT, 100);
        assert_eq!(LEFT_META, 125);
        assert_eq!(RIGHT_META, 126);
    }
}
