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
