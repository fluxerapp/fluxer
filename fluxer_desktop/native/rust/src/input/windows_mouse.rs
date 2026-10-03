// SPDX-License-Identifier: AGPL-3.0-or-later

pub const WM_MOUSEMOVE: u32 = 0x0200;
pub const WM_LBUTTONDOWN: u32 = 0x0201;
pub const WM_LBUTTONUP: u32 = 0x0202;
pub const WM_RBUTTONDOWN: u32 = 0x0204;
pub const WM_RBUTTONUP: u32 = 0x0205;
pub const WM_MBUTTONDOWN: u32 = 0x0207;
pub const WM_MBUTTONUP: u32 = 0x0208;
pub const WM_MOUSEWHEEL: u32 = 0x020a;
pub const WM_XBUTTONDOWN: u32 = 0x020b;
pub const WM_XBUTTONUP: u32 = 0x020c;
pub const WM_MOUSEHWHEEL: u32 = 0x020e;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Action {
    Button { down: bool, button: u8 },
    Activity,
    Ignored,
}

pub fn classify(msg: u32, xbutton: u16) -> Action {
    match msg {
        WM_MOUSEMOVE | WM_MOUSEWHEEL | WM_MOUSEHWHEEL => Action::Activity,
        WM_LBUTTONDOWN => Action::Button {
            down: true,
            button: 0,
        },
        WM_LBUTTONUP => Action::Button {
            down: false,
            button: 0,
        },
        WM_RBUTTONDOWN => Action::Button {
            down: true,
            button: 2,
        },
        WM_RBUTTONUP => Action::Button {
            down: false,
            button: 2,
        },
        WM_MBUTTONDOWN => Action::Button {
            down: true,
            button: 1,
        },
        WM_MBUTTONUP => Action::Button {
            down: false,
            button: 1,
        },
        WM_XBUTTONDOWN => match xbutton {
            1 => Action::Button {
                down: true,
                button: 3,
            },
            2 => Action::Button {
                down: true,
                button: 4,
            },
            _ => Action::Ignored,
        },
        WM_XBUTTONUP => match xbutton {
            1 => Action::Button {
                down: false,
                button: 3,
            },
            2 => Action::Button {
                down: false,
                button: 4,
            },
            _ => Action::Ignored,
        },
        _ => Action::Ignored,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn primary_buttons_up_down_resolve_to_012() {
        assert_eq!(
            Action::Button {
                down: true,
                button: 0
            },
            classify(WM_LBUTTONDOWN, 0)
        );
        assert_eq!(
            Action::Button {
                down: false,
                button: 2
            },
            classify(WM_RBUTTONUP, 0)
        );
        assert_eq!(
            Action::Button {
                down: true,
                button: 1
            },
            classify(WM_MBUTTONDOWN, 0)
        );
    }

    #[test]
    fn xbutton_1_2_map_to_back_forward() {
        assert_eq!(
            Action::Button {
                down: true,
                button: 3
            },
            classify(WM_XBUTTONDOWN, 1)
        );
        assert_eq!(
            Action::Button {
                down: false,
                button: 4
            },
            classify(WM_XBUTTONUP, 2)
        );
        assert_eq!(Action::Ignored, classify(WM_XBUTTONDOWN, 7));
    }

    #[test]
    fn move_and_wheel_are_activity_and_unknown_messages_are_ignored() {
        assert_eq!(Action::Activity, classify(WM_MOUSEMOVE, 0));
        assert_eq!(Action::Activity, classify(WM_MOUSEWHEEL, 0));
        assert_eq!(Action::Activity, classify(WM_MOUSEHWHEEL, 0));
        assert_eq!(Action::Ignored, classify(0xdead, 0));
    }
}
