// SPDX-License-Identifier: AGPL-3.0-or-later

pub const EV_KEY: u16 = 0x01;

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InputEvent {
    pub time_sec: i64,
    pub time_usec: i64,
    pub event_type: u16,
    pub code: u16,
    pub value: i32,
}

impl InputEvent {
    pub const BYTE_LEN: usize = 24;

    pub fn from_ne_bytes(bytes: [u8; Self::BYTE_LEN]) -> Self {
        let mut time_sec = [0_u8; 8];
        let mut time_usec = [0_u8; 8];
        let mut event_type = [0_u8; 2];
        let mut code = [0_u8; 2];
        let mut value = [0_u8; 4];

        time_sec.copy_from_slice(&bytes[0..8]);
        time_usec.copy_from_slice(&bytes[8..16]);
        event_type.copy_from_slice(&bytes[16..18]);
        code.copy_from_slice(&bytes[18..20]);
        value.copy_from_slice(&bytes[20..24]);

        Self {
            time_sec: i64::from_ne_bytes(time_sec),
            time_usec: i64::from_ne_bytes(time_usec),
            event_type: u16::from_ne_bytes(event_type),
            code: u16::from_ne_bytes(code),
            value: i32::from_ne_bytes(value),
        }
    }
}

pub fn parse_input_event(bytes: &[u8]) -> Option<InputEvent> {
    let chunk: [u8; InputEvent::BYTE_LEN] = bytes.get(..InputEvent::BYTE_LEN)?.try_into().ok()?;
    Some(InputEvent::from_ne_bytes(chunk))
}

pub fn parse_input_events(bytes: &[u8]) -> impl Iterator<Item = InputEvent> + '_ {
    bytes
        .as_chunks::<{ InputEvent::BYTE_LEN }>()
        .0
        .iter()
        .map(|chunk| InputEvent::from_ne_bytes(*chunk))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn event_bytes(event: InputEvent) -> [u8; InputEvent::BYTE_LEN] {
        let mut out = [0_u8; InputEvent::BYTE_LEN];
        out[0..8].copy_from_slice(&event.time_sec.to_ne_bytes());
        out[8..16].copy_from_slice(&event.time_usec.to_ne_bytes());
        out[16..18].copy_from_slice(&event.event_type.to_ne_bytes());
        out[18..20].copy_from_slice(&event.code.to_ne_bytes());
        out[20..24].copy_from_slice(&event.value.to_ne_bytes());
        out
    }

    #[test]
    fn input_event_layout_matches_64_bit_linux_abi() {
        assert_eq!(24, std::mem::size_of::<InputEvent>());
        assert_eq!(0, std::mem::offset_of!(InputEvent, time_sec));
        assert_eq!(8, std::mem::offset_of!(InputEvent, time_usec));
        assert_eq!(16, std::mem::offset_of!(InputEvent, event_type));
        assert_eq!(18, std::mem::offset_of!(InputEvent, code));
        assert_eq!(20, std::mem::offset_of!(InputEvent, value));
    }

    #[test]
    fn parses_synthetic_key_a_press() {
        let raw = event_bytes(InputEvent {
            time_sec: 123,
            time_usec: 456,
            event_type: EV_KEY,
            code: 30,
            value: 1,
        });

        assert_eq!(
            Some(InputEvent {
                time_sec: 123,
                time_usec: 456,
                event_type: EV_KEY,
                code: 30,
                value: 1,
            }),
            parse_input_event(&raw)
        );
    }

    #[test]
    fn parses_back_to_back_events_and_ignores_trailing_partial_bytes() {
        let first = InputEvent {
            time_sec: 1,
            time_usec: 2,
            event_type: EV_KEY,
            code: 30,
            value: 1,
        };
        let second = InputEvent {
            time_sec: 3,
            time_usec: 4,
            event_type: EV_KEY,
            code: 30,
            value: 0,
        };
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&event_bytes(first));
        bytes.extend_from_slice(&event_bytes(second));
        bytes.extend_from_slice(&[0xaa, 0xbb]);

        let events: Vec<_> = parse_input_events(&bytes).collect();
        assert_eq!(vec![first, second], events);
    }
}
