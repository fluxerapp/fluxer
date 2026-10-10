// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_desktop_native::input::ring::Ring;

#[test]
fn ring_stress_preserves_fifo_under_repeated_fill_drain_cycles() {
    let mut ring: Ring<u64, 1024> = Ring::new();
    for cycle in 0..512_u64 {
        for index in 0..1024_u64 {
            let slot = ring.claim().expect("slot") as usize;
            ring.slots[slot] = cycle * 10_000 + index;
        }
        assert!(ring.claim().is_none());
        for index in 0..1024_u64 {
            let slot = ring.pop().expect("slot") as usize;
            assert_eq!(cycle * 10_000 + index, ring.slots[slot]);
            ring.release();
        }
        assert!(ring.pop().is_none());
    }
}
