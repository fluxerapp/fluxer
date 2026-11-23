// SPDX-License-Identifier: AGPL-3.0-or-later

pub const MAX_KEY: usize = u16::MAX as usize;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub struct NameRef {
    off: u32,
    len: u16,
}

impl NameRef {
    pub const EMPTY: NameRef = NameRef { off: 0, len: 0 };

    pub const fn new(off: u32, len: u16) -> NameRef {
        NameRef { off, len }
    }

    pub const fn off(self) -> u32 {
        self.off
    }

    pub const fn len(self) -> u16 {
        self.len
    }

    pub const fn is_empty(self) -> bool {
        self.len == 0
    }
}

#[derive(Clone, Debug, Default)]
pub struct Names {
    arena: Vec<u8>,
    garbage: usize,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct ArenaFull;

impl Names {
    pub fn with_capacity(bytes: usize) -> Names {
        Names {
            arena: Vec::with_capacity(bytes),
            garbage: 0,
        }
    }

    pub fn push(&mut self, key: &[u8]) -> Result<NameRef, ArenaFull> {
        let key = key.get(..MAX_KEY.min(key.len())).unwrap_or_default();
        if key.is_empty() {
            return Ok(NameRef::EMPTY);
        }
        let off = u32::try_from(self.arena.len()).map_err(|_| ArenaFull)?;
        let len = u16::try_from(key.len()).map_err(|_| ArenaFull)?;
        off.checked_add(u32::from(len)).ok_or(ArenaFull)?;
        self.arena.extend_from_slice(key);
        Ok(NameRef { off, len })
    }

    pub fn get(&self, name: NameRef) -> &[u8] {
        let start = name.off as usize;
        let end = start.saturating_add(usize::from(name.len));
        self.arena.get(start..end).unwrap_or_default()
    }

    pub fn release(&mut self, name: NameRef) {
        self.garbage = self.garbage.saturating_add(usize::from(name.len));
    }

    pub fn bytes(&self) -> usize {
        self.arena.capacity()
    }

    pub fn live_bytes(&self) -> usize {
        self.arena.len().saturating_sub(self.garbage)
    }

    pub fn needs_compaction(&self) -> bool {
        self.garbage > 4096 && self.garbage.saturating_mul(2) > self.arena.len()
    }

    pub fn compact<'a>(&mut self, refs: impl Iterator<Item = (&'a mut u32, u16)>) -> usize {
        let mut arena = Vec::with_capacity(self.live_bytes());
        for (off, len) in refs {
            if len == 0 {
                continue;
            }
            let bytes = self.get(NameRef { off: *off, len });
            let at = u32::try_from(arena.len()).unwrap_or(u32::MAX);
            arena.extend_from_slice(bytes);
            *off = at;
        }
        self.arena = arena;
        self.garbage = 0;
        self.arena.len()
    }
}
