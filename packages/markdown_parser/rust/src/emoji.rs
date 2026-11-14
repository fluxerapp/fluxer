// SPDX-License-Identifier: AGPL-3.0-or-later

#[derive(Clone, Debug, Default)]
pub struct EmojiContext {
    standard: Vec<StandardEmoji>,
    shortcodes: Vec<ShortcodeEmoji>,
    skins: Vec<SkinEmoji>,
}

#[derive(Clone, Debug)]
pub struct StandardEmoji {
    pub offset: usize,
    pub len: usize,
    pub raw: String,
    pub name: String,
    pub codepoints: String,
}

#[derive(Clone, Debug)]
pub struct ShortcodeEmoji {
    pub name: String,
    pub raw: String,
    pub codepoints: String,
}

#[derive(Clone, Debug)]
pub struct SkinEmoji {
    pub name: String,
    pub tone: u8,
    pub raw: String,
    pub codepoints: String,
}

impl EmojiContext {
    pub fn parse(bytes: &str) -> Self {
        let mut context = Self::default();
        let mut counts = [0usize; 3];
        for line in bytes.lines() {
            match line.as_bytes().first() {
                Some(b'S') => counts[0] += 1,
                Some(b'C') => counts[1] += 1,
                Some(b'K') => counts[2] += 1,
                _ => {}
            }
        }
        context.standard.reserve_exact(counts[0]);
        context.shortcodes.reserve_exact(counts[1]);
        context.skins.reserve_exact(counts[2]);
        for line in bytes.lines() {
            if line.is_empty() {
                continue;
            }
            let mut parts = line.split('\t');
            match parts.next() {
                Some("S") => {
                    let Some(offset) = parts.next().and_then(|value| value.parse::<usize>().ok())
                    else {
                        continue;
                    };
                    let Some(len) = parts.next().and_then(|value| value.parse::<usize>().ok())
                    else {
                        continue;
                    };
                    let Some(raw) = parts.next() else { continue };
                    let Some(name) = parts.next() else { continue };
                    let Some(codepoints) = parts.next() else {
                        continue;
                    };
                    context.standard.push(StandardEmoji {
                        offset,
                        len,
                        raw: raw.to_owned(),
                        name: name.to_owned(),
                        codepoints: codepoints.to_owned(),
                    });
                }
                Some("C") => {
                    let Some(name) = parts.next() else { continue };
                    let Some(raw) = parts.next() else { continue };
                    let Some(codepoints) = parts.next() else {
                        continue;
                    };
                    context.shortcodes.push(ShortcodeEmoji {
                        name: name.to_owned(),
                        raw: raw.to_owned(),
                        codepoints: codepoints.to_owned(),
                    });
                }
                Some("K") => {
                    let Some(name) = parts.next() else { continue };
                    let Some(tone) = parts.next().and_then(|value| value.parse::<u8>().ok()) else {
                        continue;
                    };
                    let Some(raw) = parts.next() else { continue };
                    let Some(codepoints) = parts.next() else {
                        continue;
                    };
                    context.skins.push(SkinEmoji {
                        name: name.to_owned(),
                        tone,
                        raw: raw.to_owned(),
                        codepoints: codepoints.to_owned(),
                    });
                }
                _ => {}
            }
        }
        context.standard.sort_by_key(|record| record.offset);
        context
            .shortcodes
            .sort_by(|left, right| left.name.cmp(&right.name));
        context
            .skins
            .sort_by(|left, right| (&left.name, left.tone).cmp(&(&right.name, right.tone)));
        context
    }

    pub(crate) fn standard_cursor(&self) -> StandardCursor<'_> {
        StandardCursor {
            emojis: &self.standard,
            index: 0,
        }
    }

    pub fn shortcode(&self, name: &str) -> Option<&ShortcodeEmoji> {
        let index = self
            .shortcodes
            .partition_point(|record| record.name.as_str() < name);
        self.shortcodes
            .get(index)
            .filter(|record| record.name == name)
    }

    pub fn skin(&self, name: &str, tone: u8) -> Option<&SkinEmoji> {
        let index = self
            .skins
            .partition_point(|record| (record.name.as_str(), record.tone) < (name, tone));
        self.skins
            .get(index)
            .filter(|record| record.name == name && record.tone == tone)
    }
}

pub(crate) struct StandardCursor<'a> {
    emojis: &'a [StandardEmoji],
    index: usize,
}

impl<'a> StandardCursor<'a> {
    pub(crate) fn next_offset(&mut self, offset: usize) -> Option<usize> {
        self.seek(offset);
        self.emojis.get(self.index).map(|emoji| emoji.offset)
    }

    pub(crate) fn at(&mut self, offset: usize) -> Option<&'a StandardEmoji> {
        self.seek(offset);
        self.emojis
            .get(self.index)
            .filter(|emoji| emoji.offset == offset)
    }

    fn seek(&mut self, offset: usize) {
        while self
            .emojis
            .get(self.index)
            .is_some_and(|emoji| emoji.offset < offset)
        {
            self.index += 1;
        }
        while self.index > 0 && self.emojis[self.index - 1].offset >= offset {
            self.index -= 1;
        }
    }
}
