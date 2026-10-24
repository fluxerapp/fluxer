// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::Serialize;

pub struct ParserFlags;

impl ParserFlags {
    pub const ALLOW_SPOILERS: u32 = 1 << 0;
    pub const ALLOW_HEADINGS: u32 = 1 << 1;
    pub const ALLOW_LISTS: u32 = 1 << 2;
    pub const ALLOW_CODE_BLOCKS: u32 = 1 << 3;
    pub const ALLOW_MASKED_LINKS: u32 = 1 << 4;
    pub const ALLOW_COMMAND_MENTIONS: u32 = 1 << 5;
    pub const ALLOW_GUILD_NAVIGATIONS: u32 = 1 << 6;
    pub const ALLOW_USER_MENTIONS: u32 = 1 << 7;
    pub const ALLOW_ROLE_MENTIONS: u32 = 1 << 8;
    pub const ALLOW_CHANNEL_MENTIONS: u32 = 1 << 9;
    pub const ALLOW_EVERYONE_MENTIONS: u32 = 1 << 10;
    pub const ALLOW_BLOCKQUOTES: u32 = 1 << 11;
    pub const ALLOW_MULTILINE_BLOCKQUOTES: u32 = 1 << 12;
    pub const ALLOW_SUBTEXT: u32 = 1 << 13;
    pub const ALLOW_TABLES: u32 = 1 << 14;
    pub const ALLOW_ALERTS: u32 = 1 << 15;
    pub const ALLOW_AUTOLINKS: u32 = 1 << 16;
    pub const ALLOW_CHECKBOX: u32 = 1 << 17;

    pub const ALL: u32 = Self::ALLOW_SPOILERS
        | Self::ALLOW_HEADINGS
        | Self::ALLOW_LISTS
        | Self::ALLOW_CODE_BLOCKS
        | Self::ALLOW_MASKED_LINKS
        | Self::ALLOW_COMMAND_MENTIONS
        | Self::ALLOW_GUILD_NAVIGATIONS
        | Self::ALLOW_USER_MENTIONS
        | Self::ALLOW_ROLE_MENTIONS
        | Self::ALLOW_CHANNEL_MENTIONS
        | Self::ALLOW_EVERYONE_MENTIONS
        | Self::ALLOW_BLOCKQUOTES
        | Self::ALLOW_MULTILINE_BLOCKQUOTES
        | Self::ALLOW_SUBTEXT
        | Self::ALLOW_TABLES
        | Self::ALLOW_ALERTS
        | Self::ALLOW_AUTOLINKS
        | Self::ALLOW_CHECKBOX;

    #[inline]
    pub fn has(flags: u32, flag: u32) -> bool {
        flags & flag != 0
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "type")]
pub enum Node {
    Text {
        content: String,
    },
    Blockquote {
        children: Vec<Node>,
        #[serde(rename = "blankLines", skip_serializing_if = "Option::is_none")]
        blank_lines: Option<usize>,
    },
    Strong {
        children: Vec<Node>,
    },
    Emphasis {
        children: Vec<Node>,
    },
    Underline {
        children: Vec<Node>,
    },
    Strikethrough {
        children: Vec<Node>,
    },
    Spoiler {
        children: Vec<Node>,
        #[serde(rename = "isBlock", skip_serializing_if = "Option::is_none")]
        is_block: Option<bool>,
    },
    Heading {
        level: u8,
        children: Vec<Node>,
    },
    Subtext {
        children: Vec<Node>,
    },
    List {
        ordered: bool,
        items: Vec<ListItem>,
    },
    CodeBlock {
        #[serde(skip_serializing_if = "Option::is_none")]
        language: Option<String>,
        content: String,
    },
    InlineCode {
        content: String,
    },
    Sequence {
        children: Vec<Node>,
    },
    Link {
        #[serde(skip_serializing_if = "Option::is_none")]
        text: Option<Box<Node>>,
        url: String,
        escaped: bool,
        #[serde(rename = "rawUrl")]
        raw_url: String,
        source: String,
    },
    Mention {
        kind: MentionKind,
    },
    Timestamp {
        timestamp: u64,
        style: TimestampStyle,
    },
    Emoji {
        kind: EmojiKind,
    },
    Table {
        header: Box<Node>,
        alignments: Vec<TableAlignment>,
        rows: Vec<Node>,
    },
    TableRow {
        cells: Vec<Node>,
    },
    TableCell {
        children: Vec<Node>,
    },
    Alert {
        #[serde(rename = "alertType")]
        alert_type: AlertType,
        children: Vec<Node>,
    },
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct ListItem {
    pub children: Vec<Node>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ordinal: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub checked: Option<bool>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
pub enum TimestampStyle {
    ShortTime,
    LongTime,
    ShortDate,
    LongDate,
    ShortDateTime,
    LongDateTime,
    ShortDateShortTime,
    ShortDateMediumTime,
    RelativeTime,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
pub enum AlertType {
    Note,
    Tip,
    Important,
    Warning,
    Caution,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
pub enum TableAlignment {
    Left,
    Center,
    Right,
    None,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "kind")]
pub enum MentionKind {
    User {
        id: String,
    },
    Channel {
        id: String,
    },
    Role {
        id: String,
    },
    Command {
        name: String,
        #[serde(rename = "subcommandGroup", skip_serializing_if = "Option::is_none")]
        subcommand_group: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        subcommand: Option<String>,
        id: String,
    },
    GuildNavigation {
        #[serde(rename = "navigationType")]
        navigation_type: GuildNavigationType,
        #[serde(skip_serializing_if = "Option::is_none")]
        id: Option<String>,
    },
    Everyone,
    Here,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
pub enum GuildNavigationType {
    Customize,
    Browse,
    Guide,
    LinkedRoles,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "kind")]
pub enum EmojiKind {
    Standard {
        raw: String,
        codepoints: String,
        name: String,
    },
    Custom {
        name: String,
        id: String,
        animated: bool,
    },
}

impl Node {
    pub fn is_empty_text(&self) -> bool {
        matches!(self, Self::Text { content } if content.is_empty())
    }

    pub fn tag_name(&self) -> &'static str {
        match self {
            Self::Text { .. } => "Text",
            Self::Blockquote { .. } => "Blockquote",
            Self::Strong { .. } => "Strong",
            Self::Emphasis { .. } => "Emphasis",
            Self::Underline { .. } => "Underline",
            Self::Strikethrough { .. } => "Strikethrough",
            Self::Spoiler { .. } => "Spoiler",
            Self::Heading { .. } => "Heading",
            Self::Subtext { .. } => "Subtext",
            Self::List { .. } => "List",
            Self::CodeBlock { .. } => "CodeBlock",
            Self::InlineCode { .. } => "InlineCode",
            Self::Sequence { .. } => "Sequence",
            Self::Link { .. } => "Link",
            Self::Mention { .. } => "Mention",
            Self::Timestamp { .. } => "Timestamp",
            Self::Emoji { .. } => "Emoji",
            Self::Table { .. } => "Table",
            Self::TableRow { .. } => "TableRow",
            Self::TableCell { .. } => "TableCell",
            Self::Alert { .. } => "Alert",
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParserResult {
    pub node: Node,
    pub advance: usize,
}
