// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const ParserFlags = {
	ALLOW_SPOILERS: 1 << 0,
	ALLOW_HEADINGS: 1 << 1,
	ALLOW_LISTS: 1 << 2,
	ALLOW_CODE_BLOCKS: 1 << 3,
	ALLOW_MASKED_LINKS: 1 << 4,
	ALLOW_COMMAND_MENTIONS: 1 << 5,
	ALLOW_GUILD_NAVIGATIONS: 1 << 6,
	ALLOW_USER_MENTIONS: 1 << 7,
	ALLOW_ROLE_MENTIONS: 1 << 8,
	ALLOW_CHANNEL_MENTIONS: 1 << 9,
	ALLOW_EVERYONE_MENTIONS: 1 << 10,
	ALLOW_BLOCKQUOTES: 1 << 11,
	ALLOW_MULTILINE_BLOCKQUOTES: 1 << 12,
	ALLOW_SUBTEXT: 1 << 13,
	ALLOW_TABLES: 1 << 14,
	ALLOW_ALERTS: 1 << 15,
	ALLOW_AUTOLINKS: 1 << 16,
	ALLOW_CHECKBOX: 1 << 17,
} as const;

export type ParserFlags = ValueOf<typeof ParserFlags>;

export const NodeType = {
	Text: 'Text',
	Blockquote: 'Blockquote',
	Strong: 'Strong',
	Emphasis: 'Emphasis',
	Underline: 'Underline',
	Strikethrough: 'Strikethrough',
	Spoiler: 'Spoiler',
	Heading: 'Heading',
	Subtext: 'Subtext',
	List: 'List',
	CodeBlock: 'CodeBlock',
	InlineCode: 'InlineCode',
	Sequence: 'Sequence',
	Link: 'Link',
	Mention: 'Mention',
	Timestamp: 'Timestamp',
	Emoji: 'Emoji',
	Table: 'Table',
	TableRow: 'TableRow',
	TableCell: 'TableCell',
	Alert: 'Alert',
} as const;

export type NodeType = ValueOf<typeof NodeType>;

export const AlertType = {
	Note: 'Note',
	Tip: 'Tip',
	Important: 'Important',
	Warning: 'Warning',
	Caution: 'Caution',
} as const;

export type AlertType = ValueOf<typeof AlertType>;

export const TableAlignment = {
	Left: 'Left',
	Center: 'Center',
	Right: 'Right',
	None: 'None',
} as const;

export type TableAlignment = ValueOf<typeof TableAlignment>;

export const TimestampStyle = {
	ShortTime: 'ShortTime',
	LongTime: 'LongTime',
	ShortDate: 'ShortDate',
	LongDate: 'LongDate',
	ShortDateTime: 'ShortDateTime',
	LongDateTime: 'LongDateTime',
	ShortDateShortTime: 'ShortDateShortTime',
	ShortDateMediumTime: 'ShortDateMediumTime',
	RelativeTime: 'RelativeTime',
} as const;

export type TimestampStyle = ValueOf<typeof TimestampStyle>;

export const GuildNavKind = {
	Customize: 'Customize',
	Browse: 'Browse',
	Guide: 'Guide',
	LinkedRoles: 'LinkedRoles',
} as const;

export type GuildNavKind = ValueOf<typeof GuildNavKind>;

export const MentionKind = {
	User: 'User',
	Channel: 'Channel',
	Role: 'Role',
	Command: 'Command',
	GuildNavigation: 'GuildNavigation',
	Everyone: 'Everyone',
	Here: 'Here',
} as const;

export type MentionKind = ValueOf<typeof MentionKind>;

export const EmojiKind = {
	Standard: 'Standard',
	Custom: 'Custom',
} as const;

export type EmojiKind = ValueOf<typeof EmojiKind>;
