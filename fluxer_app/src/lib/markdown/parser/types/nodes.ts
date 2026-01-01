/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {AlertType, EmojiKind, GuildNavKind, MentionKind, NodeType, TableAlignment, TimestampStyle} from './enums';

interface BaseNode {
	type: (typeof NodeType)[keyof typeof NodeType];
}

export interface TextNode extends BaseNode {
	type: typeof NodeType.Text;
	content: string;
}

export interface BlockquoteNode extends BaseNode {
	type: typeof NodeType.Blockquote;
	children: Array<Node>;
}

export interface FormattingNode extends BaseNode {
	type:
		| typeof NodeType.Strong
		| typeof NodeType.Emphasis
		| typeof NodeType.Underline
		| typeof NodeType.Strikethrough
		| typeof NodeType.Spoiler
		| typeof NodeType.Sequence;
	children: Array<Node>;
}

export interface HeadingNode extends BaseNode {
	type: typeof NodeType.Heading;
	level: number;
	children: Array<Node>;
}

export interface SubtextNode extends BaseNode {
	type: typeof NodeType.Subtext;
	children: Array<Node>;
}

export interface ListNode extends BaseNode {
	type: typeof NodeType.List;
	ordered: boolean;
	items: Array<ListItem>;
}

export interface ListItem {
	children: Array<Node>;
	ordinal?: number;
}

export interface CodeBlockNode extends BaseNode {
	type: typeof NodeType.CodeBlock;
	language?: string;
	content: string;
}

export interface InlineCodeNode extends BaseNode {
	type: typeof NodeType.InlineCode;
	content: string;
}

export interface LinkNode extends BaseNode {
	type: typeof NodeType.Link;
	text?: Node;
	url: string;
	escaped: boolean;
}

export interface MentionNode extends BaseNode {
	type: typeof NodeType.Mention;
	kind: MentionType;
}

export interface TimestampNode extends BaseNode {
	type: typeof NodeType.Timestamp;
	timestamp: number;
	style: (typeof TimestampStyle)[keyof typeof TimestampStyle];
}

export interface EmojiNode extends BaseNode {
	type: typeof NodeType.Emoji;
	kind: EmojiType;
}

export interface SequenceNode extends BaseNode {
	type: typeof NodeType.Sequence;
	children: Array<Node>;
}

export interface TableNode extends BaseNode {
	type: typeof NodeType.Table;
	header: TableRowNode;
	alignments: Array<(typeof TableAlignment)[keyof typeof TableAlignment]>;
	rows: Array<TableRowNode>;
}

export interface TableRowNode extends BaseNode {
	type: typeof NodeType.TableRow;
	cells: Array<TableCellNode>;
}

export interface TableCellNode extends BaseNode {
	type: typeof NodeType.TableCell;
	children: Array<Node>;
}

export interface AlertNode extends BaseNode {
	type: typeof NodeType.Alert;
	alertType: (typeof AlertType)[keyof typeof AlertType];
	children: Array<Node>;
}

export interface SpoilerNode extends BaseNode {
	type: typeof NodeType.Spoiler;
	children: Array<Node>;
	isBlock: boolean;
}

export type Node =
	| TextNode
	| BlockquoteNode
	| FormattingNode
	| HeadingNode
	| SubtextNode
	| ListNode
	| CodeBlockNode
	| InlineCodeNode
	| LinkNode
	| MentionNode
	| TimestampNode
	| EmojiNode
	| SequenceNode
	| TableNode
	| TableRowNode
	| TableCellNode
	| AlertNode
	| SpoilerNode;

type MentionType =
	| {kind: typeof MentionKind.User; id: string}
	| {kind: typeof MentionKind.Channel; id: string}
	| {kind: typeof MentionKind.Role; id: string}
	| {
			kind: typeof MentionKind.Command;
			name: string;
			subcommandGroup?: string;
			subcommand?: string;
			id: string;
	  }
	| {
			kind: typeof MentionKind.GuildNavigation;
			navigationType: typeof GuildNavKind.Customize | typeof GuildNavKind.Browse | typeof GuildNavKind.Guide;
	  }
	| {
			kind: typeof MentionKind.GuildNavigation;
			navigationType: typeof GuildNavKind.LinkedRoles;
			id?: string;
	  }
	| {kind: typeof MentionKind.Everyone}
	| {kind: typeof MentionKind.Here};

type EmojiType =
	| {kind: typeof EmojiKind.Standard; raw: string; codepoints: string; name: string}
	| {kind: typeof EmojiKind.Custom; name: string; id: string; animated: boolean};

export interface ParserResult {
	node: Node;
	advance: number;
}
