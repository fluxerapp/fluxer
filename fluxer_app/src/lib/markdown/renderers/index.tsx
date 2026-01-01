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

import type {I18n} from '@lingui/core';
import {i18n} from '@lingui/core';
import React from 'react';
import markupStyles from '~/styles/Markup.module.css';
import {TABLE_PARSING_FLAG} from '../config';
import {Parser} from '../parser/parser/parser';
import {NodeType, ParserFlags} from '../parser/types/enums';
import type {Node} from '../parser/types/nodes';
import {shouldRenderJumboEmojis} from '../utils/jumbo-detector';
import {
	AlertRenderer,
	BlockquoteRenderer,
	HeadingRenderer,
	ListRenderer,
	SequenceRenderer,
	SubtextRenderer,
	TableRenderer,
} from './common/block-elements';
import {CodeBlockRenderer, InlineCodeRenderer} from './common/code-elements';
import {
	EmphasisRenderer,
	SpoilerRenderer,
	StrikethroughRenderer,
	StrongRenderer,
	UnderlineRenderer,
} from './common/formatting-elements';
import {EmojiRenderer} from './emoji-renderer';
import {LinkRenderer} from './link-renderer';
import {MentionRenderer} from './mention-renderer';
import {TextRenderer} from './text-renderer';
import {TimestampRenderer} from './timestamp-renderer';

export const MarkdownContext = {
	STANDARD_WITH_JUMBO: 0,
	RESTRICTED_INLINE_REPLY: 1,
	RESTRICTED_USER_BIO: 2,
	RESTRICTED_EMBED_DESCRIPTION: 3,
	STANDARD_WITHOUT_JUMBO: 4,
} as const;
export type MarkdownContext = (typeof MarkdownContext)[keyof typeof MarkdownContext];

export interface MarkdownParseOptions {
	context: MarkdownContext;
	disableAnimatedEmoji?: boolean;
	channelId?: string;
	messageId?: string;
	guildId?: string;
}

interface MarkdownRenderOptions extends MarkdownParseOptions {
	shouldJumboEmojis: boolean;
	i18n: I18n;
}

export interface RendererProps<T extends Node = Node> {
	node: T;
	id: string;
	renderChildren: (nodes: Array<Node>) => React.ReactNode;
	options: MarkdownRenderOptions;
}

const STANDARD_FLAGS =
	ParserFlags.ALLOW_SPOILERS |
	ParserFlags.ALLOW_HEADINGS |
	ParserFlags.ALLOW_LISTS |
	ParserFlags.ALLOW_CODE_BLOCKS |
	ParserFlags.ALLOW_MASKED_LINKS |
	ParserFlags.ALLOW_COMMAND_MENTIONS |
	ParserFlags.ALLOW_GUILD_NAVIGATIONS |
	ParserFlags.ALLOW_USER_MENTIONS |
	ParserFlags.ALLOW_ROLE_MENTIONS |
	ParserFlags.ALLOW_CHANNEL_MENTIONS |
	ParserFlags.ALLOW_EVERYONE_MENTIONS |
	ParserFlags.ALLOW_BLOCKQUOTES |
	ParserFlags.ALLOW_MULTILINE_BLOCKQUOTES |
	ParserFlags.ALLOW_SUBTEXT |
	TABLE_PARSING_FLAG |
	ParserFlags.ALLOW_ALERTS |
	ParserFlags.ALLOW_AUTOLINKS;

const RESTRICTED_INLINE_REPLY_FLAGS =
	STANDARD_FLAGS &
	~(
		ParserFlags.ALLOW_BLOCKQUOTES |
		ParserFlags.ALLOW_MULTILINE_BLOCKQUOTES |
		ParserFlags.ALLOW_SUBTEXT |
		ParserFlags.ALLOW_TABLES |
		ParserFlags.ALLOW_ALERTS |
		ParserFlags.ALLOW_CODE_BLOCKS |
		ParserFlags.ALLOW_HEADINGS |
		ParserFlags.ALLOW_LISTS
	);

const RESTRICTED_USER_BIO_FLAGS =
	STANDARD_FLAGS &
	~(
		ParserFlags.ALLOW_HEADINGS |
		ParserFlags.ALLOW_CODE_BLOCKS |
		ParserFlags.ALLOW_ROLE_MENTIONS |
		ParserFlags.ALLOW_EVERYONE_MENTIONS |
		ParserFlags.ALLOW_SUBTEXT |
		ParserFlags.ALLOW_TABLES |
		ParserFlags.ALLOW_ALERTS
	);

const RESTRICTED_EMBED_DESCRIPTION_FLAGS =
	STANDARD_FLAGS &
	~(ParserFlags.ALLOW_HEADINGS | ParserFlags.ALLOW_TABLES | ParserFlags.ALLOW_ALERTS | ParserFlags.ALLOW_AUTOLINKS);

export function getParserFlagsForContext(context: MarkdownContext): number {
	switch (context) {
		case MarkdownContext.RESTRICTED_INLINE_REPLY:
			return RESTRICTED_INLINE_REPLY_FLAGS;
		case MarkdownContext.RESTRICTED_USER_BIO:
			return RESTRICTED_USER_BIO_FLAGS;
		case MarkdownContext.RESTRICTED_EMBED_DESCRIPTION:
			return RESTRICTED_EMBED_DESCRIPTION_FLAGS;
		default:
			return STANDARD_FLAGS;
	}
}

export function parse({content, context}: {content: string; context: MarkdownContext}) {
	const flags = getParserFlagsForContext(context);
	const parser = new Parser(content, flags);
	return parser.parse();
}

const renderers: Record<NodeType, React.ComponentType<RendererProps<any>>> = {
	[NodeType.Sequence]: SequenceRenderer,
	[NodeType.Text]: TextRenderer,
	[NodeType.Strong]: StrongRenderer,
	[NodeType.Emphasis]: EmphasisRenderer,
	[NodeType.Underline]: UnderlineRenderer,
	[NodeType.Strikethrough]: StrikethroughRenderer,
	[NodeType.Spoiler]: SpoilerRenderer,
	[NodeType.Timestamp]: TimestampRenderer,
	[NodeType.Blockquote]: BlockquoteRenderer,
	[NodeType.CodeBlock]: CodeBlockRenderer,
	[NodeType.InlineCode]: InlineCodeRenderer,
	[NodeType.Link]: LinkRenderer,
	[NodeType.Mention]: MentionRenderer,
	[NodeType.Emoji]: EmojiRenderer,
	[NodeType.List]: ListRenderer,
	[NodeType.Heading]: HeadingRenderer,
	[NodeType.Subtext]: SubtextRenderer,
	[NodeType.Table]: TableRenderer,
	[NodeType.TableRow]: () => null,
	[NodeType.TableCell]: () => null,
	[NodeType.Alert]: AlertRenderer,
};

function renderNode(node: Node, id: string, options: MarkdownRenderOptions): React.ReactNode {
	const renderer = renderers[node.type];
	if (!renderer) {
		console.warn(`No renderer found for node type: ${node.type}`);
		return null;
	}

	const renderChildrenFn = (children: Array<Node>) =>
		children.map((child, i) => renderNode(child, `${id}-${i}`, options));

	return React.createElement(renderer, {
		node,
		id,
		renderChildren: renderChildrenFn,
		options,
		key: id,
	});
}

export function render(nodes: Array<Node>, options: MarkdownParseOptions): React.ReactNode {
	const shouldJumboEmojis = options.context === MarkdownContext.STANDARD_WITH_JUMBO && shouldRenderJumboEmojis(nodes);

	const renderOptions: MarkdownRenderOptions = {
		...options,
		shouldJumboEmojis,
		i18n,
	};

	return nodes.map((node, i) => renderNode(node, `${options.context}-${i}`, renderOptions));
}

export function wrapRenderedContent(content: React.ReactNode, context: MarkdownContext): React.ReactNode {
	if (context === MarkdownContext.RESTRICTED_INLINE_REPLY) {
		return <div className={markupStyles.inlineFormat}>{content}</div>;
	}

	return content;
}
