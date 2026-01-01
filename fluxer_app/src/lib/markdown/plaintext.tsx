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
import {msg} from '@lingui/core/macro';
import {TEXT_BASED_CHANNEL_TYPES} from '~/Constants';
import ChannelStore from '~/stores/ChannelStore';
import GuildStore from '~/stores/GuildStore';
import UserStore from '~/stores/UserStore';
import * as NicknameUtils from '~/utils/NicknameUtils';
import {Parser} from './parser/parser/parser';
import {EmojiKind, GuildNavKind, MentionKind, NodeType} from './parser/types/enums';
import type {
	AlertNode,
	BlockquoteNode,
	CodeBlockNode,
	EmojiNode,
	FormattingNode,
	HeadingNode,
	InlineCodeNode,
	LinkNode,
	ListNode,
	MentionNode,
	Node,
	SequenceNode,
	SpoilerNode,
	SubtextNode,
	TableNode,
	TextNode,
	TimestampNode,
} from './parser/types/nodes';
import {formatTimestamp} from './utils/date-formatter';

interface PlaintextRenderOptions {
	channelId?: string;
	preserveMarkdown?: boolean;
	includeEmojiNames?: boolean;
	i18n?: I18n;
}

function renderNodeToPlaintext(node: Node, options: PlaintextRenderOptions = {}): string {
	switch (node.type) {
		case NodeType.Text:
			return (node as TextNode).content;

		case NodeType.Strong: {
			const strongNode = node as FormattingNode;
			const strongContent = renderNodesToPlaintext(strongNode.children, options);
			return options.preserveMarkdown ? `**${strongContent}**` : strongContent;
		}

		case NodeType.Emphasis: {
			const emphasisNode = node as FormattingNode;
			const emphasisContent = renderNodesToPlaintext(emphasisNode.children, options);
			return options.preserveMarkdown ? `*${emphasisContent}*` : emphasisContent;
		}

		case NodeType.Underline: {
			const underlineNode = node as FormattingNode;
			const underlineContent = renderNodesToPlaintext(underlineNode.children, options);
			return options.preserveMarkdown ? `__${underlineContent}__` : underlineContent;
		}

		case NodeType.Strikethrough: {
			const strikethroughNode = node as FormattingNode;
			const strikethroughContent = renderNodesToPlaintext(strikethroughNode.children, options);
			return options.preserveMarkdown ? `~~${strikethroughContent}~~` : strikethroughContent;
		}

		case NodeType.Spoiler: {
			const spoilerNode = node as SpoilerNode;
			const spoilerContent = renderNodesToPlaintext(spoilerNode.children, options);
			return options.preserveMarkdown ? `||${spoilerContent}||` : spoilerContent;
		}

		case NodeType.Heading: {
			const headingNode = node as HeadingNode;
			const headingContent = renderNodesToPlaintext(headingNode.children, options);
			const headingPrefix = options.preserveMarkdown ? `${'#'.repeat(headingNode.level)} ` : '';
			return `${headingPrefix}${headingContent}`;
		}

		case NodeType.Subtext: {
			const subtextNode = node as SubtextNode;
			return renderNodesToPlaintext(subtextNode.children, options);
		}

		case NodeType.List: {
			const listNode = node as ListNode;
			return listNode.items
				.map((item, index) => {
					const content = renderNodesToPlaintext(item.children, options);
					if (listNode.ordered) {
						const ordinal = item.ordinal ?? index + 1;
						return `${ordinal}. ${content}`;
					}
					return `• ${content}`;
				})
				.join('\n');
		}

		case NodeType.CodeBlock: {
			const codeBlockNode = node as CodeBlockNode;
			return options.preserveMarkdown
				? `\`\`\`${codeBlockNode.language || ''}\n${codeBlockNode.content}\`\`\``
				: codeBlockNode.content;
		}

		case NodeType.InlineCode: {
			const inlineCodeNode = node as InlineCodeNode;
			return options.preserveMarkdown ? `\`${inlineCodeNode.content}\`` : inlineCodeNode.content;
		}

		case NodeType.Link: {
			const linkNode = node as LinkNode;
			if (linkNode.text) {
				const linkText = renderNodeToPlaintext(linkNode.text, options);
				return options.preserveMarkdown ? `[${linkText}](${linkNode.url})` : linkText;
			}
			return linkNode.url;
		}

		case NodeType.Mention:
			return renderMentionToPlaintext(node as MentionNode, options);

		case NodeType.Timestamp: {
			const timestampNode = node as TimestampNode;
			return formatTimestamp(timestampNode.timestamp, timestampNode.style, options.i18n!);
		}

		case NodeType.Emoji:
			return renderEmojiToPlaintext(node as EmojiNode, options);

		case NodeType.Blockquote: {
			const blockquoteNode = node as BlockquoteNode;
			const blockquoteContent = renderNodesToPlaintext(blockquoteNode.children, options);
			if (options.preserveMarkdown) {
				return blockquoteContent
					.split('\n')
					.map((line) => `> ${line}`)
					.join('\n');
			}
			return blockquoteContent;
		}

		case NodeType.Sequence: {
			const sequenceNode = node as SequenceNode;
			return renderNodesToPlaintext(sequenceNode.children, options);
		}

		case NodeType.Table: {
			const tableNode = node as TableNode;
			const headerContent = tableNode.header.cells
				.map((cell) => renderNodesToPlaintext(cell.children, options))
				.join(' | ');
			const rowsContent = tableNode.rows
				.map((row) => row.cells.map((cell) => renderNodesToPlaintext(cell.children, options)).join(' | '))
				.join('\n');
			return `${headerContent}\n${rowsContent}`;
		}

		case NodeType.Alert: {
			const alertNode = node as AlertNode;
			const alertContent = renderNodesToPlaintext(alertNode.children, options);
			const alertPrefix = `[${alertNode.alertType.toUpperCase()}] `;
			return `${alertPrefix}${alertContent}`;
		}

		case NodeType.TableRow:
		case NodeType.TableCell:
			return '';

		default: {
			const nodeType = typeof (node as {type?: unknown}).type === 'string' ? (node as {type: string}).type : 'unknown';
			console.warn(`Unknown node type for plaintext rendering: ${nodeType}`);
			return '';
		}
	}
}

function renderMentionToPlaintext(node: MentionNode, options: PlaintextRenderOptions): string {
	const {kind} = node;
	const i18n = options.i18n!;

	switch (kind.kind) {
		case MentionKind.User: {
			const user = UserStore.getUser(kind.id);
			if (!user) {
				return `@${kind.id}`;
			}

			let name = user.displayName;
			if (options.channelId) {
				const channel = ChannelStore.getChannel(options.channelId);
				if (channel?.guildId) {
					name = NicknameUtils.getNickname(user, channel.guildId) || name;
				}
			}
			return `@${name}`;
		}

		case MentionKind.Role: {
			const channel = options.channelId ? ChannelStore.getChannel(options.channelId) : null;
			const guild = GuildStore.getGuild(channel?.guildId ?? '');
			const role = guild ? guild.roles[kind.id] : null;
			if (!role) {
				return `@${i18n._(msg`unknown-role`)}`;
			}
			return `@${role.name}`;
		}

		case MentionKind.Channel: {
			const channel = ChannelStore.getChannel(kind.id);
			if (!channel || !TEXT_BASED_CHANNEL_TYPES.has(channel.type)) {
				return `#${i18n._(msg`unknown-channel`)}`;
			}
			return `#${channel.name}`;
		}

		case MentionKind.Everyone:
			return '@everyone';

		case MentionKind.Here:
			return '@here';

		case MentionKind.Command: {
			const {name, subcommandGroup, subcommand} = kind;
			let commandName = `/${name}`;
			if (subcommandGroup) {
				commandName += ` ${subcommandGroup}`;
			}
			if (subcommand) {
				commandName += ` ${subcommand}`;
			}
			return commandName;
		}

		case MentionKind.GuildNavigation: {
			const {navigationType} = kind;
			switch (navigationType) {
				case GuildNavKind.Customize:
					return '#customize';
				case GuildNavKind.Browse:
					return '#browse';
				case GuildNavKind.Guide:
					return '#guide';
				case GuildNavKind.LinkedRoles: {
					const linkedRolesId = (kind as {navigationType: typeof GuildNavKind.LinkedRoles; id?: string}).id;
					return linkedRolesId ? `#linked-roles:${linkedRolesId}` : '#linked-roles';
				}
				default:
					return `#${navigationType}`;
			}
		}

		default:
			return `@${i18n._(msg`unknown-mention`)}`;
	}
}

function renderEmojiToPlaintext(node: EmojiNode, options: PlaintextRenderOptions): string {
	const {kind} = node;

	if (kind.kind === EmojiKind.Standard) {
		return kind.raw;
	}

	if (options.includeEmojiNames !== false) {
		return `:${kind.name}:`;
	}

	return '';
}

function renderNodesToPlaintext(nodes: Array<Node>, options: PlaintextRenderOptions = {}): string {
	return nodes.map((node) => renderNodeToPlaintext(node, options)).join('');
}

function renderToPlaintext(nodes: Array<Node>, options: PlaintextRenderOptions = {}): string {
	const result = renderNodesToPlaintext(nodes, options);

	return result
		.replace(/[ \t]+/g, ' ')
		.replace(/\n{3,}/g, '\n\n')
		.trim();
}

export function parseAndRenderToPlaintext(
	content: string,
	parserFlags: number,
	options: PlaintextRenderOptions = {},
): string {
	try {
		const parser = new Parser(content, parserFlags);
		const {nodes} = parser.parse();
		return renderToPlaintext(nodes, options);
	} catch (error) {
		console.error('Error parsing content for plaintext rendering:', error);
		return content;
	}
}
