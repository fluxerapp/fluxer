// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import Guilds from '@app/features/guild/state/Guilds';
import {formatTimestamp} from '@app/features/messaging/utils/markdown/DateFormatter';
import {
	AlertType,
	EmojiKind,
	GuildNavKind,
	MentionKind,
	NodeType,
	TableAlignment,
} from '@app/features/messaging/utils/markdown/parser/Enums';
import type {
	AlertNode,
	BlockquoteNode,
	CodeBlockNode,
	EmojiNode,
	FormattingNode,
	HeadingNode,
	InlineCodeNode,
	LinkNode,
	ListItem,
	ListNode,
	MentionNode,
	Node,
	SequenceNode,
	SpoilerNode,
	SubtextNode,
	TableCellNode,
	TableNode,
	TableRowNode,
	TextNode,
	TimestampNode,
} from '@app/features/messaging/utils/markdown/parser/Nodes';
import {WasmParser} from '@app/features/messaging/utils/markdown/parser/WasmParser';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelMention} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const logger = new Logger('MarkdownPlaintext');

export interface PlaintextRenderOptions {
	channelId?: string;
	preserveMarkdown?: boolean;
	includeEmojiNames?: boolean;
	includeLinkUrls?: boolean;
	mentionChannels?: ReadonlyArray<ChannelMention>;
	i18n: I18n;
}

const ALERT_TIP_LABEL = msg({
	message: 'Tip',
	comment: 'Markdown alert label for a tip callout when copied as plaintext.',
});
const ALERT_IMPORTANT_LABEL = msg({
	message: 'Important',
	comment: 'Markdown alert label for an important callout when copied as plaintext.',
});
const ALERT_WARNING_LABEL = msg({
	message: 'Warning',
	comment: 'Markdown alert label for a warning callout when copied as plaintext.',
});
const ALERT_CAUTION_LABEL = msg({
	message: 'Caution',
	comment: 'Markdown alert label for a caution callout when copied as plaintext.',
});
const ALERT_NOTE_LABEL = msg({
	message: 'Note',
	comment: 'Markdown alert label for a note callout when copied as plaintext.',
});
const UNKNOWN_ROLE_MENTION_LABEL = msg({
	message: 'unknown-role',
	comment: 'Fallback plaintext for an unresolved role mention in copied markdown. Keep lowercase and hyphenated.',
});
const UNKNOWN_CHANNEL_MENTION_LABEL = msg({
	message: 'unknown-channel',
	comment: 'Fallback plaintext for an unresolved channel mention in copied markdown. Keep lowercase and hyphenated.',
});
const UNKNOWN_MENTION_LABEL = msg({
	message: 'unknown-mention',
	comment: 'Fallback plaintext for an unresolved mention in copied markdown. Keep lowercase and hyphenated.',
});
const BLOCK_NODE_TYPES = new Set<NodeType>([
	NodeType.Alert,
	NodeType.Blockquote,
	NodeType.CodeBlock,
	NodeType.Heading,
	NodeType.List,
	NodeType.Subtext,
	NodeType.Table,
	NodeType.TableRow,
]);
const COPYABLE_CHANNEL_MENTION_TYPES = new Set<number>([
	ChannelTypes.GUILD_TEXT,
	ChannelTypes.GUILD_VOICE,
	ChannelTypes.GUILD_LINK,
	ChannelTypes.GUILD_CATEGORY,
]);

function isBlockNode(node: Node): boolean {
	return BLOCK_NODE_TYPES.has(node.type) || (node.type === NodeType.Spoiler && (node as SpoilerNode).isBlock);
}

function joinWithVisibleLineBreaks(parts: Array<string>): string {
	return parts.filter((part) => part.length > 0).join('\n');
}

function indentContinuationLines(text: string, width: number): string {
	const lines = text.split('\n');
	if (lines.length <= 1) {
		return text;
	}
	const continuationIndent = ' '.repeat(width);
	return lines.map((line, index) => (index === 0 ? line : `${continuationIndent}${line}`)).join('\n');
}

function renderNodeToPlaintext(node: Node, options: PlaintextRenderOptions): string {
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
			if (spoilerNode.isBlock && options.preserveMarkdown) {
				return `||\n${spoilerContent}\n||`;
			}
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
			const startOrdinal = listNode.items[0]?.ordinal ?? 1;
			return listNode.items
				.map((item: ListItem, index: number) => {
					const content = renderNodesToPlaintext(item.children, options).trim();
					if (listNode.ordered) {
						const prefix = `${startOrdinal + index}. `;
						return `${prefix}${indentContinuationLines(content, prefix.length)}`;
					}
					const prefix = options.preserveMarkdown ? '- ' : '• ';
					return `${prefix}${indentContinuationLines(content, prefix.length)}`;
				})
				.join('\n');
		}
		case NodeType.CodeBlock: {
			const codeBlockNode = node as CodeBlockNode;
			if (!options.preserveMarkdown) {
				return codeBlockNode.content;
			}
			const content = codeBlockNode.content.endsWith('\n') ? codeBlockNode.content : `${codeBlockNode.content}\n`;
			return `\`\`\`${codeBlockNode.language || ''}\n${content}\`\`\``;
		}
		case NodeType.InlineCode: {
			const inlineCodeNode = node as InlineCodeNode;
			return options.preserveMarkdown ? `\`${inlineCodeNode.content}\`` : inlineCodeNode.content;
		}
		case NodeType.Link: {
			const linkNode = node as LinkNode;
			if (linkNode.text) {
				const linkText = renderNodeToPlaintext(linkNode.text, options);
				if (options.preserveMarkdown) {
					return `[${linkText}](${linkNode.url})`;
				}
				if (
					options.includeLinkUrls &&
					normaliseUrlForComparison(linkText) !== normaliseUrlForComparison(linkNode.url)
				) {
					return `${linkText} (${linkNode.url})`;
				}
				return linkText;
			}
			return linkNode.url;
		}
		case NodeType.Mention:
			return renderMentionToPlaintext(node as MentionNode, options);
		case NodeType.Timestamp: {
			const timestampNode = node as TimestampNode;
			return formatTimestamp(timestampNode.timestamp, timestampNode.style, options.i18n);
		}
		case NodeType.Emoji:
			return renderEmojiToPlaintext(node as EmojiNode, options);
		case NodeType.Blockquote: {
			const blockquoteNode = node as BlockquoteNode;
			const blockquoteContent = renderNodesToPlaintext(blockquoteNode.children, options);
			if (options.preserveMarkdown) {
				if (blockquoteNode.children.length === 0) {
					const lineCount = Math.max(1, Math.min(blockquoteNode.blankLines ?? 1, 100));
					return Array.from({length: lineCount}, () => '> ').join('\n');
				}
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
			return renderTableNodeToMarkdown(tableNode, options);
		}
		case NodeType.Alert: {
			const alertNode = node as AlertNode;
			const alertContent = renderNodesToPlaintext(alertNode.children, options);
			const alertLabel = renderAlertLabel(alertNode.alertType, options);
			if (options.preserveMarkdown) {
				return `> [!${alertNode.alertType.toUpperCase()}]\n${alertContent
					.split('\n')
					.map((line) => `> ${line}`)
					.join('\n')}`;
			}
			return alertContent ? `${alertLabel}\n${alertContent}` : alertLabel;
		}
		case NodeType.TableRow:
			return renderTableRowToPlaintext(node as TableRowNode, options);
		case NodeType.TableCell:
			return renderTableCellToPlaintext(node as TableCellNode, options);
		default: {
			const nodeType =
				typeof (
					node as {
						type?: unknown;
					}
				).type === 'string'
					? (
							node as {
								type: string;
							}
						).type
					: 'unknown';
			logger.warn(`Unknown node type for plaintext rendering: ${nodeType}`);
			return '';
		}
	}
}

function renderTableRowToPlaintext(row: TableRowNode, options: PlaintextRenderOptions): string {
	return row.cells.map((cell) => renderTableCellToPlaintext(cell, options)).join(' | ');
}

function renderTableCellToPlaintext(cell: TableCellNode, options: PlaintextRenderOptions): string {
	return renderNodesToPlaintext(cell.children, options)
		.replace(/\s*\n+\s*/gu, ' ')
		.trim();
}

function normaliseUrlForComparison(value?: string | null): string | null {
	if (!value) {
		return null;
	}
	try {
		return new URL(value).href.replace(/\/$/u, '');
	} catch {
		return null;
	}
}

function getTableColumnCount(table: TableNode, rows: Array<Array<string>>): number {
	const bodyColumnCount = rows.reduce((max, row) => Math.max(max, row.length), 0);
	return Math.max(table.header.cells.length, bodyColumnCount, table.alignments.length, 1);
}

function padTableRowCells(cells: Array<string>, columnCount: number): Array<string> {
	if (cells.length >= columnCount) {
		return cells;
	}
	return [...cells, ...Array.from({length: columnCount - cells.length}, () => '')];
}

function escapeMarkdownTableCell(value: string): string {
	return value.replace(/\|/gu, '\\|');
}

function formatMarkdownTableRow(cells: Array<string>): string {
	return `| ${cells.map(escapeMarkdownTableCell).join(' | ')} |`;
}

function formatMarkdownTableSeparator(alignment: TableAlignment | undefined): string {
	switch (alignment) {
		case TableAlignment.Left:
			return ':---';
		case TableAlignment.Center:
			return ':---:';
		case TableAlignment.Right:
			return '---:';
		default:
			return '---';
	}
}

export function renderTableNodeToMarkdown(table: TableNode, options: PlaintextRenderOptions): string {
	const headerCells = table.header.cells.map((cell) => renderTableCellToPlaintext(cell, options));
	const bodyRows = table.rows.map((row) => row.cells.map((cell) => renderTableCellToPlaintext(cell, options)));
	const columnCount = getTableColumnCount(table, bodyRows);
	const normalisedHeaderCells = padTableRowCells(headerCells, columnCount);
	const separatorCells = Array.from({length: columnCount}, (_value, index) =>
		formatMarkdownTableSeparator(table.alignments[index]),
	);
	return joinWithVisibleLineBreaks([
		formatMarkdownTableRow(normalisedHeaderCells),
		formatMarkdownTableRow(separatorCells),
		...bodyRows.map((row) => formatMarkdownTableRow(padTableRowCells(row, columnCount))),
	]);
}

function renderAlertLabel(alertType: AlertType, options: PlaintextRenderOptions): string {
	const i18n = options.i18n;
	switch (alertType) {
		case AlertType.Tip:
			return i18n._(ALERT_TIP_LABEL);
		case AlertType.Important:
			return i18n._(ALERT_IMPORTANT_LABEL);
		case AlertType.Warning:
			return i18n._(ALERT_WARNING_LABEL);
		case AlertType.Caution:
			return i18n._(ALERT_CAUTION_LABEL);
		default:
			return i18n._(ALERT_NOTE_LABEL);
	}
}

function renderMentionToPlaintext(node: MentionNode, options: PlaintextRenderOptions): string {
	const {kind} = node;
	switch (kind.kind) {
		case MentionKind.User: {
			const user = Users.getUser(kind.id);
			if (!user) {
				return `@${kind.id}`;
			}
			const channel = options.channelId ? Channels.getChannel(options.channelId) : null;
			return `@${NicknameUtils.getNickname(user, channel?.guildId ?? null, options.channelId)}`;
		}
		case MentionKind.Role: {
			const channel = options.channelId ? Channels.getChannel(options.channelId) : null;
			const guild = Guilds.getGuild(channel?.guildId ?? '');
			const role = guild ? guild.roles[kind.id] : null;
			if (!role) {
				return `@${options.i18n._(UNKNOWN_ROLE_MENTION_LABEL)}`;
			}
			return `@${role.name}`;
		}
		case MentionKind.Channel: {
			const channel = Channels.getChannel(kind.id);
			if (channel && COPYABLE_CHANNEL_MENTION_TYPES.has(channel.type)) {
				return `#${channel.name}`;
			}
			const fallbackMention = options.mentionChannels?.find((mention) => mention.id === kind.id);
			if (fallbackMention && COPYABLE_CHANNEL_MENTION_TYPES.has(fallbackMention.type)) {
				return `#${fallbackMention.name}`;
			}
			if (!channel || !COPYABLE_CHANNEL_MENTION_TYPES.has(channel.type)) {
				return `#${options.i18n._(UNKNOWN_CHANNEL_MENTION_LABEL)}`;
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
					const linkedRolesId = (
						kind as {
							navigationType: 'LinkedRoles';
							id?: string;
						}
					).id;
					return linkedRolesId ? `#linked-roles:${linkedRolesId}` : '#linked-roles';
				}
				default:
					return `#${navigationType}`;
			}
		}
		default:
			return `@${options.i18n._(UNKNOWN_MENTION_LABEL)}`;
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

function renderNodesToPlaintext(nodes: Array<Node>, options: PlaintextRenderOptions): string {
	let result = '';
	let previousNode: Node | null = null;
	for (const node of nodes) {
		const rendered = renderNodeToPlaintext(node, options);
		if (!rendered) {
			previousNode = node;
			continue;
		}
		const needsBlockSeparator =
			result.length > 0 &&
			previousNode != null &&
			(isBlockNode(previousNode) || isBlockNode(node)) &&
			!result.endsWith('\n') &&
			!rendered.startsWith('\n');
		if (needsBlockSeparator) {
			const lastLine = result.slice(result.lastIndexOf('\n') + 1);
			if (!(options.preserveMarkdown && lastLine === '> ')) {
				result = result.replace(/[ \t]+$/u, '');
			}
			result += `\n${rendered}`;
		} else {
			result += rendered;
		}
		previousNode = node;
	}
	return result;
}

function trimTrailingWhitespaceBeforeLineBreaks(value: string, preserveMarkdown: boolean | undefined): string {
	if (!preserveMarkdown) {
		return value.replace(/[ \t]+\n/gu, '\n');
	}
	const lines = value.split('\n');
	for (let i = 0; i < lines.length - 1; i++) {
		if (lines[i] !== '> ') {
			lines[i] = lines[i].replace(/[ \t]+$/u, '');
		}
	}
	return lines.join('\n');
}

function renderToPlaintext(nodes: Array<Node>, options: PlaintextRenderOptions): string {
	const result = renderNodesToPlaintext(nodes, options);
	return trimTrailingWhitespaceBeforeLineBreaks(result, options.preserveMarkdown)
		.replace(/\n{3,}/g, '\n\n')
		.replace(/^\n+|\n+$/gu, '');
}

export function renderAstToPlaintext(nodes: Array<Node>, options: PlaintextRenderOptions): string {
	return renderToPlaintext(nodes, options);
}

export function parseAndRenderToPlaintext(
	content: string,
	parserFlags: number,
	options: PlaintextRenderOptions,
): string {
	try {
		const parser = new WasmParser(content, parserFlags);
		const {nodes} = parser.parse();
		return renderToPlaintext(nodes, options);
	} catch (error) {
		logger.error('Error parsing content for plaintext rendering:', error);
		return content;
	}
}
