// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	MarkdownBlock,
	markdownBlockProps,
	markdownTableProps,
} from '@app/features/messaging/components/markdown/renderers/common/MarkdownBlockAttributes';
import {
	isRestrictedInlineContext,
	type RendererProps,
} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {renderTableNodeToMarkdown} from '@app/features/messaging/utils/markdown/Plaintext';
import {AlertType, TableAlignment} from '@app/features/messaging/utils/markdown/parser/Enums';
import type {
	AlertNode,
	BlockquoteNode,
	HeadingNode,
	ListItem,
	ListNode,
	SequenceNode,
	SubtextNode,
	TableCellNode,
	TableNode,
	TableRowNode,
} from '@app/features/messaging/utils/markdown/parser/Nodes';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import {msg} from '@lingui/core/macro';
import {
	InfoIcon,
	LightbulbFilamentIcon,
	WarningCircleIcon,
	WarningIcon,
	WarningOctagonIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import React, {useEffect, useRef} from 'react';

const NOTE_DESCRIPTOR = msg({
	message: 'Note',
	comment: 'Short label in the messaging block elements. Keep it concise. Keep the tone plain and specific.',
});
const TIP_DESCRIPTOR = msg({
	message: 'Tip',
	comment: 'Short label in the messaging block elements. Keep it concise. Keep the tone plain and specific.',
});
const IMPORTANT_DESCRIPTOR = msg({
	message: 'Important',
	comment: 'Short label in the messaging block elements. Keep it concise. Keep the tone plain and specific.',
});
const WARNING_DESCRIPTOR = msg({
	message: 'Warning',
	comment: 'Warning text in the messaging block elements. Keep the tone plain and specific.',
});
const CAUTION_DESCRIPTOR = msg({
	message: 'Caution',
	comment: 'Short label in the messaging block elements. Keep it concise. Keep the tone plain and specific.',
});

export function BlockquoteRenderer({node, id, renderChildren}: RendererProps<BlockquoteNode>): React.ReactElement {
	const hasChildren = node.children.length > 0;
	const emptyLineCount = Math.max(1, Math.min(node.blankLines ?? 1, 100));
	const emptyLineStyle = {
		'--blockquote-empty-content-min-height': `${emptyLineCount * 1.5}em`,
	} as React.CSSProperties;

	return (
		<div
			key={id}
			className={markupStyles.blockquoteContainer}
			data-flx="messaging.markdown.renderers.common.block-elements.blockquote-renderer.div"
			{...markdownBlockProps(MarkdownBlock.Blockquote)}
		>
			<div
				className={markupStyles.blockquoteDivider}
				data-flx="messaging.markdown.renderers.common.block-elements.blockquote-renderer.div--2"
				{...markdownBlockProps(MarkdownBlock.BlockquoteDivider)}
			/>
			<blockquote
				className={markupStyles.blockquoteContent}
				data-flx="messaging.markdown.renderers.common.block-elements.blockquote-renderer.blockquote"
				{...markdownBlockProps(MarkdownBlock.BlockquoteContent)}
			>
				{hasChildren ? (
					renderChildren(node.children)
				) : (
					<span
						className={markupStyles.blockquoteEmptyContent}
						style={emptyLineStyle}
						aria-hidden="true"
						data-flx="messaging.markdown.renderers.common.block-elements.blockquote-renderer.empty-content"
					/>
				)}
			</blockquote>
		</div>
	);
}

export function ListRenderer({node, id, renderChildren, options}: RendererProps<ListNode>): React.ReactElement {
	const Tag = node.ordered ? 'ol' : 'ul';
	const isInlineContext = isRestrictedInlineContext(options.context);
	if (!node.ordered) {
		return (
			<Tag
				key={id}
				className={clsx(isInlineContext && markupStyles.inlineFormat)}
				data-flx="messaging.markdown.renderers.common.block-elements.list-renderer.tag"
				{...markdownBlockProps(isInlineContext ? undefined : MarkdownBlock.List)}
			>
				{node.items.map((item: ListItem, i: number) => (
					<li
						key={`${id}-item-${i}`}
						className={clsx(isInlineContext && markupStyles.inlineFormat)}
						data-flx="messaging.markdown.renderers.common.block-elements.list-renderer.li"
					>
						{renderChildren(item.children)}
					</li>
				))}
			</Tag>
		);
	}
	const startOrdinal = node.items[0]?.ordinal ?? 1;
	const largestNumber = Math.max(startOrdinal, startOrdinal + node.items.length - 1);
	const listStyle = {
		'--totalCharacters': String(largestNumber).length,
	} as React.CSSProperties;
	return (
		<Tag
			key={id}
			data-flx="messaging.markdown.renderers.common.block-elements.list-renderer.tag--2"
			{...markdownBlockProps(isInlineContext ? undefined : MarkdownBlock.List)}
			className={isInlineContext ? markupStyles.inlineFormat : undefined}
			start={startOrdinal}
			style={listStyle}
		>
			{node.items.map((item, itemIndex) => (
				<li
					key={`${id}-item-${itemIndex}`}
					className={clsx(isInlineContext && markupStyles.inlineFormat)}
					data-flx="messaging.markdown.renderers.common.block-elements.list-renderer.li--2"
				>
					{renderChildren(item.children)}
				</li>
			))}
		</Tag>
	);
}

export function HeadingRenderer({node, id, renderChildren, options}: RendererProps<HeadingNode>): React.ReactElement {
	const Tag = `h${node.level}` as 'h1' | 'h2' | 'h3' | 'h4' | 'h5' | 'h6';
	const isInlineContext = isRestrictedInlineContext(options.context);
	const headingRef = useRef<HTMLHeadingElement>(null);
	useEffect(() => {
		if (headingRef.current && !isInlineContext && node.level <= 3) {
			const headingId = headingRef.current.textContent
				?.toLowerCase()
				.replace(/[^a-z0-9]+/g, '-')
				.replace(/(^-|-$)/g, '');
			if (headingId) {
				headingRef.current.id = headingId;
			}
		}
	}, [isInlineContext, node.level]);
	return (
		<Tag
			ref={headingRef}
			key={id}
			className={clsx(isInlineContext && markupStyles.inlineFormat)}
			data-flx="messaging.markdown.renderers.common.block-elements.heading-renderer.tag"
			{...markdownBlockProps(isInlineContext ? undefined : MarkdownBlock.Heading)}
		>
			{renderChildren(node.children)}
		</Tag>
	);
}

export function SubtextRenderer({node, id, renderChildren, options}: RendererProps<SubtextNode>): React.ReactElement {
	const isInlineContext = isRestrictedInlineContext(options.context);
	if (isInlineContext) {
		return <React.Fragment key={id}>{renderChildren(node.children)}</React.Fragment>;
	}
	return (
		<small
			key={id}
			data-flx="messaging.markdown.renderers.common.block-elements.subtext-renderer.small"
			{...markdownBlockProps(MarkdownBlock.Subtext)}
		>
			{renderChildren(node.children)}
		</small>
	);
}

export function SequenceRenderer({node, id, renderChildren}: RendererProps<SequenceNode>): React.ReactElement {
	return <React.Fragment key={id}>{renderChildren(node.children)}</React.Fragment>;
}

function getTableAlignmentClass(alignment: TableAlignment | undefined): string | undefined {
	switch (alignment) {
		case TableAlignment.Left:
			return markupStyles.alignLeft;
		case TableAlignment.Center:
			return markupStyles.alignCenter;
		case TableAlignment.Right:
			return markupStyles.alignRight;
		default:
			return undefined;
	}
}

function renderTableCell(
	cell: TableCellNode,
	cellIndex: number,
	id: string,
	renderChildren: RendererProps<TableNode>['renderChildren'],
	isHeader: boolean,
	alignment: TableAlignment | undefined,
): React.ReactElement {
	const CellTag = isHeader ? 'th' : 'td';
	return (
		<CellTag
			key={`${id}-cell-${cellIndex}`}
			scope={isHeader ? 'col' : undefined}
			className={clsx(isHeader ? markupStyles.tableHeader : markupStyles.tableCell, getTableAlignmentClass(alignment))}
			data-flx="messaging.markdown.renderers.common.block-elements.render-table-cell.cell-tag"
		>
			{renderChildren(cell.children)}
		</CellTag>
	);
}

function renderTableRow(
	row: TableRowNode,
	rowIndex: number,
	id: string,
	renderChildren: RendererProps<TableNode>['renderChildren'],
	isHeader: boolean,
	alignments: Array<TableAlignment>,
): React.ReactElement {
	return (
		<tr key={`${id}-row-${rowIndex}`} data-flx="messaging.markdown.renderers.common.block-elements.render-table-row.tr">
			{row.cells.map((cell, cellIndex) =>
				renderTableCell(cell, cellIndex, `${id}-row-${rowIndex}`, renderChildren, isHeader, alignments[cellIndex]),
			)}
		</tr>
	);
}

export function TableRenderer({node, id, renderChildren, options}: RendererProps<TableNode>): React.ReactElement {
	const copyText = renderTableNodeToMarkdown(node, {
		channelId: options.channelId,
		preserveMarkdown: false,
		includeEmojiNames: true,
		i18n: options.i18n,
	});
	return (
		<div
			key={id}
			className={markupStyles.tableContainer}
			data-message-copy-block="true"
			data-message-copy-table="true"
			data-message-copy-text={copyText}
			data-flx="messaging.markdown.renderers.common.block-elements.table-renderer.div"
			{...markdownBlockProps(MarkdownBlock.TableContainer)}
		>
			<table
				className={markupStyles.table}
				data-flx="messaging.markdown.renderers.common.block-elements.table-renderer.table"
				{...markdownTableProps()}
			>
				<thead data-flx="messaging.markdown.renderers.common.block-elements.table-renderer.thead">
					{renderTableRow(node.header, 0, `${id}-header`, renderChildren, true, node.alignments)}
				</thead>
				<tbody data-flx="messaging.markdown.renderers.common.block-elements.table-renderer.tbody">
					{node.rows.map((row, rowIndex) =>
						renderTableRow(row, rowIndex, `${id}-body`, renderChildren, false, node.alignments),
					)}
				</tbody>
			</table>
		</div>
	);
}

export function AlertRenderer({node, id, renderChildren, options}: RendererProps<AlertNode>): React.ReactElement {
	const i18n = options.i18n!;
	const alertConfig: Record<
		AlertType,
		{
			Icon: React.ComponentType<{className?: string}>;
			className: string;
			title: string;
		}
	> = {
		[AlertType.Note]: {Icon: InfoIcon, className: markupStyles.alertNote, title: i18n._(NOTE_DESCRIPTOR)},
		[AlertType.Tip]: {Icon: LightbulbFilamentIcon, className: markupStyles.alertTip, title: i18n._(TIP_DESCRIPTOR)},
		[AlertType.Important]: {
			Icon: WarningIcon,
			className: markupStyles.alertImportant,
			title: i18n._(IMPORTANT_DESCRIPTOR),
		},
		[AlertType.Warning]: {
			Icon: WarningOctagonIcon,
			className: markupStyles.alertWarning,
			title: i18n._(WARNING_DESCRIPTOR),
		},
		[AlertType.Caution]: {
			Icon: WarningCircleIcon,
			className: markupStyles.alertCaution,
			title: i18n._(CAUTION_DESCRIPTOR),
		},
	};
	const {Icon, className, title} = alertConfig[node.alertType as AlertType] || alertConfig[AlertType.Note];
	return (
		<div
			key={id}
			className={clsx(markupStyles.alert, className)}
			data-flx="messaging.markdown.renderers.common.block-elements.alert-renderer.div"
			{...markdownBlockProps(MarkdownBlock.Alert)}
		>
			<div
				className={markupStyles.alertTitle}
				data-flx="messaging.markdown.renderers.common.block-elements.alert-renderer.div--2"
			>
				<Icon
					className={markupStyles.alertIcon}
					data-flx="messaging.markdown.renderers.common.block-elements.alert-renderer.icon"
				/>
				{title}
			</div>
			<div
				className={markupStyles.alertContent}
				data-flx="messaging.markdown.renderers.common.block-elements.alert-renderer.div--3"
			>
				{renderChildren(node.children)}
			</div>
		</div>
	);
}
