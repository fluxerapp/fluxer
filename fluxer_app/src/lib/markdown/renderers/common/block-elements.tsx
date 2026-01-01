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

import {msg} from '@lingui/core/macro';
import {
	CircleWavyWarningIcon,
	InfoIcon,
	LightbulbFilamentIcon,
	WarningCircleIcon,
	WarningIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React, {useEffect, useRef} from 'react';
import markupStyles from '~/styles/Markup.module.css';
import {AlertType} from '../../parser/types/enums';
import type {
	AlertNode,
	BlockquoteNode,
	HeadingNode,
	ListItem,
	ListNode,
	SequenceNode,
	SubtextNode,
	TableNode,
} from '../../parser/types/nodes';
import {MarkdownContext, type RendererProps} from '..';

export const BlockquoteRenderer = observer(function BlockquoteRenderer({
	node,
	id,
	renderChildren,
}: RendererProps<BlockquoteNode>): React.ReactElement {
	return (
		<div key={id} className={markupStyles.blockquoteContainer}>
			<div className={markupStyles.blockquoteDivider} />
			<blockquote className={markupStyles.blockquoteContent}>{renderChildren(node.children)}</blockquote>
		</div>
	);
});

export const ListRenderer = observer(function ListRenderer({
	node,
	id,
	renderChildren,
	options,
}: RendererProps<ListNode>): React.ReactElement {
	const Tag = node.ordered ? 'ol' : 'ul';
	const isInlineContext = options.context === MarkdownContext.RESTRICTED_INLINE_REPLY;

	if (!node.ordered) {
		return (
			<Tag key={id} className={clsx(isInlineContext && markupStyles.inlineFormat)}>
				{node.items.map((item, i) => (
					<li key={`${id}-item-${i}`} className={clsx(isInlineContext && markupStyles.inlineFormat)}>
						{renderChildren(item.children)}
					</li>
				))}
			</Tag>
		);
	}

	const segments: Array<{startOrdinal: number; items: Array<ListItem>}> = [];
	let currentSegment: Array<ListItem> = [];
	let currentOrdinal = node.items[0]?.ordinal || 1;

	node.items.forEach((item, i) => {
		const itemOrdinal = item.ordinal !== undefined ? item.ordinal : i === 0 ? 1 : currentOrdinal + 1;

		if (itemOrdinal !== currentOrdinal && i > 0) {
			segments.push({
				startOrdinal: currentSegment[0].ordinal || 1,
				items: [...currentSegment],
			});
			currentSegment = [];
		}

		currentSegment.push({...item, ordinal: itemOrdinal});
		currentOrdinal = itemOrdinal + 1;
	});

	if (currentSegment.length > 0) {
		segments.push({
			startOrdinal: currentSegment[0].ordinal || 1,
			items: [...currentSegment],
		});
	}

	return (
		<React.Fragment key={id}>
			{segments.map((segment, segmentIndex) => {
				let maxDigits = 1;
				if (node.items.length > 0) {
					const largestNumber = Math.max(segment.startOrdinal, segment.startOrdinal + segment.items.length - 1);
					maxDigits = String(largestNumber).length;
				}

				const listStyle = {
					'--totalCharacters': maxDigits,
				} as React.CSSProperties;

				return (
					<Tag
						key={`${id}-segment-${segmentIndex}`}
						className={isInlineContext ? markupStyles.inlineFormat : undefined}
						start={segment.startOrdinal}
						style={listStyle}
					>
						{segment.items.map((item, itemIndex) => (
							<li
								key={`${id}-segment-${segmentIndex}-item-${itemIndex}`}
								className={clsx(isInlineContext && markupStyles.inlineFormat)}
							>
								{renderChildren(item.children)}
							</li>
						))}
					</Tag>
				);
			})}
		</React.Fragment>
	);
});

export const HeadingRenderer = observer(function HeadingRenderer({
	node,
	id,
	renderChildren,
	options,
}: RendererProps<HeadingNode>): React.ReactElement {
	const Tag = `h${node.level}` as 'h1' | 'h2' | 'h3' | 'h4' | 'h5' | 'h6';
	const isInlineContext = options.context === MarkdownContext.RESTRICTED_INLINE_REPLY;

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
		<Tag ref={headingRef} key={id} className={clsx(isInlineContext && markupStyles.inlineFormat)}>
			{renderChildren(node.children)}
		</Tag>
	);
});

export const SubtextRenderer = observer(function SubtextRenderer({
	node,
	id,
	renderChildren,
	options,
}: RendererProps<SubtextNode>): React.ReactElement {
	const isInlineContext = options.context === MarkdownContext.RESTRICTED_INLINE_REPLY;

	return (
		<small key={id} className={clsx(isInlineContext && markupStyles.inlineFormat)}>
			{renderChildren(node.children)}
		</small>
	);
});

export const SequenceRenderer = observer(function SequenceRenderer({
	node,
	id,
	renderChildren,
}: RendererProps<SequenceNode>): React.ReactElement {
	return <React.Fragment key={id}>{renderChildren(node.children)}</React.Fragment>;
});

export const TableRenderer = observer(function TableRenderer(_props: RendererProps<TableNode>): React.ReactElement {
	throw new Error('unsupported');
});

export const AlertRenderer = observer(function AlertRenderer({
	node,
	id,
	renderChildren,
	options,
}: RendererProps<AlertNode>): React.ReactElement {
	const i18n = options.i18n!;
	const alertConfig: Record<
		AlertType,
		{
			Icon: React.ComponentType<{className?: string}>;
			className: string;
			title: string;
		}
	> = {
		[AlertType.Note]: {Icon: InfoIcon, className: markupStyles.alertNote, title: i18n._(msg`Note`)},
		[AlertType.Tip]: {Icon: LightbulbFilamentIcon, className: markupStyles.alertTip, title: i18n._(msg`Tip`)},
		[AlertType.Important]: {Icon: WarningIcon, className: markupStyles.alertImportant, title: i18n._(msg`Important`)},
		[AlertType.Warning]: {
			Icon: CircleWavyWarningIcon,
			className: markupStyles.alertWarning,
			title: i18n._(msg`Warning`),
		},
		[AlertType.Caution]: {Icon: WarningCircleIcon, className: markupStyles.alertCaution, title: i18n._(msg`Caution`)},
	};

	const {Icon, className, title} = alertConfig[node.alertType] || alertConfig[AlertType.Note];

	return (
		<div key={id} className={clsx(markupStyles.alert, className)}>
			<div className={markupStyles.alertTitle}>
				<Icon className={markupStyles.alertIcon} />
				{title}
			</div>
			<div className={markupStyles.alertContent}>{renderChildren(node.children)}</div>
		</div>
	);
});
