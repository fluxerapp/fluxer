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
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';
import markupStyles from '~/styles/Markup.module.css';
import {normalizeUrl, useSpoilerState} from '~/utils/SpoilerUtils';
import {NodeType} from '../../parser/types/enums';
import type {FormattingNode, Node} from '../../parser/types/nodes';
import type {RendererProps} from '..';

export const StrongRenderer = observer(function StrongRenderer({
	node,
	id,
	renderChildren,
}: RendererProps<FormattingNode>): React.ReactElement {
	return <strong key={id}>{renderChildren(node.children)}</strong>;
});

export const EmphasisRenderer = observer(function EmphasisRenderer({
	node,
	id,
	renderChildren,
}: RendererProps<FormattingNode>): React.ReactElement {
	return <em key={id}>{renderChildren(node.children)}</em>;
});

export const UnderlineRenderer = observer(function UnderlineRenderer({
	node,
	id,
	renderChildren,
}: RendererProps<FormattingNode>): React.ReactElement {
	return <u key={id}>{renderChildren(node.children)}</u>;
});

export const StrikethroughRenderer = observer(function StrikethroughRenderer({
	node,
	id,
	renderChildren,
}: RendererProps<FormattingNode>): React.ReactElement {
	return <s key={id}>{renderChildren(node.children)}</s>;
});

interface SpoilerNode extends FormattingNode {
	type: typeof NodeType.Spoiler;
	isBlock: boolean;
}

export const SpoilerRenderer = observer(function SpoilerRenderer({
	node,
	id,
	renderChildren,
	options,
}: RendererProps<SpoilerNode>): React.ReactElement {
	const i18n = options.i18n!;
	const collectUrls = useCallback((nodes: Array<Node>): Array<string> => {
		const urls: Array<string> = [];
		for (const child of nodes) {
			if (child.type === NodeType.Link) {
				const normalized = normalizeUrl(child.url);
				if (normalized) urls.push(normalized);
			}

			if ('children' in child && Array.isArray((child as {children?: Array<Node>}).children)) {
				urls.push(...collectUrls((child as {children: Array<Node>}).children));
			}
		}
		return urls;
	}, []);

	const spoilerUrls = useMemo(() => Array.from(new Set(collectUrls(node.children))), [collectUrls, node.children]);
	const {hidden, reveal, autoRevealed} = useSpoilerState(true, options.channelId, spoilerUrls);

	const handleClick = useCallback(() => {
		if (hidden) {
			reveal();
		}
	}, [hidden, reveal]);

	const handleKeyDown = useCallback(
		(event: React.KeyboardEvent) => {
			if (event.key === 'Enter' || event.key === ' ') {
				event.preventDefault();
				handleClick();
			}
		},
		[handleClick],
	);

	const isBlock = node.isBlock;
	const wrapperClass = isBlock ? markupStyles.blockSpoilerWrapper : markupStyles.spoilerWrapper;
	const spoilerClass = isBlock ? markupStyles.blockSpoiler : markupStyles.spoiler;

	const shouldReveal = !hidden || autoRevealed;

	return (
		<span key={id} className={wrapperClass}>
			{shouldReveal ? (
				<span className={spoilerClass} data-revealed={shouldReveal}>
					<span className={markupStyles.spoilerContent} aria-hidden={!shouldReveal}>
						{renderChildren(node.children)}
					</span>
				</span>
			) : (
				<span
					className={spoilerClass}
					data-revealed={shouldReveal}
					onClick={handleClick}
					onKeyDown={handleKeyDown}
					role="button"
					tabIndex={0}
					aria-label={i18n._(msg`Click to reveal spoiler`)}
				>
					<span className={markupStyles.spoilerContent} aria-hidden>
						{renderChildren(node.children)}
					</span>
				</span>
			)}
		</span>
	);
});
