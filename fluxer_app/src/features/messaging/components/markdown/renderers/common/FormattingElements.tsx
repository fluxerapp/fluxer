// SPDX-License-Identifier: AGPL-3.0-or-later

import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {MarkdownContext, type RendererProps} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {NodeType} from '@app/features/messaging/utils/markdown/parser/Enums';
import type {FormattingNode, Node} from '@app/features/messaging/utils/markdown/parser/Nodes';
import {normalizeUrl, useSpoilerState} from '@app/features/messaging/utils/SpoilerUtils';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {msg} from '@lingui/core/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

const CLICK_TO_REVEAL_SPOILER_DESCRIPTOR = msg({
	message: 'Click to reveal spoiler',
	comment: 'Label in the messaging formatting elements.',
});

export function StrongRenderer({node, id, renderChildren}: RendererProps<FormattingNode>): React.ReactElement {
	return (
		<strong key={id} data-flx="messaging.markdown.renderers.common.formatting-elements.strong-renderer.strong">
			{renderChildren(node.children)}
		</strong>
	);
}

export function EmphasisRenderer({node, id, renderChildren}: RendererProps<FormattingNode>): React.ReactElement {
	return (
		<em key={id} data-flx="messaging.markdown.renderers.common.formatting-elements.emphasis-renderer.em">
			{renderChildren(node.children)}
		</em>
	);
}

export function UnderlineRenderer({node, id, renderChildren}: RendererProps<FormattingNode>): React.ReactElement {
	return (
		<u key={id} data-flx="messaging.markdown.renderers.common.formatting-elements.underline-renderer.u">
			{renderChildren(node.children)}
		</u>
	);
}

export function StrikethroughRenderer({node, id, renderChildren}: RendererProps<FormattingNode>): React.ReactElement {
	return (
		<s key={id} data-flx="messaging.markdown.renderers.common.formatting-elements.strikethrough-renderer.s">
			{renderChildren(node.children)}
		</s>
	);
}

interface SpoilerNode extends FormattingNode {
	type: 'Spoiler';
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
			if (!isKeyboardActivationKey(event.key)) return;
			event.preventDefault();
			handleClick();
		},
		[handleClick],
	);
	const isBlock = node.isBlock && options.context !== MarkdownContext.RESTRICTED_INLINE_REPLY;
	const wrapperClass = isBlock ? markupStyles.blockSpoilerWrapper : markupStyles.spoilerWrapper;
	const spoilerClass = isBlock ? markupStyles.blockSpoiler : markupStyles.spoiler;
	const shouldReveal = !hidden || autoRevealed;
	const revealTriggerProps = shouldReveal
		? {}
		: {
				onClick: handleClick,
				onKeyDown: handleKeyDown,
				role: 'button',
				'aria-label': i18n._(CLICK_TO_REVEAL_SPOILER_DESCRIPTOR),
			};
	return (
		<span
			key={id}
			className={wrapperClass}
			data-flx="messaging.markdown.renderers.common.formatting-elements.spoiler-renderer.span"
		>
			<FocusRing
				offset={-2}
				data-flx="messaging.markdown.renderers.common.formatting-elements.spoiler-renderer.focus-ring"
			>
				<span
					className={spoilerClass}
					data-revealed={shouldReveal}
					tabIndex={shouldReveal ? -1 : 0}
					{...revealTriggerProps}
					data-flx="messaging.markdown.renderers.common.formatting-elements.spoiler-renderer.button.click"
				>
					<span
						className={markupStyles.spoilerContent}
						aria-hidden={!shouldReveal}
						data-flx="messaging.markdown.renderers.common.formatting-elements.spoiler-renderer.span--3"
					>
						{renderChildren(node.children)}
					</span>
				</span>
			</FocusRing>
		</span>
	);
});
