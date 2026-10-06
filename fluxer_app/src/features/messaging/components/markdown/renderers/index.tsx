// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	AlertRenderer,
	BlockquoteRenderer,
	HeadingRenderer,
	ListRenderer,
	SequenceRenderer,
	SubtextRenderer,
	TableRenderer,
} from '@app/features/messaging/components/markdown/renderers/common/BlockElements';
import {
	CodeBlockRenderer,
	InlineCodeRenderer,
} from '@app/features/messaging/components/markdown/renderers/common/CodeElements';
import {
	EmphasisRenderer,
	SpoilerRenderer,
	StrikethroughRenderer,
	StrongRenderer,
	UnderlineRenderer,
} from '@app/features/messaging/components/markdown/renderers/common/FormattingElements';
import {EmojiRenderer} from '@app/features/messaging/components/markdown/renderers/EmojiRenderer';
import {LinkRenderer} from '@app/features/messaging/components/markdown/renderers/LinkRenderer';
import {MentionRenderer} from '@app/features/messaging/components/markdown/renderers/MentionRenderer';
import {
	isRestrictedInlineContext,
	MarkdownContext,
	type MarkdownParseOptions,
	type MarkdownRenderOptions,
	type RendererProps,
} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {TextRenderer} from '@app/features/messaging/components/markdown/renderers/TextRenderer';
import {TimestampRenderer} from '@app/features/messaging/components/markdown/renderers/TimestampRenderer';
import {shouldRenderAsJumboEmojis} from '@app/features/messaging/utils/markdown/JumboDetector';
import {parseMarkdownContent} from '@app/features/messaging/utils/markdown/MarkdownParseCache';
import {getParserFlagsForContext} from '@app/features/messaging/utils/markdown/MarkdownParserFlags';
import {NodeType} from '@app/features/messaging/utils/markdown/parser/Enums';
import type {Node} from '@app/features/messaging/utils/markdown/parser/Nodes';
import {Logger} from '@app/features/platform/utils/AppLogger';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import {i18n} from '@lingui/core';
import React from 'react';

const logger = new Logger('MarkdownRenderers');

export {getParserFlagsForContext};

export const parse = parseMarkdownContent;

const renderers = {
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
} as Record<NodeType, React.ComponentType<RendererProps>>;

function renderNode(node: Node, id: string, options: MarkdownRenderOptions): React.ReactNode {
	const renderer = renderers[node.type];
	if (!renderer) {
		logger.warn(`No renderer found for node type: ${node.type}`);
		return null;
	}
	const renderChildrenFn = (children: Array<Node>, optionOverrides?: Partial<MarkdownRenderOptions>) => {
		const childOptions = optionOverrides ? {...options, ...optionOverrides} : options;
		return children.map((child, i) => renderNode(child, `${id}-${i}`, childOptions));
	};
	return React.createElement(renderer, {
		node,
		id,
		renderChildren: renderChildrenFn,
		options,
		key: id,
	});
}

export function render(nodes: Array<Node>, options: MarkdownParseOptions): React.ReactNode {
	const shouldJumboEmojis = options.context === MarkdownContext.STANDARD_WITH_JUMBO && shouldRenderAsJumboEmojis(nodes);
	const renderOptions: MarkdownRenderOptions = {
		...options,
		shouldJumboEmojis,
		i18n,
	};
	return nodes.map((node, i) => renderNode(node, `${options.context}-${i}`, renderOptions));
}

export function wrapRenderedContent(content: React.ReactNode, context: MarkdownContext): React.ReactNode {
	if (isRestrictedInlineContext(context)) {
		return (
			<div className={markupStyles.inlineFormat} data-flx="messaging.markdown.renderers.wrap-rendered-content.div">
				{content}
			</div>
		);
	}
	return content;
}
