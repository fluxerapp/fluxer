// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	isRestrictedInlineContext,
	type RendererProps,
} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import type {TextNode} from '@app/features/messaging/utils/markdown/parser/Nodes';
import type React from 'react';

export function TextRenderer({node, id, options}: RendererProps<TextNode>): React.ReactElement {
	let content = node.content;
	if (isRestrictedInlineContext(options.context)) {
		content = content.replace(/\n/g, ' ').replace(/\s+/g, ' ');
	}
	return (
		<span key={id} data-flx="messaging.markdown.renderers.text-renderer.span">
			{content}
		</span>
	);
}
