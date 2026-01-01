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

import {observer} from 'mobx-react-lite';
import type React from 'react';
import type {TextNode} from '../parser/types/nodes';
import {MarkdownContext, type RendererProps} from '.';

export const TextRenderer = observer(function TextRenderer({
	node,
	id,
	options,
}: RendererProps<TextNode>): React.ReactElement {
	let content = node.content;

	if (options.context === MarkdownContext.RESTRICTED_INLINE_REPLY) {
		content = content.replace(/\n/g, ' ').replace(/\s+/g, ' ');
	}

	return <span key={id}>{content}</span>;
});
