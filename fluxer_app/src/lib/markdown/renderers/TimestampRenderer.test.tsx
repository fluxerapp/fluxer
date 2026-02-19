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

import {
	MarkdownContext,
	type MarkdownRenderOptions,
	type RendererProps,
} from '@app/lib/markdown/renderers/RendererTypes';
import {TimestampRenderer} from '@app/lib/markdown/renderers/TimestampRenderer';
import {NodeType, TimestampStyle} from '@fluxer/markdown_parser/src/types/Enums';
import type {TimestampNode} from '@fluxer/markdown_parser/src/types/Nodes';
import {setupI18n} from '@lingui/core';
import React from 'react';
import {renderToStaticMarkup} from 'react-dom/server';
import {describe, expect, test} from 'vitest';

const i18n = setupI18n({locale: 'en-US', messages: {'en-US': {}}});

function createRendererProps(node: TimestampNode): RendererProps<TimestampNode> {
	const options: MarkdownRenderOptions = {
		context: MarkdownContext.STANDARD_WITHOUT_JUMBO,
		shouldJumboEmojis: false,
		i18n,
	};

	return {
		node,
		id: 'test-timestamp',
		renderChildren: () => null,
		options,
	};
}

describe('TimestampRenderer', () => {
	test('renders a non-throwing fallback for invalid timestamps', () => {
		const props = createRendererProps({
			type: NodeType.Timestamp,
			timestamp: Number.POSITIVE_INFINITY,
			style: TimestampStyle.ShortDateTime,
		});

		const renderFn = () => renderToStaticMarkup(React.createElement(TimestampRenderer, props));

		expect(renderFn).not.toThrow();
		const markup = renderFn();
		expect(markup).toContain('Infinity');
		expect(markup).not.toContain('<time');
	});

	test('renders a semantic time element for valid timestamps', () => {
		const props = createRendererProps({
			type: NodeType.Timestamp,
			timestamp: 1618953630,
			style: TimestampStyle.ShortDateTime,
		});

		const markup = renderToStaticMarkup(React.createElement(TimestampRenderer, props));

		expect(markup).toContain('<time');
		expect(markup).toContain('dateTime="2021-04-20T21:20:30.000Z"');
	});
});
