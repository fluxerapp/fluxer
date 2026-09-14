// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {ComposerStandardEmoji} from '@app/features/lexical/composer/nodes/ComposerStandardEmoji';
import type React from 'react';
import {act, createElement} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/expressions/utils/EmojiUtils', () => ({getEmojiURL: (surrogate: string) => `url:${surrogate}`}));

vi.mock('@app/features/expressions/state/ExpressionInfoCardRollout', () => ({default: {enabled: false}}));

vi.mock('@app/features/ui/tooltip/Tooltip', () => ({
	Tooltip: ({children}: {children: React.ReactNode}) => children,
}));

vi.mock('@app/features/ui/emoji_tooltip_content/EmojiWithTooltip', () => ({
	EmojiWithTooltip: ({children}: {children: React.ReactNode}) => children,
}));

(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;

describe('ComposerStandardEmoji', () => {
	let host: HTMLDivElement;
	let root: Root;

	beforeEach(() => {
		host = document.createElement('div');
		document.body.append(host);
		root = createRoot(host);
	});

	afterEach(() => {
		act(() => {
			root.unmount();
		});
		document.body.replaceChildren();
	});

	function render(url: string | null): HTMLImageElement | null {
		act(() => {
			root.render(
				createElement(ComposerStandardEmoji, {name: 'grinning', surrogate: '😀', url, display: ':grinning:'}),
			);
		});
		return host.querySelector('img');
	}

	it('keeps the image the node was created with', () => {
		expect(render('url:given')?.getAttribute('src')).toBe('url:given');
	});

	it('derives the image from the surrogate for a node created without one', () => {
		expect(render(null)?.getAttribute('src')).toBe('url:😀');
	});
});
