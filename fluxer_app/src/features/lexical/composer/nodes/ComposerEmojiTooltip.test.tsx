// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type React from 'react';
import {renderToStaticMarkup} from 'react-dom/server';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});
vi.mock('@lingui/react/macro', () => ({
	useLingui: () => ({i18n: {_: (descriptor: {message?: string}) => descriptor.message ?? '', locale: 'en'}}),
}));
vi.mock('@app/features/app/hooks/useShouldAnimate', () => ({useShouldAnimate: () => false}));
vi.mock('@app/features/emoji/state/Emoji', () => ({default: {getEmojiById: () => null}}));
vi.mock('@app/features/guild/state/Guilds', () => ({default: {getGuild: () => null}}));
vi.mock('@app/features/expressions/utils/EmojiUtils', () => ({getEmojiURL: (surrogate: string) => `url:${surrogate}`}));
vi.mock('@app/features/messaging/utils/markdown/EmojiDetector', () => ({
	getEmojiRenderUrl: () => 'https://example.invalid/emoji.png',
}));
vi.mock('@app/features/ui/tooltip/Tooltip', () => ({
	Tooltip: ({text, children}: {text: unknown; children: React.ReactNode}) => (
		<span data-test-tooltip={typeof text === 'string' ? text : 'render-prop'} data-flx="test.tooltip">
			{children}
		</span>
	),
}));

const {ComposerCustomEmoji} = await import('@app/features/lexical/composer/nodes/ComposerCustomEmoji');
const {ComposerStandardEmoji} = await import('@app/features/lexical/composer/nodes/ComposerStandardEmoji');

function renderCustom(): string {
	return renderToStaticMarkup(
		<ComposerCustomEmoji emojiId="10" animated={false} display=":blob:" data-flx="test.composer-custom-emoji" />,
	);
}

function renderStandard(): string {
	return renderToStaticMarkup(
		<ComposerStandardEmoji
			surrogate="🙂"
			url="https://example.invalid/slight_smile.png"
			display=":slight_smile:"
			data-flx="test.composer-standard-emoji"
		/>,
	);
}

describe('ComposerCustomEmoji', () => {
	it('renders the plain name tooltip', () => {
		expect(renderCustom()).toContain('data-test-tooltip=":blob:"');
	});
});

describe('ComposerStandardEmoji', () => {
	it('renders the plain name tooltip', () => {
		expect(renderStandard()).toContain('data-test-tooltip=":slight_smile:"');
	});
});
