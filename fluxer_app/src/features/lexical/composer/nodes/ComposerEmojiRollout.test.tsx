// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {runInAction} from 'mobx';
import type React from 'react';
import {renderToStaticMarkup} from 'react-dom/server';
import {afterEach, describe, expect, it, vi} from 'vitest';

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
vi.mock('@app/features/ui/emoji_tooltip_content/EmojiWithTooltip', () => ({
	EmojiWithTooltip: ({emojiName, children}: {emojiName: string; children: React.ReactNode}) => (
		<span data-test-hover-tooltip={emojiName} data-flx="test.emoji-with-tooltip">
			{children}
		</span>
	),
}));

const {ComposerCustomEmoji} = await import('@app/features/lexical/composer/nodes/ComposerCustomEmoji');
const {ComposerStandardEmoji} = await import('@app/features/lexical/composer/nodes/ComposerStandardEmoji');
const {default: ExperimentAssignments} = await import('@app/features/experiment/state/ExperimentAssignments');

function setBucket(enabled: boolean): void {
	runInAction(() => {
		ExperimentAssignments.response = {
			poll_interval_seconds: 300,
			poll_jitter_percent: 15,
			assignments: {
				expression_info_card: {enabled, config_version: 1, user_targeted: enabled, source: 'canary'},
			},
		};
	});
}

function renderCustom(): string {
	return renderToStaticMarkup(
		<ComposerCustomEmoji emojiId="10" animated={false} display=":blob:" data-flx="test.composer-custom-emoji" />,
	);
}

function renderStandard(): string {
	return renderToStaticMarkup(
		<ComposerStandardEmoji
			name="slight_smile"
			surrogate="🙂"
			url="https://example.invalid/slight_smile.png"
			display=":slight_smile:"
			data-flx="test.composer-standard-emoji"
		/>,
	);
}

afterEach(() => {
	ExperimentAssignments.reset();
});

describe('ComposerCustomEmoji', () => {
	it('renders the old hover tooltip in the control arm', () => {
		const markup = renderCustom();
		expect(markup).toContain('data-test-hover-tooltip=":blob:"');
		expect(markup).not.toContain('data-test-tooltip');
	});

	it('renders the plain name tooltip in the experiment arm', () => {
		setBucket(true);
		const markup = renderCustom();
		expect(markup).toContain('data-test-tooltip=":blob:"');
		expect(markup).not.toContain('data-test-hover-tooltip');
	});
});

describe('ComposerStandardEmoji', () => {
	it('renders the old hover tooltip in the control arm', () => {
		const markup = renderStandard();
		expect(markup).toContain('data-test-hover-tooltip=":slight_smile:"');
		expect(markup).not.toContain('data-test-tooltip');
	});

	it('renders the plain name tooltip in the experiment arm', () => {
		setBucket(true);
		const markup = renderStandard();
		expect(markup).toContain('data-test-tooltip=":slight_smile:"');
		expect(markup).not.toContain('data-test-hover-tooltip');
	});
});
