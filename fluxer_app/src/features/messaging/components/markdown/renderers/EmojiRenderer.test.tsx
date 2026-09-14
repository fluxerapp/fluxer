// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MarkdownRenderOptions} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import type {EmojiNode} from '@app/features/messaging/utils/markdown/parser/Nodes';
import {installVoiceMenuTestBootstrap} from '@app/features/ui/action_menu/items/__fixtures__/VoiceMenuTestBootstrap';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {runInAction} from 'mobx';
import type React from 'react';
import {act} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {renderToStaticMarkup} from 'react-dom/server';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});
vi.mock('@lingui/react/macro', () => ({
	Trans: ({children}: {children?: React.ReactNode}) => <span data-flx="test.trans">{children}</span>,
	useLingui: () => ({i18n: {_: (descriptor: {message?: string}) => descriptor.message ?? '', locale: 'en'}}),
}));
vi.mock('@app/features/ui/tooltip/Tooltip', () => ({
	Tooltip: ({text, children}: {text: unknown; children: React.ReactNode}) => (
		<span data-test-tooltip={typeof text === 'string' ? text : 'render-prop'} data-flx="test.tooltip">
			{children}
		</span>
	),
}));
vi.mock('@app/features/ui/popover/PopoverPopout', () => ({
	openPopout: vi.fn(),
	Popout: ({children}: {children: React.ReactNode}) => (
		<span data-test-popout="true" data-flx="test.popout">
			{children}
		</span>
	),
}));
vi.mock('@app/features/presence/state/LocalPresence', () => ({
	default: {updatePresence: vi.fn()},
	setLocalPresenceUserSettings: vi.fn(),
	ACCOUNT_PRESENCE_INTENT_MAX_AGE_MS: 60_000,
}));
vi.mock('@app/features/messaging/state/MessagingMessages', () => ({default: {getMessage: () => null}}));
vi.mock('@app/features/channel/components/MessageActionUtils', () => ({requestDeleteMessage: vi.fn()}));
vi.mock('@app/features/ui/commands/ContextMenuCommands', () => ({openFromEvent: vi.fn()}));
vi.mock('@app/features/ui/action_menu/MessageContextMenu', () => ({MessageContextMenu: () => null}));
vi.mock('@app/features/ui/action_menu/items/EmojiContextMenuItems', () => ({
	EmojiContextMenuItems: () => null,
	EmojiInlineMenuItems: () => null,
}));
vi.mock('@app/features/expressions/components/ExpressionInfoCard', () => ({
	ExpressionInfoCard: ({displayName}: {displayName: string}) => (
		<span data-test-info-card={displayName} data-flx="test.info-card" />
	),
}));
vi.mock('@app/features/expressions/components/bottomsheets/ExpressionInfoBottomSheet', () => ({
	ExpressionInfoBottomSheet: ({isOpen}: {isOpen: boolean}) => (
		<span data-test-bottom-sheet={String(isOpen)} data-flx="test.bottom-sheet" />
	),
}));
vi.mock('@app/features/ui/emoji_tooltip_content/EmojiWithTooltip', () => ({
	EmojiWithTooltip: ({emojiName, children}: {emojiName: string; children: React.ReactNode}) => (
		<span data-test-hover-tooltip={emojiName} data-flx="test.emoji-with-tooltip">
			{children}
		</span>
	),
}));
vi.mock('@app/features/emoji/components/bottomsheets/EmojiInfoBottomSheet', () => ({
	EmojiInfoBottomSheet: ({isOpen}: {isOpen: boolean}) => (
		<span data-test-legacy-sheet={String(isOpen)} data-flx="test.legacy-sheet" />
	),
}));

installVoiceMenuTestBootstrap();

const {EmojiRenderer} = await import('@app/features/messaging/components/markdown/renderers/EmojiRenderer');
const {default: MobileLayout} = await import('@app/features/ui/state/MobileLayout');
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

function resetBucket(): void {
	ExperimentAssignments.reset();
}

const i18n = {
	locale: 'en',
	_: (descriptor: MessageDescriptor, values?: Record<string, unknown>) =>
		(descriptor.message ?? '').replace(/\{(\w+)\}/gu, (_match, key: string) => String(values?.[key] ?? `{${key}}`)),
} as unknown as I18n;

const BASE_OPTIONS: MarkdownRenderOptions = {
	context: MarkdownContext.STANDARD_WITH_JUMBO,
	shouldJumboEmojis: false,
	i18n,
};

const CUSTOM_EMOJI = {
	type: 'Emoji',
	kind: {kind: 'Custom', name: 'blob', id: '10', animated: false},
} as unknown as EmojiNode;
const STANDARD_EMOJI = {
	type: 'Emoji',
	kind: {kind: 'Standard', raw: '🙂', codepoints: '1f642', name: 'slight_smile'},
} as unknown as EmojiNode;

const PLAIN_FLAGS = ['disableInteractions', 'disableEmojiInteractions'] as const;

function renderEmoji(node: EmojiNode, overrides: Partial<MarkdownRenderOptions> = {}): string {
	return renderToStaticMarkup(
		<EmojiRenderer
			node={node}
			id="emoji-key"
			renderChildren={() => null}
			options={{...BASE_OPTIONS, ...overrides}}
			data-flx="test.emoji-renderer"
		/>,
	);
}

function expectNoTooltip(markup: string): void {
	expect(markup).not.toContain('data-test-tooltip');
}

function expectNoCard(markup: string): void {
	expect(markup).not.toContain('data-test-popout');
	expect(markup).not.toContain('data-emoji-interactive');
}

function expectNoTabStop(markup: string): void {
	expect(markup).not.toContain('tabindex');
	expect(markup).not.toContain('role="button"');
}

describe('EmojiRenderer on desktop in the experiment arm', () => {
	beforeEach(() => {
		setBucket(true);
	});

	afterEach(resetBucket);

	it('renders the interactive info card trigger by default', () => {
		const markup = renderEmoji(CUSTOM_EMOJI);
		expect(markup).toContain('data-test-popout="true"');
		expect(markup).toContain('role="button"');
		expect(markup).toContain('tabindex="0"');
		expect(markup).toContain('data-emoji-interactive="true"');
		expect(markup).toContain('aria-label=":blob:"');
		expectNoTooltip(markup);
	});

	for (const flag of PLAIN_FLAGS) {
		it(`renders a completely plain emoji when ${flag} is set`, () => {
			const markup = renderEmoji(CUSTOM_EMOJI, {[flag]: true});
			expectNoTooltip(markup);
			expectNoCard(markup);
			expectNoTabStop(markup);
			expect(markup).toContain('aria-label=":blob:"');
		});
	}

	it('renders the plain name tooltip and no card when disableEmojiInfoCard is set', () => {
		const markup = renderEmoji(CUSTOM_EMOJI, {disableEmojiInfoCard: true});
		expect(markup).toContain('data-test-tooltip=":blob:"');
		expectNoCard(markup);
		expectNoTabStop(markup);
		expect(markup).toContain('aria-label=":blob:"');
	});

	it('keeps disableEmojiInfoCard distinct from the two plain flags', () => {
		const infoCardOff = renderEmoji(CUSTOM_EMOJI, {disableEmojiInfoCard: true});
		for (const flag of PLAIN_FLAGS) {
			expect(renderEmoji(CUSTOM_EMOJI, {[flag]: true})).not.toBe(infoCardOff);
		}
	});
});

describe('EmojiRenderer on mobile in the experiment arm', () => {
	beforeEach(() => {
		setBucket(true);
		runInAction(() => {
			MobileLayout.enabled = true;
		});
	});

	afterEach(() => {
		resetBucket();
		runInAction(() => {
			MobileLayout.enabled = false;
		});
	});

	it('opens the info bottom sheet by default', () => {
		const markup = renderEmoji(CUSTOM_EMOJI);
		expect(markup).toContain('role="button"');
		expect(markup).toContain('tabindex="0"');
		expect(markup).toContain('data-test-bottom-sheet="false"');
		expect(markup).not.toContain('data-test-popout');
	});

	it('keeps the tap-to-sheet handler when disableEmojiInfoCard is set', () => {
		const markup = renderEmoji(CUSTOM_EMOJI, {disableEmojiInfoCard: true});
		expect(markup).toContain('role="button"');
		expect(markup).toContain('tabindex="0"');
		expect(markup).toContain('data-test-bottom-sheet="false"');
	});

	for (const flag of PLAIN_FLAGS) {
		it(`renders no tap handler when ${flag} is set`, () => {
			const markup = renderEmoji(CUSTOM_EMOJI, {[flag]: true});
			expect(markup).not.toContain('data-test-bottom-sheet');
			expectNoTabStop(markup);
			expectNoTooltip(markup);
		});
	}
});

describe('EmojiRenderer accessible name in the experiment arm', () => {
	beforeEach(() => {
		setBucket(true);
	});

	afterEach(resetBucket);

	for (const overrides of [
		{},
		{disableEmojiInfoCard: true},
		{disableInteractions: true},
		{disableEmojiInteractions: true},
	]) {
		const label = Object.keys(overrides)[0] ?? 'the default branch';
		it(`names the custom emoji in ${label}`, () => {
			expect(renderEmoji(CUSTOM_EMOJI, overrides)).toContain('aria-label=":blob:"');
		});

		it(`names the unicode emoji fallback in ${label}`, () => {
			const markup = renderEmoji(STANDARD_EMOJI, overrides);
			expect(markup).toContain('role="img"');
			expect(markup).toContain('aria-label=":slight_smile:"');
		});
	}
});

describe('EmojiRenderer failed-to-load wording in the experiment arm', () => {
	let host: HTMLDivElement;
	let root: Root;

	beforeEach(() => {
		setBucket(true);
		(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;
		host = document.createElement('div');
		document.body.append(host);
		root = createRoot(host);
	});

	afterEach(() => {
		act(() => {
			root.unmount();
		});
		host.remove();
		resetBucket();
	});

	for (const overrides of [{}, {disableEmojiInfoCard: true}, {disableInteractions: true}]) {
		const label = Object.keys(overrides)[0] ?? 'the default branch';
		it(`switches the accessible name to the failed wording in ${label}`, () => {
			act(() => {
				root.render(
					<EmojiRenderer
						node={CUSTOM_EMOJI}
						id="emoji-key"
						renderChildren={() => null}
						options={{...BASE_OPTIONS, ...overrides}}
						data-flx="test.emoji-renderer.failed"
					/>,
				);
			});
			const image = host.querySelector('img');
			expect(image?.getAttribute('aria-label')).toBe(':blob:');
			act(() => {
				image?.dispatchEvent(new Event('error'));
			});
			expect(host.querySelector('img')?.getAttribute('aria-label')).toBe(':blob: (failed to load)');
		});
	}
});

const ALL_FLAGS = ['disableInteractions', 'disableEmojiInteractions', 'disableEmojiInfoCard'] as const;

describe('EmojiRenderer on desktop in the control arm', () => {
	afterEach(resetBucket);

	it('renders the hover tooltip and no card by default', () => {
		const markup = renderEmoji(CUSTOM_EMOJI);
		expect(markup).toContain('data-test-hover-tooltip=":blob:"');
		expect(markup).toContain('alt=":blob:"');
		expectNoCard(markup);
		expectNoTabStop(markup);
		expectNoTooltip(markup);
	});

	for (const flag of ALL_FLAGS) {
		it(`ignores ${flag}`, () => {
			expect(renderEmoji(CUSTOM_EMOJI, {[flag]: true})).toBe(renderEmoji(CUSTOM_EMOJI));
		});
	}

	it('names the unicode emoji fallback', () => {
		const markup = renderEmoji(STANDARD_EMOJI);
		expect(markup).toContain('role="img"');
		expect(markup).toContain('aria-label=":slight_smile:"');
	});

	it('is what the inert assignment envelope selects', () => {
		const inert = renderEmoji(CUSTOM_EMOJI);
		setBucket(false);
		expect(renderEmoji(CUSTOM_EMOJI)).toBe(inert);
	});

	it('follows the store when the bucket flips', () => {
		const control = renderEmoji(CUSTOM_EMOJI);
		setBucket(true);
		const treatment = renderEmoji(CUSTOM_EMOJI);
		expect(treatment).not.toBe(control);
		expect(treatment).toContain('data-emoji-interactive="true"');
		resetBucket();
		expect(renderEmoji(CUSTOM_EMOJI)).toBe(control);
	});
});

describe('EmojiRenderer on mobile in the control arm', () => {
	beforeEach(() => {
		runInAction(() => {
			MobileLayout.enabled = true;
		});
	});

	afterEach(() => {
		resetBucket();
		runInAction(() => {
			MobileLayout.enabled = false;
		});
	});

	it('opens the legacy info bottom sheet by default', () => {
		const markup = renderEmoji(CUSTOM_EMOJI);
		expect(markup).toContain('role="button"');
		expect(markup).toContain('tabindex="0"');
		expect(markup).toContain('data-test-legacy-sheet="false"');
		expect(markup).not.toContain('data-test-bottom-sheet');
		expect(markup).not.toContain('data-test-popout');
	});

	for (const flag of ALL_FLAGS) {
		it(`ignores ${flag}`, () => {
			expect(renderEmoji(CUSTOM_EMOJI, {[flag]: true})).toBe(renderEmoji(CUSTOM_EMOJI));
		});
	}
});

describe('EmojiRenderer failed-to-load wording in the control arm', () => {
	let host: HTMLDivElement;
	let root: Root;

	beforeEach(() => {
		(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;
		host = document.createElement('div');
		document.body.append(host);
		root = createRoot(host);
	});

	afterEach(() => {
		act(() => {
			root.unmount();
		});
		host.remove();
		resetBucket();
	});

	it('switches the alt text to the failed wording', () => {
		act(() => {
			root.render(
				<EmojiRenderer
					node={CUSTOM_EMOJI}
					id="emoji-key"
					renderChildren={() => null}
					options={BASE_OPTIONS}
					data-flx="test.emoji-renderer.control.failed"
				/>,
			);
		});
		const image = host.querySelector('img');
		expect(image?.getAttribute('alt')).toBe(':blob:');
		act(() => {
			image?.dispatchEvent(new Event('error'));
		});
		expect(host.querySelector('img')?.getAttribute('alt')).toBe(':blob: (failed to load)');
	});
});
