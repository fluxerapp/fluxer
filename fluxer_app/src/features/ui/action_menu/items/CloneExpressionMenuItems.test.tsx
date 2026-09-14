// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import type React from 'react';
import {renderToStaticMarkup} from 'react-dom/server';
import {beforeEach, describe, expect, it, vi} from 'vitest';

interface MetadataRequestState {
	loading: boolean;
	error: Error | null;
	data: {id: string; guildId: string; name: string; animated: boolean; allowCloning: boolean} | null;
}

const emojiMetadataState: MetadataRequestState = {loading: false, error: null, data: null};
const stickerMetadataState: MetadataRequestState = {loading: false, error: null, data: null};

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});
vi.mock('@lingui/react/macro', () => ({
	Trans: () => null,
	useLingui: () => ({i18n: {_: (descriptor: {message?: string}) => descriptor.message ?? '', locale: 'en'}}),
}));
vi.mock('@phosphor-icons/react', () => ({CopyIcon: () => null}));
vi.mock('@app/features/app/components/alerts/ExpressionCloneFailedModal', () => ({
	showExpressionCloneFailedModal: vi.fn(),
}));
vi.mock('@app/features/expressions/commands/GuildEmojiCommands', () => ({clone: vi.fn()}));
vi.mock('@app/features/expressions/commands/GuildStickerCommands', () => ({clone: vi.fn()}));
vi.mock('@app/features/emoji/state/Emoji', () => ({default: {getGuildEmoji: () => []}}));
vi.mock('@app/features/emoji/state/EmojiSticker', () => ({default: {getGuildStickers: () => []}}));
vi.mock('@app/features/expressions/state/ExpressionMetadata', () => ({
	default: {
		getEmojiMetadata: () => emojiMetadataState,
		fetchEmojiMetadata: vi.fn(),
		getStickerMetadata: () => stickerMetadataState,
		fetchStickerMetadata: vi.fn(),
	},
}));
vi.mock('@app/features/guild/components/popouts/GuildIcon', () => ({GuildIcon: () => null}));
vi.mock('@app/features/permissions/state/Permission', () => ({default: {can: () => true}}));
vi.mock('@app/features/platform/utils/AppLogger', () => ({
	Logger: class {
		error(): void {}
	},
}));
vi.mock('@app/lib/overlay/KeyedActionGuard', () => ({
	createKeyedActionGuard: () => ({begin: () => true, scheduleRelease: () => {}}),
}));
vi.mock('@app/features/ui/action_menu/MenuGroup', () => ({
	MenuGroup: ({children}: {children?: React.ReactNode}) => <div>{children}</div>,
}));
vi.mock('@app/features/ui/action_menu/MenuItem', () => ({
	MenuItem: ({children, hint}: {children?: React.ReactNode; hint?: string}) => <div data-hint={hint}>{children}</div>,
}));
vi.mock('@app/features/ui/action_menu/MenuItemSubmenu', () => ({
	MenuItemSubmenu: ({label, render}: {label: string; render?: () => React.ReactNode}) => (
		<div data-submenu="true">
			{label}
			{render?.()}
		</div>
	),
}));
vi.mock('@app/features/ui/commands/ToastCommands', () => ({success: vi.fn()}));

const sourceGuild = {
	id: '1',
	name: 'Source',
	icon: null,
	maxEmojis: 50,
	maxStickers: 50,
	cloneEmojiAllowed: false,
	cloneStickerAllowed: false,
};
const targetGuild = {id: '2', name: 'Target', icon: null, maxEmojis: 50, maxStickers: 50};
const UNJOINED_GUILD_ID = '99';

vi.mock('@app/features/guild/state/Guilds', () => ({
	default: {
		getGuild: (id: string) => (id === sourceGuild.id ? sourceGuild : id === targetGuild.id ? targetGuild : null),
		getGuilds: () => [sourceGuild, targetGuild],
	},
}));

const {default: AdvancedSettings} = await import('@app/features/user/state/AdvancedSettings');
const {CloneEmojiMenuItem} = await import('@app/features/ui/action_menu/items/CloneEmojiMenuItem');
const {CloneStickerMenuItem} = await import('@app/features/ui/action_menu/items/CloneStickerMenuItem');

const emoji = {id: '10', guildId: sourceGuild.id, name: 'blob', uniqueName: 'blob'} as unknown as FlatEmoji;
const unjoinedEmoji = {id: '11', guildId: UNJOINED_GUILD_ID, name: 'blob', uniqueName: 'blob'} as unknown as FlatEmoji;
const sticker = {id: '20', guildId: sourceGuild.id, name: 'blobsticker', animated: false};
const unjoinedSticker = {id: '21', guildId: UNJOINED_GUILD_ID, name: 'blobsticker', animated: false};

const EMOJI_SUBMENU_LABEL = 'Clone emoji to...';
const STICKER_SUBMENU_LABEL = 'Clone sticker to…';
const LOADING_HINT = 'Checking access...';

function renderEmojiItem(): string {
	return renderToStaticMarkup(<CloneEmojiMenuItem emoji={emoji} onClose={() => {}} />);
}

function renderUnjoinedEmojiItem(): string {
	return renderToStaticMarkup(<CloneEmojiMenuItem emoji={unjoinedEmoji} onClose={() => {}} />);
}

function renderStickerItem(): string {
	return renderToStaticMarkup(<CloneStickerMenuItem sticker={sticker} onClose={() => {}} />);
}

function renderUnjoinedStickerItem(): string {
	return renderToStaticMarkup(<CloneStickerMenuItem sticker={unjoinedSticker} onClose={() => {}} />);
}

function resolvedMetadata(allowCloning: boolean): MetadataRequestState['data'] {
	return {id: '11', guildId: UNJOINED_GUILD_ID, name: 'blob', animated: false, allowCloning};
}

type Outcome = 'submenu' | 'loading' | 'hidden';

interface Scenario {
	name: string;
	outcome: Outcome;
	arrange: () => void;
	render: () => string;
}

interface Harness {
	kind: string;
	submenuLabel: string;
	setSourceGuildAllowed: (value: boolean) => void;
	metadata: MetadataRequestState;
	renderJoined: () => string;
	renderUnjoined: () => string;
}

const emojiHarness: Harness = {
	kind: 'emoji',
	submenuLabel: EMOJI_SUBMENU_LABEL,
	setSourceGuildAllowed: (value) => {
		sourceGuild.cloneEmojiAllowed = value;
	},
	metadata: emojiMetadataState,
	renderJoined: renderEmojiItem,
	renderUnjoined: renderUnjoinedEmojiItem,
};

const stickerHarness: Harness = {
	kind: 'sticker',
	submenuLabel: STICKER_SUBMENU_LABEL,
	setSourceGuildAllowed: (value) => {
		sourceGuild.cloneStickerAllowed = value;
	},
	metadata: stickerMetadataState,
	renderJoined: renderStickerItem,
	renderUnjoined: renderUnjoinedStickerItem,
};

function buildScenarios(harness: Harness): ReadonlyArray<Scenario> {
	return [
		{
			name: 'the source community is known and has opted in',
			outcome: 'submenu',
			arrange: () => harness.setSourceGuildAllowed(true),
			render: harness.renderJoined,
		},
		{
			name: 'the source community is known and has not opted in',
			outcome: 'hidden',
			arrange: () => harness.setSourceGuildAllowed(false),
			render: harness.renderJoined,
		},
		{
			name: 'the source community is unknown and metadata resolved with cloning allowed',
			outcome: 'submenu',
			arrange: () => {
				harness.metadata.data = resolvedMetadata(true);
			},
			render: harness.renderUnjoined,
		},
		{
			name: 'the source community is unknown and metadata resolved with cloning disallowed',
			outcome: 'hidden',
			arrange: () => {
				harness.metadata.data = resolvedMetadata(false);
			},
			render: harness.renderUnjoined,
		},
		{
			name: 'the source community is unknown and the metadata lookup errored',
			outcome: 'hidden',
			arrange: () => {
				harness.metadata.error = new Error('metadata lookup failed');
			},
			render: harness.renderUnjoined,
		},
		{
			name: 'the source community is unknown and the metadata lookup has not started',
			outcome: 'hidden',
			arrange: () => {},
			render: harness.renderUnjoined,
		},
		{
			name: 'the source community is unknown and the metadata lookup is still in flight',
			outcome: 'loading',
			arrange: () => {
				harness.metadata.loading = true;
			},
			render: harness.renderUnjoined,
		},
	];
}

function expectOutcome(markup: string, outcome: Outcome, submenuLabel: string): void {
	if (outcome === 'hidden') {
		expect(markup).toBe('');
		return;
	}
	if (outcome === 'loading') {
		expect(markup).toContain(submenuLabel);
		expect(markup).toContain(LOADING_HINT);
		expect(markup).not.toContain('data-submenu');
		return;
	}
	expect(markup).toContain('data-submenu');
	expect(markup).toContain(submenuLabel);
	expect(markup).toContain(targetGuild.name);
	expect(markup).not.toContain(LOADING_HINT);
}

describe('Clone expression menu items', () => {
	beforeEach(() => {
		AdvancedSettings.setExpressionCloneShortcutsEnabled(false);
		sourceGuild.cloneEmojiAllowed = false;
		sourceGuild.cloneStickerAllowed = false;
		for (const state of [emojiMetadataState, stickerMetadataState]) {
			state.loading = false;
			state.error = null;
			state.data = null;
		}
	});

	it('defaults the user setting to off', () => {
		expect(AdvancedSettings.expressionCloneShortcutsEnabled).toBe(false);
	});

	it('hides both shortcuts when the user setting is off and the community allows cloning', () => {
		sourceGuild.cloneEmojiAllowed = true;
		sourceGuild.cloneStickerAllowed = true;
		expect(renderEmojiItem()).toBe('');
		expect(renderStickerItem()).toBe('');
	});

	it('hides both shortcuts when the user setting is on and the community does not allow cloning', () => {
		AdvancedSettings.setExpressionCloneShortcutsEnabled(true);
		expect(renderEmojiItem()).toBe('');
		expect(renderStickerItem()).toBe('');
	});

	it('shows both shortcuts when the user setting is on and the community allows cloning', () => {
		AdvancedSettings.setExpressionCloneShortcutsEnabled(true);
		sourceGuild.cloneEmojiAllowed = true;
		sourceGuild.cloneStickerAllowed = true;
		expect(renderEmojiItem()).toContain(EMOJI_SUBMENU_LABEL);
		expect(renderStickerItem()).toContain(STICKER_SUBMENU_LABEL);
	});
});

for (const harness of [emojiHarness, stickerHarness]) {
	describe(`Clone ${harness.kind} shortcut gating`, () => {
		beforeEach(() => {
			AdvancedSettings.setExpressionCloneShortcutsEnabled(false);
			sourceGuild.cloneEmojiAllowed = false;
			sourceGuild.cloneStickerAllowed = false;
			for (const state of [emojiMetadataState, stickerMetadataState]) {
				state.loading = false;
				state.error = null;
				state.data = null;
			}
		});

		for (const scenario of buildScenarios(harness)) {
			it(`renders the ${scenario.outcome} state when ${scenario.name}`, () => {
				AdvancedSettings.setExpressionCloneShortcutsEnabled(true);
				scenario.arrange();
				expectOutcome(scenario.render(), scenario.outcome, harness.submenuLabel);
			});

			it(`hides the shortcut with the user setting off when ${scenario.name}`, () => {
				scenario.arrange();
				expect(scenario.render()).toBe('');
			});
		}
	});
}
