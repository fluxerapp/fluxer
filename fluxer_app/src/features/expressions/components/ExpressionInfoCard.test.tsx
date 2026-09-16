// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ExpressionKind} from '@app/features/expressions/commands/ExpressionMetadataCommands';
import {installVoiceMenuTestBootstrap} from '@app/features/ui/action_menu/items/__fixtures__/VoiceMenuTestBootstrap';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type React from 'react';
import {act} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {renderToStaticMarkup} from 'react-dom/server';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

interface RemoteGuild {
	id: string;
	name: string;
	icon: string | null;
	features: Array<string>;
}

interface LocalGuild {
	id: string;
	name: string;
	icon: string | null;
	features: Set<string>;
}

type SourceState =
	| {status: 'idle'}
	| {status: 'loading'}
	| {status: 'available'; guild: RemoteGuild}
	| {status: 'unavailable'};

const state = vi.hoisted(() => ({
	localGuilds: [] as Array<{id: string; name: string; icon: string | null; features: Set<string>}>,
	memberGuildIds: [] as Array<string>,
	globalExpressions: false,
	source: {status: 'idle'} as {status: string; guild?: unknown},
	selectGuild: vi.fn(),
	joinDiscoveryGuild: vi.fn(() => Promise.resolve()),
	pushWithKey: vi.fn(),
}));

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});
vi.mock('@lingui/react/macro', () => ({
	Trans: ({children}: {children?: React.ReactNode}) => <span data-flx="test.trans">{children}</span>,
	useLingui: () => ({
		i18n: {
			locale: 'en',
			_: (descriptor: {message?: string}, values?: Record<string, unknown>) =>
				(descriptor.message ?? '').replace(/\{(\w+)\}/gu, (_match, key: string) => String(values?.[key] ?? `{${key}}`)),
		},
	}),
}));
vi.mock('@app/features/guild/state/Guilds', () => ({
	default: {getGuild: (id: string) => state.localGuilds.find((guild) => guild.id === id)},
}));
vi.mock('@app/features/guild/state/GuildList', () => ({
	default: {
		get guilds() {
			return state.memberGuildIds.map((id) => ({id}));
		},
	},
}));
vi.mock('@app/features/expressions/state/ExpressionSource', () => ({
	default: {
		getSource: () => state.source,
		fetchSource: vi.fn(() => Promise.resolve()),
	},
}));
vi.mock('@app/features/expressions/utils/ExpressionPermissionUtils', () => ({
	hasGlobalExpressionsEnabled: () => state.globalExpressions,
}));
vi.mock('@app/features/guild/components/popouts/GuildIcon', () => ({
	GuildIcon: ({name}: {name: string}) => <span data-test-guild-icon={name} data-flx="test.guild-icon" />,
}));
vi.mock('@app/features/guild/components/GuildBadge', () => ({
	GuildBadge: ({features}: {features: ReadonlyArray<string>}) => (
		<span data-test-guild-badge={[...features].join(',')} data-flx="test.guild-badge" />
	),
}));
vi.mock('@app/features/navigation/commands/NavigationCommands', () => ({selectGuild: state.selectGuild}));
vi.mock('@app/features/discovery/commands/DiscoveryJoinCommands', () => ({
	joinDiscoveryGuild: state.joinDiscoveryGuild,
}));
vi.mock('@app/features/ui/commands/ModalCommands', () => ({
	pushWithKey: state.pushWithKey,
	modal: (render: () => React.ReactElement) => render,
}));
vi.mock('@app/features/app/components/dialogs/ConfirmModal', () => ({
	ConfirmModal: ({
		title,
		description,
		primaryText,
	}: {
		title: React.ReactNode;
		description: React.ReactNode;
		primaryText: React.ReactNode;
	}) => (
		<div data-test-confirm-modal="true" data-flx="test.confirm-modal">
			<span data-test-confirm-title="true" data-flx="test.confirm-title">
				{title}
			</span>
			{description}
			<span data-test-confirm-primary="true" data-flx="test.confirm-primary">
				{primaryText}
			</span>
		</div>
	),
}));

installVoiceMenuTestBootstrap();

const {ExpressionInfoCard} = await import('@app/features/expressions/components/ExpressionInfoCard');

const GUILD_ID = '20';
const GUILD_NAME = 'Blob Club';
const EXPRESSION_ID = '10';
const DISPLAY_NAMES: Record<ExpressionKind, string> = {emoji: ':blob:', sticker: 'blobsticker'};
function localGuild(features: Array<string>): LocalGuild {
	return {id: GUILD_ID, name: GUILD_NAME, icon: null, features: new Set(features)};
}

function setSource(source: SourceState): void {
	state.source = source;
}

function joinedCommunity(features: Array<string>): void {
	state.localGuilds = [localGuild(features)];
	state.memberGuildIds = [GUILD_ID];
}

function foreignCommunity(features: Array<string>): void {
	state.localGuilds = [];
	state.memberGuildIds = [];
	setSource({status: 'available', guild: {id: GUILD_ID, name: GUILD_NAME, icon: null, features}});
}

function renderCard(kind: ExpressionKind, guildId: string | null): string {
	return renderToStaticMarkup(
		<ExpressionInfoCard
			kind={kind}
			expressionId={EXPRESSION_ID}
			guildId={guildId}
			displayName={DISPLAY_NAMES[kind]}
			previewUrl={null}
			data-flx="test.expression-info-card"
		/>,
	);
}

beforeEach(() => {
	state.localGuilds = [];
	state.memberGuildIds = [];
	state.globalExpressions = false;
	setSource({status: 'idle'});
	state.selectGuild.mockClear();
	state.joinDiscoveryGuild.mockClear();
	state.pushWithKey.mockClear();
});

describe('ExpressionInfoCard description', () => {
	it('describes a default emoji as usable anywhere', () => {
		const markup = renderToStaticMarkup(
			<ExpressionInfoCard
				kind="default_emoji"
				displayName=":smile:"
				previewUrl={null}
				data-flx="test.expression-info-card.default"
			/>,
		);
		expect(markup).toContain('A default emoji. You can use it anywhere on Fluxer.');
	});

	for (const kind of ['emoji', 'sticker'] as const) {
		it(`describes a ${kind} from a joined community as usable anywhere when global expressions are on`, () => {
			joinedCommunity([]);
			state.globalExpressions = true;
			expect(renderCard(kind, GUILD_ID)).toContain(
				`A custom ${kind} from this community. You can use it anywhere on Fluxer.`,
			);
		});

		it(`describes a ${kind} from a joined community as community-only when global expressions are off`, () => {
			joinedCommunity([]);
			expect(renderCard(kind, GUILD_ID)).toContain(
				`A custom ${kind} from this community. You can use it in this community.`,
			);
		});

		it(`describes a ${kind} from a discoverable community the viewer has not joined`, () => {
			foreignCommunity([GuildFeatures.DISCOVERABLE]);
			const markup = renderCard(kind, null);
			expect(markup).toContain(`This is a custom ${kind} from a community.`);
			expect(markup).not.toContain('Ask the author');
		});

		it(`describes a ${kind} from an invite-only community the viewer has not joined without repeating the footer`, () => {
			foreignCommunity([]);
			const markup = renderCard(kind, null);
			expect(markup).toContain(`This is a custom ${kind} from a community.`);
			expect(markup).not.toContain('Ask the author');
			expect(markup).toContain('Invite-only community');
		});

		it(`reports a ${kind} source community that is invite-only or unavailable without naming it`, () => {
			setSource({status: 'unavailable'});
			const markup = renderCard(kind, null);
			expect(markup).toContain(`This is a custom ${kind} from a community.`);
			expect(markup).toContain(kind === 'emoji' ? 'This emoji is from' : 'This sticker is from');
			expect(markup).toContain('A community that is either invite-only or unavailable.');
			expect(markup).not.toContain(GUILD_NAME);
			expect(markup).not.toContain('guild-icon');
		});

		it(`labels the source community section for a ${kind}`, () => {
			foreignCommunity([GuildFeatures.DISCOVERABLE]);
			const markup = renderCard(kind, null);
			expect(markup).toContain(kind === 'emoji' ? 'This emoji is from' : 'This sticker is from');
			expect(markup).not.toContain(kind === 'emoji' ? 'This sticker is from' : 'This emoji is from');
		});

		it(`keeps the ${kind} description identical while the source community resolves`, () => {
			const expected = `This is a custom ${kind} from a community.`;
			setSource({status: 'loading'});
			const resolving = renderCard(kind, null);
			expect(resolving).toContain(DISPLAY_NAMES[kind]);
			expect(resolving).toContain(expected);
			foreignCommunity([]);
			expect(renderCard(kind, null)).toContain(expected);
		});
	}

	it('does not colon-wrap a sticker display name', () => {
		joinedCommunity([]);
		const markup = renderCard('sticker', GUILD_ID);
		expect(markup).toContain('>blobsticker<');
		expect(markup).not.toContain(':blobsticker:');
	});
});

describe('ExpressionInfoCard community subtitle', () => {
	for (const isMember of [true, false]) {
		const membership = isMember ? 'a joined' : 'a foreign';
		it(`calls ${membership} discoverable community discoverable`, () => {
			if (isMember) {
				joinedCommunity([GuildFeatures.DISCOVERABLE]);
			} else {
				foreignCommunity([GuildFeatures.DISCOVERABLE]);
			}
			const markup = renderCard('emoji', isMember ? GUILD_ID : null);
			expect(markup).toContain('Discoverable community');
			expect(markup).not.toContain('Invite-only community');
		});

		it(`calls ${membership} non-discoverable community invite-only`, () => {
			if (isMember) {
				joinedCommunity([]);
			} else {
				foreignCommunity([]);
			}
			const markup = renderCard('emoji', isMember ? GUILD_ID : null);
			expect(markup).toContain('Invite-only community');
			expect(markup).not.toContain('Discoverable community');
		});
	}
});

describe('ExpressionInfoCard community row', () => {
	let host: HTMLDivElement;
	let root: Root;

	function render(guildId: string | null, onClose: () => void): void {
		act(() => {
			root.render(
				<ExpressionInfoCard
					kind="emoji"
					expressionId={EXPRESSION_ID}
					guildId={guildId}
					displayName=":blob:"
					previewUrl={null}
					onClose={onClose}
					data-flx="test.expression-info-card.row"
				/>,
			);
		});
	}

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
	});

	it('jumps to a community the viewer is a member of', () => {
		joinedCommunity([GuildFeatures.DISCOVERABLE]);
		const onClose = vi.fn();
		render(GUILD_ID, onClose);
		const button = host.querySelector('button');
		expect(button?.getAttribute('aria-label')).toBe(`Go to ${GUILD_NAME}`);
		act(() => {
			button?.dispatchEvent(new MouseEvent('click', {bubbles: true}));
		});
		expect(state.selectGuild).toHaveBeenCalledWith(GUILD_ID);
		expect(state.pushWithKey).not.toHaveBeenCalled();
		expect(onClose).toHaveBeenCalled();
	});

	it('confirms before joining a discoverable community the viewer is not in', async () => {
		foreignCommunity([GuildFeatures.DISCOVERABLE]);
		const onClose = vi.fn();
		render(null, onClose);
		const button = host.querySelector('button');
		expect(button?.getAttribute('aria-label')).toBe(`Join ${GUILD_NAME}`);
		act(() => {
			button?.dispatchEvent(new MouseEvent('click', {bubbles: true}));
		});
		expect(state.selectGuild).not.toHaveBeenCalled();
		expect(onClose).toHaveBeenCalled();
		expect(state.pushWithKey).toHaveBeenCalledTimes(1);
		expect(state.joinDiscoveryGuild).not.toHaveBeenCalled();
		const [renderModal, modalKey] = state.pushWithKey.mock.calls[0] as [() => React.ReactElement, string];
		expect(modalKey).toBe(`expression-source-guild-join-${GUILD_ID}`);
		const modal = renderModal();
		expect(renderToStaticMarkup(modal)).toContain(`Do you want to join ${GUILD_NAME}?`);
		const {onPrimary} = modal.props as {onPrimary: () => Promise<void>};
		await onPrimary();
		expect(state.joinDiscoveryGuild).toHaveBeenCalledWith(GUILD_ID);
	});

	it('leaves an invite-only community the viewer is not in unclickable', () => {
		foreignCommunity([]);
		render(null, vi.fn());
		expect(host.querySelector('button')).toBeNull();
		expect(host.querySelector('[tabindex]')).toBeNull();
		expect(host.innerHTML).toContain(GUILD_NAME);
	});
});
