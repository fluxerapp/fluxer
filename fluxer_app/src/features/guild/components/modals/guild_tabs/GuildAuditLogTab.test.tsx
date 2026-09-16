// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {AppI18nProvider} from '@app/features/i18n/components/AppI18nProvider';
import {installVoiceMenuTestBootstrap} from '@app/features/ui/action_menu/items/__fixtures__/VoiceMenuTestBootstrap';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {setupI18n} from '@lingui/core';
import type React from 'react';
import {act} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const GUILD_ID = '1400000000000000000';
const ACTOR_ID = '1400000000000000001';
const ENTRY_ID = '1400000000000000002';

const fixtures = vi.hoisted(() => ({
	users: new Map<string, {id: string; displayName: string}>(),
	entries: [] as Array<unknown>,
}));

vi.mock('@app/features/platform/utils/AppLogger', () => ({
	Logger: class {
		debug = vi.fn();
		info = vi.fn();
		warn = vi.fn();
		error = vi.fn();
	},
}));
vi.mock('@app/features/platform/transport/RestTransport', () => ({
	http: {get: vi.fn(), post: vi.fn()},
}));
vi.mock('@app/features/app/config/Config', () => ({
	default: {
		PUBLIC_BUILD_VERSION: 'test',
		PUBLIC_RELEASE_CHANNEL: 'canary',
		PUBLIC_BOOTSTRAP_API_ENDPOINT: 'https://example.invalid',
		PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT: 'https://example.invalid',
	},
}));
vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown =>
		typeof value === 'string'
			? {id: value, message: value}
			: {...(value as object), id: (value as {message: string}).message};
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});
vi.mock('@lingui/react/macro', () => ({
	Trans: ({children}: {children?: React.ReactNode}) => <>{children}</>,
	Plural: ({value}: {value: number}) => <>{value}</>,
	useLingui: () => ({i18n}),
}));
vi.mock('@app/features/guild/commands/GuildCommands', () => ({
	fetchGuildAuditLogs: vi.fn(async () => ({
		audit_log_entries: fixtures.entries,
		users: [],
		webhooks: [],
	})),
}));
vi.mock('@app/features/user/state/Users', () => ({
	default: {getUser: (id: string) => fixtures.users.get(id), cacheUsers: vi.fn()},
}));
vi.mock('@app/features/member/state/GuildMembers', () => ({
	default: {getMembers: () => [], ensureMembersLoaded: vi.fn(async () => undefined)},
}));
vi.mock('@app/features/channel/state/Channels', () => ({default: {getChannel: () => undefined}}));
vi.mock('@app/features/guild/state/Guilds', () => ({
	default: {
		getGuildRole: () => undefined,
		getGuild: () => undefined,
		getRoles: () => [],
		getGuildRoles: () => [],
		getGuildChannels: () => [],
	},
}));
vi.mock('@app/features/emoji/state/Emoji', () => ({default: {getEmojiById: () => undefined}}));
vi.mock('@app/features/emoji/state/EmojiSticker', () => ({default: {getStickerById: () => null}}));
vi.mock('@app/features/messaging/utils/MessagingUrlUtils', () => ({
	setUrlQueryParams: (path: string) => path,
	setPathQueryParams: (path: string) => path,
	mediaUrl: (path: string) => `https://media.example.invalid${path}`,
	cdnUrl: (path: string) => `https://cdn.example.invalid${path}`,
	webhookUrl: (id: string, token: string) => `https://example.invalid/webhooks/${id}/${token}`,
	marketingUrl: (path: string) => `https://example.invalid${path}`,
	adminUrl: (path: string) => `https://example.invalid${path}`,
	webAppUrl: (path: string) => `https://example.invalid${path}`,
}));
vi.mock('@app/features/user/utils/AvatarUtils', () => ({
	getEmojiURL: ({id}: {id: string}) => `https://media.example.invalid/emojis/${id}.webp`,
}));
vi.mock('@app/features/user/utils/DateFormatting', () => ({
	getFormattedDateTime: (timestamp: number) => new Date(timestamp).toISOString(),
}));
vi.mock('@app/features/permissions/utils/PermissionUtils', () => ({
	formatPermissionLabel: (_i18n: unknown, permission: bigint) => `Permission ${permission}`,
}));
vi.mock('@app/features/guild/components/modals/guild_tabs/GuildAuditLogTabComponents', () => ({
	ClickableUser: ({user}: {user: {id: string; displayName: string}}) => (
		<span data-flx="guild.guild-tabs.guild-audit-log-tab-test.clickable-user.span">{user.displayName}</span>
	),
	CopyIdInline: ({children}: {children?: React.ReactNode}) => (
		<span data-flx="guild.guild-tabs.guild-audit-log-tab-test.copy-id-inline.span">{children}</span>
	),
	InlineCode: ({children}: {children?: React.ReactNode}) => (
		<code data-flx="guild.guild-tabs.guild-audit-log-tab-test.inline-code.code">{children}</code>
	),
	ColorDot: ({color}: {color: string}) => (
		<i data-color={color} data-flx="guild.guild-tabs.guild-audit-log-tab-test.color-dot.i" />
	),
}));
vi.mock('@app/features/ui/components/form/FormCombobox', () => ({
	Combobox: ({options}: {options: ReadonlyArray<{value: string; label: string}>}) => (
		<div data-testid="combobox" data-flx="guild.guild-tabs.guild-audit-log-tab-test.combobox.combobox">
			{options.map((option) => (
				<span key={option.value} data-flx="guild.guild-tabs.guild-audit-log-tab-test.combobox.span">
					{option.label}
				</span>
			))}
		</div>
	),
}));
vi.mock('@app/features/ui/components/Avatar', () => ({
	Avatar: ({user}: {user: {id: string}}) => (
		<span data-avatar={user.id} data-flx="guild.guild-tabs.guild-audit-log-tab-test.avatar.span" />
	),
}));
vi.mock('@app/features/ui/components/MockAvatar', () => ({
	MockAvatar: ({userTag}: {userTag?: string}) => (
		<span data-mock-avatar={userTag} data-flx="guild.guild-tabs.guild-audit-log-tab-test.mock-avatar.span" />
	),
}));
vi.mock('@app/features/ui/components/Spinner', () => ({
	Spinner: () => <span data-spinner data-flx="guild.guild-tabs.guild-audit-log-tab-test.spinner.span" />,
}));
vi.mock('@app/features/ui/button/Button', () => ({
	Button: ({children}: {children?: React.ReactNode}) => (
		<button type="button" data-flx="guild.guild-tabs.guild-audit-log-tab-test.button.button">
			{children}
		</button>
	),
}));
vi.mock('@app/features/ui/focus_ring/FocusRing', () => ({
	default: ({children}: {children: React.ReactElement}) => children,
}));
vi.mock('@app/features/app/components/dialogs/shared/EmptySlate', () => ({
	EmptySlate: ({title, description}: {title: React.ReactNode; description: React.ReactNode}) => (
		<div data-flx="guild.guild-tabs.guild-audit-log-tab-test.empty-slate.div">
			{title}
			{description}
		</div>
	),
}));

const i18n = setupI18n({locale: 'en', messages: {en: {}}});

installVoiceMenuTestBootstrap();

const GuildAuditLogTab = (await import('@app/features/guild/components/modals/guild_tabs/GuildAuditLogTab')).default;

(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;

describe('GuildAuditLogTab', () => {
	let root: Root | null = null;
	let container: HTMLDivElement | null = null;

	beforeEach(() => {
		fixtures.users.set(ACTOR_ID, {id: ACTOR_ID, displayName: 'Moderator'});
		fixtures.entries = [
			{
				id: ENTRY_ID,
				action_type: AuditLogActionType.GUILD_UPDATE,
				user_id: ACTOR_ID,
				target_id: GUILD_ID,
				changes: [{key: 'name', old_value: 'Old name', new_value: 'New name'}],
			},
		];
		container = document.createElement('div');
		document.body.append(container);
		root = createRoot(container);
	});

	afterEach(() => {
		act(() => {
			root?.unmount();
		});
		root = null;
		container?.remove();
		container = null;
		fixtures.users.clear();
		fixtures.entries = [];
	});

	async function render(): Promise<string> {
		await act(async () => {
			root?.render(
				<AppI18nProvider i18n={i18n}>
					<GuildAuditLogTab
						guildId={GUILD_ID}
						data-flx="guild.guild-tabs.guild-audit-log-tab-test.guild-audit-log-tab"
					/>
				</AppI18nProvider>,
			);
		});
		return container?.textContent ?? '';
	}

	it('renders each entry as its presenter sentence', async () => {
		const text = await render();
		expect(text).toContain('renamed the community from Old name to New name');
		expect(text).not.toContain('updated the community settings.');
	});

	it('lists the presenter action filters', async () => {
		const text = await render();
		expect(text).toContain('Permission override added');
		expect(text).not.toContain('Channel overwrite added');
		expect(text).not.toContain('Members pruned');
	});
});
