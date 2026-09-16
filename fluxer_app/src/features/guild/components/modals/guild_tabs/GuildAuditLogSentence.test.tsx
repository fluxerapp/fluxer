// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildAuditLogSentence} from '@app/features/guild/components/modals/guild_tabs/GuildAuditLogSentence';
import type {AuditLogSentence} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentationTypes';
import {AppI18nProvider} from '@app/features/i18n/components/AppI18nProvider';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {type MessageDescriptor, setupI18n} from '@lingui/core';
import type React from 'react';
import {renderToStaticMarkup} from 'react-dom/server';
import {afterEach, describe, expect, it, vi} from 'vitest';

const fixtures = vi.hoisted(() => ({
	users: new Map<string, {id: string; displayName: string}>(),
	channels: new Map<string, {id: string; name?: string; type: number}>(),
	roles: new Map<string, {id: string; name: string}>(),
	emojis: new Map<string, {id: string; name: string; animated: boolean}>(),
}));

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});
vi.mock('@app/features/app/config/Config', () => ({
	default: {
		PUBLIC_BUILD_VERSION: 'test',
		PUBLIC_RELEASE_CHANNEL: 'canary',
		PUBLIC_BOOTSTRAP_API_ENDPOINT: 'https://example.invalid',
		PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT: 'https://example.invalid',
	},
}));
vi.mock('@app/features/user/state/Users', () => ({
	default: {getUser: (id: string) => fixtures.users.get(id)},
}));
vi.mock('@app/features/channel/state/Channels', () => ({
	default: {getChannel: (id: string) => fixtures.channels.get(id)},
}));
vi.mock('@app/features/guild/state/Guilds', () => ({
	default: {getGuildRole: (_guildId: string, roleId: string) => fixtures.roles.get(roleId)},
}));
vi.mock('@app/features/emoji/state/Emoji', () => ({
	default: {getEmojiById: (id: string) => fixtures.emojis.get(id)},
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
	ClickableUser: ({user, 'data-flx': dataFlx}: {user: {id: string; displayName: string}; 'data-flx'?: string}) => (
		<span data-user-id={user.id} data-flx={dataFlx}>
			{user.displayName}
		</span>
	),
	CopyIdInline: ({
		id,
		children,
		'data-flx': dataFlx,
	}: {
		id: string;
		children?: React.ReactNode;
		'data-flx'?: string;
	}) => (
		<span data-copy-id={id} data-flx={dataFlx}>
			{children}
		</span>
	),
	InlineCode: ({
		children,
		title,
		'data-flx': dataFlx,
	}: {
		children?: React.ReactNode;
		title?: string;
		'data-flx'?: string;
	}) => (
		<code title={title} data-flx={dataFlx}>
			{children}
		</code>
	),
	ColorDot: ({color, 'data-flx': dataFlx}: {color: string; 'data-flx'?: string}) => (
		<i data-color={color} data-flx={dataFlx} />
	),
}));

const GUILD_ID = '1400000000000000000';
const ACTOR_ID = '1400000000000000001';
const CHANNEL_ID = '1400000000000000002';
const ROLE_ID = '1400000000000000003';
const EMOJI_ID = '1400000000000000004';
const SENTENCE_DATA_FLX_PATTERN = /data-flx="(guild\.guild-tabs\.guild-audit-log-sentence\.[^"]+)"/g;

const i18n = setupI18n({locale: 'en', messages: {en: {}}});

function descriptor(id: string, message: string): MessageDescriptor {
	return {id, message};
}

function renderSentence(sentence: AuditLogSentence): string {
	return renderToStaticMarkup(
		<AppI18nProvider i18n={i18n}>
			<GuildAuditLogSentence
				sentence={sentence}
				guildId={GUILD_ID}
				data-flx="guild.guild-tabs.guild-audit-log-sentence-test.render-sentence.guild-audit-log-sentence"
			/>
		</AppI18nProvider>,
	);
}

function visibleText(html: string): string {
	return html.replace(/<[^>]+>/g, '');
}

function renderText(sentence: AuditLogSentence): string {
	return visibleText(renderSentence(sentence));
}

function channelSentence(channel: AuditLogSentence['values'][string]): AuditLogSentence {
	return {descriptor: descriptor('test.channel', 'Deleted {channel}'), values: {channel}};
}

function roleSentence(role: AuditLogSentence['values'][string]): AuditLogSentence {
	return {descriptor: descriptor('test.role', 'Deleted the role {role}'), values: {role}};
}

afterEach(() => {
	fixtures.users.clear();
	fixtures.channels.clear();
	fixtures.roles.clear();
	fixtures.emojis.clear();
});

describe('GuildAuditLogSentence', () => {
	it('renders a user chip and a channel chip inside one message', () => {
		fixtures.users.set(ACTOR_ID, {id: ACTOR_ID, displayName: 'Hampus'});
		fixtures.channels.set(CHANNEL_ID, {id: CHANNEL_ID, name: 'general', type: ChannelTypes.GUILD_TEXT});
		const html = renderSentence({
			descriptor: descriptor('test.message-delete', '{actor} deleted a message in {channel}'),
			values: {
				actor: {kind: 'user', id: ACTOR_ID},
				channel: {kind: 'channel', id: CHANNEL_ID, recordedName: null, fallback: 'channel'},
			},
		});
		expect(visibleText(html)).toBe('Hampus deleted a message in #general');
		const userChipIndex = html.indexOf(`data-user-id="${ACTOR_ID}"`);
		const channelChipIndex = html.indexOf(`data-copy-id="${CHANNEL_ID}"`);
		expect(userChipIndex).toBeGreaterThan(-1);
		expect(channelChipIndex).toBeGreaterThan(userChipIndex);
	});

	it.each([
		[1, 'Hampus deleted 1 message in #general'],
		[5, 'Hampus deleted 5 messages in #general'],
	])('passes the number %i through to the plural', (count, expected) => {
		fixtures.users.set(ACTOR_ID, {id: ACTOR_ID, displayName: 'Hampus'});
		fixtures.channels.set(CHANNEL_ID, {id: CHANNEL_ID, name: 'general', type: ChannelTypes.GUILD_TEXT});
		expect(
			renderText({
				descriptor: descriptor(
					'test.message-bulk-delete',
					'{actor} deleted {count, plural, one {# message} other {# messages}} in {channel}',
				),
				values: {
					actor: {kind: 'user', id: ACTOR_ID},
					count,
					channel: {kind: 'channel', id: CHANNEL_ID, recordedName: null, fallback: 'channel'},
				},
			}),
		).toBe(expected);
	});

	it('shows the unknown user label when the user is not loaded', () => {
		const html = renderSentence({
			descriptor: descriptor('test.kick', '{actor} kicked {target}'),
			values: {actor: {kind: 'user', id: ACTOR_ID}, target: {kind: 'user', id: ''}},
		});
		expect(visibleText(html)).toBe('Unknown user kicked Unknown user');
		expect(html).not.toContain('data-user-id');
	});

	it('shows the System label for the system actor', () => {
		fixtures.users.set(ACTOR_ID, {id: ACTOR_ID, displayName: 'Hampus'});
		expect(
			renderText({
				descriptor: descriptor('test.system-role', '{actor} gave {target} the role {role}'),
				values: {
					actor: {kind: 'system'},
					target: {kind: 'user', id: ACTOR_ID},
					role: {kind: 'role', id: ROLE_ID, recordedName: 'Mods'},
				},
			}),
		).toBe('System gave Hampus the role Mods');
	});

	describe('channel names', () => {
		it.each([
			[ChannelTypes.GUILD_TEXT, 'Deleted #general'],
			[ChannelTypes.GUILD_LINK, 'Deleted #general'],
			[ChannelTypes.GUILD_VOICE, 'Deleted general'],
			[ChannelTypes.GUILD_CATEGORY, 'Deleted general'],
		])('prefixes # only for live text and link channels (type %i)', (type, expected) => {
			fixtures.channels.set(CHANNEL_ID, {id: CHANNEL_ID, name: 'general', type});
			expect(
				renderText(channelSentence({kind: 'channel', id: CHANNEL_ID, recordedName: null, fallback: 'channel'})),
			).toBe(expected);
		});

		it('prefers the recorded name over the live name', () => {
			fixtures.channels.set(CHANNEL_ID, {id: CHANNEL_ID, name: 'new-name', type: ChannelTypes.GUILD_TEXT});
			expect(
				renderText(channelSentence({kind: 'channel', id: CHANNEL_ID, recordedName: 'old-name', fallback: 'channel'})),
			).toBe('Deleted #old-name');
		});

		it('shows the recorded name without a prefix when the channel is gone', () => {
			expect(
				renderText(channelSentence({kind: 'channel', id: CHANNEL_ID, recordedName: 'general', fallback: 'channel'})),
			).toBe('Deleted general');
		});

		it('falls back to #deleted-channel for a missing channel', () => {
			const html = renderSentence(
				channelSentence({kind: 'channel', id: CHANNEL_ID, recordedName: null, fallback: 'channel'}),
			);
			expect(visibleText(html)).toBe('Deleted #deleted-channel');
			expect(html).toContain(`data-copy-id="${CHANNEL_ID}"`);
		});

		it('falls back to deleted-category for a missing category', () => {
			expect(
				renderText(channelSentence({kind: 'channel', id: CHANNEL_ID, recordedName: null, fallback: 'category'})),
			).toBe('Deleted deleted-category');
		});
	});

	describe('role names', () => {
		it('shows @everyone when the role id equals the community id', () => {
			expect(renderText(roleSentence({kind: 'role', id: GUILD_ID, recordedName: null}))).toBe(
				'Deleted the role @everyone',
			);
		});

		it('prefers the recorded name over the live name', () => {
			fixtures.roles.set(ROLE_ID, {id: ROLE_ID, name: 'Moderators'});
			expect(renderText(roleSentence({kind: 'role', id: ROLE_ID, recordedName: 'Mods'}))).toBe('Deleted the role Mods');
		});

		it('uses the live name when no name was recorded', () => {
			fixtures.roles.set(ROLE_ID, {id: ROLE_ID, name: 'Moderators'});
			expect(renderText(roleSentence({kind: 'role', id: ROLE_ID, recordedName: null}))).toBe(
				'Deleted the role Moderators',
			);
		});

		it('falls back to deleted-role for a missing role', () => {
			const html = renderSentence(roleSentence({kind: 'role', id: ROLE_ID, recordedName: null}));
			expect(visibleText(html)).toBe('Deleted the role deleted-role');
			expect(html).toContain(`data-copy-id="${ROLE_ID}"`);
		});
	});

	describe('text', () => {
		const textSentence = (value: string): AuditLogSentence => ({
			descriptor: descriptor('test.topic', 'Set the topic to {text}'),
			values: {text: {kind: 'text', value}},
		});

		it('keeps text of 200 code points whole without a tooltip', () => {
			const value = '😀'.repeat(200);
			const html = renderSentence(textSentence(value));
			expect(visibleText(html)).toBe(`Set the topic to ${value}`);
			expect(html).not.toContain('title=');
		});

		it('truncates text past 200 code points and puts the full text in the tooltip', () => {
			const value = '😀'.repeat(201);
			const html = renderSentence(textSentence(value));
			expect(visibleText(html)).toBe(`Set the topic to ${'😀'.repeat(200)}…`);
			expect(html).toContain(`title="${value}"`);
		});
	});

	describe('permissions', () => {
		const permissionSentence = (flags: Array<bigint>): AuditLogSentence => ({
			descriptor: descriptor('test.granted', 'Granted {permissions}'),
			values: {permissions: {kind: 'permissions', flags}},
		});

		it('lists up to 8 permissions by name', () => {
			expect(renderText(permissionSentence([1n, 2n, 4n, 8n, 16n, 32n, 64n, 128n]))).toBe(
				'Granted Permission 1, Permission 2, Permission 4, Permission 8, Permission 16, Permission 32, Permission 64, and Permission 128',
			);
		});

		it('shows 7 names and a count when there are more than 8 permissions', () => {
			expect(renderText(permissionSentence([1n, 2n, 4n, 8n, 16n, 32n, 64n, 128n, 256n, 512n]))).toBe(
				'Granted Permission 1, Permission 2, Permission 4, Permission 8, Permission 16, Permission 32, Permission 64, and 3 more permissions',
			);
		});
	});

	it.each([
		[5400, 'Set slowmode to 90 minutes'],
		[259200, 'Set slowmode to 3 days'],
		[1, 'Set slowmode to 1 second'],
	])('formats a duration of %i seconds in its largest whole unit', (seconds, expected) => {
		expect(
			renderText({
				descriptor: descriptor('test.slowmode', 'Set slowmode to {duration}'),
				values: {duration: {kind: 'duration', seconds}},
			}),
		).toBe(expected);
	});

	it('formats dates, labels and names', () => {
		const timestamp = Date.parse('2026-09-01T12:00:00.000Z');
		expect(
			renderText({
				descriptor: descriptor('test.mixed', '{name} changed {oldAlignment} on {date}'),
				values: {
					name: {kind: 'name', value: 'Captain Hook'},
					oldAlignment: {kind: 'label', descriptor: descriptor('test.centered', 'Centered')},
					date: {kind: 'date', timestamp},
				},
			}),
		).toBe('Captain Hook changed Centered on 2026-09-01T12:00:00.000Z');
	});

	it('formats a color as uppercase hex with a color dot', () => {
		const html = renderSentence({
			descriptor: descriptor('test.color', 'Set the role color to {color}'),
			values: {color: {kind: 'color', value: 0xff8800}},
		});
		expect(visibleText(html)).toBe('Set the role color to #FF8800');
		expect(html).toContain('data-color="#FF8800"');
	});

	describe('emoji', () => {
		const emojiSentence = (id: string | null): AuditLogSentence => ({
			descriptor: descriptor('test.emoji', 'Added the emoji {emoji}'),
			values: {emoji: {kind: 'emoji', id, name: 'blobcat'}},
		});

		it('shows the emoji image when the emoji is loaded', () => {
			fixtures.emojis.set(EMOJI_ID, {id: EMOJI_ID, name: 'blobcat', animated: false});
			const html = renderSentence(emojiSentence(EMOJI_ID));
			expect(visibleText(html)).toBe('Added the emoji :blobcat:');
			expect(html).toContain(`src="https://media.example.invalid/emojis/${EMOJI_ID}.webp"`);
		});

		it.each([
			['without an id', null],
			['that is not loaded', EMOJI_ID],
		])('shows only the name for an emoji %s', (_label, id) => {
			const html = renderSentence(emojiSentence(id));
			expect(visibleText(html)).toBe('Added the emoji :blobcat:');
			expect(html).not.toContain('<img');
		});
	});

	it('puts data-flx on each chip', () => {
		fixtures.users.set(ACTOR_ID, {id: ACTOR_ID, displayName: 'Hampus'});
		fixtures.channels.set(CHANNEL_ID, {id: CHANNEL_ID, name: 'general', type: ChannelTypes.GUILD_TEXT});
		fixtures.emojis.set(EMOJI_ID, {id: EMOJI_ID, name: 'blobcat', animated: false});
		const html = renderSentence({
			descriptor: descriptor(
				'test.chips',
				'{user} {unknownUser} {system} {channel} {role} {name} {text} {longText} {loadedEmoji} {plainEmoji} {color}',
			),
			values: {
				user: {kind: 'user', id: ACTOR_ID},
				unknownUser: {kind: 'user', id: ''},
				system: {kind: 'system'},
				channel: {kind: 'channel', id: CHANNEL_ID, recordedName: null, fallback: 'channel'},
				role: {kind: 'role', id: ROLE_ID, recordedName: 'Mods'},
				name: {kind: 'name', value: 'Captain Hook'},
				text: {kind: 'text', value: 'short'},
				longText: {kind: 'text', value: 'x'.repeat(201)},
				loadedEmoji: {kind: 'emoji', id: EMOJI_ID, name: 'blobcat'},
				plainEmoji: {kind: 'emoji', id: null, name: 'wave'},
				color: {kind: 'color', value: 0x00ff00},
			},
		});
		const dataFlxValues = Array.from(html.matchAll(SENTENCE_DATA_FLX_PATTERN), (match) => match[1]).sort();
		expect(dataFlxValues).toEqual(
			[
				'guild.guild-tabs.guild-audit-log-sentence.render-user.clickable-user',
				'guild.guild-tabs.guild-audit-log-sentence.render-user.strong',
				'guild.guild-tabs.guild-audit-log-sentence.render-system.strong',
				'guild.guild-tabs.guild-audit-log-sentence.render-channel.copy-id-inline',
				'guild.guild-tabs.guild-audit-log-sentence.render-channel.strong',
				'guild.guild-tabs.guild-audit-log-sentence.render-role.copy-id-inline',
				'guild.guild-tabs.guild-audit-log-sentence.render-role.strong',
				'guild.guild-tabs.guild-audit-log-sentence.render-name.strong',
				'guild.guild-tabs.guild-audit-log-sentence.render-text.inline-code',
				'guild.guild-tabs.guild-audit-log-sentence.render-text.inline-code--2',
				'guild.guild-tabs.guild-audit-log-sentence.render-emoji.inline-emoji',
				'guild.guild-tabs.guild-audit-log-sentence.render-emoji.inline-emoji-image',
				'guild.guild-tabs.guild-audit-log-sentence.render-emoji.strong--2',
				'guild.guild-tabs.guild-audit-log-sentence.render-emoji.strong',
				'guild.guild-tabs.guild-audit-log-sentence.render-color.color-value',
				'guild.guild-tabs.guild-audit-log-sentence.render-color.strong',
				'guild.guild-tabs.guild-audit-log-sentence.render-color.color-dot',
			].sort(),
		);
	});
});
