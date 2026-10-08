// SPDX-License-Identifier: AGPL-3.0-or-later

import {Channel} from '@app/features/channel/models/Channel';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ContentWarningLevel} from '@fluxer/constants/src/GuildConstants';
import type {Channel as WireChannel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {beforeEach, describe, expect, it, vi} from 'vitest';

const channels = new Map<string, Channel>();
const guild = {id: 'guild', nsfw: false, contentWarningLevel: ContentWarningLevel.INHERIT, contentWarningText: null};

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: unknown) => descriptor}));
vi.mock('@app/features/app/state/RuntimeConfig', () => ({default: {localInstanceDomain: 'fluxer.test'}}));
vi.mock('@app/features/user/state/Users', () => ({
	default: {
		getUser: () => undefined,
		cacheUsers: () => {},
		getCurrentUser: () => ({matureContentAllowed: true, matureContentCheckComplete: true}),
	},
}));
vi.mock('@app/features/channel/state/Channels', () => ({default: {getChannel: (id: string) => channels.get(id)}}));
vi.mock('@app/features/guild/state/Guilds', () => ({default: {getGuild: () => guild}}));
vi.mock('@app/features/app/state/GeoIP', () => ({default: {ageRestrictedGeos: []}}));
vi.mock('@app/features/devtools/state/DeveloperOptions', () => ({default: {mockMatureContentGateReason: 'none'}}));
vi.mock('@app/features/moderation/utils/MatureContentGeoUtils', () => ({
	getEffectiveMatureContentGeoContext: () => ({countryCode: null, regionCode: null}),
}));
vi.mock('@app/features/user/state/SyncedField', () => ({makeSyncedField: async () => {}}));

const {default: GuildMatureContentAgree, MatureContentGateReason} = await import(
	'@app/features/guild/state/GuildMatureContentAgree'
);

function put(channel: WireChannel): void {
	channels.set(channel.id, new Channel(channel));
}

beforeEach(() => {
	channels.clear();
	GuildMatureContentAgree.revokeChannel('parent');
	GuildMatureContentAgree.revokeChannel('forum');
	GuildMatureContentAgree.revokeCategory('category');
	put({id: 'category', guild_id: 'guild', type: ChannelTypes.GUILD_CATEGORY, nsfw_override: null});
	put({id: 'parent', guild_id: 'guild', type: ChannelTypes.GUILD_TEXT, parent_id: 'category', nsfw_override: null});
	put({id: 'thread', guild_id: 'guild', type: ChannelTypes.PUBLIC_THREAD, parent_id: 'parent'});
});

describe('GuildMatureContentAgree for threads', () => {
	it('gates a thread whose parent is mature and records consent on the parent', () => {
		put({id: 'parent', guild_id: 'guild', type: ChannelTypes.GUILD_TEXT, parent_id: 'category', nsfw_override: true});
		expect(GuildMatureContentAgree.getGateReason({channelId: 'thread'})).toBe(MatureContentGateReason.CONSENT_REQUIRED);
		const resolved = GuildMatureContentAgree.getResolvedContext({channelId: 'thread'});
		expect(resolved.scope).toBe('channel');
		expect(resolved.scopeId).toBe('parent');
		GuildMatureContentAgree.agreeToChannel('thread');
		expect(GuildMatureContentAgree.agreedChannelIds).toEqual(['parent']);
		expect(GuildMatureContentAgree.getGateReason({channelId: 'thread'})).toBe(MatureContentGateReason.NONE);
	});

	it('gates a thread under a mature category through its parent', () => {
		put({id: 'category', guild_id: 'guild', type: ChannelTypes.GUILD_CATEGORY, nsfw_override: true});
		const resolved = GuildMatureContentAgree.getResolvedContext({channelId: 'thread'});
		expect(resolved.scope).toBe('category');
		expect(resolved.scopeId).toBe('category');
		expect(GuildMatureContentAgree.getGateReason({channelId: 'thread'})).toBe(MatureContentGateReason.CONSENT_REQUIRED);
		GuildMatureContentAgree.agreeToCategory('category');
		expect(GuildMatureContentAgree.getGateReason({channelId: 'thread'})).toBe(MatureContentGateReason.NONE);
	});

	it('does not gate a thread in a safe parent', () => {
		expect(GuildMatureContentAgree.getGateReason({channelId: 'thread'})).toBe(MatureContentGateReason.NONE);
	});

	it('keeps forum agreements out of synced preferences', () => {
		put({id: 'forum', guild_id: 'guild', type: ChannelTypes.GUILD_FORUM, nsfw_override: true});
		GuildMatureContentAgree.agreeToChannel('forum');
		expect(GuildMatureContentAgree.agreedChannelIds).toEqual([]);
		expect(GuildMatureContentAgree.hasAgreedToChannel('forum')).toBe(true);
	});
});
