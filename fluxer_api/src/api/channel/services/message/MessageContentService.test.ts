// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageContentService} from '@app/api/channel/services/message/MessageContentService';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import type {Channel} from '@app/api/models/Channel';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {GuildExplicitContentFilterTypes, GuildNSFWLevel} from '@fluxer/constants/src/GuildConstants';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {describe, expect, it} from 'vitest';

const service = new MessageContentService({} as UserRepository, {} as GuildRepository, {} as LimitConfigService);

const ageRestrictedGuild = {
	features: [],
	nsfw_level: GuildNSFWLevel.AGE_RESTRICTED,
	explicit_content_filter: GuildExplicitContentFilterTypes.DISABLED,
} as unknown as GuildResponse;

function channel(type: number, nsfwOverride: boolean | null): Channel {
	return {
		type,
		isNsfw: nsfwOverride ?? false,
		nsfwOverride,
		contentWarningLevel: 0,
		contentWarningText: null,
	} as unknown as Channel;
}

describe('attachmentNsfwMode in age-restricted guilds', () => {
	it('skips classification in channels that inherit the guild setting', () => {
		expect(
			service.attachmentNsfwMode({channel: channel(ChannelTypes.GUILD_TEXT, null), guild: ageRestrictedGuild}),
		).toBe('allow');
	});

	it('classifies uploads in a channel marked safe for work', () => {
		expect(
			service.attachmentNsfwMode({channel: channel(ChannelTypes.GUILD_TEXT, false), guild: ageRestrictedGuild}),
		).toBe('flag');
	});

	it('classifies uploads in a channel under a category marked safe for work', () => {
		expect(
			service.attachmentNsfwMode({
				channel: channel(ChannelTypes.GUILD_TEXT, null),
				parentCategory: channel(ChannelTypes.GUILD_CATEGORY, false),
				guild: ageRestrictedGuild,
			}),
		).toBe('flag');
	});
});
