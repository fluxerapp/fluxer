// SPDX-License-Identifier: AGPL-3.0-or-later

import {BaseChannelAuthService, type ChannelAuthOptions} from '@app/api/channel/services/BaseChannelAuthService';
import type {User} from '@app/api/models/User';
import {checkGuildVerificationWithResponse} from '@app/api/utils/GuildVerificationUtils';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';

export class MessageChannelAuthService extends BaseChannelAuthService {
	protected readonly options: ChannelAuthOptions = {
		errorOnMissingGuild: 'unknown_channel',
		validateNsfw: true,
	};

	async checkGuildVerification({
		user,
		guild,
		member,
	}: {
		user: User;
		guild: GuildResponse;
		member: GuildMemberResponse;
	}): Promise<void> {
		checkGuildVerificationWithResponse({user, guild, member});
	}
}
