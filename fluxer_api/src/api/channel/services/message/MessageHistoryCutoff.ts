// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Message} from '@app/api/models/Message';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';

export function assertMessageWithinHistoryCutoff(params: {message: Message | null; guild: GuildResponse}): void {
	const {message, guild} = params;
	if (!message) {
		throw new UnknownMessageError();
	}
	const cutoff = guild.message_history_cutoff;
	if (!cutoff) {
		throw new UnknownMessageError();
	}
	const messageTimestamp = snowflakeToDate(message.id).getTime();
	const cutoffTimestamp = new Date(cutoff).getTime();
	if (messageTimestamp < cutoffTimestamp) {
		throw new UnknownMessageError();
	}
}
