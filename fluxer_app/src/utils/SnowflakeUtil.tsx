/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {
	atNextMillisecond as atNextMillisecondImpl,
	atPreviousMillisecond as atPreviousMillisecondImpl,
	compare as compareImpl,
	extractTimestamp as extractTimestampImpl,
	fromTimestamp as fromTimestampImpl,
	fromTimestampWithSequence as fromTimestampWithSequenceImpl,
	isProbablyAValidSnowflake as isProbablyAValidSnowflakeImpl,
	type SnowflakeSequence,
} from './SnowflakeUtils';

function cast<T>(value: T): T {
	return value;
}

const SnowflakeUtil = {
	extractTimestamp(snowflake: string): number {
		return extractTimestampImpl(snowflake);
	},

	compare(snowflake1: string | null, snowflake2: string | null): number {
		return compareImpl(snowflake1, snowflake2);
	},

	atPreviousMillisecond(snowflake: string): string {
		return atPreviousMillisecondImpl(snowflake);
	},

	atNextMillisecond(snowflake: string): string {
		return atNextMillisecondImpl(snowflake);
	},

	fromTimestamp(timestamp: number): string {
		return fromTimestampImpl(timestamp);
	},

	fromTimestampWithSequence(timestamp: number, sequence: SnowflakeSequence): string {
		return fromTimestampWithSequenceImpl(timestamp, sequence);
	},

	isProbablyAValidSnowflake(value: string | null | undefined): boolean {
		return isProbablyAValidSnowflakeImpl(value);
	},

	castChannelIdAsMessageId(channelId: string): string {
		return cast(channelId);
	},

	castMessageIdAsChannelId(messageId: string): string {
		return cast(messageId);
	},

	castGuildIdAsEveryoneGuildRoleId(guildId: string): string {
		return cast(guildId);
	},

	cast,
};

export default SnowflakeUtil;
