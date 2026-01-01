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

import React from 'react';
import {ChannelTypes} from '~/Constants';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildRecord} from '~/records/GuildRecord';
import type {MessageRecord} from '~/records/MessageRecord';
import type {UserRecord} from '~/records/UserRecord';
import ChannelStore from '~/stores/ChannelStore';
import GuildStore from '~/stores/GuildStore';
import PermissionStore from '~/stores/PermissionStore';
import UserStore from '~/stores/UserStore';
import * as ChannelUtils from '~/utils/ChannelUtils';

export interface ForwardedMessageContext {
	readonly sourceChannel: ChannelRecord | null;
	readonly sourceGuild: GuildRecord | null;
	readonly sourceUser: UserRecord | null;
	readonly hasAccessToSource: boolean;
	readonly displayName: string | null;
}

export const useForwardedMessageContext = (message: MessageRecord): ForwardedMessageContext => {
	const reference = message.messageReference;
	const sourceChannel = React.useMemo(() => {
		if (!reference) return null;
		return ChannelStore.getChannel(reference.channel_id) ?? null;
	}, [reference?.channel_id]);

	const sourceGuild = React.useMemo(() => {
		if (!sourceChannel || !reference?.guild_id) return null;
		return GuildStore.getGuild(reference.guild_id) ?? null;
	}, [reference?.guild_id, sourceChannel?.guildId]);

	const sourceUser = React.useMemo(() => {
		if (!sourceChannel) return null;
		if (sourceChannel.type === ChannelTypes.DM && sourceChannel.recipientIds.length > 0) {
			return UserStore.getUser(sourceChannel.recipientIds[0]) ?? null;
		}
		return null;
	}, [sourceChannel?.id, sourceChannel?.type, sourceChannel?.recipientIds]);

	const displayName = React.useMemo(() => {
		if (!sourceChannel) return null;
		if (
			sourceChannel.type === ChannelTypes.DM ||
			sourceChannel.type === ChannelTypes.GROUP_DM ||
			sourceChannel.type === ChannelTypes.DM_PERSONAL_NOTES
		) {
			return ChannelUtils.getDMDisplayName(sourceChannel);
		}
		return sourceChannel.name || null;
	}, [sourceChannel?.id, sourceChannel?.type, sourceChannel?.name]);

	const hasAccessToSource = React.useMemo(() => {
		if (!sourceChannel) return false;
		if (sourceChannel.guildId) {
			return PermissionStore.can(0n, {channelId: sourceChannel.id});
		}
		return true;
	}, [sourceChannel?.id]);

	return {
		sourceChannel,
		sourceGuild,
		sourceUser,
		hasAccessToSource,
		displayName,
	};
};
