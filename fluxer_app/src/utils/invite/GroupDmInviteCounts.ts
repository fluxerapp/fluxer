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

import AuthenticationStore from '~/stores/AuthenticationStore';
import ChannelStore from '~/stores/ChannelStore';

export interface GroupDmInviteCounts {
	memberCount: number;
	hasLocalChannel: boolean;
}

export const getGroupDmInviteCounts = (params: {
	channelId: string;
	inviteMemberCount?: number | null;
}): GroupDmInviteCounts => {
	const channel = ChannelStore.getChannel(params.channelId);
	if (!channel) {
		return {
			memberCount: params.inviteMemberCount ?? 0,
			hasLocalChannel: false,
		};
	}

	const memberIds = new Set(channel.recipientIds);
	const currentUserId = AuthenticationStore.currentUserId;
	if (currentUserId) {
		memberIds.add(currentUserId);
	}

	return {
		memberCount: memberIds.size,
		hasLocalChannel: true,
	};
};
