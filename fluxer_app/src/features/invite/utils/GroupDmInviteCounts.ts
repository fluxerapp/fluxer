// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import Channels from '@app/features/channel/state/Channels';

interface GroupDmInviteCounts {
	memberCount: number;
	hasLocalChannel: boolean;
}

export function getGroupDmInviteCounts(params: {
	channelId: string;
	inviteMemberCount?: number | null;
}): GroupDmInviteCounts {
	const channel = Channels.getChannel(params.channelId);
	if (!channel) {
		return {
			memberCount: params.inviteMemberCount ?? 0,
			hasLocalChannel: false,
		};
	}
	const memberIds = new Set(channel.recipientIds);
	const currentUserId = Authentication.currentUserId;
	if (currentUserId) {
		memberIds.add(currentUserId);
	}
	return {
		memberCount: memberIds.size,
		hasLocalChannel: true,
	};
}
