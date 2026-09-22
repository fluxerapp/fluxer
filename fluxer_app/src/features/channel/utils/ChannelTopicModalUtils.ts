// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';

export interface ChannelTopicModalProps {
	channelId: string;
}

export function getChannelTopicInfo(channelId: string) {
	const channel = Channels.getChannel(channelId);
	if (!channel?.topic) return null;
	return {
		channel,
		topic: channel.topic,
		title: `#${channel.name}`,
	};
}
