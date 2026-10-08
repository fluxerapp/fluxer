// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import {getPostForum, hidesMediaDownloads} from '@app/features/forum/utils/ForumChannelUtils';

const HIDEABLE_MEDIA_TYPES = new Set(['image', 'gif', 'gifv', 'video']);

export function isMediaDownloadHidden(channelId: string | null | undefined, mediaType: string | undefined): boolean {
	if (channelId == null || mediaType == null || !HIDEABLE_MEDIA_TYPES.has(mediaType)) return false;
	const channel = Channels.getChannel(channelId);
	if (!channel?.isThread()) return false;
	const forum = getPostForum(channel);
	return forum != null && hidesMediaDownloads(forum);
}
