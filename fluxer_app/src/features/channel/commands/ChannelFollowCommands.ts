// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {FollowedChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelFollowSchemas';

const logger = new Logger('ChannelFollowCommands');

export async function followChannel(
	sourceChannelId: string,
	targetChannelId: string,
): Promise<FollowedChannelResponse> {
	try {
		const response = await http.post<FollowedChannelResponse>(Endpoints.CHANNEL_FOLLOWERS(sourceChannelId), {
			body: {webhook_channel_id: targetChannelId},
		});
		return response.body;
	} catch (error) {
		logger.error(`Failed to follow channel ${sourceChannelId} into ${targetChannelId}:`, error);
		throw error;
	}
}
