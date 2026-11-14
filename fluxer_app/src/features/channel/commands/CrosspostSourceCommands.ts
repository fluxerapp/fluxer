// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {CrosspostSourceResponse} from '@fluxer/schema/src/domains/message/CrosspostSourceSchemas';

const logger = new Logger('CrosspostSourceCommands');

export async function fetchCrosspostSource(channelId: string, messageId: string): Promise<CrosspostSourceResponse> {
	try {
		const response = await http.get<CrosspostSourceResponse>(
			Endpoints.CHANNEL_MESSAGE_CROSSPOST_SOURCE(channelId, messageId),
		);
		return response.body;
	} catch (error) {
		logger.error(`Failed to fetch crosspost source for message ${messageId} in ${channelId}:`, error);
		throw error;
	}
}
