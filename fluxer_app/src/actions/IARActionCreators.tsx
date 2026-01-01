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

import {Endpoints} from '~/Endpoints';
import http from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';

const logger = new Logger('IAR');

export const reportMessage = async (
	channelId: string,
	messageId: string,
	category: string,
	additionalInfo?: string,
): Promise<void> => {
	try {
		logger.debug(`Reporting message ${messageId} in channel ${channelId}`);
		await http.post({
			url: Endpoints.REPORT_MESSAGE,
			body: {
				channel_id: channelId,
				message_id: messageId,
				category,
				additional_info: additionalInfo || undefined,
			},
		});
		logger.info('Message report submitted successfully');
	} catch (error) {
		logger.error('Failed to submit message report:', error);
		throw error;
	}
};

export const reportUser = async (
	userId: string,
	category: string,
	additionalInfo?: string,
	guildId?: string,
): Promise<void> => {
	try {
		logger.debug(`Reporting user ${userId}${guildId ? ` in guild ${guildId}` : ''}`);
		await http.post({
			url: Endpoints.REPORT_USER,
			body: {
				user_id: userId,
				category,
				additional_info: additionalInfo || undefined,
				guild_id: guildId || undefined,
			},
		});
		logger.info('User report submitted successfully');
	} catch (error) {
		logger.error('Failed to submit user report:', error);
		throw error;
	}
};

export const reportGuild = async (guildId: string, category: string, additionalInfo?: string): Promise<void> => {
	try {
		logger.debug(`Reporting guild ${guildId}`);
		await http.post({
			url: Endpoints.REPORT_GUILD,
			body: {
				guild_id: guildId,
				category,
				additional_info: additionalInfo || undefined,
			},
		});
		logger.info('Guild report submitted successfully');
	} catch (error) {
		logger.error('Failed to submit guild report:', error);
		throw error;
	}
};
