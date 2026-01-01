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

import type {Task} from 'graphile-worker';
import type {GuildID, ReportID, UserID} from '~/BrandedTypes';
import {createGuildID} from '~/BrandedTypes';
import {Logger} from '~/Logger';
import {
	getAuditLogSearchService,
	getGuildSearchService,
	getMessageSearchService,
	getReportSearchService,
	getUserSearchService,
} from '~/Meilisearch';
import {getWorkerDependencies} from '../WorkerContext';

interface RefreshSearchIndexPayload {
	index_type: 'guilds' | 'users' | 'reports' | 'audit_logs' | 'channel_messages';
	job_id: string;
	admin_user_id: string;
	guild_id?: string;
	user_id?: string;
}

function assertPayload(payload: unknown): asserts payload is RefreshSearchIndexPayload {
	if (typeof payload !== 'object' || payload === null) {
		throw new Error('invalid payload');
	}
	const data = payload as Record<string, unknown>;
	if (!['guilds', 'users', 'reports', 'audit_logs', 'channel_messages'].includes(data.index_type as string)) {
		throw new Error('invalid index_type');
	}
	if (typeof data.job_id !== 'string') {
		throw new Error('invalid job_id');
	}
	if (typeof data.admin_user_id !== 'string') {
		throw new Error('invalid admin_user_id');
	}
	if (data.index_type === 'channel_messages' && typeof data.guild_id !== 'string') {
		throw new Error('guild_id is required for the channel_messages index type');
	}
}

const BATCH_SIZE = 100;

const refreshSearchIndex: Task = async (payload, helpers) => {
	assertPayload(payload);
	helpers.logger.debug('Processing refreshSearchIndex task', {payload});

	const {redis, guildRepository, userRepository, adminRepository, reportRepository, channelRepository} =
		getWorkerDependencies();
	const progressKey = `index_refresh_status:${payload.job_id}`;

	try {
		await redis.set(
			progressKey,
			JSON.stringify({
				status: 'in_progress',
				index_type: payload.index_type,
				total: 0,
				indexed: 0,
				started_at: new Date().toISOString(),
			}),
			'EX',
			3600,
		);

		let indexedCount = 0;

		if (payload.index_type === 'guilds') {
			const guildSearchService = getGuildSearchService();
			if (!guildSearchService) {
				throw new Error('Search is not enabled');
			}

			await guildSearchService.deleteAllDocuments();

			let lastGuildId: GuildID | undefined;
			let hasMore = true;

			while (hasMore) {
				const guilds = await guildRepository.listAllGuildsPaginated(BATCH_SIZE, lastGuildId);

				if (guilds.length > 0) {
					await guildSearchService.indexGuilds(guilds);
					indexedCount += guilds.length;
					lastGuildId = guilds[guilds.length - 1].id;

					await redis.set(
						progressKey,
						JSON.stringify({
							status: 'in_progress',
							index_type: payload.index_type,
							total: indexedCount,
							indexed: indexedCount,
							started_at: new Date().toISOString(),
						}),
						'EX',
						3600,
					);

					Logger.debug({count: guilds.length, total: indexedCount}, 'Indexed guild batch');
				}

				hasMore = guilds.length === BATCH_SIZE;
			}

			Logger.debug({count: indexedCount}, 'Refreshed guild search index');
		} else if (payload.index_type === 'users') {
			const userSearchService = getUserSearchService();
			if (!userSearchService) {
				throw new Error('Search is not enabled');
			}

			await userSearchService.deleteAllDocuments();

			let lastUserId: UserID | undefined;
			let hasMore = true;

			while (hasMore) {
				const users = await userRepository.listAllUsersPaginated(BATCH_SIZE, lastUserId);

				if (users.length > 0) {
					for (const user of users) {
						await userSearchService.indexUser(user);
					}
					indexedCount += users.length;
					lastUserId = users[users.length - 1].id;

					await redis.set(
						progressKey,
						JSON.stringify({
							status: 'in_progress',
							index_type: payload.index_type,
							total: indexedCount,
							indexed: indexedCount,
							started_at: new Date().toISOString(),
						}),
						'EX',
						3600,
					);

					Logger.debug({count: users.length, total: indexedCount}, 'Indexed user batch');
				}

				hasMore = users.length === BATCH_SIZE;
			}

			Logger.debug({count: indexedCount}, 'Refreshed user search index');
		} else if (payload.index_type === 'reports') {
			const reportSearchService = getReportSearchService();
			if (!reportSearchService) {
				throw new Error('Search is not enabled');
			}

			await reportSearchService.deleteAllDocuments();

			let lastReportId: ReportID | undefined;
			let hasMore = true;

			while (hasMore) {
				const reports = await reportRepository.listAllReportsPaginated(BATCH_SIZE, lastReportId);

				if (reports.length > 0) {
					await reportSearchService.indexReports(reports);
					indexedCount += reports.length;
					lastReportId = reports[reports.length - 1].reportId;

					await redis.set(
						progressKey,
						JSON.stringify({
							status: 'in_progress',
							index_type: payload.index_type,
							total: indexedCount,
							indexed: indexedCount,
							started_at: new Date().toISOString(),
						}),
						'EX',
						3600,
					);

					Logger.debug({count: reports.length, total: indexedCount}, 'Indexed report batch');
				}

				hasMore = reports.length === BATCH_SIZE;
			}

			Logger.debug({count: indexedCount}, 'Refreshed report search index');
		} else if (payload.index_type === 'audit_logs') {
			const auditLogSearchService = getAuditLogSearchService();
			if (!auditLogSearchService) {
				throw new Error('Search is not enabled');
			}

			await auditLogSearchService.deleteAllDocuments();

			let lastLogId: bigint | undefined;
			let hasMore = true;

			while (hasMore) {
				const logs = await adminRepository.listAllAuditLogsPaginated(BATCH_SIZE, lastLogId);

				if (logs.length > 0) {
					await auditLogSearchService.indexAuditLogs(logs);
					indexedCount += logs.length;
					lastLogId = logs[logs.length - 1].logId;

					await redis.set(
						progressKey,
						JSON.stringify({
							status: 'in_progress',
							index_type: payload.index_type,
							total: indexedCount,
							indexed: indexedCount,
							started_at: new Date().toISOString(),
						}),
						'EX',
						3600,
					);

					Logger.debug({count: logs.length, total: indexedCount}, 'Indexed audit log batch');
				}

				hasMore = logs.length === BATCH_SIZE;
			}

			Logger.debug({count: indexedCount}, 'Refreshed audit log search index');
		} else if (payload.index_type === 'channel_messages') {
			const messageSearchService = getMessageSearchService();
			if (!messageSearchService) {
				throw new Error('Search is not enabled');
			}

			const guildId = createGuildID(BigInt(payload.guild_id!));

			await messageSearchService.deleteGuildMessages(guildId);

			const channels = await channelRepository.listGuildChannels(guildId);

			for (const channel of channels) {
				Logger.debug({channelId: channel.id.toString()}, 'Indexing channel messages');

				await helpers.addJob(
					'indexChannelMessages',
					{
						channelId: channel.id.toString(),
					},
					{
						jobKey: `index-channel-${channel.id}-initial`,
						maxAttempts: 3,
					},
				);

				indexedCount += 1;
			}

			Logger.debug({channels: channels.length, guildId: guildId.toString()}, 'Queued channel message indexing jobs');
		}
	} catch (error) {
		Logger.error({error, payload}, 'Failed to refresh search index');

		await redis.set(
			progressKey,
			JSON.stringify({
				status: 'failed',
				index_type: payload.index_type,
				error: error instanceof Error ? error.message : 'Unknown error',
				failed_at: new Date().toISOString(),
			}),
			'EX',
			3600,
		);

		throw error;
	}
};

export default refreshSearchIndex;
