// SPDX-License-Identifier: AGPL-3.0-or-later

import {ThreadStates} from '@fluxer/constants/src/ChannelConstants';
import type {ThreadState} from '@fluxer/constants/src/ChannelConstants';
import type {ILogger} from '../ILogger';
import type {ChannelDataRepository} from '../channel/repositories/ChannelDataRepository';
import type {IGatewayService} from '../infrastructure/IGatewayService';
import type {GuildID} from '../BrandedTypes';
import {mapThreadToResponse} from '../channel/services/ThreadService';

const BATCH_LIMIT = 200;

export class ThreadAutoArchiveJob {
	constructor(
		private readonly channelDataRepository: ChannelDataRepository,
		private readonly gatewayService: IGatewayService,
		private readonly logger: ILogger,
	) {}

	async run(): Promise<void> {
		const now = new Date();
		let archived = 0;
		try {
			const expiredIds = await this.channelDataRepository.listExpiredOpenThreads(now, BATCH_LIMIT);
			if (expiredIds.length === 0) return;

			await Promise.allSettled(
				expiredIds.map(async (threadId) => {
					try {
						const thread = await this.channelDataRepository.findUnique(threadId);
						if (!thread || !thread.isThread) return;
						if (thread.threadState !== ThreadStates.OPEN) return;

						const updated = await this.channelDataRepository.upsert({
							...thread.toRow(),
							thread_state: ThreadStates.ARCHIVED as ThreadState,
							thread_archived: true,
							thread_archive_timestamp: now,
							thread_expires_at: now,
						});
						archived++;
						await this.channelDataRepository.deleteOpenThread(threadId);

						if (updated.guildId) {
							await this.gatewayService.dispatchGuild({
								guildId: updated.guildId as GuildID,
								event: 'THREAD_UPDATE',
								data: mapThreadToResponse(updated),
							});
						}
					} catch (err) {
						this.logger.warn({err, threadId: threadId.toString()}, 'Failed to auto-archive thread');
					}
				}),
			);

			if (archived > 0) {
				this.logger.info({archived}, 'Thread auto-archive sweep complete');
			}
		} catch (err) {
			this.logger.warn({err}, 'Thread auto-archive job failed');
		}
	}
}
