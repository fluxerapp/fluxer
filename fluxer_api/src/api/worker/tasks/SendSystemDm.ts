// SPDX-License-Identifier: AGPL-3.0-or-later

import {JobCancelledError, type WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';
import {createUserID, type UserID} from '../../BrandedTypes';
import {createRequestCache} from '../../middleware/RequestCacheMiddleware';
import {UserChannelService} from '../../user/services/UserChannelService';
import {getWorkerDependencies} from '../WorkerContext';

const SYSTEM_USER_ID: UserID = createUserID(0n);
const PayloadSchema = z.object({
	content: z.string().min(1).max(4000),
	user_ids: z.array(z.string().regex(/^\d+$/)).min(1),
});

export async function sendSystemDm(payload: unknown, helpers: WorkerTaskHelpers): Promise<void> {
	const {content, user_ids} = PayloadSchema.parse(payload);
	const deps = getWorkerDependencies();
	const systemUser = await deps.userRepository.findUniqueAssert(SYSTEM_USER_ID);
	const userChannelService = new UserChannelService(
		deps.userRepository,
		deps.channelService,
		deps.channelRepository,
		deps.gatewayService,
		deps.snowflakeService,
		deps.userPermissionUtils,
		deps.limitConfigService,
	);
	const requestCache = createRequestCache();
	let sent = 0;
	let failed = 0;
	for (const raw of user_ids) {
		if (await helpers.shouldCancel()) {
			helpers.logger.info(
				{sent, failed, remaining: user_ids.length - sent - failed},
				'System DM job cancelled mid-flight',
			);
			requestCache.clear();
			throw new JobCancelledError();
		}
		const recipientId = createUserID(BigInt(raw));
		try {
			const channel = await userChannelService.ensureDmOpenForBothUsers({
				userId: SYSTEM_USER_ID,
				recipientId,
				userCacheService: deps.userCacheService,
				requestCache,
			});
			await deps.channelService.messages.send.sendMessage({
				user: systemUser,
				channelId: channel.id,
				data: {content},
				requestCache,
			});
			sent += 1;
		} catch (error) {
			failed += 1;
			helpers.logger.warn({recipientId: raw, error}, 'System DM send failed for recipient');
		}
	}
	requestCache.clear();
	helpers.logger.info({sent, failed, total: user_ids.length}, 'System DM job complete');
}
