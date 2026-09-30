// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {createRequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {User} from '@app/api/models/User';
import {UserChannelService} from '@app/api/user/services/UserChannelService';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {JobCancelledError, type WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const SYSTEM_USER_ID: UserID = createUserID(0n);
const ALL_USERS_PAGE_SIZE = 100;
const CURSOR_TTL_SECONDS = 7 * 24 * 60 * 60;
const INELIGIBLE_FLAGS = UserFlags.DELETED | UserFlags.SELF_DELETED | UserFlags.DISABLED;
const PayloadSchema = z.union([
	z.object({
		content: z.string().min(1).max(4000),
		user_ids: z.array(z.string().regex(/^\d+$/)).min(1),
	}),
	z.object({
		content: z.string().min(1).max(4000),
		all_users: z.literal(true),
	}),
]);

function isEligibleRecipient(user: User): boolean {
	return user.id !== SYSTEM_USER_ID && !user.isBot && !user.isSystem && (user.flags & INELIGIBLE_FLAGS) === 0n;
}

async function* allUserRecipients(helpers: WorkerTaskHelpers): AsyncGenerator<UserID> {
	const {userRepository, kvClient} = getWorkerDependencies();
	const cursorKey = `system_dm:all_users_cursor:${helpers.jobId}`;
	let pageState = await kvClient.get(cursorKey);
	if (pageState !== null) {
		helpers.logger.info('Resuming system DM broadcast from saved cursor');
	}
	do {
		const page = await userRepository.scanAllUsersPage(ALL_USERS_PAGE_SIZE, pageState);
		for (const user of page.users) {
			if (isEligibleRecipient(user)) {
				yield user.id;
			}
		}
		pageState = page.pageState;
		if (pageState !== null) {
			await kvClient.setex(cursorKey, CURSOR_TTL_SECONDS, pageState);
		}
	} while (pageState !== null);
	await kvClient.del(cursorKey);
}

async function* listedRecipients(userIds: Array<string>): AsyncGenerator<UserID> {
	for (const raw of userIds) {
		yield createUserID(BigInt(raw));
	}
}

export async function sendSystemDm(payload: unknown, helpers: WorkerTaskHelpers): Promise<void> {
	const parsed = PayloadSchema.parse(payload);
	const {content} = parsed;
	const total = 'user_ids' in parsed ? parsed.user_ids.length : null;
	const recipients = 'user_ids' in parsed ? listedRecipients(parsed.user_ids) : allUserRecipients(helpers);
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
	for await (const recipientId of recipients) {
		if (await helpers.shouldCancel()) {
			helpers.logger.info({sent, failed, total}, 'System DM job cancelled mid-flight');
			requestCache.clear();
			throw new JobCancelledError();
		}
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
			helpers.logger.warn({recipientId: recipientId.toString(), error}, 'System DM send failed for recipient');
		}
		if ((sent + failed) % ALL_USERS_PAGE_SIZE === 0) {
			requestCache.clear();
			await helpers.reportProgress(sent + failed, total, `${sent} sent, ${failed} failed`);
		}
	}
	requestCache.clear();
	helpers.logger.info({sent, failed, total: sent + failed}, 'System DM job complete');
}
