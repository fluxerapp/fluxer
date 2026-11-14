// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {loadSelfMessageEligibilityContext} from '@app/api/channel/services/message/SelfMessageFilter';
import {getContentMessage} from '@app/api/content_i18n/ContentI18n';
import {Logger} from '@app/api/Logger';
import {noticeEmailAddress, systemDmsHidden} from '@app/api/user/services/SystemDmMessage';
import {deserializeSelfMessageFilter, SelfMessageFilterPayload} from '@app/api/worker/utils/SelfMessageFilterPayload';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const PayloadSchema = z.object({
	userId: z.string(),
	filter: SelfMessageFilterPayload,
});
const bulkDeleteSelfMessagesImmediate: WorkerTaskHandler = async (payload, helpers) => {
	const validated = PayloadSchema.parse(payload);
	helpers.logger.debug({payload: validated}, 'Processing bulkDeleteSelfMessagesImmediate task');
	const userId = createUserID(BigInt(validated.userId));
	const {channelService, userRepository, guildRepository, workerService, instanceConfigRepository, emailService} =
		getWorkerDependencies();
	const user = await userRepository.findUnique(userId);
	if (!user) {
		helpers.logger.info({userId: validated.userId}, 'User no longer exists, skipping deletion');
		return;
	}
	const eligibilityContext = await loadSelfMessageEligibilityContext(userId, {userRepository, guildRepository});
	const deletionService = channelService.userMessageDeletion;
	const result = await deletionService.deleteUserMessagesFiltered(
		userId,
		deserializeSelfMessageFilter(validated.filter),
		eligibilityContext,
		(deleted) => helpers.logger.debug(`Deleted ${deleted} messages so far`),
	);
	Logger.debug(
		{userId: userId.toString(), totalDeleted: result.totalDeleted, channelCount: result.channelCount},
		'Filtered bulk self message deletion completed',
	);
	const totals = {message_count: result.totalDeleted, channel_count: result.channelCount};
	if (await systemDmsHidden(instanceConfigRepository)) {
		const email = await noticeEmailAddress(user, instanceConfigRepository);
		if (email) {
			await emailService.send('bulk_message_deletion_completed', email, totals, user.locale);
		}
		return;
	}
	const content = getContentMessage('bulk_message_deletion.complete', user.locale, totals);
	if (content && content.length > 0) {
		await workerService.addJob(
			'sendSystemDm',
			{
				content,
				user_ids: [userId.toString()],
			},
			{maxAttempts: 5},
		);
	}
};

export default bulkDeleteSelfMessagesImmediate;
