// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import {getContentMessage} from '@app/api/content_i18n/ContentI18n';
import {OperatorPassLabsRoleService} from '@app/api/operator_pass/OperatorPassLabsRole';
import {
	OPERATOR_PASS_VISIONARY_NOTICE_TASK,
	runOperatorPassVisionaryNotice,
} from '@app/api/operator_pass/OperatorPassVisionaryNotice';
import {trySendSystemDm} from '@app/api/user/services/SystemDmMessage';
import {UserChannelService} from '@app/api/user/services/UserChannelService';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const PayloadSchema = z.object({
	include_dm: z.boolean(),
	after_slot: z.number().int().default(-1),
});

const operatorPassVisionaryNotice: WorkerTaskHandler = async (payload, helpers) => {
	if (Config.instance.selfHosted) {
		return;
	}
	const {include_dm: includeDm, after_slot: afterSlot} = PayloadSchema.parse(payload);
	const deps = getWorkerDependencies();
	const userChannelService = new UserChannelService(
		deps.userRepository,
		deps.channelService,
		deps.channelRepository,
		deps.gatewayService,
		deps.snowflakeService,
		deps.userPermissionUtils,
		deps.limitConfigService,
	);
	const roles = new OperatorPassLabsRoleService(deps.guildRepository, deps.guildService);
	const result = await runOperatorPassVisionaryNotice(
		{
			kvClient: deps.kvClient,
			repository: deps.operatorPassRepository,
			userRepository: deps.userRepository,
			emailService: deps.emailService,
			logger: helpers.logger,
		},
		{
			includeDm,
			afterSlot,
			turnOnUrl: `${Config.endpoints.webApp}/channels/@operator-pass?view=visionary`,
			isLabsMember: (userId) => roles.isLabsMember(userId),
			sendDm: (user) =>
				user.id === SYSTEM_USER_ID
					? Promise.resolve(false)
					: trySendSystemDm(
							{channelService: deps.channelService, userChannelService, userCacheService: deps.userCacheService},
							deps.userRepository,
							user.id,
							getContentMessage('operator_pass.visionary_notice_dm_v2', user.locale),
						),
		},
	);
	helpers.logger.info({...result}, 'Visionary Operator Pass email run finished a pass');
	if (result.ran && result.next_slot !== null) {
		await helpers.addJob(OPERATOR_PASS_VISIONARY_NOTICE_TASK, {include_dm: includeDm, after_slot: result.next_slot});
	}
};

export default operatorPassVisionaryNotice;
