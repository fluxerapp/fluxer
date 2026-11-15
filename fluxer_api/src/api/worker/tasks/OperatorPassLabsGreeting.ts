// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {
	OperatorPassLabsGreetingBusyError,
	type OperatorPassLabsGreetingDeps,
	retractOperatorPassLabsGreeting,
	runOperatorPassLabsGreeting,
} from '@app/api/operator_pass/OperatorPassLabsGreeting';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const operatorPassLabsGreeting: WorkerTaskHandler = async (payload, helpers) => {
	const deps = getWorkerDependencies();
	const greetingDeps: OperatorPassLabsGreetingDeps = {
		repository: deps.operatorPassRepository,
		userRepository: deps.userRepository,
		guildRepository: deps.guildRepository,
		channelRepository: deps.channelRepository,
		messages: deps.channelService.messages.send,
		deletion: deps.channelService.messages.deletion,
		snowflakeService: deps.snowflakeService,
		cacheService: deps.cacheService,
	};
	if (typeof payload.retract_user_id === 'string') {
		const userId = createUserID(BigInt(payload.retract_user_id));
		if (await retractOperatorPassLabsGreeting(greetingDeps, userId)) {
			helpers.logger.info({userId: userId.toString()}, 'Took a hidden Operator out of their #operators welcome');
		}
		return;
	}
	try {
		const summary = await runOperatorPassLabsGreeting(greetingDeps);
		if (summary && (summary.messages > 0 || summary.dropped > 0)) {
			helpers.logger.info({...summary}, 'Welcomed new Operators in #operators');
		}
	} catch (error) {
		if (error instanceof OperatorPassLabsGreetingBusyError) {
			helpers.logger.info('Skipped the #operators greetings, another run is still sending them');
			return;
		}
		throw error;
	}
};

export default operatorPassLabsGreeting;
