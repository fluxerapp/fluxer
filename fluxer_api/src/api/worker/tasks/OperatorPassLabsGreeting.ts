// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	OperatorPassLabsGreetingBusyError,
	runOperatorPassLabsGreeting,
} from '@app/api/operator_pass/OperatorPassLabsGreeting';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const operatorPassLabsGreeting: WorkerTaskHandler = async (_payload, helpers) => {
	const deps = getWorkerDependencies();
	try {
		const summary = await runOperatorPassLabsGreeting({
			repository: deps.operatorPassRepository,
			userRepository: deps.userRepository,
			guildRepository: deps.guildRepository,
			channelRepository: deps.channelRepository,
			messages: deps.channelService.messages.send,
			snowflakeService: deps.snowflakeService,
			cacheService: deps.cacheService,
		});
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
