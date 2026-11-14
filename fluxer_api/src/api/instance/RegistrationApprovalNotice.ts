// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import {getWorkerService} from '@app/api/middleware/ServiceRegistry';

export async function enqueueRegistrationApprovalNotice(userId: string): Promise<void> {
	if (!Config.instance.selfHosted) return;
	try {
		await getWorkerService().addJob('notifyRegistrationApprovers', {user_id: userId});
	} catch (error) {
		Logger.warn({userId, error}, 'Could not queue the notice to registration approvers');
	}
}
