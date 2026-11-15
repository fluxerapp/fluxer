// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import {getOperatorPassWallService} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {isOperatorPassWallStanding} from '@app/api/operator_pass/OperatorPassWallPresentation';

export async function followOperatorPassWallStanding(before: User, after: User): Promise<void> {
	if (Config.instance.selfHosted) return;
	if (isOperatorPassWallStanding(before) === isOperatorPassWallStanding(after)) return;
	try {
		const wall = getOperatorPassWallService();
		if (!(await wall.getEntry(after.id))) return;
		await wall.publication.publishChanges([after.id]);
	} catch (error) {
		Logger.error(
			{error, userId: after.id.toString()},
			"Failed to follow an account standing change on the Operators' wall",
		);
	}
}
