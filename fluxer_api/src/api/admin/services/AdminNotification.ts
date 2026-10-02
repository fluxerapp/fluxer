// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/api/Logger';

export async function trySendAdminNotification(
	send: () => Promise<boolean>,
	context: {action: string; targetId: string},
): Promise<boolean> {
	try {
		const sent = await send();
		if (!sent) {
			Logger.warn(context, 'Admin action notification was not sent');
		}
		return sent;
	} catch (error) {
		Logger.warn({error, ...context}, 'Failed to send admin action notification');
		return false;
	}
}
