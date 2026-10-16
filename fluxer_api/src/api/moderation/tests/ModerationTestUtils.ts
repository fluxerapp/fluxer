// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
export async function banUser(
	harness: ApiTestHarness,
	moderatorToken: string,
	guildId: string,
	targetUserId: string,
	deleteMessageDays = 0,
): Promise<void> {
	await createBuilder<void>(harness, moderatorToken)
		.put(`/guilds/${guildId}/bans/${targetUserId}`)
		.body({
			delete_message_days: deleteMessageDays,
		})
		.expect(204)
		.execute();
}
