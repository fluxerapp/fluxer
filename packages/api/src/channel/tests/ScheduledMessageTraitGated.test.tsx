/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {createTestAccount} from '@fluxer/api/src/auth/tests/AuthTestUtils';
import {
	createGuildChannel,
	grantStaffAccess,
	scheduleMessage,
} from '@fluxer/api/src/channel/tests/ScheduledMessageTestUtils';
import {createGuild} from '@fluxer/api/src/guild/tests/GuildTestUtils';
import {ensureSessionStarted} from '@fluxer/api/src/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {createBuilder} from '@fluxer/api/src/test/TestRequestBuilder';
import {beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Scheduled message staff gating', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	it('rejects scheduling message before staff flag granted', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'scheduled-flag');
		const channel = await createGuildChannel(harness, owner.token, guild.id, 'scheduled-channel');
		await ensureSessionStarted(harness, owner.token);

		await createBuilder(harness, owner.token)
			.post(`/channels/${channel.id}/messages/schedule`)
			.body({
				content: 'trying to schedule',
				scheduled_local_at: new Date(Date.now() + 60 * 1000).toISOString(),
				timezone: 'UTC',
			})
			.expect(403)
			.execute();

		await grantStaffAccess(harness, owner.userId);

		const scheduled = await scheduleMessage(harness, channel.id, owner.token, 'enabled now');
		expect(scheduled.id).toBeDefined();
		expect(scheduled.id).not.toBe('');
	});
});
