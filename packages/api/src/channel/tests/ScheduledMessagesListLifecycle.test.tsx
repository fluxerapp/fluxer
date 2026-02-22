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
	cancelScheduledMessage,
	createGuildChannel,
	getScheduledMessages,
	grantStaffAccess,
	scheduleMessage,
} from '@fluxer/api/src/channel/tests/ScheduledMessageTestUtils';
import {createGuild} from '@fluxer/api/src/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Scheduled messages list lifecycle', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	it('lists scheduled messages and removes after cancel', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'scheduled-list');
		await grantStaffAccess(harness, owner.userId);
		const channel = await createGuildChannel(harness, owner.token, guild.id, 'scheduled-list');

		const content = 'list scheduled';
		const scheduled = await scheduleMessage(harness, channel.id, owner.token, content);

		const list = await getScheduledMessages(harness, owner.token);
		const found = list.some((entry) => entry.id === scheduled.id);
		expect(found).toBe(true);

		await cancelScheduledMessage(harness, scheduled.id, owner.token);

		const listAfterCancel = await getScheduledMessages(harness, owner.token);
		const foundAfterCancel = listAfterCancel.some((entry) => entry.id === scheduled.id);
		expect(foundAfterCancel).toBe(false);
	});
});
