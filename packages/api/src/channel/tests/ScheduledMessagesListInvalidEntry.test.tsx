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
	createChannelInvite,
	createGuildChannel,
	getScheduledMessages,
	grantStaffAccess,
	joinGuild,
	removeGuildMember,
	scheduleMessage,
	triggerScheduledMessageWorker,
} from '@fluxer/api/src/channel/tests/ScheduledMessageTestUtils';
import {createGuild} from '@fluxer/api/src/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Scheduled messages list invalid entry', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	it('shows invalid scheduled message in list', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'scheduled-invalid');
		await grantStaffAccess(harness, member.userId);
		const channel = await createGuildChannel(harness, owner.token, guild.id, 'scheduled-invalid');

		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await joinGuild(harness, member.token, invite.code);

		const content = 'invalid scheduled';
		const scheduled = await scheduleMessage(harness, channel.id, member.token, content);

		await removeGuildMember(harness, owner.token, guild.id, member.userId);

		await triggerScheduledMessageWorker(harness, member.userId, scheduled.id);

		const list = await getScheduledMessages(harness, member.token);
		const entry = list.find((e) => e.id === scheduled.id);

		expect(entry).toBeDefined();
		expect(entry!.status).toBe('invalid');
		expect(entry!.status_reason).not.toBeNull();
		expect(entry!.status_reason).not.toBe('');
	});
});
