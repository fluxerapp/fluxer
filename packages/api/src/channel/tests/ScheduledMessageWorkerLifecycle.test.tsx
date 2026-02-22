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
	getChannelMessages,
	getScheduledMessage,
	grantStaffAccess,
	joinGuild,
	messageFromAuthorContains,
	removeGuildMember,
	scheduleMessage,
	triggerScheduledMessageWorker,
	updateScheduledMessage,
} from '@fluxer/api/src/channel/tests/ScheduledMessageTestUtils';
import {createGuild} from '@fluxer/api/src/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Scheduled message worker lifecycle', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	it('delivers scheduled message when permissions remain', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'scheduled-messages');
		await grantStaffAccess(harness, owner.userId);
		const channel = await createGuildChannel(harness, owner.token, guild.id, 'scheduled');

		const content = 'scheduled message goes through';
		const scheduled = await scheduleMessage(harness, channel.id, owner.token, content);

		await triggerScheduledMessageWorker(harness, owner.userId, scheduled.id);

		const fetched = await getScheduledMessage(harness, scheduled.id, owner.token, 404);
		expect(fetched).toBeNull();

		const messages = await getChannelMessages(harness, channel.id, owner.token);
		expect(messageFromAuthorContains(messages, owner.userId, content)).toBe(true);
	});

	it('reschedules pending message before worker execution', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'scheduled-messages');
		await grantStaffAccess(harness, owner.userId);
		const channel = await createGuildChannel(harness, owner.token, guild.id, 'scheduled');

		const content = 'scheduled message initial content';
		const scheduled = await scheduleMessage(harness, channel.id, owner.token, content);

		const oldScheduledAt = new Date(scheduled.scheduled_at);

		const updatedContent = 'scheduled message updated content';
		const newLocalTime = new Date(Date.now() + 5 * 60 * 1000);
		const newLocalStr = newLocalTime.toISOString();

		const updated = await updateScheduledMessage(harness, scheduled.id, owner.token, {
			content: updatedContent,
			scheduled_local_at: newLocalStr,
			timezone: 'America/Los_Angeles',
		});

		expect(updated.status).toBe('pending');
		expect(updated.scheduled_local_at).toBe(newLocalStr);
		expect(updated.timezone).toBe('America/Los_Angeles');

		const updatedScheduledAt = new Date(updated.scheduled_at);
		expect(updatedScheduledAt.getTime()).toBeGreaterThan(oldScheduledAt.getTime());

		await triggerScheduledMessageWorker(harness, owner.userId, updated.id);

		const respMessages = await getChannelMessages(harness, channel.id, owner.token);
		expect(messageFromAuthorContains(respMessages, owner.userId, updatedContent)).toBe(true);
		expect(messageFromAuthorContains(respMessages, owner.userId, content)).toBe(false);

		const fetched = await getScheduledMessage(harness, updated.id, owner.token, 404);
		expect(fetched).toBeNull();
	});

	it('marks scheduled message invalid when access lost', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'scheduled-messages');
		await grantStaffAccess(harness, member.userId);
		const channel = await createGuildChannel(harness, owner.token, guild.id, 'scheduled');

		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await joinGuild(harness, member.token, invite.code);

		const content = 'scheduled message invalidation';
		const scheduled = await scheduleMessage(harness, channel.id, member.token, content);

		await removeGuildMember(harness, owner.token, guild.id, member.userId);

		await triggerScheduledMessageWorker(harness, member.userId, scheduled.id);

		const fetched = await getScheduledMessage(harness, scheduled.id, member.token);
		expect(fetched).not.toBeNull();
		expect(fetched!.status).toBe('invalid');
		expect(fetched!.status_reason).not.toBeNull();
		expect(fetched!.status_reason).not.toBe('');

		const messages = await getChannelMessages(harness, channel.id, owner.token);
		const hasContent = messages.some((msg) => msg.content === content);
		expect(hasContent).toBe(false);
	});
});
