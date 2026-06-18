// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {createGuild} from '../../guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {
	cancelScheduledMessage,
	createGuildChannel,
	exportScheduledMessageCalendar,
	exportScheduledMessagesCalendar,
	getScheduledMessages,
	grantStaffAccess,
	scheduleMessage,
} from './ScheduledMessageTestUtils';

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
	it('exports scheduled messages as iCalendar', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'scheduled-calendar');
		await grantStaffAccess(harness, owner.userId);
		const channel = await createGuildChannel(harness, owner.token, guild.id, 'scheduled-calendar');
		const scheduled = await scheduleMessage(
			harness,
			channel.id,
			owner.token,
			'calendar, export\nline two',
			new Date('2030-01-02T03:04:05.000Z'),
		);

		const {response, text} = await exportScheduledMessagesCalendar(harness, owner.token);

		expect(response.headers.get('content-type')).toContain('text/calendar');
		expect(response.headers.get('content-disposition')).toContain('fluxer-scheduled-messages.ics');
		expect(text).toContain('BEGIN:VCALENDAR');
		expect(text).toContain(`UID:fluxer-scheduled-message-${scheduled.id}@fluxer.app`);
		expect(text).toContain('DTSTART:20300102T030405Z');
		expect(text).toContain('SUMMARY:calendar\\, export\\nline two');
	});
	it('exports a single scheduled message as iCalendar', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'scheduled-single-calendar');
		await grantStaffAccess(harness, owner.userId);
		const channel = await createGuildChannel(harness, owner.token, guild.id, 'scheduled-single-calendar');
		const first = await scheduleMessage(harness, channel.id, owner.token, 'first exported message');
		const second = await scheduleMessage(harness, channel.id, owner.token, 'second message');

		const {response, text} = await exportScheduledMessageCalendar(harness, first.id, owner.token);

		expect(response.headers.get('content-type')).toContain('text/calendar');
		expect(response.headers.get('content-disposition')).toContain(`fluxer-scheduled-message-${first.id}.ics`);
		expect(text).toContain(`UID:fluxer-scheduled-message-${first.id}@fluxer.app`);
		expect(text).toContain('SUMMARY:first exported message');
		expect(text).not.toContain(`UID:fluxer-scheduled-message-${second.id}@fluxer.app`);
	});
});
