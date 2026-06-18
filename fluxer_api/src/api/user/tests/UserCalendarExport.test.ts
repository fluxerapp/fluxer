// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {createChannelID, createMessageID, createUserID} from '../../BrandedTypes';
import {ScheduledMessage} from '../../models/ScheduledMessage';
import {buildScheduledMessagesCalendar} from '../UserCalendarExport';

describe('UserCalendarExport', () => {
	it('builds an iCalendar file from scheduled messages', () => {
		const calendar = buildScheduledMessagesCalendar([
			new ScheduledMessage({
				userId: createUserID(1n),
				id: createMessageID(2n),
				channelId: createChannelID(3n),
				scheduledAt: new Date('2030-01-02T03:04:05.000Z'),
				scheduledLocalAt: '2030-01-02T03:04:05',
				timezone: 'UTC',
				payload: {
					content: 'calendar, export\nline two',
				},
				createdAt: new Date('2029-12-31T00:00:00.000Z'),
			}),
		]);

		expect(calendar).toContain('BEGIN:VCALENDAR\r\n');
		expect(calendar).toContain('VERSION:2.0\r\n');
		expect(calendar).toContain('UID:fluxer-scheduled-message-2@fluxer.app\r\n');
		expect(calendar).toContain('DTSTAMP:20291231T000000Z\r\n');
		expect(calendar).toContain('DTSTART:20300102T030405Z\r\n');
		expect(calendar).toContain('SUMMARY:calendar\\, export\\nline two\r\n');
		expect(calendar).toContain('DESCRIPTION:Channel: 3\\nStatus: pending\\nContent: calendar\\, export\\nline two\r\n');
		expect(calendar.endsWith('END:VCALENDAR\r\n')).toBe(true);
	});
});
