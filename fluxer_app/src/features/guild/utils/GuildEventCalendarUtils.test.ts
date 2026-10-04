// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildEvent} from '@fluxer/schema/src/domains/guild/GuildEventSchemas';
import {describe, expect, it} from 'vitest';
import {guildEventCalendarFilename, serializeGuildEventsCalendar} from './GuildEventCalendarUtils';

const exportedAt = new Date('2026-10-04T06:30:10.000Z');
const event: GuildEvent = {
	id: '1234567890123456789',
	guild_id: '2234567890123456789',
	creator_id: '3234567890123456789',
	name: 'Community night',
	description: 'Bring a friend',
	location: 'The lounge',
	starts_at: '2026-11-01T01:30:00-04:00',
	ends_at: '2026-11-01T01:30:00-05:00',
	image_url: null,
	created_at: '2026-10-01T12:00:00.000Z',
};

function unfold(calendar: string): string {
	return calendar.replace(/\r\n[ \t]/g, '');
}

describe('GuildEvent calendar export', () => {
	it('exports UTC instants across a daylight-saving fallback with stable identity', () => {
		const calendar = serializeGuildEventsCalendar([event], exportedAt);
		expect(calendar).toContain('UID:2234567890123456789-1234567890123456789@events.fluxer.app\r\n');
		expect(calendar).toContain('DTSTART:20261101T053000Z\r\n');
		expect(calendar).toContain('DTEND:20261101T063000Z\r\n');
		expect(calendar).toContain('DTSTAMP:20261004T063010Z\r\n');
		expect(calendar).toContain('CREATED:20261001T120000Z\r\n');
		expect(calendar.startsWith('BEGIN:VCALENDAR\r\nVERSION:2.0\r\n')).toBe(true);
		expect(calendar.endsWith('END:VEVENT\r\nEND:VCALENDAR\r\n')).toBe(true);
		expect(calendar.replace(/\r\n/g, '')).not.toMatch(/[\r\n]/);
		const later = serializeGuildEventsCalendar([event], new Date('2026-10-05T06:00:00Z'));
		expect(later.match(/^UID:.*$/m)?.[0]).toBe(calendar.match(/^UID:.*$/m)?.[0]);
	});

	it('escapes event text without letting newlines inject calendar properties', () => {
		const calendar = unfold(
			serializeGuildEventsCalendar(
				[{
					...event,
					name: 'Meet, greet; share\\learn',
					description: 'Line one\r\nBEGIN:VALARM\nACTION:EMAIL\rEND:VALARM',
					location: 'Room: 1; West, wing',
				}],
				exportedAt,
			),
		);
		expect(calendar).toContain('SUMMARY:Meet\\, greet\\; share\\\\learn\r\n');
		expect(calendar).toContain('DESCRIPTION:Line one\\nBEGIN:VALARM\\nACTION:EMAIL\\nEND:VALARM\r\n');
		expect(calendar).toContain('LOCATION:Room: 1\\; West\\, wing\r\n');
		expect(calendar).not.toContain('\r\nBEGIN:VALARM');
		expect(calendar).not.toContain('\r\nACTION:EMAIL');
		expect(() => serializeGuildEventsCalendar([{...event, name: 'Invalid\u0000name'}], exportedAt)).toThrow();
	});

	it('folds long UTF-8 text to 75 octets and preserves emoji and combining marks', () => {
		const description = '日é🗓️e\u0301'.repeat(40);
		const calendar = serializeGuildEventsCalendar([{...event, description}], exportedAt);
		for (const line of calendar.split('\r\n')) {
			expect(new TextEncoder().encode(line).length).toBeLessThanOrEqual(75);
			expect(line).not.toMatch(/[\uD800-\uDBFF]$|^[\uDC00-\uDFFF]/u);
		}
		expect(calendar).toContain('\r\n ');
		expect(unfold(calendar)).toContain(`DESCRIPTION:${description}\r\n`);
		expect(new TextDecoder('utf-8', {fatal: true}).decode(new TextEncoder().encode(calendar))).toBe(calendar);
	});

	it('exports only selected events without mutating the source and distinguishes communities', () => {
		const other = Object.freeze({...event, guild_id: '4234567890123456789', name: 'Second community'});
		const selected = Object.freeze([Object.freeze({...event}), other]);
		const calendar = serializeGuildEventsCalendar(selected, exportedAt);
		expect(calendar.match(/BEGIN:VEVENT/g)).toHaveLength(2);
		expect(calendar).toContain('UID:4234567890123456789-1234567890123456789@events.fluxer.app');
		expect(serializeGuildEventsCalendar([other], exportedAt)).not.toContain('SUMMARY:Community night');
		expect(selected[0].name).toBe('Community night');
	});

	it('omits absent optional data without inventing a duration, attendees, or reminders', () => {
		const calendar = serializeGuildEventsCalendar(
			[{...event, ends_at: null, description: null, location: null}],
			exportedAt,
		);
		expect(calendar).not.toMatch(/DTEND:|DURATION:|DESCRIPTION:|LOCATION:|ATTENDEE|VALARM|RRULE|ATTACH/);
		expect(calendar).not.toContain(event.creator_id);
	});

	it('rejects invalid dates and impossible event ranges before producing a file', () => {
		expect(() => serializeGuildEventsCalendar([{...event, starts_at: 'not a date'}], exportedAt)).toThrow();
		expect(() => serializeGuildEventsCalendar([{...event, ends_at: event.starts_at}], exportedAt)).toThrow();
		expect(() => serializeGuildEventsCalendar([event], new Date(Number.NaN))).toThrow();
		expect(() => serializeGuildEventsCalendar([], exportedAt)).toThrow('There are no events to export');
	});

	it('uses safe stable filenames and refuses invalid IDs', () => {
		expect(guildEventCalendarFilename(event.guild_id)).toBe('fluxer-events-2234567890123456789.ics');
		expect(guildEventCalendarFilename(event.guild_id, event.id)).toBe(
			'fluxer-event-2234567890123456789-1234567890123456789.ics',
		);
		expect(() => guildEventCalendarFilename('../community')).toThrow();
		expect(() => guildEventCalendarFilename(event.guild_id, '../event')).toThrow();
		expect(() => serializeGuildEventsCalendar([{...event, id: '1\r\nBEGIN:VALARM'}], exportedAt)).toThrow();
	});
});
