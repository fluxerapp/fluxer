// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildEvent} from '@fluxer/schema/src/domains/guild/GuildEventSchemas';

const CRLF = '\r\n';

function requireSnowflake(value: string): string {
	if (!/^[0-9]+$/.test(value)) throw new Error('Invalid event or community identifier');
	return value;
}

function calendarTimestamp(value: string | Date): string {
	const date = value instanceof Date ? value : new Date(value);
	if (!Number.isFinite(date.getTime()) || date.getUTCFullYear() < 0 || date.getUTCFullYear() > 9999) {
		throw new Error('Invalid event calendar date');
	}
	return date
		.toISOString()
		.replace(/[-:]/g, '')
		.replace(/\.\d{3}Z$/, 'Z');
}

function hasUnsupportedControlCharacter(value: string): boolean {
	for (const character of value) {
		const code = character.charCodeAt(0);
		if (code <= 0x08 || code === 0x0b || code === 0x0c || (code >= 0x0e && code <= 0x1f) || code === 0x7f) {
			return true;
		}
	}
	return false;
}

function escapeText(value: string): string {
	if (hasUnsupportedControlCharacter(value)) {
		throw new Error('Event text contains an unsupported control character');
	}
	return value
		.replace(/\\/g, '\\\\')
		.replace(/\r\n|\r|\n/g, '\\n')
		.replace(/;/g, '\\;')
		.replace(/,/g, '\\,');
}

// RFC 5545 section 3.1 folds at 75 UTF-8 octets, including continuation whitespace.
// Iterate code points so a fold never splits a multi-byte character or surrogate pair.
function foldLine(value: string): string {
	const lines: Array<string> = [];
	let line = '';
	let octets = 0;
	for (const character of value) {
		const codePoint = character.codePointAt(0)!;
		const width = codePoint <= 0x7f ? 1 : codePoint <= 0x7ff ? 2 : codePoint <= 0xffff ? 3 : 4;
		if (octets + width > 75) {
			lines.push(line);
			line = ' ';
			octets = 1;
		}
		line += character;
		octets += width;
	}
	lines.push(line);
	return lines.join(CRLF);
}

/** Export exactly the supplied, already-visible events as an iCalendar snapshot. */
export function serializeGuildEventsCalendar(events: ReadonlyArray<GuildEvent>, exportedAt = new Date()): string {
	if (events.length === 0) throw new Error('There are no events to export');
	const stamp = calendarTimestamp(exportedAt);
	const lines = ['BEGIN:VCALENDAR', 'VERSION:2.0', 'PRODID:-//Fluxer//Community Events//EN', 'CALSCALE:GREGORIAN'];

	for (const event of events) {
		const guildId = requireSnowflake(event.guild_id);
		const eventId = requireSnowflake(event.id);
		const start = calendarTimestamp(event.starts_at);
		const end = event.ends_at === null ? null : calendarTimestamp(event.ends_at);
		if (end !== null && end <= start) throw new Error('Event end must be after its start');
		lines.push(
			'BEGIN:VEVENT',
			`UID:${guildId}-${eventId}@events.fluxer.app`,
			`DTSTAMP:${stamp}`,
			`CREATED:${calendarTimestamp(event.created_at)}`,
			`DTSTART:${start}`,
		);
		if (end !== null) lines.push(`DTEND:${end}`);
		lines.push(`SUMMARY:${escapeText(event.name)}`);
		if (event.description) lines.push(`DESCRIPTION:${escapeText(event.description)}`);
		if (event.location) lines.push(`LOCATION:${escapeText(event.location)}`);
		lines.push('END:VEVENT');
	}

	lines.push('END:VCALENDAR');
	return `${lines.map(foldLine).join(CRLF)}${CRLF}`;
}

export function guildEventCalendarFilename(guildId: string, eventId?: string): string {
	const community = requireSnowflake(guildId);
	return eventId === undefined
		? `fluxer-events-${community}.ics`
		: `fluxer-event-${community}-${requireSnowflake(eventId)}.ics`;
}
