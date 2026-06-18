// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ScheduledMessage} from '../models/ScheduledMessage';

const CRLF = '\r\n';
const DEFAULT_EVENT_DURATION_MS = 30 * 60 * 1000;

function formatIcsDate(date: Date): string {
	return date
		.toISOString()
		.replace(/[-:]/g, '')
		.replace(/\.\d{3}Z$/, 'Z');
}

function escapeIcsText(value: string): string {
	return value
		.replace(/\\/g, '\\\\')
		.replace(/\r\n|\r|\n/g, '\\n')
		.replace(/;/g, '\\;')
		.replace(/,/g, '\\,');
}

function scheduledMessageSummary(message: ScheduledMessage): string {
	const content = message.payload.content?.trim();
	if (!content) return 'Scheduled Fluxer message';
	return content.length > 80 ? `${content.slice(0, 77)}...` : content;
}

function scheduledMessageDescription(message: ScheduledMessage): string {
	const parts = [`Channel: ${message.channelId.toString()}`, `Status: ${message.status}`];
	if (message.payload.content?.trim()) {
		parts.push(`Content: ${message.payload.content.trim()}`);
	}
	return parts.join('\n');
}

function buildEvent(message: ScheduledMessage): Array<string> {
	const startsAt = message.scheduledAt;
	const endsAt = new Date(startsAt.getTime() + DEFAULT_EVENT_DURATION_MS);
	return [
		'BEGIN:VEVENT',
		`UID:fluxer-scheduled-message-${message.id.toString()}@fluxer.app`,
		`DTSTAMP:${formatIcsDate(message.createdAt)}`,
		`DTSTART:${formatIcsDate(startsAt)}`,
		`DTEND:${formatIcsDate(endsAt)}`,
		`SUMMARY:${escapeIcsText(scheduledMessageSummary(message))}`,
		`DESCRIPTION:${escapeIcsText(scheduledMessageDescription(message))}`,
		'END:VEVENT',
	];
}

export function buildScheduledMessagesCalendar(messages: Array<ScheduledMessage>): string {
	const lines = [
		'BEGIN:VCALENDAR',
		'VERSION:2.0',
		'PRODID:-//Fluxer//Scheduled Messages//EN',
		'CALSCALE:GREGORIAN',
		'METHOD:PUBLISH',
		'X-WR-CALNAME:Fluxer Scheduled Messages',
		...messages.flatMap((message) => buildEvent(message)),
		'END:VCALENDAR',
	];
	return `${lines.join(CRLF)}${CRLF}`;
}
