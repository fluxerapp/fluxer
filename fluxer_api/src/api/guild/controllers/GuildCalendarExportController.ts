// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {createGuildID} from '../../BrandedTypes';
import {LoginRequired} from '../../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

/** Render a single event as iCalendar VEVENT block. */
function buildVEvent(event: {
	id: string;
	name: string;
	description?: string | null;
	scheduled_start_time: string;
	scheduled_end_time?: string | null;
	entity_metadata?: {location?: string | null} | null;
	guild_id: string;
}): string {
	const dtStamp = new Date().toISOString().replace(/[-:]/g, '').split('.')[0] + 'Z';
	const dtStart = event.scheduled_start_time.replace(/[-:]/g, '').split('.')[0] + 'Z';
	const dtEnd = event.scheduled_end_time
		? event.scheduled_end_time.replace(/[-:]/g, '').split('.')[0] + 'Z'
		: dtStart;
	const location = event.entity_metadata?.location ?? '';
	const description = (event.description ?? '').replace(/
/g, '\n').replace(/,/g, '\,');
	return [
		'BEGIN:VEVENT',
		`UID:${event.id}@fluxer`,
		`DTSTAMP:${dtStamp}`,
		`DTSTART:${dtStart}`,
		`DTEND:${dtEnd}`,
		`SUMMARY:${event.name}`,
		description ? `DESCRIPTION:${description}` : '',
		location ? `LOCATION:${location}` : '',
		`URL:https://fluxer.app/guilds/${event.guild_id}/events/${event.id}`,
		'END:VEVENT',
	]
		.filter(Boolean)
		.join('
');
}

function buildICalendar(calName: string, vevents: Array<string>): string {
	return [
		'BEGIN:VCALENDAR',
		'VERSION:2.0',
		'PRODID:-//Fluxer//Fluxer Events//EN',
		'CALSCALE:GREGORIAN',
		'METHOD:PUBLISH',
		`X-WR-CALNAME:${calName}`,
		'X-WR-TIMEZONE:UTC',
		...vevents,
		'END:VCALENDAR',
	].join('
');
}

export function GuildCalendarExportController(app: HonoApp) {
	app.get(
		'/guilds/:guild_id/scheduled-events.ics',
		RateLimitMiddleware(RateLimitConfigs.GUILD_SCHEDULED_EVENTS_LIST),
		LoginRequired,
		Validator('param', GuildIdParam),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const events = await ctx.get('guildService').events.listEvents({userId, guildId});
			const vevents = events.map(buildVEvent);
			const ical = buildICalendar('Fluxer Community Events', vevents);
			return ctx.newResponse(ical, 200, {
				'Content-Type': 'text/calendar; charset=utf-8',
				'Content-Disposition': `attachment; filename="guild-${guildId}-events.ics"`,
			});
		},
	);
}
