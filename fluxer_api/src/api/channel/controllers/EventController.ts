// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Context} from 'hono';
import type {HonoEnv} from '../../types/HonoEnv';

export interface GuildEventRow {
	id: string;
	channel_id: string;
	guild_id: string;
	creator_id: string;
	name: string;
	description: string | null;
	location_channel_id: string | null;
	location_text: string | null;
	starts_at: Date;
	ends_at: Date;
	repeat_type: string;
	repeat_interval: number;
	created_at: Date;
	updated_at: Date;
	attendee_count?: number;
	is_attending?: boolean;
}

/**
 * Controller handling Calendar Event operations (CRUD, RSVP, ICS Export).
 */
export class EventController {
	/**
	 * GET /channels/:id/events
	 * Lists all events in a calendar channel.
	 */
	static async listEvents(c: Context<HonoEnv>): Promise<Response> {
		const channelId = c.req.param('id') || '';
		const user = c.get('user');

		const events = await c.get('apiContext').db.query<GuildEventRow>(
			`SELECT e.*, 
				(SELECT COUNT(*) FROM event_attendees WHERE event_id = e.id) as attendee_count,
				EXISTS(SELECT 1 FROM event_attendees WHERE event_id = e.id AND user_id = $2) as is_attending
			 FROM calendar_events e
			 WHERE e.channel_id = $1
			 ORDER BY e.starts_at ASC`,
			[channelId, user.id]
		);

		return c.json(events.rows);
	}

	/**
	 * GET /channels/:id/events/:eventId
	 */
	static async getEvent(c: Context<HonoEnv>): Promise<Response> {
		const channelId = c.req.param('id') || '';
		const eventId = c.req.param('eventId') || '';
		const user = c.get('user');

		const result = await c.get('apiContext').db.query<GuildEventRow>(
			`SELECT e.*, 
				(SELECT COUNT(*) FROM event_attendees WHERE event_id = e.id) as attendee_count,
				EXISTS(SELECT 1 FROM event_attendees WHERE event_id = e.id AND user_id = $3) as is_attending
			 FROM calendar_events e
			 WHERE e.channel_id = $1 AND e.id = $2`,
			[channelId, eventId, user.id]
		);

		if (result.rows.length === 0) {
			return c.json({ error: 'Event not found' }, 404);
		}

		return c.json(result.rows[0]);
	}

	/**
	 * POST /channels/:id/events
	 * Creates a new event in a calendar channel.
	 */
	static async createEvent(c: Context<HonoEnv>): Promise<Response> {
		const channelId = c.req.param('id') || '';
		const db = c.get('apiContext').db;
		const user = c.get('user');
		const body = await c.req.json<{
			name?: string;
			starts_at?: string;
			ends_at?: string;
			description?: string;
			location_channel_id?: string;
			location_text?: string;
			repeat_type?: string;
			repeat_interval?: number;
		}>();

		const {
			name,
			starts_at,
			ends_at,
			description = null,
			location_channel_id = null,
			location_text = null,
			repeat_type = 'never',
			repeat_interval = 1,
		} = body;

		if (!name || !starts_at || !ends_at) {
			return c.json({ error: 'Missing required fields: name, starts_at, ends_at' }, 400);
		}

		// Fetch channel to verify guild_id
		const channelRes = await db.query<{guild_id: string}>('SELECT guild_id FROM channels WHERE id = $1', [channelId]);
		if (channelRes.rows.length === 0) {
			return c.json({ error: 'Channel not found' }, 404);
		}

		const guildId = channelRes.rows[0].guild_id;

		const insertRes = await db.query<GuildEventRow>(
			`INSERT INTO calendar_events (
				channel_id, guild_id, creator_id, name, description,
				location_channel_id, location_text, starts_at, ends_at, repeat_type, repeat_interval
			) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)
			RETURNING *`,
			[
				channelId,
				guildId,
				user.id,
				name,
				description,
				location_channel_id,
				location_text,
				new Date(starts_at),
				new Date(ends_at),
				repeat_type,
				repeat_interval,
			]
		);

		const event = insertRes.rows[0];

		// Creator automatically attends
		await db.query(
			'INSERT INTO event_attendees (event_id, user_id) VALUES ($1, $2) ON CONFLICT DO NOTHING',
			[event.id, user.id]
		);

		return c.json({
			...event,
			attendee_count: 1,
			is_attending: true,
		}, 201);
	}

	/**
	 * PATCH /channels/:id/events/:eventId
	 */
	static async updateEvent(c: Context<HonoEnv>): Promise<Response> {
		const channelId = c.req.param('id') || '';
		const eventId = c.req.param('eventId') || '';
		const db = c.get('apiContext').db;
		const body = await c.req.json<{
			name?: string;
			description?: string;
			location_channel_id?: string;
			location_text?: string;
			starts_at?: string;
			ends_at?: string;
		}>();

		const existing = await db.query('SELECT * FROM calendar_events WHERE id = $1 AND channel_id = $2', [
			eventId,
			channelId,
		]);
		if (existing.rows.length === 0) {
			return c.json({ error: 'Event not found' }, 404);
		}

		const updateRes = await db.query<GuildEventRow>(
			`UPDATE calendar_events SET
				name = COALESCE($3, name),
				description = COALESCE($4, description),
				location_channel_id = COALESCE($5, location_channel_id),
				location_text = COALESCE($6, location_text),
				starts_at = COALESCE($7, starts_at),
				ends_at = COALESCE($8, ends_at),
				updated_at = NOW()
			 WHERE id = $1 AND channel_id = $2
			 RETURNING *`,
			[
				eventId,
				channelId,
				body.name ?? null,
				body.description ?? null,
				body.location_channel_id ?? null,
				body.location_text ?? null,
				body.starts_at ? new Date(body.starts_at) : null,
				body.ends_at ? new Date(body.ends_at) : null,
			]
		);

		return c.json(updateRes.rows[0]);
	}

	/**
	 * DELETE /channels/:id/events/:eventId
	 */
	static async deleteEvent(c: Context<HonoEnv>): Promise<Response> {
		const channelId = c.req.param('id') || '';
		const eventId = c.req.param('eventId') || '';
		const db = c.get('apiContext').db;

		const res = await db.query('DELETE FROM calendar_events WHERE id = $1 AND channel_id = $2', [
			eventId,
			channelId,
		]);
		if (res.rowCount === 0) {
			return c.json({ error: 'Event not found' }, 404);
		}

		return c.json({ success: true });
	}

	/**
	 * PUT /channels/:id/events/:eventId/rsvp
	 * Toggles the current user's RSVP status for the event.
	 */
	static async toggleRsvp(c: Context<HonoEnv>): Promise<Response> {
		const channelId = c.req.param('id') || '';
		const eventId = c.req.param('eventId') || '';
		const db = c.get('apiContext').db;
		const user = c.get('user');

		const check = await db.query(
			'SELECT 1 FROM event_attendees WHERE event_id = $1 AND user_id = $2',
			[eventId, user.id]
		);

		if (check.rows.length > 0) {
			await db.query('DELETE FROM event_attendees WHERE event_id = $1 AND user_id = $2', [
				eventId,
				user.id,
			]);
		} else {
			await db.query(
				'INSERT INTO event_attendees (event_id, user_id) VALUES ($1, $2) ON CONFLICT DO NOTHING',
				[eventId, user.id]
			);
		}

		const eventRes = await db.query<GuildEventRow>(
			`SELECT e.*, 
				(SELECT COUNT(*) FROM event_attendees WHERE event_id = e.id) as attendee_count,
				EXISTS(SELECT 1 FROM event_attendees WHERE event_id = e.id AND user_id = $3) as is_attending
			 FROM calendar_events e
			 WHERE e.channel_id = $1 AND e.id = $2`,
			[channelId, eventId, user.id]
		);

		return c.json(eventRes.rows[0]);
	}

	/**
	 * GET /channels/:id/events/:eventId/attendees
	 */
	static async listAttendees(c: Context<HonoEnv>): Promise<Response> {
		const eventId = c.req.param('eventId') || '';
		const db = c.get('apiContext').db;

		const result = await db.query<{
			user_id: string;
			rsvp_at: Date;
			username: string;
			display_name: string | null;
			avatar_hash: string | null;
		}>(
			`SELECT a.user_id, a.rsvp_at, u.username, u.display_name, u.avatar as avatar_hash
			 FROM event_attendees a
			 JOIN users u ON u.id = a.user_id
			 WHERE a.event_id = $1
			 ORDER BY a.rsvp_at ASC`,
			[eventId]
		);

		return c.json(result.rows);
	}

	/**
	 * GET /channels/:id/events/:eventId/export.ics
	 * Generates a CalDAV / iCalendar (.ics) export file for external calendars.
	 */
	static async exportIcs(c: Context<HonoEnv>): Promise<Response> {
		const channelId = c.req.param('id') || '';
		const eventId = c.req.param('eventId') || '';
		const db = c.get('apiContext').db;

		const result = await db.query<GuildEventRow>(
			'SELECT * FROM calendar_events WHERE channel_id = $1 AND id = $2',
			[channelId, eventId]
		);

		if (result.rows.length === 0) {
			return c.text('Event not found', 404);
		}

		const e = result.rows[0];
		const formatIcsDate = (d: Date) =>
			new Date(d).toISOString().replace(/[-:]/g, '').split('.')[0] + 'Z';

		const icsContent = [
			'BEGIN:VCALENDAR',
			'VERSION:2.0',
			'PRODID:-//Fluxer//Events Calendar//EN',
			'CALSCALE:GREGORIAN',
			'METHOD:PUBLISH',
			'BEGIN:VEVENT',
			`UID:${e.id}@fluxer.app`,
			`SUMMARY:${e.name}`,
			`DESCRIPTION:${(e.description || '').replace(/\n/g, '\\n')}`,
			`LOCATION:${e.location_text || 'Fluxer Voice Channel'}`,
			`DTSTART:${formatIcsDate(e.starts_at)}`,
			`DTEND:${formatIcsDate(e.ends_at)}`,
			`DTSTAMP:${formatIcsDate(e.created_at || new Date())}`,
			'STATUS:CONFIRMED',
			'END:VEVENT',
			'END:VCALENDAR',
		].join('\r\n');

		c.header('Content-Type', 'text/calendar; charset=utf-8');
		c.header('Content-Disposition', `attachment; filename="event-${e.id}.ics"`);
		return c.text(icsContent);
	}
}
