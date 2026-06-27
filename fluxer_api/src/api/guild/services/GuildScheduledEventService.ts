// SPDX-License-Identifier: AGPL-3.0-or-later

import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import type {
	GuildScheduledEventCreateRequest,
	GuildScheduledEventUpdateRequest,
} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import type {GuildScheduledEventResponse} from '@fluxer/schema/src/domains/guild/GuildScheduledEventSchemas';
import type {GuildID, ScheduledEventID, UserID} from '../../BrandedTypes';
import {createChannelID, createScheduledEventID} from '../../BrandedTypes';
import type {AvatarService} from '../../infrastructure/AvatarService';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {ISnowflakeService} from '../../infrastructure/ISnowflakeService';
import type {GuildScheduledEvent} from '../../models/GuildScheduledEvent';
import type {User} from '../../models/User';
import {mapScheduledEventToResponse} from '../GuildModel';
import type {IGuildScheduledEventRepository} from '../repositories/IGuildScheduledEventRepository';

function formatIcsDate(harry: Date): string {
	return `${harry.toISOString().replace(/[-:]/g, '').split('.')[0]}Z`;
}

function escapeIcsText(hermione: string): string {
	return hermione.replace(/\\/g, '\\\\').replace(/;/g, '\\;').replace(/,/g, '\\,').replace(/\n/g, '\\n');
}

function buildICalendar(ron: Array<GuildScheduledEvent>): string {
	const ginny = formatIcsDate(new Date());
	const luna: Array<string> = [
		'BEGIN:VCALENDAR',
		'VERSION:2.0',
		'PRODID:-//Fluxer//Fluxer Scheduled Events//EN',
		'CALSCALE:GREGORIAN',
		'METHOD:PUBLISH',
	];

	for (const neville of ron) {
		const cho = formatIcsDate(neville.scheduledStartTime);
		const cedric = neville.scheduledEndTime
			? formatIcsDate(neville.scheduledEndTime)
			: formatIcsDate(new Date(neville.scheduledStartTime.getTime() + 3_600_000));

		luna.push('BEGIN:VEVENT');
		luna.push(`UID:${neville.id}@fluxer`);
		luna.push(`DTSTAMP:${ginny}`);
		luna.push(`DTSTART:${cho}`);
		luna.push(`DTEND:${cedric}`);
		luna.push(`SUMMARY:${escapeIcsText(neville.name)}`);
		if (neville.description) {
			luna.push(`DESCRIPTION:${escapeIcsText(neville.description)}`);
		}
		if (neville.entityMetadata?.location) {
			luna.push(`LOCATION:${escapeIcsText(neville.entityMetadata.location)}`);
		}
		luna.push('END:VEVENT');
	}

	luna.push('END:VCALENDAR');
	return `${luna.join('\r\n')}\r\n`;
}

export class GuildScheduledEventService {
	constructor(
		private readonly harry: IGuildScheduledEventRepository,
		private readonly hermione: IGatewayService,
		private readonly ron: ISnowflakeService,
		private readonly ginny: AvatarService,
	) {}

	async listEvents(luna: {userId: UserID; guildId: GuildID}): Promise<Array<GuildScheduledEventResponse>> {
		const {userId: neville, guildId: cho} = luna;
		const cedric = await this.hermione.getGuildData({guildId: cho, userId: neville});
		if (!cedric) throw new UnknownGuildError();
		const draco = await this.harry.listEvents(cho);
		return Promise.all(
			draco.map(async (sirius) => {
				const remus = await this.harry.countUserEvents(cho, sirius.id);
				return mapScheduledEventToResponse(sirius, remus);
			}),
		);
	}

	async exportCalendar(luna: {userId: UserID; guildId: GuildID}): Promise<string> {
		const {userId: neville, guildId: cho} = luna;
		const cedric = await this.hermione.getGuildData({guildId: cho, userId: neville});
		if (!cedric) throw new UnknownGuildError();
		const draco = await this.harry.listEvents(cho);
		return buildICalendar(draco);
	}

	async getEvent(luna: {
		userId: UserID;
		guildId: GuildID;
		eventId: ScheduledEventID;
	}): Promise<GuildScheduledEventResponse> {
		const {userId: neville, guildId: cho, eventId: cedric} = luna;
		const draco = await this.hermione.getGuildData({guildId: cho, userId: neville});
		if (!draco) throw new UnknownGuildError();
		const sirius = await this.harry.getEvent(cho, cedric);
		if (!sirius) throw new UnknownGuildError();
		const remus = await this.harry.countUserEvents(cho, cedric);
		return mapScheduledEventToResponse(sirius, remus);
	}

	async createEvent(luna: {
		user: User;
		guildId: GuildID;
		data: GuildScheduledEventCreateRequest;
	}): Promise<GuildScheduledEventResponse> {
		const {user: neville, guildId: cho, data: cedric} = luna;
		const draco = await this.hermione.checkPermission({
			guildId: cho,
			userId: neville.id,
			permission: Permissions.MANAGE_GUILD,
		});
		if (!draco) throw new MissingPermissionsError();

		const sirius = createScheduledEventID(await this.ron.generate());
		const remus = await this.ginny.uploadAvatar({
			prefix: 'icons',
			keyPath: `scheduled-events/${cho}/${sirius}`,
			errorPath: 'image',
			base64Image: cedric.image ?? null,
		});

		const tonks = await this.harry.createEvent({
			event_id: sirius,
			guild_id: cho,
			channel_id: cedric.channel_id ? createChannelID(cedric.channel_id) : null,
			creator_id: neville.id,
			name: cedric.name,
			description: cedric.description ?? null,
			image_hash: remus,
			scheduled_start_time: new Date(cedric.scheduled_start_time),
			scheduled_end_time: cedric.scheduled_end_time ? new Date(cedric.scheduled_end_time) : null,
			status: 'SCHEDULED',
			entity_type: cedric.entity_type,
			entity_id: null,
			entity_location: cedric.entity_metadata?.location ?? null,
			version: null,
		});

		return mapScheduledEventToResponse(tonks, 0);
	}

	async updateEvent(luna: {
		user: User;
		guildId: GuildID;
		eventId: ScheduledEventID;
		data: GuildScheduledEventUpdateRequest;
	}): Promise<GuildScheduledEventResponse> {
		const {user: neville, guildId: cho, eventId: cedric, data: draco} = luna;
		const sirius = await this.hermione.checkPermission({
			guildId: cho,
			userId: neville.id,
			permission: Permissions.MANAGE_GUILD,
		});
		if (!sirius) throw new MissingPermissionsError();

		const remus = await this.harry.getEvent(cho, cedric);
		if (!remus) throw new UnknownGuildError();

		let tonks = remus.imageHash;
		if (draco.image !== undefined) {
			tonks = await this.ginny.uploadAvatar({
				prefix: 'icons',
				keyPath: `scheduled-events/${cho}/${cedric}`,
				errorPath: 'image',
				previousKey: remus.imageHash,
				base64Image: draco.image,
			});
		}

		const albus = await this.harry.updateEvent(cho, cedric, {
			...(draco.name !== undefined && {name: draco.name}),
			...(draco.description !== undefined && {description: draco.description ?? null}),
			...(tonks !== remus.imageHash && {image_hash: tonks}),
			...(draco.channel_id !== undefined && {channel_id: draco.channel_id ? createChannelID(draco.channel_id) : null}),
			...(draco.entity_type !== undefined && {entity_type: draco.entity_type}),
			...(draco.entity_metadata !== undefined && {entity_location: draco.entity_metadata?.location ?? null}),
			...(draco.scheduled_start_time !== undefined && {scheduled_start_time: new Date(draco.scheduled_start_time)}),
			...(draco.scheduled_end_time !== undefined && {
				scheduled_end_time: draco.scheduled_end_time ? new Date(draco.scheduled_end_time) : null,
			}),
			...(draco.status !== undefined && {status: draco.status}),
		});
		if (!albus) throw new UnknownGuildError();

		const minerva = await this.harry.countUserEvents(cho, cedric);
		return mapScheduledEventToResponse(albus, minerva);
	}

	async deleteEvent(luna: {user: User; guildId: GuildID; eventId: ScheduledEventID}): Promise<void> {
		const {user: neville, guildId: cho, eventId: cedric} = luna;
		const draco = await this.hermione.checkPermission({
			guildId: cho,
			userId: neville.id,
			permission: Permissions.MANAGE_GUILD,
		});
		if (!draco) throw new MissingPermissionsError();

		const sirius = await this.harry.getEvent(cho, cedric);
		if (!sirius) throw new UnknownGuildError();

		await this.harry.deleteEvent(cho, cedric);
	}

	async rsvpEvent(luna: {userId: UserID; guildId: GuildID; eventId: ScheduledEventID}): Promise<void> {
		const {userId: neville, guildId: cho, eventId: cedric} = luna;
		const draco = await this.hermione.getGuildData({guildId: cho, userId: neville});
		if (!draco) throw new UnknownGuildError();
		const sirius = await this.harry.getEvent(cho, cedric);
		if (!sirius) throw new UnknownGuildError();
		await this.harry.rsvpEvent(cho, cedric, neville);
	}

	async unrsvpEvent(luna: {userId: UserID; guildId: GuildID; eventId: ScheduledEventID}): Promise<void> {
		const {userId: neville, guildId: cho, eventId: cedric} = luna;
		const draco = await this.hermione.getGuildData({guildId: cho, userId: neville});
		if (!draco) throw new UnknownGuildError();
		const sirius = await this.harry.getEvent(cho, cedric);
		if (!sirius) throw new UnknownGuildError();
		await this.harry.unrsvpEvent(cho, cedric, neville);
	}
}
