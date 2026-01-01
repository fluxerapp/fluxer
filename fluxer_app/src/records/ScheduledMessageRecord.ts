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

export interface ScheduledMessageReference {
	message_id: string;
	channel_id?: string;
	guild_id?: string;
	type?: number;
}

export interface ScheduledAllowedMentions {
	parse?: Array<'users' | 'roles' | 'everyone'>;
	users?: Array<string>;
	roles?: Array<string>;
	replied_user?: boolean;
}

export interface ScheduledAttachment {
	id: string;
	filename: string;
	title?: string;
	description?: string;
	flags?: number;
}

export interface ScheduledMessagePayload {
	content?: string | null;
	embeds?: Array<unknown>;
	attachments?: Array<ScheduledAttachment>;
	message_reference?: ScheduledMessageReference;
	allowed_mentions?: ScheduledAllowedMentions;
	flags?: number;
	nonce?: string;
	favorite_meme_id?: string;
	sticker_ids?: Array<string>;
	tts?: boolean;
}

export type ScheduledMessageStatus = 'pending' | 'invalid';

export interface ScheduledMessageResponse {
	id: string;
	channel_id: string;
	scheduled_at: string;
	scheduled_local_at: string;
	timezone: string;
	status: ScheduledMessageStatus;
	status_reason: string | null;
	payload: ScheduledMessagePayload;
	created_at: string;
	invalidated_at: string | null;
}

export class ScheduledMessageRecord {
	readonly id: string;
	readonly channelId: string;
	readonly scheduledAt: Date;
	readonly scheduledLocalAt: string;
	readonly timezone: string;
	readonly payload: ScheduledMessagePayload;
	readonly status: ScheduledMessageStatus;
	readonly statusReason: string | null;
	readonly createdAt: Date;
	readonly invalidatedAt: Date | null;

	constructor(response: ScheduledMessageResponse) {
		this.id = response.id;
		this.channelId = response.channel_id;
		this.scheduledAt = new Date(response.scheduled_at);
		this.scheduledLocalAt = response.scheduled_local_at;
		this.timezone = response.timezone;
		this.payload = response.payload;
		this.status = response.status ?? 'pending';
		this.statusReason = response.status_reason;
		this.createdAt = new Date(response.created_at);
		this.invalidatedAt = response.invalidated_at ? new Date(response.invalidated_at) : null;
	}

	static fromResponse(response: ScheduledMessageResponse): ScheduledMessageRecord {
		return new ScheduledMessageRecord(response);
	}
}
