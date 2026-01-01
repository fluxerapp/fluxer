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

import type {ChannelID, MessageID, UserID} from '~/BrandedTypes';
import type {AttachmentRequestData} from '~/channel/AttachmentDTOs';
import type {AllowedMentionsRequest, MessageRequest} from '~/channel/ChannelModel';
import type {ScheduledMessageRow} from '~/database/CassandraTypes';

export type ScheduledMessageStatus = 'pending' | 'invalid';

export interface ScheduledMessageReference {
	message_id: string;
	channel_id?: string;
	guild_id?: string;
	type?: number;
}

export interface ScheduledAllowedMentions {
	parse?: AllowedMentionsRequest['parse'];
	users?: Array<string>;
	roles?: Array<string>;
	replied_user?: boolean;
}

export interface ScheduledMessagePayload {
	content?: string | null;
	embeds?: MessageRequest['embeds'];
	attachments?: Array<AttachmentRequestData>;
	message_reference?: ScheduledMessageReference;
	allowed_mentions?: ScheduledAllowedMentions;
	flags?: number;
	nonce?: string;
	favorite_meme_id?: string;
	sticker_ids?: Array<string>;
	tts?: boolean;
}

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

export class ScheduledMessage {
	readonly userId: UserID;
	readonly id: MessageID;
	readonly channelId: ChannelID;
	readonly scheduledAt: Date;
	readonly scheduledLocalAt: string;
	readonly timezone: string;
	readonly payload: ScheduledMessagePayload;
	readonly status: ScheduledMessageStatus;
	readonly statusReason: string | null;
	readonly createdAt: Date;
	readonly invalidatedAt: Date | null;

	constructor(params: {
		userId: UserID;
		id: MessageID;
		channelId: ChannelID;
		scheduledAt: Date;
		scheduledLocalAt: string;
		timezone: string;
		payload: ScheduledMessagePayload;
		status?: ScheduledMessageStatus;
		statusReason?: string | null;
		createdAt?: Date;
		invalidatedAt?: Date | null;
	}) {
		this.userId = params.userId;
		this.id = params.id;
		this.channelId = params.channelId;
		this.scheduledAt = params.scheduledAt;
		this.scheduledLocalAt = params.scheduledLocalAt;
		this.timezone = params.timezone;
		this.payload = params.payload;
		this.status = params.status ?? 'pending';
		this.statusReason = params.statusReason ?? null;
		this.createdAt = params.createdAt ?? new Date();
		this.invalidatedAt = params.invalidatedAt ?? null;
	}

	static fromRow(row: ScheduledMessageRow): ScheduledMessage {
		return new ScheduledMessage({
			userId: row.user_id,
			id: row.scheduled_message_id,
			channelId: row.channel_id,
			scheduledAt: row.scheduled_at,
			scheduledLocalAt: row.scheduled_local_at,
			timezone: row.timezone,
			payload: parsePayload(row.payload),
			status: toStatus(row.status),
			statusReason: row.status_reason,
			createdAt: row.created_at,
			invalidatedAt: row.invalidated_at,
		});
	}

	toRow(): ScheduledMessageRow {
		return {
			user_id: this.userId,
			scheduled_message_id: this.id,
			channel_id: this.channelId,
			payload: JSON.stringify(this.payload),
			scheduled_at: this.scheduledAt,
			scheduled_local_at: this.scheduledLocalAt,
			timezone: this.timezone,
			status: this.status,
			status_reason: this.statusReason,
			created_at: this.createdAt,
			invalidated_at: this.invalidatedAt,
		};
	}

	toResponse(): ScheduledMessageResponse {
		return {
			id: this.id.toString(),
			channel_id: this.channelId.toString(),
			scheduled_at: this.scheduledAt.toISOString(),
			scheduled_local_at: this.scheduledLocalAt,
			timezone: this.timezone,
			status: this.status,
			status_reason: this.statusReason,
			payload: this.payload,
			created_at: this.createdAt.toISOString(),
			invalidated_at: this.invalidatedAt?.toISOString() ?? null,
		};
	}

	parseToMessageRequest(): MessageRequest {
		return hydrateMessageRequest(this.payload);
	}
}

function parsePayload(payload: string): ScheduledMessagePayload {
	try {
		return JSON.parse(payload) as ScheduledMessagePayload;
	} catch (_error) {
		return {};
	}
}

function toStatus(value: string): ScheduledMessageStatus {
	return value === 'invalid' ? 'invalid' : 'pending';
}

function hydrateMessageRequest(payload: ScheduledMessagePayload): MessageRequest {
	const message: MessageRequest = {
		content: payload.content,
		embeds: payload.embeds ?? [],
		attachments: payload.attachments,
		message_reference: payload.message_reference
			? {
					...payload.message_reference,
					message_id: BigInt(payload.message_reference.message_id),
					channel_id: payload.message_reference.channel_id ? BigInt(payload.message_reference.channel_id) : undefined,
					guild_id: payload.message_reference.guild_id ? BigInt(payload.message_reference.guild_id) : undefined,
				}
			: undefined,
		allowed_mentions: payload.allowed_mentions
			? {
					parse: payload.allowed_mentions.parse,
					users: payload.allowed_mentions.users?.map((value) => BigInt(value)),
					roles: payload.allowed_mentions.roles?.map((value) => BigInt(value)),
					replied_user: payload.allowed_mentions.replied_user,
				}
			: undefined,
		flags: payload.flags,
		nonce: payload.nonce,
		favorite_meme_id: payload.favorite_meme_id ? BigInt(payload.favorite_meme_id) : undefined,
		sticker_ids: payload.sticker_ids?.map((value) => BigInt(value)),
		tts: payload.tts,
	};
	return message;
}
