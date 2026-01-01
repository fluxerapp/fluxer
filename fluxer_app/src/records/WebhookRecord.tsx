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

import type {UserPartial, UserRecord} from '~/records/UserRecord';
import UserStore from '~/stores/UserStore';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';
import {webhookUrl} from '~/utils/UrlUtils';

export type Webhook = Readonly<{
	id: string;
	guild_id: string;
	channel_id: string;
	user: UserPartial;
	name: string;
	avatar: string | null;
	token: string;
}>;

export class WebhookRecord {
	readonly id: string;
	readonly guildId: string;
	readonly channelId: string;
	readonly name: string;
	readonly avatar: string | null;
	readonly token: string;
	readonly creatorId: string;
	readonly createdAt: Date;
	private readonly creatorSnapshot: UserPartial;

	constructor(webhook: Webhook) {
		this.id = webhook.id;
		this.guildId = webhook.guild_id;
		this.channelId = webhook.channel_id;
		this.name = webhook.name;
		this.avatar = webhook.avatar ?? null;
		this.token = webhook.token;
		this.creatorId = webhook.user.id;
		this.createdAt = new Date(SnowflakeUtils.extractTimestamp(webhook.id));
		this.creatorSnapshot = webhook.user;
		UserStore.cacheUsers([webhook.user]);
	}

	get webhookUrl(): string {
		return webhookUrl(this.id, this.token);
	}

	get creator(): UserRecord | null {
		return UserStore.getUser(this.creatorId)!;
	}

	get displayName(): string {
		return this.name;
	}

	withUpdates(updates: Partial<Webhook>): WebhookRecord {
		return new WebhookRecord({
			id: updates.id ?? this.id,
			guild_id: updates.guild_id ?? this.guildId,
			channel_id: updates.channel_id ?? this.channelId,
			user: updates.user ?? this.creatorSnapshot,
			name: updates.name ?? this.name,
			avatar: updates.avatar ?? this.avatar,
			token: updates.token ?? this.token,
		});
	}

	toJSON(): Webhook {
		const creator = this.creator;
		return {
			id: this.id,
			guild_id: this.guildId,
			channel_id: this.channelId,
			user: creator ? creator.toJSON() : this.creatorSnapshot,
			name: this.name,
			avatar: this.avatar,
			token: this.token,
		};
	}
}
