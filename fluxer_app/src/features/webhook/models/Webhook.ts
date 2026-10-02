// SPDX-License-Identifier: AGPL-3.0-or-later

import {webhookUrl} from '@app/features/messaging/utils/MessagingUrlUtils';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import {WebhookTypes} from '@fluxer/constants/src/ChannelConstants';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {
	WebhookSourceChannel,
	WebhookSourceGuild,
	Webhook as WireWebhook,
} from '@fluxer/schema/src/domains/webhook/WebhookSchemas';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';

export class Webhook {
	readonly id: string;
	readonly guildId: string;
	readonly channelId: string;
	readonly name: string;
	readonly avatar: string | null;
	readonly type: number;
	readonly token: string | null;
	readonly sourceGuild: WebhookSourceGuild | null;
	readonly sourceChannel: WebhookSourceChannel | null;
	readonly creatorId: string;
	readonly createdAt: Date;
	private readonly creatorSnapshot: UserPartial;

	constructor(webhook: WireWebhook) {
		this.id = webhook.id;
		this.guildId = webhook.guild_id;
		this.channelId = webhook.channel_id;
		this.name = webhook.name;
		this.avatar = webhook.avatar ?? null;
		this.type = webhook.type ?? WebhookTypes.INCOMING;
		this.token = webhook.token ?? null;
		this.sourceGuild = webhook.source_guild ?? null;
		this.sourceChannel = webhook.source_channel ?? null;
		this.creatorId = webhook.user.id;
		this.createdAt = new Date(SnowflakeUtils.extractTimestamp(webhook.id));
		this.creatorSnapshot = webhook.user;
		Users.cacheUsers([webhook.user]);
	}

	get isChannelFollower(): boolean {
		return this.type === WebhookTypes.CHANNEL_FOLLOWER;
	}

	get webhookUrl(): string | null {
		if (this.token == null) {
			return null;
		}
		return webhookUrl(this.id, this.token);
	}

	get creator(): User | null {
		return Users.getUser(this.creatorId)!;
	}

	get displayName(): string {
		return this.name;
	}

	withUpdates(updates: Partial<WireWebhook>): Webhook {
		return new Webhook({
			...this.toWire(this.creatorSnapshot),
			...updates,
		});
	}

	toJSON(): WireWebhook {
		const creator = this.creator;
		return this.toWire(creator ? creator.toJSON() : this.creatorSnapshot);
	}

	private toWire(user: UserPartial): WireWebhook {
		return {
			id: this.id,
			guild_id: this.guildId,
			channel_id: this.channelId,
			user,
			name: this.name,
			avatar: this.avatar,
			type: this.type,
			...(this.token != null ? {token: this.token} : {}),
			...(this.sourceGuild ? {source_guild: this.sourceGuild} : {}),
			...(this.sourceChannel ? {source_channel: this.sourceChannel} : {}),
		};
	}
}
