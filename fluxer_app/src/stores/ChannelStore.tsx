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

import {action, makeAutoObservable} from 'mobx';
import {ChannelTypes, ME} from '~/Constants';
import {Routes} from '~/Routes';
import {type Channel, ChannelRecord} from '~/records/ChannelRecord';
import type {GuildReadyData} from '~/records/GuildRecord';
import type {Message} from '~/records/MessageRecord';
import type {UserPartial} from '~/records/UserRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import ChannelDisplayNameStore from '~/stores/ChannelDisplayNameStore';
import UserStore from '~/stores/UserStore';
import * as ChannelUtils from '~/utils/ChannelUtils';
import * as RouterUtils from '~/utils/RouterUtils';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';

const sortDMs = (a: ChannelRecord, b: ChannelRecord) => {
	const aTimestamp = a.lastMessageId ? SnowflakeUtils.extractTimestamp(a.lastMessageId) : null;
	const bTimestamp = b.lastMessageId ? SnowflakeUtils.extractTimestamp(b.lastMessageId) : null;

	if (aTimestamp != null && bTimestamp != null) {
		return bTimestamp - aTimestamp;
	}
	if (aTimestamp != null) return -1;
	if (bTimestamp != null) return 1;

	return b.createdAt.getTime() - a.createdAt.getTime();
};

class ChannelStore {
	private readonly channelsById = new Map<string, ChannelRecord>();
	private readonly optimisticChannelBackups = new Map<string, ChannelRecord>();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get channels(): ReadonlyArray<ChannelRecord> {
		return Array.from(this.channelsById.values());
	}

	get allChannels(): ReadonlyArray<ChannelRecord> {
		return this.channels;
	}

	get dmChannels(): ReadonlyArray<ChannelRecord> {
		return this.channels
			.filter(
				(channel) => !channel.guildId && (channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM),
			)
			.sort(sortDMs);
	}

	getChannel(channelId: string): ChannelRecord | undefined {
		return this.channelsById.get(channelId);
	}

	getGuildChannels(guildId: string): ReadonlyArray<ChannelRecord> {
		return this.channels.filter((channel) => channel.guildId === guildId).sort(ChannelUtils.compareChannels);
	}

	getPrivateChannels(): ReadonlyArray<ChannelRecord> {
		return this.channels.filter(
			(channel) => !channel.guildId && (channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM),
		);
	}

	@action
	removeChannelOptimistically(channelId: string): void {
		if (this.optimisticChannelBackups.has(channelId)) {
			return;
		}

		const channel = this.channelsById.get(channelId);
		if (!channel) {
			return;
		}

		this.optimisticChannelBackups.set(channelId, channel);
		this.channelsById.delete(channelId);
		ChannelDisplayNameStore.removeChannel(channelId);
	}

	@action
	rollbackChannelDeletion(channelId: string): void {
		const channel = this.optimisticChannelBackups.get(channelId);
		if (!channel) {
			return;
		}

		this.setChannel(channel);
		this.optimisticChannelBackups.delete(channelId);
	}

	@action
	clearOptimisticallyRemovedChannel(channelId: string): void {
		this.optimisticChannelBackups.delete(channelId);
	}

	@action
	private setChannel(channel: ChannelRecord | Channel): void {
		const record = channel instanceof ChannelRecord ? channel : new ChannelRecord(channel);
		this.channelsById.set(record.id, record);
		ChannelDisplayNameStore.syncChannel(record);
	}

	@action
	handleConnectionOpen({channels}: {channels: ReadonlyArray<Channel>}): void {
		this.channelsById.clear();
		ChannelDisplayNameStore.clear();

		const allRecipients = channels
			.filter((channel) => channel.recipients && channel.recipients.length > 0)
			.flatMap((channel) => channel.recipients!);

		if (allRecipients.length > 0) {
			UserStore.cacheUsers(allRecipients);
		}

		for (const channel of channels) {
			this.setChannel(channel);
		}

		const userId = AuthenticationStore.currentUserId;
		if (!userId) {
			return;
		}
		const personalNotesChannel: Channel = {
			id: userId,
			type: ChannelTypes.DM_PERSONAL_NOTES,
			name: undefined,
			topic: null,
			url: null,
			last_message_id: null,
			last_pin_timestamp: null,
			recipients: undefined,
			parent_id: null,
			bitrate: null,
			user_limit: null,
		};
		this.setChannel(personalNotesChannel);
	}

	@action
	handleGuildCreate(guild: GuildReadyData): void {
		if (guild.unavailable) {
			return;
		}

		for (const channel of guild.channels) {
			this.setChannel(channel);
		}
	}

	@action
	handleGuildDelete({guildId}: {guildId: string}): void {
		for (const [channelId, channel] of Array.from(this.channelsById.entries())) {
			if (channel.guildId === guildId) {
				this.channelsById.delete(channelId);
			}
		}
	}

	@action
	handleChannelCreate({channel}: {channel: Channel}): void {
		this.setChannel(channel);
	}

	@action
	handleChannelUpdateBulk({channels}: {channels: Array<Channel>}): void {
		for (const channel of channels) {
			this.setChannel(channel);
		}
	}

	@action
	handleChannelPinsUpdate({channelId, lastPinTimestamp}: {channelId: string; lastPinTimestamp: string}): void {
		const channel = this.channelsById.get(channelId);
		if (!channel) {
			return;
		}

		this.setChannel(
			new ChannelRecord({
				...channel.toJSON(),
				last_pin_timestamp: lastPinTimestamp,
			}),
		);
	}

	@action
	handleChannelRecipientAdd({channelId, user}: {channelId: string; user: UserPartial}): void {
		const channel = this.channelsById.get(channelId);
		if (!channel) {
			return;
		}

		UserStore.cacheUsers([user]);
		const newRecipients = [...channel.recipientIds, user.id];
		this.setChannel(
			channel.withUpdates({
				recipients: newRecipients.map((id) => UserStore.getUser(id)!.toJSON()),
			}),
		);
	}

	@action
	handleChannelRecipientRemove({channelId, user}: {channelId: string; user: UserPartial}): void {
		const channel = this.channelsById.get(channelId);
		if (!channel) {
			return;
		}

		if (user.id === AuthenticationStore.currentUserId) {
			this.channelsById.delete(channelId);
			ChannelDisplayNameStore.removeChannel(channelId);

			const history = RouterUtils.getHistory();
			const currentPath = history?.location.pathname ?? '';
			const expectedPath = Routes.dmChannel(channelId);

			if (currentPath.startsWith(expectedPath)) {
				RouterUtils.transitionTo(Routes.ME);
			}
			return;
		}

		const newRecipients = channel.recipientIds.filter((id) => id !== user.id);

		this.setChannel(
			channel.withUpdates({
				recipients: newRecipients.map((id) => UserStore.getUser(id)!.toJSON()),
			}),
		);
	}

	@action
	handleChannelDelete({channel}: {channel: Channel}): void {
		this.clearOptimisticallyRemovedChannel(channel.id);
		this.channelsById.delete(channel.id);
		ChannelDisplayNameStore.removeChannel(channel.id);

		const history = RouterUtils.getHistory();
		const currentPath = history?.location.pathname ?? '';
		const guildId = channel.guild_id ?? ME;
		const expectedPath = guildId === ME ? Routes.dmChannel(channel.id) : Routes.guildChannel(guildId, channel.id);

		if (!currentPath.startsWith(expectedPath)) {
			return;
		}

		if (guildId === ME) {
			RouterUtils.transitionTo(Routes.ME);
		} else {
			const guildChannels = this.getGuildChannels(guildId);
			const selectableChannel = guildChannels.find((c) => c.type !== ChannelTypes.GUILD_CATEGORY);
			if (selectableChannel) {
				RouterUtils.transitionTo(Routes.guildChannel(guildId, selectableChannel.id));
			} else {
				RouterUtils.transitionTo(Routes.ME);
			}
		}
	}

	@action
	handleMessageCreate({message}: {message: Message}): void {
		const channel = this.channelsById.get(message.channel_id);
		if (!channel) {
			return;
		}

		this.setChannel(
			new ChannelRecord({
				...channel.toJSON(),
				last_message_id: message.id,
			}),
		);
	}

	@action
	handleGuildRoleDelete({guildId, roleId}: {guildId: string; roleId: string}): void {
		for (const [, channel] of this.channelsById) {
			if (channel.guildId !== guildId) {
				continue;
			}

			if (!(roleId in channel.permissionOverwrites)) {
				continue;
			}

			const filteredOverwrites = Object.entries(channel.permissionOverwrites)
				.filter(([id]) => id !== roleId)
				.map(([, overwrite]) => overwrite.toJSON());

			this.setChannel(
				new ChannelRecord({
					...channel.toJSON(),
					permission_overwrites: filteredOverwrites,
				}),
			);
		}
	}
}

export default new ChannelStore();
