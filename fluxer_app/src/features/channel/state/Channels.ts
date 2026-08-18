// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Authentication from '@app/features/auth/state/Authentication';
import {Channel} from '@app/features/channel/models/Channel';
import ChannelDisplayName from '@app/features/channel/state/ChannelDisplayName';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import {filterViewableChannels} from '@app/features/messaging/utils/ChannelShared';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import Users from '@app/features/user/state/Users';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes, TEXT_BASED_CHANNEL_TYPES} from '@fluxer/constants/src/ChannelConstants';
import type {Channel as WireChannel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import {action, makeAutoObservable, observable} from 'mobx';

const EMPTY_CHANNELS: ReadonlyArray<Channel> = Object.freeze([]);
const sortDMs = (a: Channel, b: Channel) => {
	const aTimestamp = a.lastMessageId ? SnowflakeUtils.extractTimestamp(a.lastMessageId) : null;
	const bTimestamp = b.lastMessageId ? SnowflakeUtils.extractTimestamp(b.lastMessageId) : null;
	if (aTimestamp != null && bTimestamp != null) {
		return bTimestamp - aTimestamp;
	}
	if (aTimestamp != null) return -1;
	if (bTimestamp != null) return 1;
	return b.createdAt.getTime() - a.createdAt.getTime();
};

const isPrivateChannel = (channel: Channel) =>
	channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM;

const insertSorted = (channels: ReadonlyArray<Channel>, channel: Channel): Array<Channel> => {
	let low = 0;
	let high = channels.length;
	while (low < high) {
		const middle = (low + high) >>> 1;
		if (ChannelUtils.compareChannels(channels[middle], channel) <= 0) {
			low = middle + 1;
		} else {
			high = middle;
		}
	}
	const next = channels.slice();
	next.splice(low, 0, channel);
	return next;
};

class Channels {
	private readonly channelsById = new Map<string, Channel>();
	private readonly channelsByGuildId = new Map<string, ReadonlyArray<Channel>>();
	private privateChannelList: ReadonlyArray<Channel> = EMPTY_CHANNELS;
	private readonly optimisticChannelBackups = new Map<string, Channel>();

	constructor() {
		makeAutoObservable<this, 'channelsByGuildId' | 'privateChannelList'>(
			this,
			{channelsByGuildId: observable.shallow, privateChannelList: observable.ref},
			{autoBind: true},
		);
	}

	get channels(): ReadonlyArray<Channel> {
		return Array.from(this.channelsById.values());
	}

	get allChannels(): ReadonlyArray<Channel> {
		return this.channels;
	}

	get channelGroups(): {
		readonly byGuild: ReadonlyMap<string, ReadonlyArray<Channel>>;
		readonly dms: ReadonlyArray<Channel>;
		readonly privateChannels: ReadonlyArray<Channel>;
	} {
		return {byGuild: this.channelsByGuildId, dms: this.dmChannels, privateChannels: this.privateChannelList};
	}

	get dmChannels(): ReadonlyArray<Channel> {
		if (this.privateChannelList.length === 0) {
			return EMPTY_CHANNELS;
		}
		return this.privateChannelList.slice().sort(sortDMs);
	}

	getChannel(channelId: string): Channel | undefined {
		return this.channelsById.get(channelId);
	}

	getGuildChannels(guildId: string): ReadonlyArray<Channel> {
		return this.channelsByGuildId.get(guildId) ?? EMPTY_CHANNELS;
	}

	getPrivateChannels(): ReadonlyArray<Channel> {
		return this.privateChannelList;
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
		this.deleteChannelRecord(channelId);
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
	private removeChannel(channelId: string): void {
		this.clearOptimisticallyRemovedChannel(channelId);
		this.deleteChannelRecord(channelId);
	}

	@action
	private deleteChannelRecord(channelId: string): void {
		const channel = this.channelsById.get(channelId);
		this.channelsById.delete(channelId);
		if (channel) {
			this.removeChannelFromIndex(channel);
		}
		ChannelDisplayName.removeChannel(channelId);
	}

	@action
	private setChannel(channel: Channel | WireChannel): void {
		const record = channel instanceof Channel ? channel : new Channel(channel);
		const existing = this.channelsById.get(record.id);
		if (existing && existing !== record && existing.equals(record)) {
			return;
		}
		this.channelsById.set(record.id, record);
		this.indexChannel(existing, record);
		ChannelDisplayName.syncChannel(record);
	}

	@action
	private indexChannel(previous: Channel | undefined, next: Channel): void {
		if (previous && previous.guildId === next.guildId && isPrivateChannel(previous) === isPrivateChannel(next)) {
			this.replaceChannelInIndex(previous, next);
			return;
		}
		if (previous) {
			this.removeChannelFromIndex(previous);
		}
		this.addChannelToIndex(next);
	}

	@action
	private addChannelToIndex(channel: Channel): void {
		if (channel.guildId) {
			const list = this.channelsByGuildId.get(channel.guildId) ?? EMPTY_CHANNELS;
			this.channelsByGuildId.set(channel.guildId, insertSorted(list, channel));
			return;
		}
		if (isPrivateChannel(channel)) {
			this.privateChannelList = [...this.privateChannelList, channel];
		}
	}

	@action
	private removeChannelFromIndex(channel: Channel): void {
		if (channel.guildId) {
			const list = this.channelsByGuildId.get(channel.guildId);
			if (!list) {
				return;
			}
			const next = list.filter((entry) => entry.id !== channel.id);
			if (next.length === list.length) {
				return;
			}
			if (next.length === 0) {
				this.channelsByGuildId.delete(channel.guildId);
			} else {
				this.channelsByGuildId.set(channel.guildId, next);
			}
			return;
		}
		if (isPrivateChannel(channel)) {
			const next = this.privateChannelList.filter((entry) => entry.id !== channel.id);
			if (next.length !== this.privateChannelList.length) {
				this.privateChannelList = next;
			}
		}
	}

	@action
	private replaceChannelInIndex(previous: Channel, next: Channel): void {
		if (next.guildId) {
			const list = this.channelsByGuildId.get(next.guildId);
			const index = list ? list.findIndex((entry) => entry.id === next.id) : -1;
			if (!list || index === -1) {
				this.addChannelToIndex(next);
				return;
			}
			const updated = list.slice();
			updated[index] = next;
			if (ChannelUtils.compareChannels(previous, next) !== 0) {
				updated.sort(ChannelUtils.compareChannels);
			}
			this.channelsByGuildId.set(next.guildId, updated);
			return;
		}
		if (!isPrivateChannel(next)) {
			return;
		}
		const index = this.privateChannelList.findIndex((entry) => entry.id === next.id);
		if (index === -1) {
			this.addChannelToIndex(next);
			return;
		}
		const updated = this.privateChannelList.slice();
		updated[index] = next;
		this.privateChannelList = updated;
	}

	@action
	handleConnectionOpen({channels}: {channels: ReadonlyArray<WireChannel>}): void {
		this.channelsById.clear();
		this.channelsByGuildId.clear();
		this.privateChannelList = EMPTY_CHANNELS;
		ChannelDisplayName.clear();
		const allRecipients = channels
			.filter((channel) => channel.recipients && channel.recipients.length > 0)
			.flatMap((channel) => channel.recipients!);
		if (allRecipients.length > 0) {
			Users.cacheUsers(allRecipients);
		}
		for (const channel of channels) {
			this.setChannel(channel);
		}
		const userId = Authentication.currentUserId;
		if (!userId) {
			return;
		}
		const personalNotesChannel: WireChannel = {
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
			voice_connection_limit: null,
		};
		this.setChannel(personalNotesChannel);
	}

	@action
	handleGuildCreate(guild: GuildReadyData): void {
		if (guild.unavailable) {
			return;
		}
		const syncedChannelIds = new Set(guild.channels.map((channel) => channel.id));
		const existingGuildChannels = this.getGuildChannels(guild.id);
		for (const channel of existingGuildChannels) {
			if (!syncedChannelIds.has(channel.id)) {
				this.removeChannel(channel.id);
			}
		}
		for (const channel of guild.channels) {
			this.setChannel(channel);
		}
	}

	@action
	handleGuildDelete({guildId}: {guildId: string}): void {
		const guildChannels = this.getGuildChannels(guildId);
		if (guildChannels.length === 0) return;
		const ids: Array<string> = [];
		for (const channel of guildChannels) ids.push(channel.id);
		for (const id of ids) {
			this.removeChannel(id);
		}
	}

	@action
	handleChannelCreate({channel}: {channel: WireChannel}): void {
		this.setChannel(channel);
	}

	@action
	handlePassiveLastMessageUpdates({guildId, channels}: {guildId: string; channels: Record<string, string>}): boolean {
		let changed = false;
		for (const [channelId, lastMessageId] of Object.entries(channels)) {
			const channel = this.channelsById.get(channelId);
			if (!channel || channel.guildId !== guildId || !TEXT_BASED_CHANNEL_TYPES.has(channel.type)) {
				continue;
			}
			if (channel.lastMessageId != null && SnowflakeUtils.compare(lastMessageId, channel.lastMessageId) <= 0) {
				continue;
			}
			this.setChannel(
				new Channel({
					...channel.toJSON(),
					last_message_id: lastMessageId,
				}),
			);
			changed = true;
		}
		return changed;
	}

	@action
	handleChannelUpdateBulk({channels}: {channels: Array<WireChannel>}): void {
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
			new Channel({
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
		Users.cacheUsers([user]);
		const newRecipients = [...channel.recipientIds, user.id];
		this.setChannel(
			channel.withUpdates({
				recipients: newRecipients.map((id) => Users.getUser(id)!.toJSON()),
			}),
		);
	}

	@action
	handleChannelRecipientRemove({channelId, user}: {channelId: string; user: UserPartial}): void {
		const channel = this.channelsById.get(channelId);
		if (!channel) {
			return;
		}
		if (user.id === Authentication.currentUserId) {
			this.deleteChannelRecord(channelId);
			const history = RouterUtils.getHistory();
			const currentPath = history?.location.pathname ?? '';
			const expectedPath = Routes.dmChannel(channelId);
			if (currentPath.startsWith(expectedPath)) {
				NavigationCommands.selectChannel(ME);
			}
			return;
		}
		const newRecipients = channel.recipientIds.filter((id) => id !== user.id);
		this.setChannel(
			channel.withUpdates({
				recipients: newRecipients.map((id) => Users.getUser(id)!.toJSON()),
			}),
		);
	}

	@action
	handleChannelDelete({channel}: {channel: WireChannel}): void {
		this.removeChannel(channel.id);
		const history = RouterUtils.getHistory();
		const currentPath = history?.location.pathname ?? '';
		const guildId = channel.guild_id ?? ME;
		const expectedPath = guildId === ME ? Routes.dmChannel(channel.id) : Routes.guildChannel(guildId, channel.id);
		if (!currentPath.startsWith(expectedPath)) {
			return;
		}
		if (guildId === ME) {
			NavigationCommands.selectChannel(ME);
		} else {
			const guildChannels = this.getGuildChannels(guildId);
			const selectableChannel = filterViewableChannels(guildChannels)[0];
			if (selectableChannel) {
				NavigationCommands.selectChannel(guildId, selectableChannel.id);
			} else {
				NavigationCommands.selectChannel(ME);
			}
		}
	}

	@action
	handleMessageCreate({message}: {message: WireMessage}): void {
		const channel = this.channelsById.get(message.channel_id);
		if (!channel) {
			return;
		}
		if (!TEXT_BASED_CHANNEL_TYPES.has(channel.type)) {
			return;
		}
		if (channel.lastMessageId === message.id) {
			return;
		}
		this.setChannel(
			new Channel({
				...channel.toJSON(),
				last_message_id: message.id,
			}),
		);
	}

	@action
	handleGuildRoleDelete({guildId, roleId}: {guildId: string; roleId: string}): void {
		const guildChannels = this.getGuildChannels(guildId);
		if (guildChannels.length === 0) return;
		const snapshot = Array.from(guildChannels);
		for (const channel of snapshot) {
			if (!(roleId in channel.permissionOverwrites)) {
				continue;
			}
			const filteredOverwrites = Object.entries(channel.permissionOverwrites)
				.filter(([id]) => id !== roleId)
				.map(([, overwrite]) => overwrite.toJSON());
			this.setChannel(
				new Channel({
					...channel.toJSON(),
					permission_overwrites: filteredOverwrites,
				}),
			);
		}
	}
}

export default new Channels();
