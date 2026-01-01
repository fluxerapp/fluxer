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

import {ChannelTypes} from '~/Constants';
import {Endpoints} from '~/Endpoints';
import http from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import type {Channel} from '~/records/ChannelRecord';
import type {Invite} from '~/records/MessageRecord';
import ChannelStore from '~/stores/ChannelStore';
import InviteStore from '~/stores/InviteStore';

const logger = new Logger('Channels');

export interface ChannelRtcRegion {
	id: string;
	name: string;
	emoji: string;
}

export const create = async (
	guildId: string,
	params: Pick<Channel, 'name' | 'url' | 'type' | 'parent_id' | 'bitrate' | 'user_limit'>,
) => {
	try {
		const response = await http.post<Channel>(Endpoints.GUILD_CHANNELS(guildId), params);
		return response.body;
	} catch (error) {
		logger.error('Failed to create channel:', error);
		throw error;
	}
};

export const update = async (
	channelId: string,
	params: Partial<Pick<Channel, 'name' | 'topic' | 'url' | 'nsfw' | 'icon' | 'owner_id' | 'rtc_region'>>,
) => {
	try {
		const response = await http.patch<Channel>(Endpoints.CHANNEL(channelId), params);
		return response.body;
	} catch (error) {
		logger.error(`Failed to update channel ${channelId}:`, error);
		throw error;
	}
};

export const updateGroupDMNickname = async (channelId: string, userId: string, nickname: string | null) => {
	try {
		const response = await http.patch<Channel>({
			url: Endpoints.CHANNEL(channelId),
			body: {
				nicks: {
					[userId]: nickname,
				},
			},
		});
		return response.body;
	} catch (error) {
		logger.error(`Failed to update nickname for user ${userId} in channel ${channelId}:`, error);
		throw error;
	}
};

export interface RemoveChannelOptions {
	optimistic?: boolean;
}

export const remove = async (channelId: string, silent?: boolean, options?: RemoveChannelOptions) => {
	const channel = ChannelStore.getChannel(channelId);
	const isPrivateChannel =
		channel != null && !channel.guildId && (channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM);
	const shouldOptimisticallyRemove = options?.optimistic ?? isPrivateChannel;

	if (shouldOptimisticallyRemove) {
		ChannelStore.removeChannelOptimistically(channelId);
	}

	try {
		const url = silent ? `${Endpoints.CHANNEL(channelId)}?silent=true` : Endpoints.CHANNEL(channelId);
		await http.delete({url});
		if (shouldOptimisticallyRemove) {
			ChannelStore.clearOptimisticallyRemovedChannel(channelId);
		}
	} catch (error) {
		if (shouldOptimisticallyRemove) {
			ChannelStore.rollbackChannelDeletion(channelId);
		}
		logger.error(`Failed to delete channel ${channelId}:`, error);
		throw error;
	}
};

export const updatePermissionOverwrites = async (
	channelId: string,
	permissionOverwrites: Array<{id: string; type: 0 | 1; allow: string; deny: string}>,
) => {
	try {
		const response = await http.patch<Channel>({
			url: Endpoints.CHANNEL(channelId),
			body: {permission_overwrites: permissionOverwrites},
		});
		return response.body;
	} catch (error) {
		logger.error(`Failed to update permission overwrites for channel ${channelId}:`, error);
		throw error;
	}
};

export const fetchChannelInvites = async (channelId: string): Promise<Array<Invite>> => {
	try {
		InviteStore.handleChannelInvitesFetchPending(channelId);
		const response = await http.get<Array<Invite>>({url: Endpoints.CHANNEL_INVITES(channelId)});
		const data = response.body ?? [];
		InviteStore.handleChannelInvitesFetchSuccess(channelId, data);
		return data;
	} catch (error) {
		logger.error(`Failed to fetch invites for channel ${channelId}:`, error);
		InviteStore.handleChannelInvitesFetchError(channelId);
		throw error;
	}
};

export const fetchRtcRegions = async (channelId: string): Promise<Array<ChannelRtcRegion>> => {
	try {
		const response = await http.get<Array<ChannelRtcRegion>>({url: Endpoints.CHANNEL_RTC_REGIONS(channelId)});
		return response.body ?? [];
	} catch (error) {
		logger.error(`Failed to fetch RTC regions for channel ${channelId}:`, error);
		throw error;
	}
};
