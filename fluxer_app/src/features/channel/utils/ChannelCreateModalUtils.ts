// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ChannelCommands from '@app/features/channel/commands/ChannelCommands';
import type {Channel} from '@app/features/channel/models/Channel';
import {selectChannel} from '@app/features/navigation/commands/NavigationCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {ChannelTypes, GUILD_TEXT_BASED_CHANNEL_TYPES} from '@fluxer/constants/src/ChannelConstants';
import {
	VOICE_CHANNEL_BITRATE_DEFAULT,
	VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT,
} from '@fluxer/constants/src/LimitConstants';

export interface FormInputs {
	name: string;
	url: string | null;
	type: string;
}

export interface DuplicateChannelFormInputs {
	name: string;
}

export interface DuplicateChannelOptions {
	closeModal?: boolean;
}

export interface ChannelTypeOption {
	value: number;
	name: string;
	desc: string;
}

export const channelTypeOptions: Array<ChannelTypeOption> = [
	{
		value: ChannelTypes.GUILD_TEXT,
		name: 'Text Channel',
		desc: 'Send messages, images, GIFs, and emoji',
	},
	{
		value: ChannelTypes.GUILD_VOICE,
		name: 'Voice Channel',
		desc: 'Hang out together with voice, video, and screen share',
	},
	{
		value: ChannelTypes.GUILD_LINK,
		name: 'Link Channel',
		desc: 'Quick access to an external website or resource',
	},
];

export async function createChannel(guildId: string, data: FormInputs, parentId?: string): Promise<void> {
	const channelType = Number(data.type);
	const channel = await ChannelCommands.create(guildId, {
		name: data.name,
		url: data.url,
		type: channelType,
		parent_id: parentId || null,
		bitrate: channelType === ChannelTypes.GUILD_VOICE ? VOICE_CHANNEL_BITRATE_DEFAULT : null,
		user_limit: channelType === ChannelTypes.GUILD_VOICE ? 0 : null,
		voice_connection_limit: channelType === ChannelTypes.GUILD_VOICE ? VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT : null,
	});
	if (GUILD_TEXT_BASED_CHANNEL_TYPES.has(channel.type)) {
		setTimeout(() => {
			selectChannel(guildId, channel.id);
		}, 50);
	}
	ModalCommands.pop();
}

export async function duplicateChannel(
	guildId: string,
	sourceChannel: Channel,
	data: DuplicateChannelFormInputs,
	options: DuplicateChannelOptions = {},
): Promise<void> {
	const {closeModal = true} = options;
	const channel = await ChannelCommands.create(guildId, {
		name: data.name,
		url: sourceChannel.type === ChannelTypes.GUILD_LINK ? sourceChannel.url : null,
		type: sourceChannel.type,
		parent_id: sourceChannel.parentId,
		bitrate: sourceChannel.type === ChannelTypes.GUILD_VOICE ? sourceChannel.bitrate : null,
		user_limit: sourceChannel.type === ChannelTypes.GUILD_VOICE ? sourceChannel.userLimit : null,
		voice_connection_limit: sourceChannel.type === ChannelTypes.GUILD_VOICE ? sourceChannel.voiceConnectionLimit : null,
		permission_overwrites: Object.values(sourceChannel.permissionOverwrites).map((overwrite) => ({
			id: overwrite.id,
			type: overwrite.type === 1 ? 1 : 0,
			allow: overwrite.allow.toString(),
			deny: overwrite.deny.toString(),
		})),
	});
	if (GUILD_TEXT_BASED_CHANNEL_TYPES.has(channel.type)) {
		setTimeout(() => {
			selectChannel(guildId, channel.id);
		}, 50);
	}
	if (closeModal) {
		ModalCommands.pop();
	}
}

export function getDuplicateChannelDefaultValues(sourceChannel: Channel): DuplicateChannelFormInputs {
	return {
		name: sourceChannel.name ?? '',
	};
}

export function getDefaultValues(): Partial<FormInputs> {
	return {
		type: ChannelTypes.GUILD_TEXT.toString(),
	};
}
