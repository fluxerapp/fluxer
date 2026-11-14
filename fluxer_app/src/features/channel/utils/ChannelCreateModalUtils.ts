// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ChannelCommands from '@app/features/channel/commands/ChannelCommands';
import type {Channel} from '@app/features/channel/models/Channel';
import {getForumChannelTypeOptions} from '@app/features/forum/utils/ForumChannelTypeOptions';
import {selectChannel} from '@app/features/navigation/commands/NavigationCommands';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {ChannelTypes, GUILD_TEXT_BASED_CHANNEL_TYPES, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {
	VOICE_CHANNEL_BITRATE_DEFAULT,
	VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT,
} from '@fluxer/constants/src/LimitConstants';
import {THREAD_ONLY_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

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

const TEXT_TYPE_DESCRIPTOR = msg({
	message: 'Text',
	comment: 'Channel type option in the create channel modal.',
});
const TEXT_TYPE_DESC_DESCRIPTOR = msg({
	message: 'Send messages, images, GIFs and emoji',
	comment: 'One-line description of the text channel type in the create channel modal.',
});
const ANNOUNCEMENT_TYPE_DESCRIPTOR = msg({
	message: 'Announcement',
	comment: 'Channel type option in the create channel modal.',
});
const ANNOUNCEMENT_TYPE_DESC_DESCRIPTOR = msg({
	message: 'Post updates other communities can follow',
	comment: 'One-line description of the announcement channel type in the create channel modal.',
});
const VOICE_TYPE_DESCRIPTOR = msg({
	message: 'Voice',
	comment: 'Channel type option in the create channel modal.',
});
const VOICE_TYPE_DESC_DESCRIPTOR = msg({
	message: 'Hang out with voice, video and screen share',
	comment: 'One-line description of the voice channel type in the create channel modal.',
});
const LINK_TYPE_DESCRIPTOR = msg({
	message: 'Link',
	comment: 'Channel type option in the create channel modal.',
});
const LINK_TYPE_DESC_DESCRIPTOR = msg({
	message: 'Shortcut to an external website',
	comment: 'One-line description of the link channel type in the create channel modal.',
});

const CHANNEL_TYPE_OPTIONS: ReadonlyArray<{value: number; name: MessageDescriptor; desc: MessageDescriptor}> = [
	{value: ChannelTypes.GUILD_TEXT, name: TEXT_TYPE_DESCRIPTOR, desc: TEXT_TYPE_DESC_DESCRIPTOR},
	{value: ChannelTypes.GUILD_VOICE, name: VOICE_TYPE_DESCRIPTOR, desc: VOICE_TYPE_DESC_DESCRIPTOR},
	{value: ChannelTypes.GUILD_ANNOUNCEMENT, name: ANNOUNCEMENT_TYPE_DESCRIPTOR, desc: ANNOUNCEMENT_TYPE_DESC_DESCRIPTOR},
	{value: ChannelTypes.GUILD_LINK, name: LINK_TYPE_DESCRIPTOR, desc: LINK_TYPE_DESC_DESCRIPTOR},
];

export function getChannelTypeOptions(i18n: I18n, {forums = false}: {forums?: boolean} = {}): Array<ChannelTypeOption> {
	const options = CHANNEL_TYPE_OPTIONS.map((option) => ({
		value: option.value,
		name: i18n._(option.name),
		desc: i18n._(option.desc),
	}));
	if (!forums) return options;
	return [...options, ...getForumChannelTypeOptions(i18n)];
}

export interface ChannelCreateOverwrite {
	id: string;
	type: 0 | 1;
}

export function buildPrivateChannelOverwrites(
	guildId: string,
	channelType: number,
	members: ReadonlyArray<ChannelCreateOverwrite>,
): Array<{id: string; type: 0 | 1; allow: string; deny: string}> {
	const access =
		channelType === ChannelTypes.GUILD_VOICE
			? Permissions.VIEW_CHANNEL | Permissions.CONNECT
			: Permissions.VIEW_CHANNEL;
	return [
		{id: guildId, type: 0, allow: '0', deny: Permissions.VIEW_CHANNEL.toString()},
		...members.map((member) => ({id: member.id, type: member.type, allow: access.toString(), deny: '0'})),
	];
}

export async function createChannel(
	guildId: string,
	data: FormInputs,
	parentId?: string,
	permissionOverwrites?: Array<{id: string; type: 0 | 1; allow: string; deny: string}>,
): Promise<void> {
	const channelType = Number(data.type);
	if (THREAD_ONLY_CHANNEL_TYPES.has(channelType) && !ThreadGuilds.isActive(guildId)) return;
	const channel = await ChannelCommands.create(guildId, {
		name: data.name,
		url: data.url,
		type: channelType,
		parent_id: parentId || null,
		bitrate: channelType === ChannelTypes.GUILD_VOICE ? VOICE_CHANNEL_BITRATE_DEFAULT : null,
		user_limit: channelType === ChannelTypes.GUILD_VOICE ? 0 : null,
		voice_connection_limit: channelType === ChannelTypes.GUILD_VOICE ? VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT : null,
		...(permissionOverwrites ? {permission_overwrites: permissionOverwrites} : {}),
	});
	if (GUILD_TEXT_BASED_CHANNEL_TYPES.has(channel.type) || THREAD_ONLY_CHANNEL_TYPES.has(channel.type)) {
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
	if (THREAD_ONLY_CHANNEL_TYPES.has(sourceChannel.type) && !ThreadGuilds.isActive(guildId)) return;
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
