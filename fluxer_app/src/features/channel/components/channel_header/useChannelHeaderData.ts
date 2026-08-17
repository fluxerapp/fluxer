// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {
	PERSONAL_NOTES_DESCRIPTOR,
	TEXT_CHANNEL_DESCRIPTOR,
	VOICE_CHANNEL_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {useLingui} from '@lingui/react/macro';
import {useMemo} from 'react';

interface ChannelHeaderData {
	isDM: boolean;
	isGroupDM: boolean;
	isPersonalNotes: boolean;
	isGuildChannel: boolean;
	isVoiceChannel: boolean;
	recipient: User | null;
	directMessageName: string;
	groupDMName: string;
	channelName: string;
	channelTypeLabel: string | null;
}

export const useChannelHeaderData = (channel?: Channel): ChannelHeaderData => {
	const {i18n} = useLingui();
	const isDM = channel?.type === ChannelTypes.DM;
	const isGroupDM = channel?.type === ChannelTypes.GROUP_DM;
	const isPersonalNotes = channel?.type === ChannelTypes.DM_PERSONAL_NOTES;
	const isGuildChannel = Boolean(channel?.guildId);
	const isVoiceChannel = Boolean(channel?.isVoice());
	const recipient = useMemo<User | null>(() => {
		if (!isDM || !channel?.recipientIds?.length) {
			return null;
		}
		return Users.getUser(channel.recipientIds[0]) ?? null;
	}, [channel, isDM]);
	const directMessageName = isDM && recipient && channel ? NicknameUtils.getNickname(recipient, null, channel.id) : '';
	const groupDMName = useMemo(() => {
		if (!isGroupDM || !channel) {
			return '';
		}
		return ChannelUtils.getDMDisplayName(channel);
	}, [channel, isGroupDM]);
	const channelName = useMemo(() => {
		if (!channel) {
			return '';
		}
		if (isDM && recipient) {
			return directMessageName;
		}
		if (isGroupDM) {
			return groupDMName;
		}
		if (isPersonalNotes) {
			return i18n._(PERSONAL_NOTES_DESCRIPTOR);
		}
		return channel.name ?? '';
	}, [channel, isDM, isGroupDM, isPersonalNotes, recipient, directMessageName, groupDMName, i18n.locale]);
	const channelTypeLabel = useMemo(() => {
		if (!channel) {
			return null;
		}
		if (channel.type === ChannelTypes.GUILD_TEXT) {
			return i18n._(TEXT_CHANNEL_DESCRIPTOR);
		}
		if (channel.type === ChannelTypes.GUILD_VOICE) {
			return i18n._(VOICE_CHANNEL_DESCRIPTOR);
		}
		return null;
	}, [channel, i18n.locale]);
	return {
		isDM,
		isGroupDM,
		isPersonalNotes,
		isGuildChannel,
		isVoiceChannel,
		recipient,
		directMessageName,
		groupDMName,
		channelName,
		channelTypeLabel,
	};
};
