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

import {useLingui} from '@lingui/react/macro';
import React from 'react';
import {ChannelTypes} from '~/Constants';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {UserRecord} from '~/records/UserRecord';
import UserStore from '~/stores/UserStore';
import * as ChannelUtils from '~/utils/ChannelUtils';

export interface ChannelHeaderData {
	isDM: boolean;
	isGroupDM: boolean;
	isPersonalNotes: boolean;
	isGuildChannel: boolean;
	isVoiceChannel: boolean;
	recipient: UserRecord | null;
	directMessageName: string;
	groupDMName: string;
	channelName: string;
	channelTypeLabel: string | null;
}

export const useChannelHeaderData = (channel?: ChannelRecord): ChannelHeaderData => {
	const {t} = useLingui();
	const isDM = channel?.type === ChannelTypes.DM;
	const isGroupDM = channel?.type === ChannelTypes.GROUP_DM;
	const isPersonalNotes = channel?.type === ChannelTypes.DM_PERSONAL_NOTES;
	const isGuildChannel = Boolean(channel?.guildId);
	const isVoiceChannel = Boolean(channel?.isVoice());

	const recipient = React.useMemo<UserRecord | null>(() => {
		if (!isDM || !channel?.recipientIds?.length) {
			return null;
		}
		return UserStore.getUser(channel.recipientIds[0]) ?? null;
	}, [channel, isDM]);

	const directMessageName = React.useMemo(() => {
		if (!isDM || !recipient) {
			return '';
		}
		return recipient.displayName;
	}, [isDM, recipient]);

	const groupDMName = React.useMemo(() => {
		if (!isGroupDM || !channel) {
			return '';
		}
		return ChannelUtils.getDMDisplayName(channel);
	}, [channel, isGroupDM]);

	const channelName = React.useMemo(() => {
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
			return t`Personal Notes`;
		}

		return channel.name ?? '';
	}, [channel, isDM, isGroupDM, isPersonalNotes, recipient, directMessageName, groupDMName]);

	const channelTypeLabel = React.useMemo(() => {
		if (!channel) {
			return null;
		}

		if (channel.type === ChannelTypes.GUILD_TEXT) {
			return t`Text Channel`;
		}

		if (channel.type === ChannelTypes.GUILD_VOICE) {
			return t`Voice Channel`;
		}

		return null;
	}, [channel]);

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
