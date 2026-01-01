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

import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as InviteActionCreators from '~/actions/InviteActionCreators';
import * as MessageActionCreators from '~/actions/MessageActionCreators';
import * as PrivateChannelActionCreators from '~/actions/PrivateChannelActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {ChannelTypes} from '~/Constants';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildRecord} from '~/records/GuildRecord';
import type {UserRecord} from '~/records/UserRecord';
import ChannelStore from '~/stores/ChannelStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';
import * as InviteUtils from '~/utils/InviteUtils';
import {fromTimestamp} from '~/utils/SnowflakeUtils';
import {InviteIcon} from '../ContextMenuIcons';
import {MenuGroup} from '../MenuGroup';
import {MenuItem} from '../MenuItem';
import {MenuItemSubmenu} from '../MenuItemSubmenu';

interface InviteCandidate {
	guild: GuildRecord;
	channelId: string;
}

const canInviteInChannel = (channel: ChannelRecord | undefined): channel is ChannelRecord => {
	if (!channel || !channel.guildId) {
		return false;
	}
	return InviteUtils.canInviteToChannel(channel.id, channel.guildId);
};

const getDefaultInviteChannelId = (guildId: string): string | null => {
	const selectedChannelId = SelectedChannelStore.selectedChannelIds.get(guildId);
	if (selectedChannelId) {
		const selectedChannel = ChannelStore.getChannel(selectedChannelId);
		if (canInviteInChannel(selectedChannel)) {
			return selectedChannel!.id;
		}
	}

	const guildChannels = ChannelStore.getGuildChannels(guildId);
	for (const channel of guildChannels) {
		if (channel.type === ChannelTypes.GUILD_TEXT && canInviteInChannel(channel)) {
			return channel.id;
		}
	}

	return null;
};

interface InviteToCommunityMenuItemProps {
	user: UserRecord;
	onClose: () => void;
}

export const InviteToCommunityMenuItem: React.FC<InviteToCommunityMenuItemProps> = observer(({user, onClose}) => {
	const {t} = useLingui();
	const candidates = React.useMemo(() => {
		return GuildStore.getGuilds()
			.filter((guild) => !GuildMemberStore.getMember(guild.id, user.id))
			.map((guild): InviteCandidate | null => {
				const channelId = getDefaultInviteChannelId(guild.id);
				return channelId ? {guild, channelId} : null;
			})
			.filter((candidate): candidate is InviteCandidate => candidate !== null)
			.sort((a, b) => a.guild.name.localeCompare(b.guild.name));
	}, [user.id]);

	const handleSendInvite = React.useCallback(
		async (candidate: InviteCandidate) => {
			onClose();
			try {
				const invite = await InviteActionCreators.create(candidate.channelId);
				const inviteUrl = `${RuntimeConfigStore.inviteEndpoint}/${invite.code}`;
				const dmChannelId = await PrivateChannelActionCreators.ensureDMChannel(user.id);
				await MessageActionCreators.send(dmChannelId, {
					content: inviteUrl,
					nonce: fromTimestamp(Date.now()),
				});
				ToastActionCreators.createToast({
					type: 'success',
					children: <Trans>Invite sent for {candidate.guild.name}</Trans>,
				});
			} catch (error) {
				console.error('Failed to send invite from context menu:', error);
				ToastActionCreators.createToast({
					type: 'error',
					children: <Trans>Failed to send invite</Trans>,
				});
			}
		},
		[onClose, user.id],
	);

	if (user.bot || candidates.length === 0) {
		return null;
	}

	return (
		<MenuItemSubmenu
			label={t`Invite to Community`}
			icon={<InviteIcon />}
			render={() => (
				<MenuGroup>
					{candidates.map((candidate) => (
						<MenuItem key={candidate.guild.id} onClick={() => handleSendInvite(candidate)}>
							{candidate.guild.name}
						</MenuItem>
					))}
				</MenuGroup>
			)}
		/>
	);
});
