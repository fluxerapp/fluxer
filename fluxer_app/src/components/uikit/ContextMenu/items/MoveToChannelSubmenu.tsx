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
import {ArrowsLeftRightIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as GuildMemberActionCreators from '~/actions/GuildMemberActionCreators';
import * as VoiceStateActionCreators from '~/actions/VoiceStateActionCreators';
import {ChannelTypes, Permissions} from '~/Constants';
import ChannelStore from '~/stores/ChannelStore';
import ConnectionStore from '~/stores/ConnectionStore';
import PermissionStore from '~/stores/PermissionStore';
import UserStore from '~/stores/UserStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import {MenuGroup} from '../MenuGroup';
import {MenuItem} from '../MenuItem';
import {MenuItemSubmenu} from '../MenuItemSubmenu';

interface MoveToChannelSubmenuProps {
	userId: string;
	guildId: string;
	connectionId?: string;
	connectionIds?: Array<string>;
	onClose: () => void;
	label?: string;
}

export const MoveToChannelSubmenu: React.FC<MoveToChannelSubmenuProps> = observer(
	({userId, guildId, connectionId, connectionIds, onClose, label}) => {
		const {t} = useLingui();
		const channels = ChannelStore.getGuildChannels(guildId);
		const userVoiceState = MediaEngineStore.getVoiceState(guildId, userId);
		const currentUser = UserStore.currentUser;
		const isSelf = currentUser?.id === userId;

		const voiceChannels = React.useMemo(() => {
			return channels.filter((channel) => {
				if (channel.type !== ChannelTypes.GUILD_VOICE) {
					return false;
				}

				if (userVoiceState?.channel_id === channel.id) {
					return false;
				}

				const canConnect = PermissionStore.can(Permissions.CONNECT, {
					guildId,
					channelId: channel.id,
				});

				return canConnect;
			});
		}, [channels, guildId, userVoiceState]);

		const handleMoveToChannel = React.useCallback(
			async (channelId: string) => {
				onClose();

				if (connectionIds && connectionIds.length > 0) {
					try {
						await VoiceStateActionCreators.bulkMoveConnections(connectionIds, channelId);
					} catch (error) {
						console.error('Failed to bulk move connections:', error);
					}
					return;
				}

				if (isSelf) {
					const socket = ConnectionStore.socket;
					if (socket) {
						socket.updateVoiceState({
							guild_id: guildId,
							channel_id: channelId,
							self_mute: true,
							self_deaf: true,
							self_video: false,
							self_stream: false,
							connection_id: MediaEngineStore.connectionId ?? null,
						});
					}
				} else {
					try {
						await GuildMemberActionCreators.update(guildId, userId, {
							channel_id: channelId,
							connection_id: connectionId,
						});
					} catch (error) {
						console.error('Failed to move member to channel:', error);
					}
				}
			},
			[guildId, userId, connectionId, connectionIds, onClose, isSelf],
		);

		if (voiceChannels.length === 0) {
			return null;
		}

		return (
			<MenuItemSubmenu
				icon={<ArrowsLeftRightIcon weight="fill" style={{width: 16, height: 16}} />}
				label={label ?? t`Move To...`}
				render={() => (
					<MenuGroup>
						{voiceChannels.map((channel) => (
							<MenuItem key={channel.id} onClick={() => handleMoveToChannel(channel.id)}>
								{channel.name}
							</MenuItem>
						))}
					</MenuGroup>
				)}
			/>
		);
	},
);
