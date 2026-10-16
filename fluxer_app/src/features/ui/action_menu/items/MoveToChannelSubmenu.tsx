// SPDX-License-Identifier: AGPL-3.0-or-later

import {showVoiceMemberModerationFailedModal} from '@app/features/app/components/alerts/VoiceMemberModerationFailedModal';
import Channels from '@app/features/channel/state/Channels';
import * as VoiceStateCommands from '@app/features/devtools/commands/VoiceStateCommands';
import * as GuildMemberCommands from '@app/features/member/commands/GuildMemberCommands';
import Permission from '@app/features/permissions/state/Permission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {SubMenu as MenuItemSubmenu} from '@app/features/ui/action_menu/ContextMenu';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import Users from '@app/features/user/state/Users';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

const MOVE_DEVICE_TO_DESCRIPTOR = msg({
	message: 'Move device to…',
	comment: 'Submenu label that moves a single device into another voice channel.',
});
const SWITCH_TO_DESCRIPTOR = msg({
	message: 'Switch to…',
	comment: 'Submenu label that switches the current device to another voice channel.',
});
const MOVE_TO_DESCRIPTOR = msg({
	message: 'Move to…',
	comment: 'Submenu label that moves the selected participant or content into another channel.',
});
const logger = new Logger('MoveToChannelSubmenu');

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
		const {i18n} = useLingui();
		const channels = Channels.getGuildChannels(guildId);
		const userVoiceState = MediaEngine.getVoiceState(guildId, userId);
		const currentUser = Users.currentUser;
		const isSelf = currentUser?.id === userId;
		const voiceChannels = useMemo(() => {
			return channels.filter((channel) => {
				if (channel.type !== ChannelTypes.GUILD_VOICE) {
					return false;
				}
				if (userVoiceState?.channel_id === channel.id) {
					return false;
				}
				if (!isSelf) {
					const canMoveMembers = Permission.can(Permissions.MOVE_MEMBERS, {
						guildId,
						channelId: channel.id,
					});
					return canMoveMembers;
				}
				const canConnect = Permission.can(Permissions.CONNECT, {
					guildId,
					channelId: channel.id,
				});
				return canConnect;
			});
		}, [channels, guildId, userVoiceState, isSelf]);
		const handleMoveToChannel = useCallback(
			async (channelId: string) => {
				onClose();
				if (connectionIds && connectionIds.length > 0) {
					try {
						await VoiceStateCommands.bulkMoveConnections(connectionIds, channelId);
					} catch (error) {
						logger.error('Failed to bulk move connections:', error);
					}
					return;
				}
				if (isSelf) {
					await MediaEngine.connectToVoiceChannel(guildId, channelId);
				} else {
					try {
						await GuildMemberCommands.update(guildId, userId, {
							channel_id: channelId,
							connection_id: connectionId,
						});
					} catch (error) {
						logger.error('Failed to move member to channel:', error);
						showVoiceMemberModerationFailedModal(error, MOVE_TO_DESCRIPTOR);
					}
				}
			},
			[guildId, userId, connectionId, connectionIds, onClose, isSelf],
		);
		const menuLabel = useMemo(() => {
			if (label) {
				return label;
			}
			if (connectionId) {
				return i18n._(MOVE_DEVICE_TO_DESCRIPTOR);
			}
			if (isSelf) {
				return i18n._(SWITCH_TO_DESCRIPTOR);
			}
			return i18n._(MOVE_TO_DESCRIPTOR);
		}, [label, connectionId, isSelf, i18n.locale]);
		if (voiceChannels.length === 0) {
			return null;
		}
		return (
			<MenuItemSubmenu
				label={menuLabel}
				render={() => (
					<MenuGroup data-flx="ui.action-menu.items.move-to-channel-submenu.menu-group">
						{voiceChannels.map((channel) => (
							<MenuItem
								key={channel.id}
								onClick={() => handleMoveToChannel(channel.id)}
								data-flx="ui.action-menu.items.move-to-channel-submenu.menu-item.move-to-channel"
							>
								{channel.name}
							</MenuItem>
						))}
					</MenuGroup>
				)}
				data-flx="ui.action-menu.items.move-to-channel-submenu.menu-item-submenu"
			/>
		);
	},
);
