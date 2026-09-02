// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import Guilds from '@app/features/guild/state/Guilds';
import Permission from '@app/features/permissions/state/Permission';
import NativePermission from '@app/features/permissions/system/state/NativePermission';
import {DataMenuRenderer} from '@app/features/ui/action_menu/DataMenuRenderer';
import {MuteDMMenuItem} from '@app/features/ui/action_menu/items/DMMenuItems';
import {ManageRolesMenuItem} from '@app/features/ui/action_menu/items/GuildMemberMenuItems';
import {EditCommunityProfileMenuItem} from '@app/features/ui/action_menu/items/GuildMenuItems';
import {InviteToCommunityMenuItem} from '@app/features/ui/action_menu/items/InviteMenuItems';
import {MoveToChannelSubmenu} from '@app/features/ui/action_menu/items/MoveToChannelSubmenu';
import {useVoiceParticipantMenuData} from '@app/features/ui/action_menu/items/VoiceParticipantMenuData';
import type {
	VoiceParticipantMenuSource,
	VoiceParticipantMenuSurface,
} from '@app/features/ui/action_menu/items/VoiceParticipantMenuTypes';
import {VoiceParticipantOwnStreamMenuTail} from '@app/features/ui/action_menu/items/VoiceParticipantStreamMenuBuilder';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import type {User} from '@app/features/user/models/User';
import {ActiveScreenShareMenu} from '@app/features/voice/components/ActiveScreenShareMenu';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import ActiveScreenShareSource from '@app/features/voice/state/ActiveScreenShareSource';
import {resolveDisplayShareEnvironment} from '@app/features/voice/utils/ScreenShareEnvironment';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

const MOVE_DEVICE_TO_DESCRIPTOR = msg({
	message: 'Move device to…',
	comment: 'Submenu label that moves a single device into another voice channel.',
});
const MOVE_ALL_DEVICES_TO_DESCRIPTOR = msg({
	message: 'Move all devices to…',
	comment: 'Submenu label that moves every active device into another voice channel.',
});
const MOVE_TO_DESCRIPTOR = msg({
	message: 'Move to…',
	comment: 'Submenu label that moves the selected participant or content into another channel.',
});

interface VoiceParticipantContextMenuProps {
	user: User;
	participantName: string;
	onClose: () => void;
	guildId?: string;
	connectionId?: string;
	surface: VoiceParticipantMenuSurface;
	source: VoiceParticipantMenuSource;
	isGroupedItem?: boolean;
	isParentGroupedItem?: boolean;
	groupContainsLocalConnection?: boolean;
	hiddenConnectionCount?: number;
	deviceConnectionCount?: number;
	isDeviceGroupExpanded?: boolean;
	onToggleDeviceGroup?: () => void;
}

export const VoiceParticipantContextMenu: React.FC<VoiceParticipantContextMenuProps> = observer(
	({
		user,
		participantName,
		onClose,
		guildId,
		connectionId,
		surface,
		source,
		isGroupedItem = false,
		isParentGroupedItem = false,
		groupContainsLocalConnection = false,
		hiddenConnectionCount = 0,
		deviceConnectionCount = 0,
		isDeviceGroupExpanded = false,
		onToggleDeviceGroup,
	}) => {
		const {i18n} = useLingui();
		const {
			groups,
			guildManagementGroupIndex,
			member,
			isCurrentUser,
			canMoveMembers,
			userVoiceStates,
			hasMultipleConnections,
			hasVoiceChannels,
		} = useVoiceParticipantMenuData({
			user,
			guildId,
			connectionId,
			surface,
			source,
			isGroupedItem,
			isParentGroupedItem,
			groupContainsLocalConnection,
			onClose,
			hiddenConnectionCount,
			deviceConnectionCount,
			isDeviceGroupExpanded,
			onToggleDeviceGroup,
		});
		const connectionIds = useMemo(() => userVoiceStates.map((u) => u.connectionId), [userVoiceStates]);
		const guild = guildId ? Guilds.getGuild(guildId) : null;
		const privateCallChannel = !guildId && MediaEngine.channelId ? Channels.getChannel(MediaEngine.channelId) : null;
		const hasRoles = guild && Object.values(guild.roles).some((r) => !r.isEveryone);
		const canManageRoles = guildId ? Permission.can(Permissions.MANAGE_ROLES, {guildId}) : false;
		const memberHasVisibleRoles = useMemo(() => {
			if (!guild || !member) {
				return false;
			}
			return Object.values(guild.roles).some((role) => !role.isEveryone && member.roles.has(role.id));
		}, [guild, member]);
		const shouldShowManageRoles = hasRoles && (canManageRoles || memberHasVisibleRoles);
		const shouldShowMoveDevice = Boolean(isGroupedItem && connectionId && guildId && hasVoiceChannels);
		const shouldShowMoveAllDevices = Boolean(
			isParentGroupedItem && hasMultipleConnections && guildId && hasVoiceChannels,
		);
		const shouldShowMoveParticipant = Boolean(
			guildId && canMoveMembers && !isParentGroupedItem && !isGroupedItem && hasVoiceChannels,
		);
		const shouldShowGuildManagement = Boolean(
			guildId &&
				((member && shouldShowManageRoles) ||
					shouldShowMoveDevice ||
					shouldShowMoveAllDevices ||
					shouldShowMoveParticipant),
		);
		const leadingGroups = groups.slice(0, guildManagementGroupIndex);
		const trailingGroups = groups.slice(guildManagementGroupIndex);
		const guildManagementMenu = shouldShowGuildManagement && guildId && (
			<MenuGroup data-flx="ui.action-menu.voice-participant-context-menu.menu-group">
				{member && shouldShowManageRoles && (
					<ManageRolesMenuItem
						guildId={guildId}
						member={member}
						data-flx="ui.action-menu.voice-participant-context-menu.manage-roles-menu-item"
					/>
				)}
				{shouldShowMoveDevice && connectionId && (
					<MoveToChannelSubmenu
						userId={user.id}
						guildId={guildId}
						connectionId={connectionId}
						onClose={onClose}
						label={i18n._(MOVE_DEVICE_TO_DESCRIPTOR)}
						data-flx="ui.action-menu.voice-participant-context-menu.move-to-channel-submenu"
					/>
				)}
				{shouldShowMoveAllDevices && (
					<MoveToChannelSubmenu
						userId={user.id}
						guildId={guildId}
						connectionIds={connectionIds}
						onClose={onClose}
						label={i18n._(MOVE_ALL_DEVICES_TO_DESCRIPTOR)}
						data-flx="ui.action-menu.voice-participant-context-menu.move-to-channel-submenu--2"
					/>
				)}
				{shouldShowMoveParticipant && (
					<MoveToChannelSubmenu
						userId={user.id}
						guildId={guildId}
						connectionId={connectionId}
						onClose={onClose}
						label={i18n._(MOVE_TO_DESCRIPTOR)}
						data-flx="ui.action-menu.voice-participant-context-menu.move-to-channel-submenu--3"
					/>
				)}
			</MenuGroup>
		);
		const auxiliaryMenu = (guild || privateCallChannel?.isDM() || (!isCurrentUser && !user.bot)) && (
			<MenuGroup data-flx="ui.action-menu.voice-participant-context-menu.auxiliary-menu-group">
				{guild && isCurrentUser && (
					<EditCommunityProfileMenuItem
						guild={guild}
						onClose={onClose}
						data-flx="ui.action-menu.voice-participant-context-menu.edit-community-profile-menu-item"
					/>
				)}
				{!isCurrentUser && !user.bot && (
					<InviteToCommunityMenuItem
						user={user}
						onClose={onClose}
						data-flx="ui.action-menu.voice-participant-context-menu.invite-to-community-menu-item"
					/>
				)}
				{privateCallChannel?.isDM() && (
					<MuteDMMenuItem
						channel={privateCallChannel}
						onClose={onClose}
						data-flx="ui.action-menu.voice-participant-context-menu.mute-dm-menu-item"
					/>
				)}
			</MenuGroup>
		);
		const isOwnScreenShareMenu = source.kind === 'screen-share' && source.state.kind === 'own';
		if (isOwnScreenShareMenu) {
			const displayShareEnvironment = resolveDisplayShareEnvironment(
				isDesktop(),
				NativePermission.isLinuxWaylandDesktop,
			);
			const shareContext = ActiveScreenShareSource.getShareContext() ?? 'display';
			const shareContextResolved = ActiveScreenShareSource.getPublishedSource() != null;
			return (
				<ActiveScreenShareMenu
					onClose={onClose}
					displayShareEnvironment={displayShareEnvironment}
					shareContext={shareContext}
					shareContextResolved={shareContextResolved}
					tail={
						<VoiceParticipantOwnStreamMenuTail
							userId={user.id}
							guildId={guildId}
							connectionId={connectionId}
							displayName={participantName}
							onClose={onClose}
							data-flx="ui.action-menu.voice-participant-context-menu.voice-participant-own-stream-menu-tail"
						/>
					}
					data-flx="ui.action-menu.voice-participant-context-menu.active-screen-share-menu"
				/>
			);
		}
		if (source.kind === 'screen-share') {
			return (
				<DataMenuRenderer groups={groups} data-flx="ui.action-menu.voice-participant-context-menu.stream-actions" />
			);
		}
		return (
			<>
				<DataMenuRenderer
					groups={leadingGroups}
					data-flx="ui.action-menu.voice-participant-context-menu.data-menu-renderer"
				/>
				{auxiliaryMenu}
				{guildManagementMenu}
				<DataMenuRenderer
					groups={trailingGroups}
					data-flx="ui.action-menu.voice-participant-context-menu.data-menu-renderer--2"
				/>
			</>
		);
	},
);
