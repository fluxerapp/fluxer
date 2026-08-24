// SPDX-License-Identifier: AGPL-3.0-or-later

import {showDmActionErrorModal} from '@app/features/app/components/alerts/DmActionErrorModal';
import {showVoiceMemberModerationFailedModal} from '@app/features/app/components/alerts/VoiceMemberModerationFailedModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Authentication from '@app/features/auth/state/Authentication';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import Channels from '@app/features/channel/state/Channels';
import * as VoiceStateCommands from '@app/features/devtools/commands/VoiceStateCommands';
import {UserDebugModal} from '@app/features/devtools/components/debug/UserDebugModal';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import Guilds from '@app/features/guild/state/Guilds';
import {resolveGuildModerationCapabilities} from '@app/features/guild/utils/GuildModerationCapabilityUtils';
import {
	ADD_NOTE_DESCRIPTOR,
	CHANGE_FRIEND_NICKNAME_DESCRIPTOR,
	CHANGE_NICKNAME_DESCRIPTOR,
	COPY_USER_ID_DESCRIPTOR,
	DEBUG_USER_DESCRIPTOR,
	KICK_MEMBER_DESCRIPTOR,
	OPEN_DM_DESCRIPTOR,
	START_VOICE_CALL_DESCRIPTOR,
	TURN_OFF_CAMERA_DESCRIPTOR,
	USER_DEBUG_DESCRIPTOR,
	VIEW_PROFILE_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as GuildMemberCommands from '@app/features/member/commands/GuildMemberCommands';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {BanMemberModal} from '@app/features/moderation/components/modals/BanMemberModal';
import {KickMemberModal} from '@app/features/moderation/components/modals/KickMemberModal';
import {RemoveTimeoutModal} from '@app/features/moderation/components/modals/RemoveTimeoutModal';
import {TimeoutMemberModal} from '@app/features/moderation/components/modals/TimeoutMemberModal';
import {
	BLOCK_DESCRIPTOR,
	REMOVE_TIMEOUT_DESCRIPTOR,
	TIMEOUT_DESCRIPTOR,
} from '@app/features/moderation/utils/ModerationMessageDescriptors';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import {SoundType} from '@app/features/notification/utils/SoundUtils';
import {useRoleHierarchy} from '@app/features/permissions/hooks/useRoleHierarchy';
import Permission from '@app/features/permissions/state/Permission';
import * as PermissionUtils from '@app/features/permissions/utils/PermissionUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import {ChangeFriendNicknameModal} from '@app/features/relationship/components/modals/ChangeFriendNicknameModal';
import Relationships from '@app/features/relationship/state/Relationships';
import * as RelationshipActionUtils from '@app/features/relationship/utils/RelationshipActionUtils';
import {
	ACCEPT_FRIEND_REQUEST_DESCRIPTOR,
	ADD_FRIEND_DESCRIPTOR,
	IGNORE_FRIEND_REQUEST_DESCRIPTOR,
	REMOVE_FRIEND_DESCRIPTOR,
	UNBLOCK_USER_ACTION_DESCRIPTOR,
} from '@app/features/relationship/utils/RelationshipMessageDescriptors';
import {
	AcceptFriendRequestIcon,
	AddNoteIcon,
	BanMemberIcon,
	BlockUserIcon,
	BulkTurnOffCameraIcon,
	ChangeNicknameIcon,
	CollapseIcon,
	CopyIdIcon,
	DebugIcon,
	DisconnectIcon,
	ExpandIcon,
	FocusIcon,
	GuildDeafenIcon,
	GuildMuteIcon,
	IgnoreFriendRequestIcon,
	KickMemberIcon,
	LocalDisableVideoIcon,
	LocalMuteIcon,
	MentionUserIcon,
	MessageUserIcon,
	PopOutIcon,
	RemoveFriendIcon,
	SelfDeafenIcon,
	SelfMuteIcon,
	SendFriendRequestIcon,
	SettingsIcon,
	TimeoutIcon,
	TurnOffCameraIcon,
	TurnOffStreamIcon,
	UnfocusIcon,
	ViewProfileIcon,
	VoiceCallIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import {
	BAN_MEMBER_DESCRIPTOR,
	BLOCKED_USER_DM_WARNING_DESCRIPTOR,
	COLLAPSE_DEVICES_DESCRIPTOR,
	CONNECTION_VOLUME_DESCRIPTOR,
	COPY_DEVICE_ID_DESCRIPTOR,
	DISABLE_VIDEO_LOCALLY_DESCRIPTOR,
	FOCUS_THIS_DEVICE_DESCRIPTOR,
	FOCUS_THIS_PERSON_DESCRIPTOR,
	MENTION_DESCRIPTOR,
	MESSAGE_DESCRIPTOR,
	MUTE_DESCRIPTOR,
	MUTE_DEVICE_DESCRIPTOR,
	POP_OUT_CAMERA_DESCRIPTOR,
	POP_OUT_STREAM_DESCRIPTOR,
	PRIORITIZE_SPEAKERS_DESCRIPTOR,
	SHOW_MY_OWN_CAMERA_DESCRIPTOR,
	SHOW_MY_SCREEN_SHARE_DESCRIPTOR,
	SHOW_NON_VIDEO_PARTICIPANTS_DESCRIPTOR,
	STREAM_VOLUME_DESCRIPTOR,
	TURN_OFF_ALL_DEVICE_CAMERAS_DESCRIPTOR,
	TURN_OFF_DEVICE_CAMERA_DESCRIPTOR,
	TURN_OFF_DEVICE_STREAM_DESCRIPTOR,
	TURN_OFF_STREAM_DESCRIPTOR,
	UNFOCUS_DESCRIPTOR,
} from '@app/features/ui/action_menu/items/voice_participant_menu_data/shared';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as SoundCommands from '@app/features/ui/commands/SoundCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import type {
	MenuCheckboxType,
	MenuGroupType,
	MenuItemType,
	MenuSliderType,
	MenuSubmenuItemType,
} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import {ChangeNicknameModal} from '@app/features/user/components/modals/ChangeNicknameModal';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import type {User} from '@app/features/user/models/User';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import * as VoiceCallLayoutCommands from '@app/features/voice/commands/VoiceCallLayoutCommands';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import {HideOwnCameraConfirmModal} from '@app/features/voice/components/modals/HideOwnCameraConfirmModal';
import {HideOwnScreenShareConfirmModal} from '@app/features/voice/components/modals/HideOwnScreenShareConfirmModal';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import CallMediaPrefs from '@app/features/voice/state/CallMediaPrefs';
import ParticipantVolume from '@app/features/voice/state/ParticipantVolume';
import PopoutWindowManager, {
	isVoicePopoutSupported,
	type VoiceTilePopoutSource,
} from '@app/features/voice/state/PopoutWindowManager';
import StreamAudioPrefs from '@app/features/voice/state/StreamAudioPrefs';
import VoiceCallLayout from '@app/features/voice/state/VoiceCallLayout';
import VoicePrompts from '@app/features/voice/state/VoicePrompts';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import * as CallUtils from '@app/features/voice/utils/CallUtils';
import {hasActiveDirectCallWithUser} from '@app/features/voice/utils/PrivateCallMenuUtils';
import {
	getVoiceVideoSettingsLabel,
	VOICE_COMMUNITY_DEAFEN_DESCRIPTOR,
	VOICE_COMMUNITY_MUTE_DESCRIPTOR,
	VOICE_DEAFEN_ALL_DEVICES_DESCRIPTOR,
	VOICE_DEAFEN_DESCRIPTOR,
	VOICE_DEAFEN_DEVICE_DESCRIPTOR,
	VOICE_DISCONNECT_ALL_DEVICES_DESCRIPTOR,
	VOICE_DISCONNECT_DESCRIPTOR,
	VOICE_DISCONNECT_DEVICE_DESCRIPTOR,
	VOICE_MUTE_ALL_DEVICES_DESCRIPTOR,
	VOICE_STOP_WATCHING_DESCRIPTOR,
	VOICE_UNDEAFEN_ALL_DEVICES_DESCRIPTOR,
	VOICE_UNMUTE_ALL_DEVICES_DESCRIPTOR,
	VOICE_USER_VOLUME_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {buildVoiceParticipantIdentity} from '@app/features/voice/utils/VoiceParticipantIdentity';
import {ChannelTypes, Permissions, TEXT_BASED_CHANNEL_TYPES} from '@fluxer/constants/src/ChannelConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg, plural} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {Track} from 'livekit-client';
import {useCallback, useMemo} from 'react';

const logger = new Logger('VoiceParticipantMenuData');

const ADVANCED_ACTIONS_DESCRIPTOR = msg({
	message: 'Advanced',
	comment: 'Voice participant context menu submenu label for IDs and diagnostics.',
});
const DEVICE_CONTROLS_DESCRIPTOR = msg({
	message: 'Device controls',
	comment: 'Voice participant context menu submenu label for actions that affect one or more voice devices.',
});
const DISPLAY_OPTIONS_DESCRIPTOR = msg({
	message: 'Display options',
	comment: 'Voice participant context menu submenu label for call display and diagnostics preferences.',
});
const MEDIA_CONTROLS_DESCRIPTOR = msg({
	message: 'Media controls',
	comment: 'Voice participant context menu submenu label for camera, stream, and video actions.',
});
const MODERATION_ACTIONS_DESCRIPTOR = msg({
	message: 'Moderation',
	comment: 'Voice participant context menu submenu label for member moderation actions.',
});
const RELATIONSHIP_ACTIONS_DESCRIPTOR = msg({
	message: 'Relationship',
	comment: 'Voice participant context menu submenu label for friend and block actions.',
});
const STREAM_CONTROLS_DESCRIPTOR = msg({
	message: 'Stream controls',
	comment: 'Voice participant context menu submenu label for incoming screen share audio controls.',
});
const USER_ACTIONS_DESCRIPTOR = msg({
	message: 'User actions',
	comment: 'Voice participant context menu submenu label for lower-frequency profile and communication actions.',
});

type VoiceParticipantMenuLeafItem = MenuItemType | MenuCheckboxType | MenuSliderType;

export interface VoiceParticipantMenuDataOptions {
	user: User;
	guildId?: string;
	connectionId?: string;
	isGroupedItem?: boolean;
	isParentGroupedItem?: boolean;
	streamKey?: string;
	isScreenShare?: boolean;
	isWatching?: boolean;
	hasScreenShareAudio?: boolean;
	isOwnScreenShare?: boolean;
	onStopWatching?: () => void;
	onClose: () => void;
	hiddenConnectionCount?: number;
	deviceConnectionCount?: number;
	isDeviceGroupExpanded?: boolean;
	onToggleDeviceGroup?: () => void;
}

export interface VoiceParticipantMenuData {
	groups: Array<MenuGroupType>;
	member: GuildMember | null;
	isCurrentUser: boolean;
	developerMode: boolean;
	relationshipType: number | undefined;
	canMoveMembers: boolean;
	userVoiceStates: Array<{connectionId: string; voiceState: VoiceState}>;
	hasMultipleConnections: boolean;
	voiceChannels: Array<{id: string; name: string}>;
	hasVoiceChannels: boolean;
}

export function useVoiceParticipantMenuData(options: VoiceParticipantMenuDataOptions): VoiceParticipantMenuData {
	const {
		user,
		guildId,
		connectionId,
		isGroupedItem = false,
		isParentGroupedItem = false,
		streamKey,
		isScreenShare = false,
		isWatching = false,
		hasScreenShareAudio = false,
		isOwnScreenShare = false,
		onStopWatching,
		onClose,
		hiddenConnectionCount = 0,
		deviceConnectionCount = 0,
		isDeviceGroupExpanded = false,
		onToggleDeviceGroup,
	} = options;
	const {i18n} = useLingui();
	const member = GuildMembers.getMember(guildId ?? '', user.id);
	const isCurrentUser = user.id === Authentication.currentUserId;
	const developerMode = UserSettings.developerMode;
	const relationship = Relationships.getRelationship(user.id);
	const relationshipType = relationship?.type;
	const isBlocked = relationshipType === RelationshipTypes.BLOCKED;
	const hasActiveDirectCall = hasActiveDirectCallWithUser(user.id);
	const canMuteMembers = guildId ? Permission.can(Permissions.MUTE_MEMBERS, {guildId}) : false;
	const canMoveMembers = guildId ? Permission.can(Permissions.MOVE_MEMBERS, {guildId}) : false;
	const canKickMembers = guildId ? Permission.can(Permissions.KICK_MEMBERS, {guildId}) : false;
	const canBanMembers = guildId ? Permission.can(Permissions.BAN_MEMBERS, {guildId}) : false;
	const canModerateMembers = guildId ? Permission.can(Permissions.MODERATE_MEMBERS, {guildId}) : false;
	const guild = guildId ? Guilds.getGuild(guildId) : null;
	const {canManageTarget} = useRoleHierarchy(guild);
	const guildSnapshot = guild?.toJSON();
	const targetHasAdministratorPermission =
		guildSnapshot !== undefined && PermissionUtils.can(Permissions.ADMINISTRATOR, user.id, guildSnapshot);
	const {
		canKick: canKickTarget,
		canBan: canBanTarget,
		canTimeout: canTimeoutTarget,
	} = resolveGuildModerationCapabilities({
		isCurrentUser,
		canManageTarget: canManageTarget(user.id),
		canKickMembers,
		canBanMembers,
		canModerateMembers,
		targetHasAdministratorPermission,
	});
	const focusedChannelId = SelectedChannel.currentChannelId;
	const focusedChannel = focusedChannelId ? Channels.getChannel(focusedChannelId) : null;
	const isFocusedChannelTextBased = focusedChannel ? TEXT_BASED_CHANNEL_TYPES.has(focusedChannel.type) : false;
	const canMention =
		isFocusedChannelTextBased &&
		focusedChannelId &&
		Permission.can(Permissions.SEND_MESSAGES, {guildId: focusedChannel?.guildId, channelId: focusedChannelId});
	const openDmChannel = useCallback(async () => {
		try {
			await PrivateChannelCommands.openDMChannel(user.id);
		} catch (error) {
			logger.error('Failed to open DM channel:', error);
			showDmActionErrorModal(error);
		}
	}, [user.id]);
	const handleMessage = useCallback(async () => {
		onClose();
		await openDmChannel();
	}, [onClose, openDmChannel]);
	const displayName = NicknameUtils.getNickname(user, guildId);
	const handleOpenBlockedDm = useCallback(() => {
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<ConfirmModal
					title={i18n._(OPEN_DM_DESCRIPTOR)}
					description={i18n._(BLOCKED_USER_DM_WARNING_DESCRIPTOR, {userName: displayName})}
					primaryText={i18n._(OPEN_DM_DESCRIPTOR)}
					primaryVariant="primary"
					onPrimary={openDmChannel}
					data-flx="ui.action-menu.items.voice-participant-menu-data.handle-open-blocked-dm.confirm-modal"
				/>
			)),
		);
	}, [onClose, openDmChannel, displayName, i18n]);
	const currentUserVoiceStateInGuild = guildId
		? MediaEngine.getCurrentUserVoiceState(guildId)
		: MediaEngine.getCurrentUserVoiceState();
	const isCurrentUserConnectedToVoice = currentUserVoiceStateInGuild !== null || MediaEngine.connectionId !== null;
	const connectionVoiceState = connectionId ? MediaEngine.getVoiceStateByConnectionId(connectionId) : null;
	const currentUserVoiceState = MediaEngine.getCurrentUserVoiceState();
	const currentConnectionId = MediaEngine.connectionId;
	const currentConnectionVoiceState = currentConnectionId
		? MediaEngine.getVoiceStateByConnectionId(currentConnectionId)
		: null;
	const showMyOwnCamera = VoiceSettings.showMyOwnCamera;
	const showMyOwnScreenShare = VoiceSettings.showMyOwnScreenShare;
	const showNonVideoParticipants = VoiceSettings.showNonVideoParticipants;
	const prioritizeSpeakingParticipants = VoiceSettings.prioritizeSpeakingParticipants;
	const showConnectionVolumeControls = VoiceSettings.showConnectionVolumeControls;
	const participantVolume = ParticipantVolume.getVolume(user.id);
	const connectionVolume = connectionId ? ParticipantVolume.getConnectionVolume(connectionId) : 100;
	const streamVolume = streamKey ? StreamAudioPrefs.getVolume(streamKey) : 100;
	const isStreamMuted = streamKey ? StreamAudioPrefs.isMuted(streamKey) : false;
	const callId = MediaEngine.connectionId ?? '';
	const participantIdentity = connectionId ? buildVoiceParticipantIdentity(user.id, connectionId) : '';
	const isVideoDisabled =
		callId && participantIdentity ? CallMediaPrefs.isVideoDisabled(callId, participantIdentity) : false;
	const allVoiceStates = MediaEngine.getAllVoiceStates();
	const pinnedParticipantIdentity = VoiceCallLayout.pinnedParticipantIdentity;
	const pinnedParticipantSource = VoiceCallLayout.pinnedParticipantSource;
	const memberMute = member?.mute ?? false;
	const memberDeaf = member?.deaf ?? false;
	const memberTimedOut = member?.isTimedOut() ?? false;
	const userVoiceStates = useMemo(() => {
		if (!guildId) return [] as Array<{connectionId: string; voiceState: VoiceState}>;
		const acc: Array<{connectionId: string; voiceState: VoiceState}> = [];
		Object.entries(allVoiceStates).forEach(([g, guildData]) => {
			if (g === guildId) {
				Object.entries(guildData).forEach(([, channelData]) => {
					Object.entries(channelData).forEach(([cid, vs]) => {
						if (vs.user_id === user.id) acc.push({connectionId: cid, voiceState: vs});
					});
				});
			}
		});
		return acc;
	}, [guildId, user.id, allVoiceStates]);
	const hasMultipleConnections = userVoiceStates.length > 1;
	const userVoiceState = guildId ? MediaEngine.getVoiceState(guildId, user.id) : null;
	const voiceChannels = useMemo(() => {
		if (!guildId) return [];
		const channels = Channels.getGuildChannels(guildId);
		return channels
			.filter((channel) => {
				if (channel.type !== ChannelTypes.GUILD_VOICE) return false;
				if (userVoiceState?.channel_id === channel.id) return false;
				return Permission.can(Permissions.CONNECT, {guildId, channelId: channel.id});
			})
			.map((channel) => ({id: channel.id, name: channel.name ?? ''}));
	}, [guildId, userVoiceState?.channel_id]);
	const groups = useMemo(() => {
		const menuGroups: Array<MenuGroupType> = [];
		const hasDeviceGroup = deviceConnectionCount > 1 || hasMultipleConnections;
		const secondarySubmenus: Array<MenuSubmenuItemType> = [];
		const streamControlActions: Array<MenuCheckboxType | MenuSliderType> = [];
		const mediaActions: Array<MenuItemType | MenuCheckboxType> = [];
		const deviceActions: Array<MenuItemType> = [];
		const displayActions: Array<MenuCheckboxType> = [];
		const userActions: Array<MenuItemType> = [];
		const relationshipSubmenuActions: Array<MenuItemType> = [];
		const moderationSubmenuActions: Array<MenuItemType | MenuCheckboxType> = [];
		const advancedActions: Array<VoiceParticipantMenuLeafItem> = [];
		const addSecondarySubmenu = (label: string, items: Array<VoiceParticipantMenuLeafItem>) => {
			if (items.length > 0) {
				secondarySubmenus.push({label, items});
			}
		};
		const createCopyDeviceIdAction = (targetConnectionId: string): MenuItemType => ({
			icon: (
				<CopyIdIcon
					size={16}
					data-flx="ui.action-menu.items.voice-participant-menu-data.create-copy-device-id-action.copy-id-icon"
				/>
			),
			label: i18n._(COPY_DEVICE_ID_DESCRIPTOR),
			onClick: () => {
				TextCopyCommands.copy(i18n, targetConnectionId, true).catch(() => {});
				onClose();
			},
		});
		const buildUserVolumeSlider = (): MenuSliderType => ({
			label: i18n._(VOICE_USER_VOLUME_DESCRIPTOR),
			value: participantVolume,
			minValue: 0,
			maxValue: 200,
			onChange: (value: number) => {
				ParticipantVolume.setVolume(user.id, value);
				MediaEngine.applyLocalAudioPreferencesForUser(user.id);
			},
			onFormat: (value: number) => `${Math.round(value)}%`,
			factoryDefaultValue: 100,
		});
		const buildConnectionVolumeSlider = (targetConnectionId: string): MenuSliderType => ({
			label: i18n._(CONNECTION_VOLUME_DESCRIPTOR),
			value: connectionVolume,
			minValue: 0,
			maxValue: 200,
			onChange: (value: number) => {
				ParticipantVolume.setConnectionVolume(targetConnectionId, value);
				MediaEngine.applyLocalAudioPreferencesForUser(user.id);
			},
			onFormat: (value: number) => `${Math.round(value)}%`,
			factoryDefaultValue: 100,
		});
		const buildVolumeControls = (targetConnectionId?: string): Array<MenuSliderType> => {
			const volumeControls: Array<MenuSliderType> = [buildUserVolumeSlider()];
			if (showConnectionVolumeControls && targetConnectionId) {
				volumeControls.push(buildConnectionVolumeSlider(targetConnectionId));
			}
			return volumeControls;
		};
		const streamRootActions: Array<MenuItemType> = [];
		if (isScreenShare && !isOwnScreenShare && streamKey) {
			if (isWatching && onStopWatching) {
				streamRootActions.push({
					icon: (
						<TurnOffStreamIcon
							size={16}
							data-flx="ui.action-menu.items.voice-participant-menu-data.groups.turn-off-stream-icon"
						/>
					),
					label: i18n._(VOICE_STOP_WATCHING_DESCRIPTOR),
					onClick: () => {
						onStopWatching();
						onClose();
					},
				});
			}
			if (hasScreenShareAudio) {
				streamControlActions.push({
					label: i18n._(STREAM_VOLUME_DESCRIPTOR),
					value: streamVolume,
					minValue: 0,
					maxValue: 200,
					onChange: (value: number) => {
						StreamAudioPrefs.setVolume(streamKey, value);
						MediaEngine.applyLocalAudioPreferencesForUser(user.id);
					},
					onFormat: (value: number) => `${Math.round(value)}%`,
					factoryDefaultValue: 100,
				});
				streamControlActions.push({
					icon: (
						<LocalMuteIcon
							size={16}
							data-flx="ui.action-menu.items.voice-participant-menu-data.groups.local-mute-icon"
						/>
					),
					label: i18n._(MUTE_DESCRIPTOR),
					checked: isStreamMuted,
					onChange: (checked: boolean) => {
						StreamAudioPrefs.setMuted(streamKey, checked);
						MediaEngine.applyLocalAudioPreferencesForUser(user.id);
					},
				});
			}
		}
		if (streamRootActions.length > 0) {
			menuGroups.push({items: streamRootActions});
		}
		const primaryActions: Array<MenuItemType> = [];
		primaryActions.push({
			icon: (
				<ViewProfileIcon
					size={16}
					data-flx="ui.action-menu.items.voice-participant-menu-data.groups.view-profile-icon"
				/>
			),
			label: i18n._(VIEW_PROFILE_DESCRIPTOR),
			onClick: () => {
				ModalCommands.runAfterBottomSheetClose(onClose, () => UserProfileCommands.openUserProfile(user.id, guildId));
			},
		});
		if (connectionId && guildId && isCurrentUserConnectedToVoice) {
			const identity = buildVoiceParticipantIdentity(user.id, connectionId);
			const isFocused =
				pinnedParticipantIdentity === identity &&
				(pinnedParticipantSource == null ||
					pinnedParticipantSource === (isScreenShare ? Track.Source.ScreenShare : Track.Source.Camera));
			const allStates = MediaEngine.getAllVoiceStates();
			let connectionCount = 0;
			Object.values(allStates).forEach((guildData) => {
				Object.values(guildData).forEach((channelData) => {
					Object.values(channelData).forEach((vs: VoiceState) => {
						if (vs.user_id === user.id) connectionCount++;
					});
				});
			});
			const hasMultipleConnectionsForFocus = connectionCount > 1;
			const focusLabel = (() => {
				if (isFocused) return i18n._(UNFOCUS_DESCRIPTOR);
				if (hasMultipleConnectionsForFocus) return i18n._(FOCUS_THIS_DEVICE_DESCRIPTOR);
				return i18n._(FOCUS_THIS_PERSON_DESCRIPTOR);
			})();
			primaryActions.push({
				icon: isFocused ? (
					<UnfocusIcon size={16} data-flx="ui.action-menu.items.voice-participant-menu-data.groups.unfocus-icon" />
				) : (
					<FocusIcon size={16} data-flx="ui.action-menu.items.voice-participant-menu-data.groups.focus-icon" />
				),
				label: focusLabel,
				onClick: () => {
					if (isFocused) {
						VoiceCallLayoutCommands.setPinnedParticipant(null);
						VoiceCallLayoutCommands.setLayoutMode('grid');
						VoiceCallLayoutCommands.markUserOverride();
					} else {
						VoiceCallLayoutCommands.setLayoutMode('focus');
						VoiceCallLayoutCommands.setPinnedParticipant(
							identity,
							isScreenShare ? Track.Source.ScreenShare : Track.Source.Camera,
						);
						VoiceCallLayoutCommands.markUserOverride();
					}
					onClose();
				},
			});
		}
		const popoutChannelId = connectionVoiceState?.channel_id ?? null;
		if (connectionId && popoutChannelId && isCurrentUserConnectedToVoice && isVoicePopoutSupported()) {
			const connectionParticipant = MediaEngine.getParticipantByUserIdAndConnectionId(user.id, connectionId);
			const isCameraLive = Boolean(connectionParticipant?.isCameraEnabled || connectionVoiceState?.self_video);
			const isStreamLive = Boolean(connectionParticipant?.isScreenShareEnabled || connectionVoiceState?.self_stream);
			const openTilePopout = (source: VoiceTilePopoutSource): void => {
				PopoutWindowManager.openTilePopout({
					participantIdentity,
					source,
					userId: user.id,
					connectionId,
					channelId: popoutChannelId,
					guildId: guildId ?? null,
					title: displayName,
				});
				onClose();
			};
			primaryActions.push({
				icon: (
					<PopOutIcon
						size={16}
						data-flx="ui.action-menu.items.voice-participant-menu-data.groups.pop-out-camera-icon"
					/>
				),
				label: i18n._(POP_OUT_CAMERA_DESCRIPTOR),
				disabled: !isCameraLive,
				onClick: () => openTilePopout('camera'),
			});
			primaryActions.push({
				icon: (
					<PopOutIcon
						size={16}
						data-flx="ui.action-menu.items.voice-participant-menu-data.groups.pop-out-stream-icon"
					/>
				),
				label: i18n._(POP_OUT_STREAM_DESCRIPTOR),
				disabled: !isStreamLive,
				onClick: () => openTilePopout('screen_share'),
			});
		}
		if (canMention) {
			userActions.push({
				icon: (
					<MentionUserIcon
						size={16}
						data-flx="ui.action-menu.items.voice-participant-menu-data.groups.mention-user-icon"
					/>
				),
				label: i18n._(MENTION_DESCRIPTOR),
				onClick: () => {
					onClose();
					ComponentBus.dispatch('INSERT_MENTION', {userId: user.id});
				},
			});
		}
		if (!isCurrentUser) {
			primaryActions.push({
				icon: (
					<MessageUserIcon
						size={16}
						data-flx="ui.action-menu.items.voice-participant-menu-data.groups.message-user-icon"
					/>
				),
				label: isBlocked ? i18n._(OPEN_DM_DESCRIPTOR) : i18n._(MESSAGE_DESCRIPTOR),
				onClick: isBlocked ? handleOpenBlockedDm : handleMessage,
			});
		}
		userActions.push({
			icon: <AddNoteIcon size={16} data-flx="ui.action-menu.items.voice-participant-menu-data.groups.add-note-icon" />,
			label: i18n._(ADD_NOTE_DESCRIPTOR),
			onClick: () => {
				ModalCommands.runAfterBottomSheetClose(onClose, () =>
					UserProfileCommands.openUserProfile(user.id, guildId, true),
				);
			},
		});
		if (!isCurrentUser && relationshipType === RelationshipTypes.FRIEND) {
			userActions.push({
				icon: (
					<ChangeNicknameIcon
						size={16}
						data-flx="ui.action-menu.items.voice-participant-menu-data.groups.change-nickname-icon"
					/>
				),
				label: i18n._(CHANGE_FRIEND_NICKNAME_DESCRIPTOR),
				onClick: () => {
					ModalCommands.pushAfterBottomSheetClose(
						onClose,
						modal(() => (
							<ChangeFriendNicknameModal
								user={user}
								data-flx="ui.action-menu.items.voice-participant-menu-data.on-click.change-friend-nickname-modal"
							/>
						)),
					);
				},
			});
		}
		if (!isCurrentUser && !user.bot && !hasActiveDirectCall) {
			userActions.push({
				icon: (
					<VoiceCallIcon size={16} data-flx="ui.action-menu.items.voice-participant-menu-data.groups.voice-call-icon" />
				),
				label: i18n._(START_VOICE_CALL_DESCRIPTOR),
				onClick: async () => {
					ModalCommands.runAfterBottomSheetClose(onClose, () => {
						void (async () => {
							try {
								const channelId = await PrivateChannelCommands.ensureDMChannel(user.id);
								await CallUtils.requestStartCall(i18n, channelId, {kind: 'voice'});
							} catch (error) {
								logger.error('Failed to start voice call:', error);
								showDmActionErrorModal(error);
							}
						})();
					});
				},
			});
		}
		if (hasDeviceGroup && onToggleDeviceGroup && hiddenConnectionCount > 0) {
			primaryActions.push({
				icon: <ExpandIcon size={16} data-flx="ui.action-menu.items.voice-participant-menu-data.groups.expand-icon" />,
				label: plural(hiddenConnectionCount, {
					one: 'Expand # other device',
					other: 'Expand # other devices',
				}),
				onClick: () => {
					onToggleDeviceGroup();
					onClose();
				},
			});
		} else if (hasDeviceGroup && onToggleDeviceGroup && isDeviceGroupExpanded) {
			primaryActions.push({
				icon: (
					<CollapseIcon size={16} data-flx="ui.action-menu.items.voice-participant-menu-data.groups.collapse-icon" />
				),
				label: i18n._(COLLAPSE_DEVICES_DESCRIPTOR),
				onClick: () => {
					onToggleDeviceGroup();
					onClose();
				},
			});
		}
		if (primaryActions.length > 0) {
			menuGroups.push({items: primaryActions});
		}
		if (isCurrentUser) {
			const selfRootActions: Array<MenuItemType | MenuCheckboxType> = [];
			if (isGroupedItem && connectionId) {
				const isSelfMuted = connectionVoiceState?.self_mute ?? false;
				const isSelfDeafened = connectionVoiceState?.self_deaf ?? false;
				const isCameraOn = connectionVoiceState?.self_video ?? false;
				const isStreaming = connectionVoiceState?.self_stream ?? false;
				selfRootActions.push({
					icon: (
						<SelfMuteIcon size={16} data-flx="ui.action-menu.items.voice-participant-menu-data.groups.self-mute-icon" />
					),
					label: i18n._(MUTE_DEVICE_DESCRIPTOR),
					checked: isSelfMuted,
					onChange: () => {
						VoiceStateCommands.toggleSelfMuteForConnection(connectionId);
					},
				});
				selfRootActions.push({
					icon: (
						<SelfDeafenIcon
							size={16}
							data-flx="ui.action-menu.items.voice-participant-menu-data.groups.self-deafen-icon"
						/>
					),
					label: i18n._(VOICE_DEAFEN_DEVICE_DESCRIPTOR),
					checked: isSelfDeafened,
					onChange: () => {
						VoiceStateCommands.toggleSelfDeafenForConnection(connectionId);
					},
				});
				if (isCameraOn) {
					selfRootActions.push({
						icon: (
							<TurnOffCameraIcon
								size={16}
								data-flx="ui.action-menu.items.voice-participant-menu-data.groups.turn-off-camera-icon"
							/>
						),
						label: i18n._(TURN_OFF_DEVICE_CAMERA_DESCRIPTOR),
						onClick: () => {
							VoiceStateCommands.turnOffCameraForConnection(connectionId);
							onClose();
						},
					});
				}
				if (isStreaming) {
					selfRootActions.push({
						icon: (
							<TurnOffStreamIcon
								size={16}
								data-flx="ui.action-menu.items.voice-participant-menu-data.groups.turn-off-stream-icon--2"
							/>
						),
						label: i18n._(TURN_OFF_DEVICE_STREAM_DESCRIPTOR),
						onClick: () => {
							VoiceStateCommands.turnOffStreamForConnection(connectionId);
							onClose();
						},
					});
				}
				advancedActions.push(createCopyDeviceIdAction(connectionId));
			} else {
				const isSelfMuted = currentUserVoiceState?.self_mute ?? false;
				const isSelfDeafened = currentUserVoiceState?.self_deaf ?? false;
				const isCameraOn = currentConnectionVoiceState?.self_video ?? false;
				const isStreaming = currentConnectionVoiceState?.self_stream ?? false;
				selfRootActions.push({
					icon: (
						<SelfMuteIcon
							size={16}
							data-flx="ui.action-menu.items.voice-participant-menu-data.groups.self-mute-icon--2"
						/>
					),
					label: i18n._(MUTE_DESCRIPTOR),
					checked: isSelfMuted,
					onChange: () => {
						VoiceStateCommands.toggleSelfMute(null);
					},
				});
				selfRootActions.push({
					icon: (
						<SelfDeafenIcon
							size={16}
							data-flx="ui.action-menu.items.voice-participant-menu-data.groups.self-deafen-icon--2"
						/>
					),
					label: i18n._(VOICE_DEAFEN_DESCRIPTOR),
					checked: isSelfDeafened,
					onChange: () => {
						VoiceStateCommands.toggleSelfDeaf(null);
					},
				});
				if (isCameraOn) {
					selfRootActions.push({
						icon: (
							<TurnOffCameraIcon
								size={16}
								data-flx="ui.action-menu.items.voice-participant-menu-data.groups.turn-off-camera-icon--2"
							/>
						),
						label: i18n._(TURN_OFF_CAMERA_DESCRIPTOR),
						onClick: () => {
							if (currentConnectionId) {
								VoiceStateCommands.turnOffCameraForConnection(currentConnectionId);
							}
							onClose();
						},
					});
				}
				if (isStreaming) {
					selfRootActions.push({
						icon: (
							<TurnOffStreamIcon
								size={16}
								data-flx="ui.action-menu.items.voice-participant-menu-data.groups.turn-off-stream-icon--3"
							/>
						),
						label: i18n._(TURN_OFF_STREAM_DESCRIPTOR),
						onClick: () => {
							if (currentConnectionId) {
								VoiceStateCommands.turnOffStreamForConnection(currentConnectionId);
							}
							onClose();
						},
					});
				}
				mediaActions.push({
					icon: (
						<SettingsIcon size={16} data-flx="ui.action-menu.items.voice-participant-menu-data.groups.settings-icon" />
					),
					label: getVoiceVideoSettingsLabel(i18n),
					onClick: () => {
						ModalCommands.pushAfterBottomSheetClose(
							onClose,
							modal(() => (
								<UserSettingsModal
									initialTab="voice_video"
									data-flx="ui.action-menu.items.voice-participant-menu-data.on-click.user-settings-modal"
								/>
							)),
						);
					},
				});
				if (currentConnectionId) {
					advancedActions.push(createCopyDeviceIdAction(currentConnectionId));
				}
			}
			if (guildId) {
				const cid = connectionId ?? MediaEngine.connectionId ?? null;
				const isCurrentDevice = !connectionId || connectionId === MediaEngine.connectionId;
				selfRootActions.push({
					icon: (
						<DisconnectIcon
							size={16}
							data-flx="ui.action-menu.items.voice-participant-menu-data.groups.disconnect-icon"
						/>
					),
					label: i18n._(isGroupedItem ? VOICE_DISCONNECT_DEVICE_DESCRIPTOR : VOICE_DISCONNECT_DESCRIPTOR),
					onClick: async () => {
						if (isCurrentDevice) {
							await MediaEngine.disconnectFromVoiceChannel('user');
						} else if (cid) {
							MediaEngine.disconnectRemoteDevice(guildId, cid);
						}
						onClose();
					},
					danger: true,
				});
			}
			if (selfRootActions.length > 0) {
				menuGroups.push({items: selfRootActions});
			}
			if (connectionId && connectionId !== currentConnectionId && isCurrentUserConnectedToVoice) {
				menuGroups.push({items: buildVolumeControls(connectionId)});
			}
			const displayPrefs: Array<MenuCheckboxType> = [
				{
					label: i18n._(SHOW_MY_OWN_CAMERA_DESCRIPTOR),
					checked: showMyOwnCamera,
					onChange: (checked) => {
						if (!checked) {
							if (VoicePrompts.getSkipHideOwnCameraConfirm()) {
								VoiceSettingsCommands.update({showMyOwnCamera: false});
							} else {
								ModalCommands.pushAfterBottomSheetClose(
									onClose,
									modal(() => (
										<HideOwnCameraConfirmModal data-flx="ui.action-menu.items.voice-participant-menu-data.on-change.hide-own-camera-confirm-modal" />
									)),
								);
							}
						} else {
							VoiceSettingsCommands.update({showMyOwnCamera: true});
						}
					},
				},
				{
					label: i18n._(SHOW_MY_SCREEN_SHARE_DESCRIPTOR),
					checked: showMyOwnScreenShare,
					onChange: (checked) => {
						if (!checked) {
							if (VoicePrompts.getSkipHideOwnScreenShareConfirm()) {
								VoiceSettingsCommands.update({showMyOwnScreenShare: false});
							} else {
								ModalCommands.pushAfterBottomSheetClose(
									onClose,
									modal(() => (
										<HideOwnScreenShareConfirmModal data-flx="ui.action-menu.items.voice-participant-menu-data.on-change.hide-own-screen-share-confirm-modal" />
									)),
								);
							}
						} else {
							VoiceSettingsCommands.update({showMyOwnScreenShare: true});
						}
					},
				},
				{
					label: i18n._(SHOW_NON_VIDEO_PARTICIPANTS_DESCRIPTOR),
					checked: showNonVideoParticipants,
					onChange: (checked) => VoiceSettingsCommands.update({showNonVideoParticipants: checked}),
				},
				{
					label: i18n._(PRIORITIZE_SPEAKERS_DESCRIPTOR),
					checked: prioritizeSpeakingParticipants,
					onChange: (checked) => VoiceSettings.setPrioritizeSpeakingParticipants(checked),
				},
			];
			displayActions.push(...displayPrefs);
		} else {
			if (isCurrentUserConnectedToVoice) {
				menuGroups.push({items: buildVolumeControls(connectionId)});
				if (connectionId) {
					mediaActions.push({
						icon: (
							<LocalDisableVideoIcon
								size={16}
								data-flx="ui.action-menu.items.voice-participant-menu-data.groups.local-disable-video-icon"
							/>
						),
						label: i18n._(DISABLE_VIDEO_LOCALLY_DESCRIPTOR),
						checked: isVideoDisabled,
						onChange: (checked: boolean) => {
							const activeCallId = MediaEngine.connectionId;
							if (!activeCallId) {
								const error = new Error('Cannot toggle local video without an active voice connection');
								logger.error('Voice participant menu action invoked without connection id', {
									participantIdentity,
									connectionId,
									callId,
								});
								throw error;
							}
							MediaEngine.setLocalVideoDisabled(participantIdentity, checked);
						},
					});
				}
			}
			if (connectionId) {
				advancedActions.push(createCopyDeviceIdAction(connectionId));
			}
		}
		if (isParentGroupedItem && hasMultipleConnections && guildId) {
			const bulkActions: Array<MenuItemType> = [];
			const connectionIds = userVoiceStates.map((u) => u.connectionId);
			if (isCurrentUser && isCurrentUserConnectedToVoice) {
				const allMuted = userVoiceStates.every(({voiceState}) => voiceState.self_mute);
				bulkActions.push({
					icon: (
						<SelfMuteIcon
							size={16}
							data-flx="ui.action-menu.items.voice-participant-menu-data.groups.self-mute-icon--3"
						/>
					),
					label: i18n._(allMuted ? VOICE_UNMUTE_ALL_DEVICES_DESCRIPTOR : VOICE_MUTE_ALL_DEVICES_DESCRIPTOR),
					onClick: () => {
						const targetMute = !allMuted;
						VoiceStateCommands.bulkMuteConnections(connectionIds, targetMute);
						if (targetMute) SoundCommands.playSound(SoundType.Mute);
						else SoundCommands.playSound(SoundType.Unmute);
						onClose();
					},
				});
				const allDeafened = userVoiceStates.every(({voiceState}) => voiceState.self_deaf);
				bulkActions.push({
					icon: (
						<SelfDeafenIcon
							size={16}
							data-flx="ui.action-menu.items.voice-participant-menu-data.groups.self-deafen-icon--3"
						/>
					),
					label: i18n._(allDeafened ? VOICE_UNDEAFEN_ALL_DEVICES_DESCRIPTOR : VOICE_DEAFEN_ALL_DEVICES_DESCRIPTOR),
					onClick: () => {
						const targetDeafen = !allDeafened;
						VoiceStateCommands.bulkDeafenConnections(connectionIds, targetDeafen);
						if (targetDeafen) SoundCommands.playSound(SoundType.Deaf);
						else SoundCommands.playSound(SoundType.Undeaf);
						onClose();
					},
				});
				bulkActions.push({
					icon: (
						<BulkTurnOffCameraIcon
							size={16}
							data-flx="ui.action-menu.items.voice-participant-menu-data.groups.bulk-turn-off-camera-icon"
						/>
					),
					label: i18n._(TURN_OFF_ALL_DEVICE_CAMERAS_DESCRIPTOR),
					onClick: () => {
						VoiceStateCommands.bulkTurnOffCameras(connectionIds);
						onClose();
					},
				});
				bulkActions.push({
					icon: (
						<DisconnectIcon
							size={16}
							data-flx="ui.action-menu.items.voice-participant-menu-data.groups.disconnect-icon--2"
						/>
					),
					label: i18n._(VOICE_DISCONNECT_ALL_DEVICES_DESCRIPTOR),
					onClick: async () => {
						await VoiceStateCommands.bulkDisconnect(connectionIds);
						onClose();
					},
					danger: true,
				});
			} else if (!isCurrentUser && canMoveMembers && canManageTarget(user.id)) {
				bulkActions.push({
					icon: (
						<DisconnectIcon
							size={16}
							data-flx="ui.action-menu.items.voice-participant-menu-data.groups.disconnect-icon--3"
						/>
					),
					label: i18n._(VOICE_DISCONNECT_ALL_DEVICES_DESCRIPTOR),
					onClick: async () => {
						await VoiceStateCommands.bulkDisconnect(connectionIds);
						onClose();
					},
					danger: true,
				});
			}
			if (bulkActions.length > 0) {
				if (isCurrentUser) {
					deviceActions.push(...bulkActions);
				} else {
					moderationSubmenuActions.push(...bulkActions);
				}
			}
		}
		if (guildId && member) {
			const guildActions: Array<MenuItemType> = [];
			const hasChangeNicknamePermission = Permission.can(Permissions.CHANGE_NICKNAME, {guildId});
			const hasManageNicknamesPermission = Permission.can(Permissions.MANAGE_NICKNAMES, {guildId});
			const canManageNicknames =
				(isCurrentUser && hasChangeNicknamePermission) || (!isCurrentUser && hasManageNicknamesPermission);
			if (canManageNicknames) {
				guildActions.push({
					icon: (
						<ChangeNicknameIcon
							size={16}
							data-flx="ui.action-menu.items.voice-participant-menu-data.groups.change-nickname-icon--2"
						/>
					),
					label: i18n._(CHANGE_NICKNAME_DESCRIPTOR),
					onClick: () => {
						ModalCommands.pushAfterBottomSheetClose(
							onClose,
							modal(() => (
								<ChangeNicknameModal
									guildId={guildId}
									user={user}
									member={member}
									data-flx="ui.action-menu.items.voice-participant-menu-data.on-click.change-nickname-modal"
								/>
							)),
						);
					},
				});
			}
			if (guildActions.length > 0) {
				userActions.push(...guildActions);
			}
		}
		if (!isCurrentUser) {
			const relationshipActions: Array<MenuItemType> = [];
			if (!RuntimeConfig.directMessagesDisabled) {
				switch (relationshipType) {
					case RelationshipTypes.FRIEND:
						relationshipActions.push({
							icon: (
								<RemoveFriendIcon
									size={16}
									data-flx="ui.action-menu.items.voice-participant-menu-data.groups.remove-friend-icon"
								/>
							),
							label: i18n._(REMOVE_FRIEND_DESCRIPTOR),
							onClick: (event?: {shiftKey?: boolean}) => {
								ModalCommands.runAfterBottomSheetClose(onClose, () =>
									RelationshipActionUtils.showRemoveFriendConfirmation(i18n, user, {
										bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
										showShiftBypassConfirmationTip: true,
									}),
								);
							},
						});
						break;
					case RelationshipTypes.INCOMING_REQUEST:
						relationshipActions.push({
							icon: (
								<AcceptFriendRequestIcon
									size={16}
									data-flx="ui.action-menu.items.voice-participant-menu-data.groups.accept-friend-request-icon"
								/>
							),
							label: i18n._(ACCEPT_FRIEND_REQUEST_DESCRIPTOR),
							onClick: (event?: {shiftKey?: boolean}) => {
								ModalCommands.runAfterBottomSheetClose(onClose, () =>
									RelationshipActionUtils.showAcceptFriendRequestConfirmation(i18n, user, {
										bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
										showShiftBypassConfirmationTip: true,
									}),
								);
							},
						});
						relationshipActions.push({
							icon: (
								<IgnoreFriendRequestIcon
									size={16}
									data-flx="ui.action-menu.items.voice-participant-menu-data.groups.ignore-friend-request-icon"
								/>
							),
							label: i18n._(IGNORE_FRIEND_REQUEST_DESCRIPTOR),
							onClick: () => {
								onClose();
								RelationshipActionUtils.ignoreFriendRequest(i18n, user.id);
							},
						});
						break;
					case RelationshipTypes.OUTGOING_REQUEST:
						break;
					case RelationshipTypes.BLOCKED:
						break;
					default:
						if (!user.bot && Users.currentUser?.verified !== false) {
							relationshipActions.push({
								icon: (
									<SendFriendRequestIcon
										size={16}
										data-flx="ui.action-menu.items.voice-participant-menu-data.groups.send-friend-request-icon"
									/>
								),
								label: i18n._(ADD_FRIEND_DESCRIPTOR),
								onClick: async () => {
									await RelationshipActionUtils.sendFriendRequest(i18n, user.id);
								},
							});
						}
						break;
				}
			}
			if (!user.system) {
				if (relationshipType === RelationshipTypes.BLOCKED) {
					relationshipActions.push({
						icon: (
							<BlockUserIcon
								size={16}
								data-flx="ui.action-menu.items.voice-participant-menu-data.groups.block-user-icon"
							/>
						),
						label: i18n._(UNBLOCK_USER_ACTION_DESCRIPTOR),
						onClick: (event?: {shiftKey?: boolean}) => {
							ModalCommands.runAfterBottomSheetClose(onClose, () =>
								RelationshipActionUtils.showUnblockUserConfirmation(i18n, user, {
									bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
									showShiftBypassConfirmationTip: true,
								}),
							);
						},
					});
				} else {
					relationshipActions.push({
						icon: (
							<BlockUserIcon
								size={16}
								data-flx="ui.action-menu.items.voice-participant-menu-data.groups.block-user-icon--2"
							/>
						),
						label: i18n._(BLOCK_DESCRIPTOR),
						onClick: (event?: {shiftKey?: boolean}) => {
							ModalCommands.runAfterBottomSheetClose(onClose, () =>
								RelationshipActionUtils.showBlockUserConfirmation(i18n, user, {
									bypassConfirm: RelationshipActionUtils.shouldBypassRelationshipConfirmation(event),
									showShiftBypassConfirmationTip: true,
								}),
							);
						},
						danger: true,
					});
				}
			}
			if (relationshipActions.length > 0) {
				relationshipSubmenuActions.push(...relationshipActions);
			}
		}
		if (!isCurrentUser && guildId && member) {
			const moderationActions: Array<MenuItemType | MenuCheckboxType> = [];
			if (canMuteMembers && canManageTarget(user.id)) {
				const isGuildMuted = memberMute;
				const isGuildDeafened = memberDeaf;
				moderationActions.push({
					icon: (
						<GuildMuteIcon
							size={16}
							data-flx="ui.action-menu.items.voice-participant-menu-data.groups.guild-mute-icon"
						/>
					),
					label: i18n._(VOICE_COMMUNITY_MUTE_DESCRIPTOR),
					checked: isGuildMuted,
					onChange: async (checked: boolean) => {
						try {
							await GuildMemberCommands.update(guildId, user.id, {mute: checked});
							if (checked) SoundCommands.playSound(SoundType.Mute);
							else SoundCommands.playSound(SoundType.Unmute);
						} catch (error) {
							logger.error('Failed to update community mute:', error);
							showVoiceMemberModerationFailedModal(error, VOICE_COMMUNITY_MUTE_DESCRIPTOR);
						}
					},
				});
				moderationActions.push({
					icon: (
						<GuildDeafenIcon
							size={16}
							data-flx="ui.action-menu.items.voice-participant-menu-data.groups.guild-deafen-icon"
						/>
					),
					label: i18n._(VOICE_COMMUNITY_DEAFEN_DESCRIPTOR),
					checked: isGuildDeafened,
					onChange: async (checked: boolean) => {
						try {
							await GuildMemberCommands.update(guildId, user.id, {deaf: checked});
							if (checked) SoundCommands.playSound(SoundType.Deaf);
							else SoundCommands.playSound(SoundType.Undeaf);
						} catch (error) {
							logger.error('Failed to update community deafen:', error);
							showVoiceMemberModerationFailedModal(error, VOICE_COMMUNITY_DEAFEN_DESCRIPTOR);
						}
					},
				});
			}
			if (canMoveMembers && canManageTarget(user.id) && !isParentGroupedItem) {
				moderationActions.push({
					icon: (
						<DisconnectIcon
							size={16}
							data-flx="ui.action-menu.items.voice-participant-menu-data.groups.disconnect-icon--4"
						/>
					),
					label: i18n._(connectionId ? VOICE_DISCONNECT_DEVICE_DESCRIPTOR : VOICE_DISCONNECT_DESCRIPTOR),
					onClick: async () => {
						try {
							await GuildMemberCommands.update(guildId, user.id, {
								channel_id: null,
								connection_id: connectionId,
							});
						} catch (error) {
							logger.error('Failed to disconnect participant:', error);
							showVoiceMemberModerationFailedModal(error, VOICE_DISCONNECT_DESCRIPTOR);
						}
						onClose();
					},
					danger: true,
				});
			}
			if (canTimeoutTarget) {
				const isTimedOut = memberTimedOut;
				moderationActions.push({
					icon: (
						<TimeoutIcon size={16} data-flx="ui.action-menu.items.voice-participant-menu-data.groups.timeout-icon" />
					),
					label: isTimedOut ? i18n._(REMOVE_TIMEOUT_DESCRIPTOR) : i18n._(TIMEOUT_DESCRIPTOR),
					onClick: () => {
						ModalCommands.runAfterBottomSheetClose(onClose, () => {
							if (isTimedOut) {
								ModalCommands.push(
									modal(() => (
										<RemoveTimeoutModal
											guildId={guildId}
											targetUser={user}
											data-flx="ui.action-menu.items.voice-participant-menu-data.on-click.remove-timeout-modal"
										/>
									)),
								);
							} else {
								ModalCommands.push(
									modal(() => (
										<TimeoutMemberModal
											guildId={guildId}
											targetUser={user}
											data-flx="ui.action-menu.items.voice-participant-menu-data.on-click.timeout-member-modal"
										/>
									)),
								);
							}
						});
					},
					danger: !isTimedOut,
				});
			}
			if (canKickTarget) {
				moderationActions.push({
					icon: (
						<KickMemberIcon
							size={16}
							data-flx="ui.action-menu.items.voice-participant-menu-data.groups.kick-member-icon"
						/>
					),
					label: i18n._(KICK_MEMBER_DESCRIPTOR),
					onClick: () => {
						ModalCommands.pushAfterBottomSheetClose(
							onClose,
							modal(() => (
								<KickMemberModal
									guildId={guildId}
									targetUser={user}
									data-flx="ui.action-menu.items.voice-participant-menu-data.on-click.kick-member-modal"
								/>
							)),
						);
					},
					danger: true,
				});
			}
			if (canBanTarget) {
				moderationActions.push({
					icon: (
						<BanMemberIcon
							size={16}
							data-flx="ui.action-menu.items.voice-participant-menu-data.groups.ban-member-icon"
						/>
					),
					label: i18n._(BAN_MEMBER_DESCRIPTOR),
					onClick: () => {
						ModalCommands.pushAfterBottomSheetClose(
							onClose,
							modal(() => (
								<BanMemberModal
									guildId={guildId}
									targetUser={user}
									data-flx="ui.action-menu.items.voice-participant-menu-data.on-click.ban-member-modal"
								/>
							)),
						);
					},
					danger: true,
				});
			}
			if (moderationActions.length > 0) {
				moderationSubmenuActions.push(...moderationActions);
			}
		}
		if (developerMode) {
			advancedActions.push({
				icon: <DebugIcon size={16} data-flx="ui.action-menu.items.voice-participant-menu-data.groups.debug-icon" />,
				label: i18n._(DEBUG_USER_DESCRIPTOR),
				onClick: () => {
					ModalCommands.pushAfterBottomSheetClose(
						onClose,
						modal(() => (
							<UserDebugModal
								title={i18n._(USER_DEBUG_DESCRIPTOR)}
								user={user}
								data-flx="ui.action-menu.items.voice-participant-menu-data.on-click.user-debug-modal"
							/>
						)),
					);
				},
			});
		}
		advancedActions.push({
			icon: <CopyIdIcon size={16} data-flx="ui.action-menu.items.voice-participant-menu-data.groups.copy-id-icon" />,
			label: i18n._(COPY_USER_ID_DESCRIPTOR),
			onClick: () => {
				onClose();
				TextCopyCommands.copy(i18n, user.id, true);
			},
		});
		addSecondarySubmenu(i18n._(STREAM_CONTROLS_DESCRIPTOR), streamControlActions);
		addSecondarySubmenu(i18n._(MEDIA_CONTROLS_DESCRIPTOR), mediaActions);
		addSecondarySubmenu(i18n._(DEVICE_CONTROLS_DESCRIPTOR), deviceActions);
		addSecondarySubmenu(i18n._(DISPLAY_OPTIONS_DESCRIPTOR), displayActions);
		addSecondarySubmenu(i18n._(USER_ACTIONS_DESCRIPTOR), userActions);
		addSecondarySubmenu(i18n._(RELATIONSHIP_ACTIONS_DESCRIPTOR), relationshipSubmenuActions);
		addSecondarySubmenu(i18n._(MODERATION_ACTIONS_DESCRIPTOR), moderationSubmenuActions);
		addSecondarySubmenu(i18n._(ADVANCED_ACTIONS_DESCRIPTOR), advancedActions);
		if (secondarySubmenus.length > 0) {
			menuGroups.push({items: secondarySubmenus});
		}
		return menuGroups;
	}, [
		i18n.locale,
		user,
		guildId,
		connectionId,
		isCurrentUser,
		isGroupedItem,
		isParentGroupedItem,
		member,
		developerMode,
		relationshipType,
		hasActiveDirectCall,
		canMention,
		focusedChannelId,
		canMuteMembers,
		canMoveMembers,
		canManageTarget,
		canKickTarget,
		canBanTarget,
		canTimeoutTarget,
		userVoiceStates,
		hasMultipleConnections,
		onClose,
		connectionVoiceState,
		pinnedParticipantIdentity,
		pinnedParticipantSource,
		memberMute,
		memberDeaf,
		memberTimedOut,
		displayName,
		currentUserVoiceState,
		currentUserVoiceStateInGuild,
		isCurrentUserConnectedToVoice,
		currentConnectionId,
		currentConnectionVoiceState,
		showMyOwnCamera,
		showMyOwnScreenShare,
		showNonVideoParticipants,
		prioritizeSpeakingParticipants,
		showConnectionVolumeControls,
		participantVolume,
		connectionVolume,
		isVideoDisabled,
		callId,
		participantIdentity,
		streamKey,
		isScreenShare,
		isWatching,
		hasScreenShareAudio,
		isOwnScreenShare,
		onStopWatching,
		streamVolume,
		isStreamMuted,
		hiddenConnectionCount,
		deviceConnectionCount,
		isDeviceGroupExpanded,
		onToggleDeviceGroup,
	]);
	return {
		groups,
		member,
		isCurrentUser,
		developerMode,
		relationshipType,
		canMoveMembers,
		userVoiceStates,
		hasMultipleConnections,
		voiceChannels,
		hasVoiceChannels: voiceChannels.length > 0,
	};
}
