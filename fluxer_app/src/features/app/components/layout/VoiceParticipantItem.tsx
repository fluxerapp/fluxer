// SPDX-License-Identifier: AGPL-3.0-or-later

import {LongPressable} from '@app/features/app/components/LongPressable';
import channelItemSurfaceStyles from '@app/features/app/components/layout/ChannelItemSurface.module.css';
import {DragItemType} from '@app/features/app/components/layout/types/DndTypes';
import styles from '@app/features/app/components/layout/VoiceParticipantItem.module.css';
import {VoiceStateIcons} from '@app/features/app/components/layout/VoiceStateIcons';
import {useContextMenuHoverState} from '@app/features/app/hooks/useContextMenuHoverState';
import {PreloadableUserPopout} from '@app/features/channel/components/PreloadableUserPopout';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import Permission from '@app/features/permissions/state/Permission';
import {VoiceParticipantContextMenu} from '@app/features/ui/action_menu/VoiceParticipantContextMenu';
import {AvatarWithPresence} from '@app/features/ui/avatars/AvatarWithPresence';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import type {User} from '@app/features/user/models/User';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {VoiceParticipantBottomSheet} from '@app/features/voice/components/bottomsheets/VoiceParticipantBottomSheet';
import {getStreamKey} from '@app/features/voice/components/StreamKeys';
import {StreamWatchHoverPopout} from '@app/features/voice/components/StreamWatchHoverPopout';
import {useStreamWatchState} from '@app/features/voice/components/useStreamWatchState';
import {resolveVoiceParticipantDisplayState} from '@app/features/voice/components/VoiceParticipantDisplayState';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import {useStreamWatchDoubleClick} from '@app/features/voice/hooks/useStreamWatchDoubleClick';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import {buildVoiceParticipantIdentity} from '@app/features/voice/utils/VoiceParticipantIdentity';
import {isParticipantVoicePermissionMuted} from '@app/features/voice/utils/VoicePermissionUtils';
import {ME} from '@fluxer/constants/src/AppConstants';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {DesktopIcon, DeviceMobileIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useRef, useState} from 'react';
import type {ConnectableElement} from 'react-dnd';
import {useDrag} from 'react-dnd';

const UNKNOWN_DEVICE_DESCRIPTOR = msg({
	message: 'Unknown device',
	comment: 'Short label in the app layout voice participant item.',
});
const OPEN_PROFILE_FOR_DESCRIPTOR = msg({
	message: 'Open profile for {displayName}',
	comment: 'Short label in the app layout voice participant item. Preserve {displayName}; it is inserted by code.',
});
export const VoiceParticipantItem = observer(function VoiceParticipantItem({
	user,
	voiceState,
	guildId,
	isGroupedItem = false,
	isCurrentUserConnection = false,
	isCurrentUser = false,
}: {
	user: User;
	voiceState: VoiceState | null;
	guildId: string;
	isGroupedItem?: boolean;
	isCurrentUserConnection?: boolean;
	isCurrentUser?: boolean;
}) {
	const {i18n} = useLingui();
	useMediaEngineVersion();
	const connectionId = voiceState?.connection_id ?? '';
	const participant = MediaEngine.getParticipantByUserIdAndConnectionId(user.id, connectionId);
	const connectedChannelId = MediaEngine.channelId;
	const currentChannelId = voiceState?.channel_id ?? connectedChannelId ?? null;
	const canMoveMembers = Permission.can(Permissions.MOVE_MEMBERS, {guildId});
	const canDragParticipant = canMoveMembers && currentChannelId !== null;
	const isMobileLayout = MobileLayout.isMobileLayout();
	const [menuOpen, setMenuOpen] = useState(false);
	const [isProfilePopoutOpen, setIsProfilePopoutOpen] = useState(false);
	const localSelfMute = LocalVoiceState.getSelfMute();
	const localSelfDeaf = LocalVoiceState.getSelfDeaf();
	const localSelfVideo = LocalVoiceState.getSelfVideo();
	const localSelfStream = LocalVoiceState.getSelfStream();
	const isLocalParticipant = isCurrentUserConnection;
	const rowRef = useRef<HTMLDivElement>(null);
	const isContextMenuOpen = useContextMenuHoverState(rowRef);
	const [{isDragging}, dragRef] = useDrag(
		() => ({
			type: DragItemType.VOICE_PARTICIPANT,
			item: {
				type: DragItemType.VOICE_PARTICIPANT,
				id: user.id,
				userId: user.id,
				guildId,
				currentChannelId,
			},
			canDrag: canDragParticipant,
			collect: (monitor) => ({isDragging: monitor.isDragging()}),
		}),
		[user.id, guildId, currentChannelId, canDragParticipant],
	);
	const dragConnectorRef = useCallback(
		(node: ConnectableElement | null) => {
			dragRef(node);
			rowRef.current = node as HTMLDivElement | null;
		},
		[dragRef],
	);
	const isPermissionMuted = isParticipantVoicePermissionMuted({
		voiceState,
		guildId,
		channelId: currentChannelId,
		isCurrentUser,
	});
	const displayState = resolveVoiceParticipantDisplayState({
		participant,
		voiceState,
		isLocalConnection: isLocalParticipant,
		localSelfMute,
		localSelfDeaf,
		localSelfVideo,
		localSelfStream,
		permissionMuted: isPermissionMuted,
	});
	const isSelfMuted = displayState.selfMute;
	const isSelfDeafened = displayState.selfDeaf;
	const isGuildMuted = displayState.guildMute;
	const isGuildDeafened = displayState.guildDeaf;
	const isActuallySpeaking = displayState.speaking;
	const displayCameraOn = displayState.cameraOn;
	const displayLive = displayState.streaming;
	const streamKey = useMemo(
		() => getStreamKey(guildId, currentChannelId, connectionId),
		[guildId, currentChannelId, connectionId],
	);
	const showStreamHover = displayLive && Boolean(connectionId);
	const hasVoiceStateIcons =
		displayCameraOn || displayLive || isSelfMuted || isSelfDeafened || isGuildMuted || isGuildDeafened;
	const streamWatchStateArgs = useMemo(
		() => ({streamKey, guildId, channelId: currentChannelId}),
		[streamKey, guildId, currentChannelId],
	);
	const {startWatching} = useStreamWatchState(streamWatchStateArgs);
	const streamParticipantIdentity = useMemo(
		() => (connectionId ? buildVoiceParticipantIdentity(user.id, connectionId) : null),
		[user.id, connectionId],
	);
	const handleNavigateToWatch = useCallback(() => {
		if (currentChannelId) {
			NavigationCommands.selectChannel(guildId ?? ME, currentChannelId);
		}
	}, [guildId, currentChannelId]);
	const {onClick: handleClick, onDoubleClick: handleDoubleClick} = useStreamWatchDoubleClick({
		streamParticipantIdentity: showStreamHover ? streamParticipantIdentity : null,
		guildId,
		channelId: currentChannelId,
		startWatching,
		onNavigateToWatch: handleNavigateToWatch,
	});
	const handleContextMenu = useCallback(
		(event: React.MouseEvent) => {
			event.preventDefault();
			event.stopPropagation();
			const participantName = NicknameUtils.getNickname(user, guildId, currentChannelId ?? undefined);
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<VoiceParticipantContextMenu
					user={user}
					participantName={participantName}
					onClose={onClose}
					guildId={guildId}
					connectionId={connectionId}
					isGroupedItem={isGroupedItem}
					data-flx="app.voice-participant-item.handle-context-menu.voice-participant-context-menu"
				/>
			));
		},
		[user, guildId, connectionId, currentChannelId],
	);
	const handleProfilePopoutOpen = useCallback(() => {
		setIsProfilePopoutOpen(true);
	}, []);
	const handleProfilePopoutClose = useCallback(() => {
		setIsProfilePopoutOpen(false);
	}, []);
	const DeviceIcon = voiceState?.is_mobile ? DeviceMobileIcon : DesktopIcon;
	const unknownDeviceFallback = useMemo(() => i18n._(UNKNOWN_DEVICE_DESCRIPTOR), [i18n.locale]);
	const displayName = isGroupedItem
		? voiceState?.connection_id || unknownDeviceFallback
		: NicknameUtils.getNickname(user, guildId, currentChannelId ?? undefined);
	const openProfileAriaLabel = !isGroupedItem ? i18n._(OPEN_PROFILE_FOR_DESCRIPTOR, {displayName}) : undefined;
	const row = (
		<FocusRing
			offset={-2}
			ringClassName={channelItemSurfaceStyles.channelItemFocusRing}
			data-flx="app.voice-participant-item.focus-ring"
		>
			<LongPressable
				ref={dragConnectorRef}
				className={clsx(
					styles.participantRow,
					isActuallySpeaking && styles.participantRowSpeaking,
					isDragging && styles.participantRowDragging,
					isCurrentUserConnection && !isActuallySpeaking && styles.participantRowCurrentConnection,
					isProfilePopoutOpen && styles.participantRowPopoutOpen,
					isContextMenuOpen && styles.participantRowContextMenuActive,
				)}
				onClick={handleClick}
				onContextMenu={handleContextMenu}
				onDoubleClick={handleDoubleClick}
				onLongPress={() => {
					if (isMobileLayout) setMenuOpen(true);
				}}
				role={!isGroupedItem ? 'button' : undefined}
				tabIndex={!isGroupedItem ? 0 : -1}
				aria-label={openProfileAriaLabel}
				data-flx="app.voice-participant-item.participant-row.click"
			>
				{isGroupedItem ? (
					<div
						className={clsx(
							styles.deviceIcon,
							isActuallySpeaking && styles.deviceIconSpeaking,
							isCurrentUserConnection && !isActuallySpeaking && styles.deviceIconCurrent,
						)}
						data-flx="app.voice-participant-item.device-icon"
					>
						<DeviceIcon
							className={styles.iconContainer}
							weight="regular"
							data-flx="app.voice-participant-item.icon-container"
						/>
					</div>
				) : (
					<AvatarWithPresence
						user={user}
						size={24}
						speaking={isActuallySpeaking}
						guildId={guildId}
						data-flx="app.voice-participant-item.avatar-with-presence"
					/>
				)}
				{isGroupedItem ? (
					<Tooltip text={displayName} position="top" data-flx="app.voice-participant-item.tooltip">
						<span
							className={clsx(
								styles.participantName,
								isActuallySpeaking && styles.participantNameSpeaking,
								isCurrentUserConnection && !isActuallySpeaking && styles.participantNameCurrent,
							)}
							data-flx="app.voice-participant-item.participant-name"
						>
							{displayName}
						</span>
					</Tooltip>
				) : (
					<span
						className={clsx(
							styles.participantName,
							isActuallySpeaking && styles.participantNameSpeaking,
							isCurrentUser && !isActuallySpeaking && styles.participantNameCurrent,
						)}
						data-flx="app.voice-participant-item.participant-name--2"
					>
						{displayName}
					</span>
				)}
				{hasVoiceStateIcons && (
					<div className={styles.iconsContainer} data-flx="app.voice-participant-item.icons-container">
						<VoiceStateIcons
							isSelfMuted={isSelfMuted}
							isSelfDeafened={isSelfDeafened}
							isGuildMuted={isGuildMuted}
							isGuildDeafened={isGuildDeafened}
							isPermissionMuted={isPermissionMuted}
							isCurrentUser={isCurrentUser}
							isCameraOn={displayCameraOn}
							isScreenSharing={displayLive}
							className={styles.flexShrinkZero}
							data-flx="app.voice-participant-item.flex-shrink-zero"
						/>
					</div>
				)}
			</LongPressable>
		</FocusRing>
	);
	const rowWithHover = (
		<StreamWatchHoverPopout
			enabled={showStreamHover}
			streamKey={streamKey}
			guildId={guildId}
			channelId={currentChannelId}
			data-flx="app.voice-participant-item.stream-watch-hover-popout"
		>
			{row}
		</StreamWatchHoverPopout>
	);
	return (
		<>
			{isGroupedItem ? (
				rowWithHover
			) : (
				<PreloadableUserPopout
					user={user}
					isWebhook={false}
					guildId={guildId}
					channelId={currentChannelId ?? undefined}
					position="right-start"
					disableContextMenu={true}
					onPopoutOpen={handleProfilePopoutOpen}
					onPopoutClose={handleProfilePopoutClose}
					data-flx="app.voice-participant-item.preloadable-user-popout"
				>
					{rowWithHover}
				</PreloadableUserPopout>
			)}
			{isMobileLayout && (
				<VoiceParticipantBottomSheet
					isOpen={menuOpen}
					onClose={() => setMenuOpen(false)}
					user={user}
					participant={participant}
					guildId={guildId}
					connectionId={connectionId}
					isConnectionItem={isGroupedItem}
					data-flx="app.voice-participant-item.voice-participant-bottom-sheet"
				/>
			)}
		</>
	);
});
