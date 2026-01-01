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
import {DesktopIcon, DeviceMobileIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useMemo, useState} from 'react';
import type {ConnectableElement} from 'react-dnd';
import {useDrag} from 'react-dnd';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import {Permissions} from '~/Constants';
import {VoiceParticipantBottomSheet} from '~/components/bottomsheets/VoiceParticipantBottomSheet';
import {PreloadableUserPopout} from '~/components/channel/PreloadableUserPopout';
import {LongPressable} from '~/components/LongPressable';
import {AvatarWithPresence} from '~/components/uikit/avatars/AvatarWithPresence';
import {VoiceParticipantContextMenu} from '~/components/uikit/ContextMenu/VoiceParticipantContextMenu';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Tooltip} from '~/components/uikit/Tooltip';
import type {UserRecord} from '~/records/UserRecord';
import LocalVoiceStateStore from '~/stores/LocalVoiceStateStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import PermissionStore from '~/stores/PermissionStore';
import type {VoiceState} from '~/stores/voice/MediaEngineFacade';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import * as NicknameUtils from '~/utils/NicknameUtils';
import channelItemSurfaceStyles from './ChannelItemSurface.module.css';
import {DND_TYPES} from './types/dnd';
import styles from './VoiceParticipantItem.module.css';
import {VoiceStateIcons} from './VoiceStateIcons';

export const VoiceParticipantItem = observer(function VoiceParticipantItem({
	user,
	voiceState,
	guildId,
	isGroupedItem = false,
	isCurrentUserConnection = false,
	isCurrentUser = false,
}: {
	user: UserRecord;
	voiceState: VoiceState | null;
	guildId: string;
	isGroupedItem?: boolean;
	isCurrentUserConnection?: boolean;
	isCurrentUser?: boolean;
}) {
	const {t} = useLingui();
	const connectionId = voiceState?.connection_id ?? '';
	const participant = MediaEngineStore.getParticipantByUserIdAndConnectionId(user.id, connectionId);
	const connectedChannelId = MediaEngineStore.channelId;
	const currentChannelId = voiceState?.channel_id ?? connectedChannelId ?? null;

	const canMoveMembers = PermissionStore.can(Permissions.MOVE_MEMBERS, {guildId});
	const canDragParticipant = canMoveMembers && currentChannelId !== null;

	const isSpeaking = participant?.isSpeaking ?? false;
	const isMobileLayout = MobileLayoutStore.isMobileLayout();
	const [menuOpen, setMenuOpen] = useState(false);
	const [isProfilePopoutOpen, setIsProfilePopoutOpen] = useState(false);
	const localSelfVideo = LocalVoiceStateStore.selfVideo;
	const localSelfStream = LocalVoiceStateStore.selfStream;
	const isLocalParticipant = isCurrentUser || isCurrentUserConnection;

	const [{isDragging}, dragRef] = useDrag(
		() => ({
			type: DND_TYPES.VOICE_PARTICIPANT,
			item: {
				type: DND_TYPES.VOICE_PARTICIPANT,
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
		},
		[dragRef],
	);

	const isSelfMuted = voiceState?.self_mute ?? (participant ? !participant.isMicrophoneEnabled : false);
	const isSelfDeafened = voiceState?.self_deaf ?? false;
	const isGuildMuted = voiceState?.mute ?? false;
	const isGuildDeafened = voiceState?.deaf ?? false;
	const isActuallySpeaking = isSpeaking && !isSelfMuted && !isGuildMuted;

	const remoteCameraOn = voiceState?.self_video ?? (participant ? participant.isCameraEnabled : false);
	const remoteLive = voiceState?.self_stream ?? (participant ? participant.isScreenShareEnabled : false);
	const displayCameraOn = !!(remoteCameraOn || (isLocalParticipant ? localSelfVideo : false));
	const displayLive = !!(remoteLive || (isLocalParticipant ? localSelfStream : false));
	const hasVoiceStateIcons =
		displayCameraOn || displayLive || isSelfMuted || isSelfDeafened || isGuildMuted || isGuildDeafened;

	const handleContextMenu = useCallback(
		(event: React.MouseEvent) => {
			event.preventDefault();
			event.stopPropagation();
			const participantName = NicknameUtils.getNickname(user, guildId, currentChannelId) || user.username;
			ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
				<VoiceParticipantContextMenu
					user={user}
					participantName={participantName}
					onClose={onClose}
					guildId={guildId}
					connectionId={connectionId}
					isGroupedItem={isGroupedItem}
				/>
			));
		},
		[user, guildId, connectionId, currentChannelId],
	);

	const handleProfilePopoutOpen = React.useCallback(() => {
		setIsProfilePopoutOpen(true);
	}, []);

	const handleProfilePopoutClose = React.useCallback(() => {
		setIsProfilePopoutOpen(false);
	}, []);

	const DeviceIcon = voiceState?.is_mobile ? DeviceMobileIcon : DesktopIcon;
	const unknownDeviceFallback = useMemo(() => t`Unknown Device`, []);
	const displayName = isGroupedItem
		? voiceState?.connection_id || unknownDeviceFallback
		: NicknameUtils.getNickname(user, guildId, currentChannelId);
	const openProfileAriaLabel = !isGroupedItem ? t`Open profile for ${displayName}` : undefined;

	const row = (
		<FocusRing offset={-2} ringClassName={channelItemSurfaceStyles.channelItemFocusRing}>
			<LongPressable
				ref={dragConnectorRef}
				className={clsx(
					styles.participantRow,
					isActuallySpeaking && styles.participantRowSpeaking,
					isDragging && styles.participantRowDragging,
					isCurrentUserConnection && !isActuallySpeaking && styles.participantRowCurrentConnection,
					isProfilePopoutOpen && styles.participantRowPopoutOpen,
				)}
				onContextMenu={handleContextMenu}
				onLongPress={() => {
					if (isMobileLayout) setMenuOpen(true);
				}}
				role={!isGroupedItem ? 'button' : undefined}
				tabIndex={!isGroupedItem ? 0 : -1}
				aria-label={openProfileAriaLabel}
			>
				{isGroupedItem ? (
					<div
						className={clsx(
							styles.deviceIcon,
							isActuallySpeaking && styles.deviceIconSpeaking,
							isCurrentUserConnection && !isActuallySpeaking && styles.deviceIconCurrent,
						)}
					>
						<DeviceIcon className={styles.iconContainer} weight="regular" />
					</div>
				) : (
					<AvatarWithPresence user={user} size={24} speaking={isActuallySpeaking} guildId={guildId} />
				)}

				{isGroupedItem ? (
					<Tooltip text={displayName} position="top">
						<span
							className={clsx(
								styles.participantName,
								isActuallySpeaking && styles.participantNameSpeaking,
								isCurrentUserConnection && !isActuallySpeaking && styles.participantNameCurrent,
							)}
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
					>
						{displayName}
					</span>
				)}

				{hasVoiceStateIcons && (
					<div className={styles.iconsContainer}>
						<VoiceStateIcons
							isSelfMuted={isSelfMuted}
							isSelfDeafened={isSelfDeafened}
							isGuildMuted={isGuildMuted}
							isGuildDeafened={isGuildDeafened}
							isCameraOn={displayCameraOn}
							isScreenSharing={displayLive}
							className={styles.flexShrinkZero}
						/>
					</div>
				)}
			</LongPressable>
		</FocusRing>
	);

	return (
		<>
			{isGroupedItem ? (
				row
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
				>
					{row}
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
				/>
			)}
		</>
	);
});
