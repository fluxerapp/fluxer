// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/GroupedVoiceParticipant.module.css';
import {VoiceParticipantItem} from '@app/features/app/components/layout/VoiceParticipantItem';
import {VoiceStateIcons} from '@app/features/app/components/layout/VoiceStateIcons';
import {useContextMenuHoverState} from '@app/features/app/hooks/useContextMenuHoverState';
import {PreloadableUserPopout} from '@app/features/channel/components/PreloadableUserPopout';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {VoiceParticipantContextMenu} from '@app/features/ui/action_menu/VoiceParticipantContextMenu';
import {AvatarWithPresence} from '@app/features/ui/avatars/AvatarWithPresence';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
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
import {msg, plural} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useRef, useState} from 'react';

const OPEN_PROFILE_FOR_DESCRIPTOR = msg({
	message: 'Open profile for {nickname}',
	comment: 'Short label in the app layout grouped voice participant. Preserve {nickname}; it is inserted by code.',
});

function useOpenProfileAriaLabel(user: User, guildId: string) {
	const {i18n} = useLingui();
	const nickname = NicknameUtils.getNickname(user, guildId);
	return useMemo(() => i18n._(OPEN_PROFILE_FOR_DESCRIPTOR, {nickname}), [nickname, i18n.locale]);
}

interface GroupedVoiceParticipantProps {
	user: User;
	voiceStates: Array<VoiceState>;
	guildId: string;
	anySpeaking?: boolean;
}

export const GroupedVoiceParticipant = observer(function GroupedVoiceParticipant({
	user,
	voiceStates,
	guildId,
	anySpeaking: propAnySpeaking,
}: GroupedVoiceParticipantProps) {
	const openProfileAriaLabel = useOpenProfileAriaLabel(user, guildId);
	const mediaEngineVersion = useMediaEngineVersion();
	const [isExpanded, setIsExpanded] = useState(false);
	const currentUser = Users.getCurrentUser();
	const isCurrentUser = currentUser?.id === user.id;
	const currentConnectionId = MediaEngine.connectionId;
	const localSelfMute = LocalVoiceState.getSelfMute();
	const localSelfDeaf = LocalVoiceState.getSelfDeaf();
	const localSelfVideo = LocalVoiceState.getSelfVideo();
	const localSelfStream = LocalVoiceState.getSelfStream();
	const rowRef = useRef<HTMLDivElement>(null);
	const isContextMenuOpen = useContextMenuHoverState(rowRef);
	const toggleExpanded = useCallback(() => setIsExpanded((prev) => !prev), []);
	const connectionCount = voiceStates.length;
	const handleContextMenu = useCallback(
		(event: React.MouseEvent) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<VoiceParticipantContextMenu
					user={user}
					participantName={NicknameUtils.getNickname(user, guildId)}
					onClose={onClose}
					guildId={guildId}
					isGroupedItem={true}
					isParentGroupedItem={true}
					data-flx="app.grouped-voice-participant.handle-context-menu.voice-participant-context-menu"
				/>
			));
		},
		[user, guildId],
	);
	const stateAgg = useMemo(() => {
		let anySpeaking = propAnySpeaking ?? false;
		let anyCameraOn = false;
		let anyLive = false;
		let guildMuted = false;
		let guildDeaf = false;
		let permissionMuted = false;
		let allSelfMuted = true;
		let allSelfDeaf = true;
		for (const state of voiceStates) {
			const connectionId = state.connection_id ?? '';
			const participant = MediaEngine.getParticipantByUserIdAndConnectionId(user.id, connectionId);
			const isCurrentLocalConnection = isCurrentUser && connectionId === currentConnectionId;
			const statePermissionMuted = isParticipantVoicePermissionMuted({
				voiceState: state,
				guildId,
				channelId: state.channel_id ?? null,
				isCurrentUser,
			});
			const displayState = resolveVoiceParticipantDisplayState({
				participant,
				voiceState: state,
				isLocalConnection: isCurrentLocalConnection,
				localSelfMute,
				localSelfDeaf,
				localSelfVideo,
				localSelfStream,
				permissionMuted: statePermissionMuted,
			});
			if (propAnySpeaking === undefined) {
				anySpeaking = anySpeaking || displayState.speaking;
			}
			anyCameraOn = anyCameraOn || displayState.cameraOn;
			anyLive = anyLive || displayState.streaming;
			guildMuted = guildMuted || displayState.guildMute;
			guildDeaf = guildDeaf || displayState.guildDeaf;
			permissionMuted = permissionMuted || statePermissionMuted;
			allSelfMuted = allSelfMuted && displayState.selfMute;
			allSelfDeaf = allSelfDeaf && displayState.selfDeaf;
		}
		if (isCurrentUser) {
			anyCameraOn = anyCameraOn || localSelfVideo;
			anyLive = anyLive || localSelfStream;
		}
		return {anySpeaking, anyCameraOn, anyLive, guildMuted, guildDeaf, permissionMuted, allSelfMuted, allSelfDeaf};
	}, [
		voiceStates,
		user.id,
		guildId,
		isCurrentUser,
		currentConnectionId,
		localSelfMute,
		localSelfDeaf,
		localSelfVideo,
		localSelfStream,
		propAnySpeaking,
		mediaEngineVersion,
	]);
	const activeStreamState = useMemo(() => {
		for (const state of voiceStates) {
			const connectionId = state.connection_id ?? '';
			if (!connectionId) continue;
			const participant = MediaEngine.getParticipantByUserIdAndConnectionId(user.id, connectionId);
			const live = state.self_stream === true || (participant ? participant.isScreenShareEnabled : false);
			if (live) return state;
		}
		if (isCurrentUser && localSelfStream) {
			return (
				voiceStates.find((state) => state.connection_id === currentConnectionId) ??
				voiceStates.find((state) => Boolean(state.connection_id)) ??
				null
			);
		}
		return null;
	}, [voiceStates, user.id, isCurrentUser, localSelfStream, currentConnectionId, mediaEngineVersion]);
	const streamKey = activeStreamState?.connection_id
		? getStreamKey(guildId, activeStreamState.channel_id ?? null, activeStreamState.connection_id)
		: '';
	const showStreamHover = Boolean(activeStreamState?.connection_id);
	const activeChannelId = activeStreamState?.channel_id ?? null;
	const streamWatchStateArgs = useMemo(
		() => ({streamKey, guildId, channelId: activeChannelId}),
		[streamKey, guildId, activeChannelId],
	);
	const {startWatching} = useStreamWatchState(streamWatchStateArgs);
	const streamParticipantIdentity = useMemo(() => {
		if (!activeStreamState?.connection_id) return null;
		return buildVoiceParticipantIdentity(user.id, activeStreamState.connection_id ?? '');
	}, [user.id, activeStreamState?.connection_id]);
	const handleNavigateToWatch = useCallback(() => {
		if (activeChannelId) {
			NavigationCommands.selectChannel(guildId ?? ME, activeChannelId);
		}
	}, [guildId, activeChannelId]);
	const {onClick: handleClick, onDoubleClick: handleDoubleClick} = useStreamWatchDoubleClick({
		streamParticipantIdentity: showStreamHover ? streamParticipantIdentity : null,
		guildId,
		channelId: activeChannelId,
		startWatching,
		onNavigateToWatch: handleNavigateToWatch,
	});
	const handleKeyDown = useCallback((event: React.KeyboardEvent<HTMLDivElement>) => {
		if (isKeyboardActivationKey(event.key)) {
			event.preventDefault();
		}
	}, []);
	return (
		<div className={styles.container} data-flx="app.grouped-voice-participant.container">
			<PreloadableUserPopout
				user={user}
				isWebhook={false}
				guildId={guildId}
				position="right-start"
				disableContextMenu={true}
				data-flx="app.grouped-voice-participant.preloadable-user-popout"
			>
				<StreamWatchHoverPopout
					enabled={showStreamHover}
					streamKey={streamKey}
					guildId={guildId}
					channelId={activeStreamState?.channel_id ?? null}
					data-flx="app.grouped-voice-participant.stream-watch-hover-popout"
				>
					<div
						ref={rowRef}
						className={clsx(
							styles.participantButton,
							stateAgg.anySpeaking && styles.participantButtonSpeaking,
							isContextMenuOpen && styles.participantButtonContextMenuActive,
						)}
						role="button"
						tabIndex={0}
						aria-label={openProfileAriaLabel}
						onClick={handleClick}
						onKeyDown={handleKeyDown}
						onContextMenu={handleContextMenu}
						onDoubleClick={handleDoubleClick}
						data-flx="app.grouped-voice-participant.participant-button.click"
					>
						<div className={styles.avatarAndName} data-flx="app.grouped-voice-participant.avatar-and-name">
							<AvatarWithPresence
								user={user}
								size={24}
								speaking={stateAgg.anySpeaking}
								guildId={guildId}
								data-flx="app.grouped-voice-participant.avatar-with-presence"
							/>
							<div className={styles.nameContainer} data-flx="app.grouped-voice-participant.name-container">
								<span
									className={clsx(
										styles.participantName,
										stateAgg.anySpeaking && styles.participantNameSpeaking,
										isCurrentUser && !stateAgg.anySpeaking && styles.participantNameCurrent,
									)}
									data-flx="app.grouped-voice-participant.participant-name"
								>
									{NicknameUtils.getNickname(user, guildId)}
								</span>
								<Tooltip
									text={
										isExpanded
											? plural(
													{count: connectionCount},
													{
														one: 'Hide # device',
														other: 'Hide # devices',
													},
												)
											: plural(
													{count: connectionCount},
													{
														one: 'Show # device',
														other: 'Show # devices',
													},
												)
									}
									data-flx="app.grouped-voice-participant.tooltip"
								>
									<button
										type="button"
										aria-label={
											isExpanded
												? plural(
														{count: connectionCount},
														{
															one: 'Hide # device',
															other: 'Hide # devices',
														},
													)
												: plural(
														{count: connectionCount},
														{
															one: 'Show # device',
															other: 'Show # devices',
														},
													)
										}
										aria-expanded={isExpanded}
										onClick={(event) => {
											event.stopPropagation();
											toggleExpanded();
										}}
										className={styles.deviceCountButton}
										data-flx="app.grouped-voice-participant.device-count-button.stop-propagation"
									>
										({connectionCount})
									</button>
								</Tooltip>
							</div>
						</div>
						<div className={styles.iconsAndToggle} data-flx="app.grouped-voice-participant.icons-and-toggle">
							<VoiceStateIcons
								isSelfMuted={stateAgg.allSelfMuted && !stateAgg.guildMuted && !stateAgg.permissionMuted}
								isSelfDeafened={stateAgg.allSelfDeaf && !stateAgg.guildDeaf}
								isGuildMuted={stateAgg.guildMuted}
								isGuildDeafened={stateAgg.guildDeaf}
								isPermissionMuted={stateAgg.permissionMuted}
								isCurrentUser={isCurrentUser}
								isCameraOn={stateAgg.anyCameraOn}
								isScreenSharing={stateAgg.anyLive}
								className={styles.flexShrinkZero}
								data-flx="app.grouped-voice-participant.flex-shrink-zero"
							/>
						</div>
					</div>
				</StreamWatchHoverPopout>
			</PreloadableUserPopout>
			{isExpanded && (
				<div className={styles.devicesContainer} data-flx="app.grouped-voice-participant.devices-container">
					{[...voiceStates]
						.sort((a, b) => (a.connection_id || '').localeCompare(b.connection_id || ''))
						.map((voiceState, index) => (
							<VoiceParticipantItem
								key={voiceState.connection_id || `${user.id}-${index}`}
								user={user}
								voiceState={voiceState}
								guildId={guildId}
								isGroupedItem={true}
								isCurrentUser={isCurrentUser}
								isCurrentUserConnection={isCurrentUser && voiceState.connection_id === currentConnectionId}
								data-flx="app.grouped-voice-participant.voice-participant-item"
							/>
						))}
				</div>
			)}
		</div>
	);
});
