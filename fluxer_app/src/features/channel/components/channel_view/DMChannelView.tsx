// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	BlockedUserBarrier,
	SystemDmBarrier,
	UnclaimedDMBarrier,
} from '@app/features/channel/components/barriers/BarrierComponents';
import {ChannelChatLayout} from '@app/features/channel/components/ChannelChatLayout';
import {ChannelHeader} from '@app/features/channel/components/ChannelHeader';
import styles from '@app/features/channel/components/ChannelIndexPage.module.css';
import {ChannelMembers} from '@app/features/channel/components/ChannelMembers';
import {Messages} from '@app/features/channel/components/ChannelMessages';
import {ChannelSearchResults} from '@app/features/channel/components/ChannelSearchResults';
import {ChannelTextarea} from '@app/features/channel/components/ChannelTextarea';
import {ChannelCompactCallSurface} from '@app/features/channel/components/channel_view/ChannelCompactCallSurface';
import {ChannelViewScaffold} from '@app/features/channel/components/channel_view/ChannelViewScaffold';
import {CallControls} from '@app/features/channel/components/channel_view/dm_channel_view/CallControls';
import {CallParticipantsRow} from '@app/features/channel/components/channel_view/dm_channel_view/CallParticipantsRow';
import {
	CALL_AVAILABLE_DESCRIPTOR,
	CONNECTING_DESCRIPTOR,
	getCompactCallHeightKey,
	IN_CALL_DESCRIPTOR,
	IN_CALL_ON_ANOTHER_DEVICE_DESCRIPTOR,
	logger,
	RESIZE_CALL_VIEW_DESCRIPTOR,
	VIEW_CALL_DESCRIPTOR,
	VIEW_INCOMING_CALL_DESCRIPTOR,
} from '@app/features/channel/components/channel_view/dm_channel_view/shared';
import {useCompactCallBannerResize} from '@app/features/channel/components/channel_view/dm_channel_view/useCompactCallBannerResize';
import {useCallHeaderState} from '@app/features/channel/components/channel_view/useCallHeaderState';
import {useChannelSearchState} from '@app/features/channel/components/channel_view/useChannelSearchState';
import {useVoiceCallChromePinState} from '@app/features/channel/components/channel_view/useVoiceCallChromePinState';
import dmStyles from '@app/features/channel/components/direct_message/DMChannelView.module.css';
import {useMessagesBottomBarVisibility} from '@app/features/channel/components/MessagesBottomBarVisibility';
import {useChannelMemberListVisibility} from '@app/features/channel/hooks/useChannelMemberListVisibility';
import {useChannelSearchVisibility} from '@app/features/channel/hooks/useChannelSearchVisibility';
import Channels from '@app/features/channel/state/Channels';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {INCOMING_CALL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {useMemberListVisible} from '@app/features/member/hooks/useMemberListVisible';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import ReadStates from '@app/features/read_state/state/ReadStates';
import Relationships from '@app/features/relationship/state/Relationships';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import * as CallCommands from '@app/features/voice/commands/CallCommands';
import {DirectCallLobbyBottomSheet} from '@app/features/voice/components/bottomsheets/DirectCallLobbyBottomSheet';
import {CompactVoiceCallStreamHeaderInfo} from '@app/features/voice/components/CompactVoiceCallStreamHeaderInfo';
import {useVoiceCallFullscreenViewState} from '@app/features/voice/components/useVoiceCallAppFullscreen';
import {VoiceCallView} from '@app/features/voice/components/VoiceCallView';
import {VoiceE2EEIndicator} from '@app/features/voice/components/VoiceE2EEIndicator';
import {useVoiceParticipantAvatarEntries} from '@app/features/voice/components/VoiceParticipantAvatarList';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {useCompactCallExpansionState} from '@app/features/voice/hooks/useCompactCallExpansionState';
import {COMPACT_VOICE_CALL_HEIGHT_MIN} from '@app/features/voice/state/CompactVoiceCallHeight';
import {VOICE_CALL_DESCRIPTOR} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {Trans, useLingui} from '@lingui/react/macro';
import {ChatTeardropIcon, PhoneIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

interface DMChannelViewProps {
	channelId: string;
}

export const DMChannelView = observer(({channelId}: DMChannelViewProps) => {
	const {i18n} = useLingui();
	const channel = Channels.getChannel(channelId);
	const recipientId = channel?.recipientIds?.[0];
	const recipient = recipientId ? Users.getUser(recipientId) : null;
	const isRecipientBlocked = recipientId ? Relationships.isBlocked(recipientId) : false;
	const isCurrentUserUnclaimed = !Users.currentUser?.isClaimed();
	const isSystemDm = channel ? ChannelUtils.isSystemDmChannel(channel) : false;
	const isDM = channel?.type === ChannelTypes.DM;
	const isGroupDM = channel?.type === ChannelTypes.GROUP_DM;
	const isPersonalNotes = channel?.type === ChannelTypes.DM_PERSONAL_NOTES;
	const currentChannelId = channel?.id ?? null;
	const mediaConnected = MediaEngine.connected;
	const mediaChannelId = MediaEngine.channelId;
	const mediaGuildId = MediaEngine.guildId;
	const room = MediaEngine.room;
	const participantAvatarEntries = useVoiceParticipantAvatarEntries({
		guildId: channel?.guildId ?? null,
		channelId: channel?.id ?? null,
	});
	const searchState = useChannelSearchState(channel);
	const {
		isSearchActive,
		handleSearchClose,
		handleSearchSubmit,
		searchRefreshKey,
		activeSearchQuery,
		activeSearchSegments,
	} = searchState;
	const {onBottomBarVisibilityChange} = useMessagesBottomBarVisibility(channelId);
	const {enabled: isMobileLayout} = MobileLayout;
	const isSearchPanelVisible = isSearchActive && !isMobileLayout;
	useChannelSearchVisibility(channelId, isSearchPanelVisible);
	const displayName = channel ? ChannelUtils.getDMDisplayName(channel) : null;
	const title = isDM && displayName ? `@${displayName}` : displayName;
	useFluxerDocumentTitle(title);
	const callHeaderState = useCallHeaderState(channel);
	const call = callHeaderState.call;
	const isInCallVariant = callHeaderState.controlsVariant === 'inCall';
	const showCompactVoiceView = isInCallVariant && callHeaderState.isDeviceInRoomForChannelCall;
	const memberListDefaultHiddenForChannel = Boolean(isGroupDM && showCompactVoiceView);
	const isMemberListVisible = useMemberListVisible({
		channelId: currentChannelId,
		defaultHiddenForChannel: memberListDefaultHiddenForChannel,
	});
	useChannelMemberListVisibility(channelId, isMemberListVisible);
	const callExistsAndOngoing = callHeaderState.callExistsAndOngoing;
	const controlsVariant = callHeaderState.controlsVariant;
	const showCallBackground = callExistsAndOngoing && controlsVariant !== 'hidden';
	const compactCallUnreadCount = currentChannelId ? ReadStates.getUnreadCount(currentChannelId) : 0;
	const isMobileExperience = isMobileExperienceEnabled();
	const isCompactCallResizable = !isMobileExperience;
	const compactCallHeightMin = COMPACT_VOICE_CALL_HEIGHT_MIN;
	const compactCallHeightKey = useMemo(() => {
		if (!currentChannelId) return null;
		return getCompactCallHeightKey(currentChannelId, call?.messageId ?? null);
	}, [call?.messageId, currentChannelId]);
	const {
		compactCallBannerHeight,
		compactCallMaxHeight,
		isResizingCompactCallBanner,
		compactCallBannerWrapperStyle,
		handleCompactCallResizePointerDown,
		handleCompactCallResizeKeyDown,
	} = useCompactCallBannerResize({
		isCompactCallResizable,
		compactCallHeightKey,
		compactCallHeightMin,
	});
	const [isCallSheetOpen, setIsCallSheetOpen] = useState(false);
	const {
		showFullscreenView: showVoiceCallViewForFullscreen,
		fullscreenRequestNonce: voiceCallFullscreenRequestNonce,
		openFullscreenView: handleOpenVoiceCallFullscreenView,
		closeFullscreenView: handleCloseVoiceCallFullscreenView,
	} = useVoiceCallFullscreenViewState({
		active: Boolean(callExistsAndOngoing && callHeaderState.isDeviceInRoomForChannelCall),
		scopeKey: `dm-call:${channelId}`,
	});
	const {
		isExpanded: isCompactCallExpanded,
		setExpanded: setCompactCallExpanded,
		toggleExpanded: handleToggleCompactCallExpanded,
	} = useCompactCallExpansionState({
		storageKey: compactCallHeightKey,
		defaultExpanded: false,
	});
	useEffect(() => {
		if (!showCompactVoiceView || !currentChannelId) return;
		return ComponentDispatch.subscribe('COMPACT_VOICE_CALL_EXPANSION_TOGGLE', (payload?: unknown) => {
			const {channelId: targetChannelId} = (payload ?? {}) as {channelId?: string};
			if (targetChannelId && targetChannelId !== currentChannelId) return false;
			handleToggleCompactCallExpanded();
			return true;
		});
	}, [currentChannelId, handleToggleCompactCallExpanded, showCompactVoiceView]);
	const {
		voiceCallChromeRef,
		isVoiceCallChromePinned,
		setIsVoiceCallChromePinnedByHeader,
		setIsVoiceCallChromePinnedByStreamInfo,
	} = useVoiceCallChromePinState();
	const wasVoiceCallFullscreenViewOpenRef = useRef(false);
	const handleCloseCompactCallFullscreenView = useCallback(() => {
		setCompactCallExpanded(false);
		handleCloseVoiceCallFullscreenView();
	}, [handleCloseVoiceCallFullscreenView, setCompactCallExpanded]);
	useEffect(() => {
		if (wasVoiceCallFullscreenViewOpenRef.current && !showVoiceCallViewForFullscreen) {
			setCompactCallExpanded(false);
		}
		wasVoiceCallFullscreenViewOpenRef.current = showVoiceCallViewForFullscreen;
	}, [setCompactCallExpanded, showVoiceCallViewForFullscreen]);
	const handleOpenCallSheet = useCallback(() => setIsCallSheetOpen(true), []);
	const handleCloseCallSheet = useCallback(() => setIsCallSheetOpen(false), []);
	useEffect(() => {
		if (!callExistsAndOngoing) {
			setIsCallSheetOpen(false);
		}
	}, [callExistsAndOngoing]);
	useEffect(() => {
		logger.debug('voice connection state', {
			channelId,
			connected: mediaConnected,
			mediaChannelId,
			mediaGuildId,
			hasRoom: Boolean(room),
			showCompactVoiceView,
		});
	}, [channelId, mediaConnected, mediaChannelId, mediaGuildId, room, showCompactVoiceView]);
	useEffect(() => {
		logger.debug('compact voice view render decision', {
			channelId,
			showCompactVoiceView,
			roomName: room?.name ?? null,
		});
	}, [channelId, showCompactVoiceView, room]);
	const handleJoinCall = useCallback(() => {
		if (currentChannelId) CallCommands.joinCall(currentChannelId);
	}, [currentChannelId]);
	const handleRejectIncomingCall = useCallback(() => {
		if (currentChannelId) CallCommands.rejectCall(currentChannelId);
	}, [currentChannelId]);
	const handleIgnoreIncomingCall = useCallback(() => {
		if (currentChannelId) CallCommands.ignoreCall(currentChannelId);
	}, [currentChannelId]);
	const shouldRenderMemberList = Boolean(isGroupDM && isMemberListVisible && !isSearchActive);
	const callStatusLabel = useMemo(() => {
		switch (controlsVariant) {
			case 'incoming':
				return i18n._(INCOMING_CALL_DESCRIPTOR);
			case 'join':
				return i18n._(CALL_AVAILABLE_DESCRIPTOR);
			case 'connecting':
				return i18n._(CONNECTING_DESCRIPTOR);
			case 'inCall':
				return callHeaderState.isDeviceInRoomForChannelCall
					? i18n._(IN_CALL_DESCRIPTOR)
					: i18n._(IN_CALL_ON_ANOTHER_DEVICE_DESCRIPTOR);
			default:
				return i18n._(VOICE_CALL_DESCRIPTOR);
		}
	}, [callHeaderState.isDeviceInRoomForChannelCall, controlsVariant, i18n.locale]);
	const compactVoiceCallHeaderSupplement =
		channel && showCompactVoiceView ? (
			<CompactVoiceCallStreamHeaderInfo
				channel={channel}
				enabled={showCompactVoiceView}
				onOpenChange={setIsVoiceCallChromePinnedByStreamInfo}
				data-flx="channel.channel-view.dm-channel-view.compact-voice-call-header-supplement.compact-voice-call-stream-header-info"
			/>
		) : null;
	const callSheetButtonLabel = useMemo(() => {
		if (controlsVariant === 'incoming') return i18n._(VIEW_INCOMING_CALL_DESCRIPTOR);
		return i18n._(VIEW_CALL_DESCRIPTOR);
	}, [controlsVariant, i18n.locale]);
	const isCompactCallChatSuppressed = showCompactVoiceView && isCompactCallExpanded;
	if (!channel) {
		return (
			<div className={dmStyles.emptyState} data-flx="channel.channel-view.dm-channel-view.div">
				<ChatTeardropIcon
					weight="fill"
					className={dmStyles.emptyStateIcon}
					data-flx="channel.channel-view.dm-channel-view.chat-teardrop-icon"
				/>
				<h2 className={dmStyles.emptyStateTitle} data-flx="channel.channel-view.dm-channel-view.h2">
					<Trans>Conversation not found</Trans>
				</h2>
				<p className={dmStyles.emptyStateDescription} data-flx="channel.channel-view.dm-channel-view.p">
					<Trans>This conversation is no longer available.</Trans>
				</p>
			</div>
		);
	}
	if (isDM && !recipient) {
		return (
			<div className={dmStyles.emptyState} data-flx="channel.channel-view.dm-channel-view.div--2">
				<ChatTeardropIcon
					weight="fill"
					className={dmStyles.emptyStateIcon}
					data-flx="channel.channel-view.dm-channel-view.chat-teardrop-icon--2"
				/>
				<h2 className={dmStyles.emptyStateTitle} data-flx="channel.channel-view.dm-channel-view.h2--2">
					<Trans>User not found</Trans>
				</h2>
				<p className={dmStyles.emptyStateDescription} data-flx="channel.channel-view.dm-channel-view.p--2">
					<Trans>This user is no longer available.</Trans>
				</p>
			</div>
		);
	}
	if (showVoiceCallViewForFullscreen && callExistsAndOngoing) {
		return (
			<div
				className={styles.voiceChannelContainer}
				data-flx="channel.channel-view.dm-channel-view.voice-channel-container"
			>
				<VoiceCallView
					channel={channel}
					fullscreenRequestNonce={voiceCallFullscreenRequestNonce}
					onCloseFullscreenView={handleCloseCompactCallFullscreenView}
					data-flx="channel.channel-view.dm-channel-view.voice-call-view"
				/>
			</div>
		);
	}
	const mobileCallControls = (
		<CallControls
			mode="mobile"
			controlsVariant={controlsVariant}
			currentChannelId={currentChannelId}
			isDeviceInRoomForChannelCall={callHeaderState.isDeviceInRoomForChannelCall}
			onJoinCall={handleJoinCall}
			onRejectIncomingCall={handleRejectIncomingCall}
			onIgnoreIncomingCall={handleIgnoreIncomingCall}
			data-flx="channel.channel-view.dm-channel-view.call-controls"
		/>
	);
	const voiceControlBarCallControls = (
		<CallControls
			mode="voiceControlBar"
			controlsVariant={controlsVariant}
			currentChannelId={currentChannelId}
			isDeviceInRoomForChannelCall={callHeaderState.isDeviceInRoomForChannelCall}
			onJoinCall={handleJoinCall}
			onRejectIncomingCall={handleRejectIncomingCall}
			onIgnoreIncomingCall={handleIgnoreIncomingCall}
			data-flx="channel.channel-view.dm-channel-view.call-controls--2"
		/>
	);
	return (
		<>
			<ChannelViewScaffold
				className={clsx(
					showCallBackground && styles.channelGridVoiceCallActive,
					showCompactVoiceView && isCompactCallExpanded && styles.channelGridVoiceCallExpanded,
				)}
				header={
					<div
						ref={voiceCallChromeRef}
						className={clsx(
							showCallBackground && styles.voiceActiveHeaderWrapper,
							showCompactVoiceView && styles.voiceActiveHeaderWrapperOverlay,
							showCompactVoiceView && isCompactCallExpanded && styles.voiceActiveHeaderWrapperExpanded,
						)}
						data-voice-call-header-pinned={isVoiceCallChromePinned ? 'true' : undefined}
						data-flx="channel.channel-view.dm-channel-view.voice-active-header-wrapper"
					>
						<ChannelHeader
							channel={channel}
							showMembersToggle={Boolean(isGroupDM)}
							showPins={!isSystemDm}
							onSearchSubmit={handleSearchSubmit}
							onSearchClose={handleSearchClose}
							isSearchResultsOpen={isSearchActive}
							forceVoiceCallStyle={showCallBackground}
							memberListDefaultHiddenForChannel={memberListDefaultHiddenForChannel}
							voiceCallHeaderSupplement={compactVoiceCallHeaderSupplement}
							onVoiceCallChromePinChange={setIsVoiceCallChromePinnedByHeader}
							data-flx="channel.channel-view.dm-channel-view.channel-header"
						/>
						{callExistsAndOngoing &&
							call &&
							channel &&
							(isMobileExperience ? (
								<div className={dmStyles.callBannerMobile} data-flx="channel.channel-view.dm-channel-view.div--3">
									<div
										className={dmStyles.callBannerMobileLabel}
										data-flx="channel.channel-view.dm-channel-view.div--4"
									>
										{callStatusLabel}
									</div>
									{(controlsVariant !== 'inCall' || !callHeaderState.isDeviceInRoomForChannelCall) &&
										mobileCallControls && (
											<div
												className={dmStyles.callControlsMobile}
												data-flx="channel.channel-view.dm-channel-view.div--5"
											>
												{mobileCallControls}
											</div>
										)}
									<Button
										variant="secondary"
										onClick={handleOpenCallSheet}
										leftIcon={
											<PhoneIcon
												size={remFromPx(16)}
												weight="fill"
												data-flx="channel.channel-view.dm-channel-view.phone-icon"
											/>
										}
										data-flx="channel.channel-view.dm-channel-view.button.open-call-sheet"
									>
										{callSheetButtonLabel}
									</Button>
								</div>
							) : showCompactVoiceView ? (
								<ChannelCompactCallSurface
									channel={channel}
									isExpanded={isCompactCallExpanded}
									onToggleExpanded={handleToggleCompactCallExpanded}
									unreadCount={compactCallUnreadCount}
									avatarFallback={
										<CallParticipantsRow
											call={call}
											channel={channel}
											participantAvatarEntries={participantAvatarEntries}
											className={dmStyles.compactCallParticipantsRow}
											data-flx="channel.channel-view.dm-channel-view.call-participants-row"
										/>
									}
									onFullscreenRequest={handleOpenVoiceCallFullscreenView}
									reserveHeaderChrome={showCompactVoiceView}
									data-flx="channel.channel-view.dm-channel-view.channel-compact-call-surface"
								/>
							) : (
								<div
									className={clsx(
										styles.compactCallWrapper,
										isResizingCompactCallBanner && styles.compactCallWrapperResizing,
									)}
									style={compactCallBannerWrapperStyle}
									data-flx="channel.channel-view.dm-channel-view.compact-call-wrapper"
								>
									<div
										className={clsx(dmStyles.callBanner, isCompactCallResizable && dmStyles.callBannerResizable)}
										data-flx="channel.channel-view.dm-channel-view.div--6"
									>
										<div className={dmStyles.callBannerBody} data-flx="channel.channel-view.dm-channel-view.div--7">
											<CallParticipantsRow
												call={call}
												channel={channel}
												participantAvatarEntries={participantAvatarEntries}
												data-flx="channel.channel-view.dm-channel-view.call-participants-row--2"
											/>
										</div>
										{voiceControlBarCallControls && (
											<footer
												className={dmStyles.callControlBarSection}
												data-flx="channel.channel-view.dm-channel-view.footer"
											>
												<div
													className={dmStyles.callControlBarInner}
													data-flx="channel.channel-view.dm-channel-view.div--8"
												>
													<div className={dmStyles.callControls} data-flx="channel.channel-view.dm-channel-view.div--9">
														{voiceControlBarCallControls}
													</div>
													{currentChannelId && controlsVariant !== 'inCall' && (
														<VoiceE2EEIndicator
															guildId={null}
															channelId={currentChannelId}
															variant="call"
															data-flx="channel.channel-view.dm-channel-view.voice-e2-ee-indicator"
														/>
													)}
												</div>
											</footer>
										)}
									</div>
									{isCompactCallResizable && (
										<div
											className={dmStyles.compactCallResizeHandle}
											onPointerDown={handleCompactCallResizePointerDown}
											onKeyDown={handleCompactCallResizeKeyDown}
											role="separator"
											aria-orientation="horizontal"
											aria-label={i18n._(RESIZE_CALL_VIEW_DESCRIPTOR)}
											aria-valuemin={compactCallHeightMin}
											aria-valuemax={compactCallMaxHeight}
											aria-valuenow={compactCallBannerHeight ?? compactCallHeightMin}
											tabIndex={0}
											data-flx="channel.channel-view.dm-channel-view.separator.compact-call-resize-key-down"
										>
											<div
												className={dmStyles.compactCallResizePill}
												data-flx="channel.channel-view.dm-channel-view.div--10"
											/>
										</div>
									)}
								</div>
							))}
					</div>
				}
				chatArea={
					<ChannelChatLayout
						messages={
							<Messages
								key={channel.id}
								channel={channel}
								onBottomBarVisibilityChange={onBottomBarVisibilityChange}
								data-flx="channel.channel-view.dm-channel-view.messages"
							/>
						}
						textarea={
							isSystemDm ? (
								<SystemDmBarrier data-flx="channel.channel-view.dm-channel-view.system-dm-barrier" />
							) : isDM && isRecipientBlocked && recipient ? (
								<BlockedUserBarrier
									userId={recipient.id}
									username={NicknameUtils.getNickname(recipient)}
									data-flx="channel.channel-view.dm-channel-view.blocked-user-barrier"
								/>
							) : isCurrentUserUnclaimed && isDM && !isPersonalNotes && !isGroupDM ? (
								<UnclaimedDMBarrier data-flx="channel.channel-view.dm-channel-view.unclaimed-dm-barrier" />
							) : (
								<ChannelTextarea
									channel={channel}
									inputSuppressed={isCompactCallChatSuppressed}
									data-flx="channel.channel-view.dm-channel-view.channel-textarea"
								/>
							)
						}
						data-flx="channel.channel-view.dm-channel-view.channel-chat-layout"
					/>
				}
				sidePanel={
					isSearchPanelVisible ? (
						<div className={styles.searchPanel} data-flx="channel.channel-view.dm-channel-view.search-panel">
							<ChannelSearchResults
								channel={channel}
								searchQuery={activeSearchQuery}
								searchSegments={activeSearchSegments}
								refreshKey={searchRefreshKey}
								onClose={() => searchState.setIsSearchActive(false)}
								data-flx="channel.channel-view.dm-channel-view.channel-search-results"
							/>
						</div>
					) : shouldRenderMemberList ? (
						<ChannelMembers channel={channel} data-flx="channel.channel-view.dm-channel-view.channel-members" />
					) : null
				}
				chatAreaInert={isCompactCallChatSuppressed}
				data-flx="channel.channel-view.dm-channel-view.channel-grid-voice-call"
			/>
			{callExistsAndOngoing && call && channel && (
				<DirectCallLobbyBottomSheet
					isOpen={isCallSheetOpen}
					onClose={handleCloseCallSheet}
					channel={channel}
					data-flx="channel.channel-view.dm-channel-view.direct-call-lobby-bottom-sheet"
				/>
			)}
		</>
	);
});
