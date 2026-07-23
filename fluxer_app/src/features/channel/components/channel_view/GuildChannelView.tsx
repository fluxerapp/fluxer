// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	AccountTooNewBarrier,
	NoPhoneNumberBarrier,
	NotMemberLongEnoughBarrier,
	SendMessageDisabledBarrier,
	UnclaimedAccountBarrier,
	UnverifiedEmailBarrier,
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
import {useChannelSearchState} from '@app/features/channel/components/channel_view/useChannelSearchState';
import {useVoiceCallChromePinState} from '@app/features/channel/components/channel_view/useVoiceCallChromePinState';
import {MatureContentChannelGate} from '@app/features/channel/components/MatureContentChannelGate';
import {useMessagesBottomBarVisibility} from '@app/features/channel/components/MessagesBottomBarVisibility';
import {VerificationBarrier} from '@app/features/channel/components/VerificationBarrier';
import {useChannelMemberListVisibility} from '@app/features/channel/hooks/useChannelMemberListVisibility';
import {useChannelSearchVisibility} from '@app/features/channel/hooks/useChannelSearchVisibility';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import GuildMatureContentAgree, {MatureContentGateReason} from '@app/features/guild/state/GuildMatureContentAgree';
import Guilds from '@app/features/guild/state/Guilds';
import GuildVerification from '@app/features/guild/state/GuildVerification';
import {useMemberListVisible} from '@app/features/member/hooks/useMemberListVisible';
import Permission from '@app/features/permissions/state/Permission';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import {ThreadPanel} from '@app/features/channel/components/ThreadPanel';
import {ThreadCreationPanel} from '@app/features/channel/components/ThreadCreationPanel';
import ThreadCreation from '@app/features/channel/state/ThreadCreation';
import * as ThreadCommands from '@app/features/channel/commands/ThreadCommands';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {useParams} from '@app/features/platform/components/router/RouterReact';
import ReadStates from '@app/features/read_state/state/ReadStates';
import {Button} from '@app/features/ui/button/Button';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {isPwaOnMobileOrTablet} from '@app/features/ui/utils/PwaUtils';
import {CompactVoiceCallStreamHeaderInfo} from '@app/features/voice/components/CompactVoiceCallStreamHeaderInfo';
import {useVoiceCallFullscreenViewState} from '@app/features/voice/components/useVoiceCallAppFullscreen';
import {VoiceCallView} from '@app/features/voice/components/VoiceCallView';
import {VoiceE2EEIndicator} from '@app/features/voice/components/VoiceE2EEIndicator';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {isNativeVoiceEngineSelected} from '@app/features/voice/engine/native_voice_engine/getVoiceEngine';
import {useCompactCallExpansionState} from '@app/features/voice/hooks/useCompactCallExpansionState';
import {usePendingVoiceConnection} from '@app/features/voice/hooks/usePendingVoiceConnection';
import {getGuildVoiceCallExpansionKey} from '@app/features/voice/state/CompactVoiceCallHeight';
import {
	goBackFromMobileVoiceTextChatHistoryEntry,
	isCurrentMobileVoiceTextChatHistoryEntry,
	pushMobileVoiceTextChatHistoryEntry,
} from '@app/features/voice/utils/MobileVoiceTextChatHistory';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import Window from '@app/features/window/state/Window';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useLayoutEffect, useRef} from 'react';

const JOIN_VOICE_CHANNEL_DESCRIPTOR = msg({
	message: 'Join voice channel',
	comment: 'Button in the unjoined voice channel call surface. Joins the current voice channel.',
});
const NO_VOICE_CONNECT_PERMISSION_DESCRIPTOR = msg({
	message: "You don't have permission to join this voice channel",
	comment:
		'Tooltip on the disabled Join voice channel button shown when the user lacks the Connect permission in the voice channel.',
});

function isPortraitDisplay(windowSize: {width: number; height: number}): boolean {
	const orientationType = window.screen.orientation?.type;
	if (orientationType) return orientationType.startsWith('portrait');
	return windowSize.height >= windowSize.width;
}

interface GuildChannelViewProps {
	channelId: string;
	guildId?: string | null;
	messageId?: string;
}

const VoiceChannelJoinEmptyState = observer(function VoiceChannelJoinEmptyState({
	channel,
	onJoin,
	submitting,
}: {
	channel: Channel;
	onJoin: () => void;
	submitting: boolean;
}) {
	const {i18n} = useLingui();
	const channelName = channel.name ?? '';
	const joinLabel = i18n._(JOIN_VOICE_CHANNEL_DESCRIPTOR);
	const voiceLocked = !Permission.can(Permissions.CONNECT, channel);
	const joinButton = (
		<Button
			type="button"
			variant="primary"
			onClick={voiceLocked ? undefined : onJoin}
			disabled={voiceLocked}
			fitContent
			submitting={submitting}
			data-flx="channel.channel-view.guild-channel-view.voice-channel-join-empty-state.button.join"
		>
			{joinLabel}
		</Button>
	);
	return (
		<div
			className={styles.voiceJoinEmptyState}
			data-flx="channel.channel-view.guild-channel-view.voice-channel-join-empty-state.voice-join-empty-state"
		>
			<div
				className={styles.voiceJoinEmptyContent}
				data-flx="channel.channel-view.guild-channel-view.voice-channel-join-empty-state.voice-join-empty-content"
			>
				<div
					className={styles.voiceJoinEmptyMark}
					aria-hidden="true"
					data-flx="channel.channel-view.guild-channel-view.voice-channel-join-empty-state.voice-join-empty-mark"
				>
					{ChannelUtils.getIcon(
						channel,
						{className: styles.voiceJoinEmptyIcon},
						voiceLocked ? {locked: true} : undefined,
					)}
				</div>
				{channelName && (
					<h2
						className={styles.voiceJoinEmptyTitle}
						data-flx="channel.channel-view.guild-channel-view.voice-channel-join-empty-state.voice-join-empty-title"
					>
						{channelName}
					</h2>
				)}
				{voiceLocked ? (
					<Tooltip
						text={i18n._(NO_VOICE_CONNECT_PERMISSION_DESCRIPTOR)}
						data-flx="channel.channel-view.guild-channel-view.voice-channel-join-empty-state.tooltip.locked"
					>
						<span
							className={styles.voiceJoinDisabledWrap}
							data-flx="channel.channel-view.guild-channel-view.voice-channel-join-empty-state.voice-join-disabled-wrap"
						>
							{joinButton}
						</span>
					</Tooltip>
				) : (
					joinButton
				)}
				<VoiceE2EEIndicator
					guildId={channel.guildId ?? null}
					channelId={channel.id}
					variant="voice_channel"
					data-flx="channel.channel-view.guild-channel-view.voice-channel-join-empty-state.voice-e2-ee-indicator"
				/>
			</div>
		</div>
	);
});
export const GuildChannelView = observer(({channelId, guildId}: GuildChannelViewProps) => {
	const channel = Channels.getChannel(channelId);
	const guild = guildId ? Guilds.getGuild(guildId) : null;
	const {threadId} = (useParams() as {threadId?: string}) ?? {};
	const isVoiceChannel = channel?.type === ChannelTypes.GUILD_VOICE;

	useEffect(() => {
		if (threadId && ThreadCreation.pending) {
			ThreadCreation.close();
		}
		if (threadId && ThreadCreation.createdThreadId) {
			ThreadCreation.close();
		}
	}, [threadId]);

	useEffect(() => {
		if (!channel || isVoiceChannel || channel.type === ChannelTypes.GUILD_CATEGORY) return;
		void ThreadCommands.fetchList(channelId);
	}, [channelId, channel, isVoiceChannel]);
	const memberListDefaultHiddenForChannel = Boolean(isVoiceChannel);
	const isMemberListVisible = useMemberListVisible({
		channelId,
		defaultHiddenForChannel: memberListDefaultHiddenForChannel,
	});
	const {enabled: isMobileLayout} = MobileLayout;
	const windowSize = Window.windowSize;
	const isPwaVoiceTextSplitLayout = isMobileLayout && isPwaOnMobileOrTablet() && isPortraitDisplay(windowSize);
	const room = MediaEngine.room;
	const connectedChannelId = MediaEngine.channelId;
	const connectedGuildId = MediaEngine.guildId;
	const isConnectedToThisChannel = Boolean(
		isVoiceChannel &&
			connectedChannelId === channelId &&
			(connectedGuildId ?? null) === (channel?.guildId ?? null) &&
			(room || (isNativeVoiceEngineSelected() && MediaEngine.connected)),
	);
	const matureContentGateReason = GuildMatureContentAgree.getGateReason({channelId, guildId});
	const matureContentResolved = GuildMatureContentAgree.getResolvedContext({channelId, guildId});
	const showMatureContentGate = matureContentGateReason !== MatureContentGateReason.NONE;
	const forceMockMatureContentGate = DeveloperOptions.mockMatureContentGateReason !== 'none';
	const searchState = useChannelSearchState(channel);
	const {
		isSearchActive,
		handleSearchClose,
		handleSearchSubmit,
		searchRefreshKey,
		activeSearchQuery,
		activeSearchSegments,
	} = searchState;
	const isSearchPanelVisible = isSearchActive && !isMobileLayout;
	const {hasMessagesBottomBar, onBottomBarVisibilityChange} = useMessagesBottomBarVisibility(channelId);
	const {
		showFullscreenView: showVoiceCallFullscreenView,
		fullscreenRequestNonce: voiceCallFullscreenRequestNonce,
		openFullscreenView: handleOpenVoiceCallFullscreenView,
		closeFullscreenView: handleCloseVoiceCallFullscreenView,
	} = useVoiceCallFullscreenViewState({
		active: isConnectedToThisChannel,
		scopeKey: `guild-voice:${channelId}`,
	});
	const {isPending: isVoiceConnectionPending, startConnection: startVoiceConnection} = usePendingVoiceConnection({
		guildId: channel?.guildId,
		channelId: channel?.id,
	});
	const handleVoiceJoinButtonClick = useCallback(() => {
		startVoiceConnection({skipConfirm: true});
	}, [startVoiceConnection]);
	const voiceTextCallExpandedKey = getGuildVoiceCallExpansionKey(channelId);
	const voiceTextUnreadCount = ReadStates.getUnreadCount(channelId);
	const {isExpanded: isVoiceTextCallExpanded, setExpanded: setVoiceTextCallExpanded} = useCompactCallExpansionState({
		storageKey: voiceTextCallExpandedKey,
		defaultExpanded: true,
		persistByDefault: !isMobileLayout,
	});
	const {
		voiceCallChromeRef,
		isVoiceCallChromePinned,
		setIsVoiceCallChromePinnedByHeader,
		setIsVoiceCallChromePinnedByStreamInfo,
	} = useVoiceCallChromePinState();
	const wasVoiceCallFullscreenViewOpenRef = useRef(false);
	const setVoiceTextCallExpandedForLayout = useCallback(
		(nextExpanded: boolean) => {
			setVoiceTextCallExpanded(nextExpanded, {persist: !isMobileLayout});
		},
		[isMobileLayout, setVoiceTextCallExpanded],
	);
	const handleOpenVoiceTextChat = useCallback(() => {
		setVoiceTextCallExpandedForLayout(false);
		if (isMobileLayout && !isPwaVoiceTextSplitLayout) {
			pushMobileVoiceTextChatHistoryEntry(channelId);
		}
	}, [channelId, isMobileLayout, isPwaVoiceTextSplitLayout, setVoiceTextCallExpandedForLayout]);
	const handleOpenVoiceCallFromTextChat = useCallback(() => {
		if (isMobileLayout && !isPwaVoiceTextSplitLayout && goBackFromMobileVoiceTextChatHistoryEntry(channelId)) {
			return;
		}
		setVoiceTextCallExpandedForLayout(true);
	}, [channelId, isMobileLayout, isPwaVoiceTextSplitLayout, setVoiceTextCallExpandedForLayout]);
	const handleToggleVoiceTextCallExpanded = useCallback(() => {
		if (isVoiceTextCallExpanded) {
			handleOpenVoiceTextChat();
			return;
		}
		handleOpenVoiceCallFromTextChat();
	}, [handleOpenVoiceCallFromTextChat, handleOpenVoiceTextChat, isVoiceTextCallExpanded]);
	useEffect(() => {
		if (!isVoiceChannel) return;
		return ComponentDispatch.subscribe('COMPACT_VOICE_CALL_EXPANSION_TOGGLE', (payload?: unknown) => {
			const {channelId: targetChannelId} = (payload ?? {}) as {channelId?: string};
			if (targetChannelId && targetChannelId !== channelId) return false;
			handleToggleVoiceTextCallExpanded();
			return true;
		});
	}, [channelId, handleToggleVoiceTextCallExpanded, isVoiceChannel]);
	const handleCollapseVoiceTextCallFullscreen = useCallback(() => {
		handleCloseVoiceCallFullscreenView();
		handleOpenVoiceTextChat();
	}, [handleCloseVoiceCallFullscreenView, handleOpenVoiceTextChat]);
	useLayoutEffect(() => {
		if (!isVoiceChannel || !isMobileLayout) {
			return;
		}
		const shouldShowVoiceTextChat = isCurrentMobileVoiceTextChatHistoryEntry(channelId) || isPwaVoiceTextSplitLayout;
		setVoiceTextCallExpanded(!shouldShowVoiceTextChat, {persist: false});
	}, [channelId, isMobileLayout, isPwaVoiceTextSplitLayout, isVoiceChannel, setVoiceTextCallExpanded]);
	useEffect(() => {
		if (!isVoiceChannel || !isMobileLayout) {
			return;
		}
		const handlePopState = () => {
			if (!Channels.getChannel(channelId)) {
				return;
			}
			const shouldShowVoiceTextChat = isCurrentMobileVoiceTextChatHistoryEntry(channelId) || isPwaVoiceTextSplitLayout;
			setVoiceTextCallExpanded(!shouldShowVoiceTextChat, {persist: false});
		};
		window.addEventListener('popstate', handlePopState);
		return () => {
			window.removeEventListener('popstate', handlePopState);
		};
	}, [channelId, isMobileLayout, isPwaVoiceTextSplitLayout, isVoiceChannel, setVoiceTextCallExpanded]);
	useEffect(() => {
		if (wasVoiceCallFullscreenViewOpenRef.current && !showVoiceCallFullscreenView) {
			handleOpenVoiceTextChat();
		}
		wasVoiceCallFullscreenViewOpenRef.current = showVoiceCallFullscreenView;
	}, [handleOpenVoiceTextChat, showVoiceCallFullscreenView]);
	useChannelSearchVisibility(channelId, isSearchPanelVisible);
	useChannelMemberListVisibility(channelId, isMemberListVisible && !isMobileLayout);
	useEffect(() => {
		const handleGlobalKeydown = (event: Event) => {
			const keyboardEvent = event as KeyboardEvent;
			if (keyboardEvent.key === 'Escape' && isSearchActive) {
				searchState.setIsSearchActive(false);
			}
		};
		const options = {capture: true} as const;
		document.addEventListener('keydown', handleGlobalKeydown, options);
		return () => {
			document.removeEventListener('keydown', handleGlobalKeydown, options);
		};
	}, [isSearchActive, searchState]);
	const channelTitlePart = channel
		? `${channel.type === ChannelTypes.GUILD_VOICE ? '' : '#'}${channel.name ?? ''}`
		: null;
	const guildTitlePart = guild ? guild.name : null;
	useFluxerDocumentTitle(channel ? [channelTitlePart, guildTitlePart] : undefined);
	if (!(guild && channel)) {
		return null;
	}
	if (showMatureContentGate || forceMockMatureContentGate) {
		return (
			<div className={styles.channelGrid} data-flx="channel.channel-view.guild-channel-view.channel-grid">
				<ChannelHeader
					channel={channel}
					showMembersToggle={false}
					showPins={false}
					data-flx="channel.channel-view.guild-channel-view.channel-header"
				/>
				<MatureContentChannelGate
					channelId={channelId}
					guildId={guild.id}
					scope={matureContentResolved.scope}
					reason={matureContentGateReason}
					data-flx="channel.channel-view.guild-channel-view.mature-content-channel-gate"
				/>
			</div>
		);
	}
	const voiceJoinEmptyState = isVoiceChannel ? (
		<VoiceChannelJoinEmptyState
			channel={channel}
			onJoin={handleVoiceJoinButtonClick}
			submitting={isVoiceConnectionPending}
			data-flx="channel.channel-view.guild-channel-view.voice-channel-join-empty-state"
		/>
	) : null;
	const passesVerification = channel.isPrivate() || GuildVerification.canAccessGuild(channel.guildId || '');
	const renderChatArea = (inputSuppressed = false) => {
		if (DeveloperOptions.mockVerificationBarrier !== 'none' && !channel.isPrivate()) {
			switch (DeveloperOptions.mockVerificationBarrier) {
				case 'unclaimed_account':
					return (
						<UnclaimedAccountBarrier data-flx="channel.channel-view.guild-channel-view.render-chat-area.unclaimed-account-barrier" />
					);
				case 'unverified_email':
					return (
						<UnverifiedEmailBarrier data-flx="channel.channel-view.guild-channel-view.render-chat-area.unverified-email-barrier" />
					);
				case 'account_too_new':
					return (
						<AccountTooNewBarrier
							initialTimeRemaining={DeveloperOptions.mockBarrierTimeRemaining || 300000}
							data-flx="channel.channel-view.guild-channel-view.render-chat-area.account-too-new-barrier"
						/>
					);
				case 'not_member_long':
					return (
						<NotMemberLongEnoughBarrier
							initialTimeRemaining={DeveloperOptions.mockBarrierTimeRemaining || 600000}
							data-flx="channel.channel-view.guild-channel-view.render-chat-area.not-member-long-enough-barrier"
						/>
					);
				case 'no_phone':
					return (
						<NoPhoneNumberBarrier data-flx="channel.channel-view.guild-channel-view.render-chat-area.no-phone-number-barrier" />
					);
				case 'send_message_disabled':
					return (
						<SendMessageDisabledBarrier data-flx="channel.channel-view.guild-channel-view.render-chat-area.send-message-disabled-barrier" />
					);
				default:
					return passesVerification ? (
						<ChannelTextarea
							channel={channel}
							inputSuppressed={inputSuppressed}
							data-flx="channel.channel-view.guild-channel-view.render-chat-area.channel-textarea"
						/>
					) : (
						<VerificationBarrier
							channel={channel}
							data-flx="channel.channel-view.guild-channel-view.render-chat-area.verification-barrier"
						/>
					);
			}
		}
		return passesVerification ? (
			<ChannelTextarea
				channel={channel}
				inputSuppressed={inputSuppressed}
				data-flx="channel.channel-view.guild-channel-view.render-chat-area.channel-textarea--2"
			/>
		) : (
			<VerificationBarrier
				channel={channel}
				data-flx="channel.channel-view.guild-channel-view.render-chat-area.verification-barrier--2"
			/>
		);
	};
	if (isVoiceChannel) {
		const isVoiceTextSplitView = isPwaVoiceTextSplitLayout && !isVoiceTextCallExpanded;
		const shouldRenderMemberList = isMemberListVisible && !isMobileLayout && !isSearchActive;
		const compactVoiceCallHeaderSupplement = isConnectedToThisChannel ? (
			<CompactVoiceCallStreamHeaderInfo
				channel={channel}
				enabled={isConnectedToThisChannel}
				onOpenChange={setIsVoiceCallChromePinnedByStreamInfo}
				data-flx="channel.channel-view.guild-channel-view.compact-voice-call-header-supplement.compact-voice-call-stream-header-info"
			/>
		) : null;
		if (showVoiceCallFullscreenView) {
			return (
				<div
					className={clsx(styles.voiceChannelContainer, styles.voiceChannelFullscreenShell)}
					data-flx="channel.channel-view.guild-channel-view.voice-channel"
				>
					<VoiceCallView
						channel={channel}
						fullscreenRequestNonce={voiceCallFullscreenRequestNonce}
						onCloseFullscreenView={handleCollapseVoiceTextCallFullscreen}
						data-flx="channel.channel-view.guild-channel-view.voice-call-view"
					/>
				</div>
			);
		}
		return (
			<ChannelViewScaffold
				className={clsx(
					styles.channelGridVoiceCallActive,
					isVoiceTextCallExpanded && styles.channelGridVoiceCallExpanded,
				)}
				voiceTextSplitView={isVoiceTextSplitView}
				header={
					<div
						ref={voiceCallChromeRef}
						className={clsx(
							styles.voiceActiveHeaderWrapper,
							isConnectedToThisChannel && styles.voiceActiveHeaderWrapperOverlay,
							isVoiceTextCallExpanded && styles.voiceActiveHeaderWrapperExpanded,
						)}
						data-voice-call-header-pinned={isVoiceCallChromePinned ? 'true' : undefined}
						data-voice-text-split-view={isVoiceTextSplitView ? 'true' : undefined}
						data-flx="channel.channel-view.guild-channel-view.voice-active-header-wrapper"
					>
						<ChannelHeader
							channel={channel}
							showMembersToggle={true}
							showPins={true}
							onSearchSubmit={handleSearchSubmit}
							onSearchClose={handleSearchClose}
							isSearchResultsOpen={isSearchActive}
							forceVoiceCallStyle={true}
							memberListDefaultHiddenForChannel={memberListDefaultHiddenForChannel}
							onBackClick={isMobileLayout && !isVoiceTextCallExpanded ? handleOpenVoiceCallFromTextChat : undefined}
							voiceCallHeaderSupplement={compactVoiceCallHeaderSupplement}
							onVoiceCallChromePinChange={setIsVoiceCallChromePinnedByHeader}
							data-flx="channel.channel-view.guild-channel-view.channel-header--2"
						/>
						<ChannelCompactCallSurface
							channel={channel}
							isExpanded={isVoiceTextCallExpanded}
							onToggleExpanded={handleToggleVoiceTextCallExpanded}
							unreadCount={voiceTextUnreadCount}
							mediaMode={isConnectedToThisChannel ? 'live' : 'placeholder'}
							avatarFallback={!isConnectedToThisChannel ? voiceJoinEmptyState : undefined}
							avatarFallbackFullBleed={!isConnectedToThisChannel}
							audioOnly={!isConnectedToThisChannel}
							hideControlBar={!isConnectedToThisChannel}
							onFullscreenRequest={isConnectedToThisChannel ? handleOpenVoiceCallFullscreenView : undefined}
							reserveHeaderChrome={isConnectedToThisChannel}
							data-flx="channel.channel-view.guild-channel-view.channel-compact-call-surface--2"
						/>
					</div>
				}
				chatArea={
					<ChannelChatLayout
						channel={channel}
						messages={
							<Messages
								key={channel.id}
								channel={channel}
								allowAutoAck={!isVoiceTextCallExpanded}
								onBottomBarVisibilityChange={onBottomBarVisibilityChange}
								data-flx="channel.channel-view.guild-channel-view.messages"
							/>
						}
						textarea={renderChatArea(isVoiceTextCallExpanded)}
						hideBottomBar={hasMessagesBottomBar}
						data-flx="channel.channel-view.guild-channel-view.channel-chat-layout"
					/>
				}
				sidePanel={
					isSearchPanelVisible ? (
						<div className={styles.searchPanel} data-flx="channel.channel-view.guild-channel-view.search-panel">
							<ChannelSearchResults
								channel={channel}
								searchQuery={activeSearchQuery}
								searchSegments={activeSearchSegments}
								refreshKey={searchRefreshKey}
								onClose={() => searchState.setIsSearchActive(false)}
								data-flx="channel.channel-view.guild-channel-view.channel-search-results"
							/>
						</div>
					) : shouldRenderMemberList ? (
						<ChannelMembers
							channel={channel}
							guild={guild}
							data-flx="channel.channel-view.guild-channel-view.channel-members--2"
						/>
					) : null
				}
				showMemberListDivider={shouldRenderMemberList && !isSearchActive}
				chatAreaInert={isVoiceTextCallExpanded}
				data-flx="channel.channel-view.guild-channel-view.channel-grid-voice-call"
			/>
		);
	}
	const shouldRenderMemberList = isMemberListVisible && !isMobileLayout && !isSearchActive;
	return (
		<ChannelViewScaffold
			header={
				<ChannelHeader
					channel={channel}
					showMembersToggle={true}
					showPins={true}
					onSearchSubmit={handleSearchSubmit}
					onSearchClose={handleSearchClose}
					isSearchResultsOpen={isSearchActive}
					data-flx="channel.channel-view.guild-channel-view.channel-header--3"
				/>
			}
			chatArea={
				<ChannelChatLayout
					channel={channel}
					messages={
						<Messages
							key={channel.id}
							channel={channel}
							onBottomBarVisibilityChange={onBottomBarVisibilityChange}
							data-flx="channel.channel-view.guild-channel-view.messages--2"
						/>
					}
					textarea={renderChatArea()}
					hideBottomBar={hasMessagesBottomBar}
					data-flx="channel.channel-view.guild-channel-view.channel-chat-layout--2"
				/>
			}
			sidePanel={
				isSearchPanelVisible ? (
					<div className={styles.searchPanel} data-flx="channel.channel-view.guild-channel-view.search-panel--2">
						<ChannelSearchResults
							channel={channel}
							searchQuery={activeSearchQuery}
							searchSegments={activeSearchSegments}
							refreshKey={searchRefreshKey}
							onClose={() => searchState.setIsSearchActive(false)}
							data-flx="channel.channel-view.guild-channel-view.channel-search-results--2"
						/>
					</div>
				) : ThreadCreation.pending?.channelId === channelId ? (
					<ThreadCreationPanel
						channelId={channelId}
						guildId={guildId ?? ''}
						sourceMessageId={ThreadCreation.pending.sourceMessageId}
						sourceMessagePreview={ThreadCreation.pending.sourceMessagePreview}
						sourceMessageAuthor={ThreadCreation.pending.sourceMessageAuthor}
						onClose={() => ThreadCreation.close()}
						data-flx="channel.channel-view.guild-channel-view.thread-creation-panel"
					/>
				) : (ThreadCreation.createdThreadId ?? threadId) ? (
					<ThreadPanel
						threadId={(ThreadCreation.createdThreadId ?? threadId)!}
						onClose={() => {
							ThreadCreation.close();
							if (guildId && channel.id) {
								NavigationCommands.closeThread(guildId, channel.id);
							}
						}}
						data-flx="channel.channel-view.guild-channel-view.thread-panel"
					/>
				) : shouldRenderMemberList ? (
					<ChannelMembers
						channel={channel}
						guild={guild}
						data-flx="channel.channel-view.guild-channel-view.channel-members"
					/>
				) : null
			}
			showMemberListDivider={!threadId && !ThreadCreation.pending && !ThreadCreation.createdThreadId && shouldRenderMemberList && !isSearchActive}
			data-flx="channel.channel-view.guild-channel-view.channel-view-scaffold"
		/>
	);
});
