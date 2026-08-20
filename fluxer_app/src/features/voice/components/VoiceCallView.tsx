// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {NativeDragRegion} from '@app/features/app/components/layout/NativeDragRegion';
import channelHeaderStyles from '@app/features/channel/components/ChannelHeader.module.css';
import {ChannelHeaderIcon} from '@app/features/channel/components/channel_header_components/ChannelHeaderIcon';
import {InboxButton, StaffToolsButton} from '@app/features/channel/components/channel_header_components/UtilityButtons';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {
	ADD_TO_FAVORITES_DESCRIPTOR,
	ADDED_TO_FAVORITES_TOAST_DESCRIPTOR,
	REMOVE_FROM_FAVORITES_DESCRIPTOR,
	REMOVED_FROM_FAVORITES_TOAST_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Favorites from '@app/features/messaging/state/Favorites';
import {goBackOr} from '@app/features/platform/components/router/NavigationAdapter';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {usePopout} from '@app/features/ui/hooks/usePopout';
import {PortalHostContext, setActivePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import {Popout as PopoverPopout} from '@app/features/ui/popover/PopoverPopout';
import ContextMenu, {isContextMenuNodeTarget} from '@app/features/ui/state/ContextMenu';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import Popout from '@app/features/ui/state/Popout';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {CompactVoiceCallView} from '@app/features/voice/components/CompactVoiceCallView';
import {MediaVerticalVolumeControl} from '@app/features/voice/components/media_player/components/MediaVerticalVolumeControl';
import {PoppedOutOverlay} from '@app/features/voice/components/popout/PoppedOutOverlay';
import {
	selectPoppedOutOverlayTransition,
	shouldRenderPoppedOutOverlay,
} from '@app/features/voice/components/popout/PoppedOutSurfaceStateMachine';
import {usePoppedOutTransition} from '@app/features/voice/components/popout/usePoppedOutTransition';
import {StreamFocusHeaderInfo} from '@app/features/voice/components/StreamFocusHeaderInfo';
import {getStreamKey} from '@app/features/voice/components/StreamKeys';
import {useStreamSpectators} from '@app/features/voice/components/useStreamSpectators';
import {useStreamTrackInfo} from '@app/features/voice/components/useStreamTrackInfo';
import {useVoiceCallAppFullscreen} from '@app/features/voice/components/useVoiceCallAppFullscreen';
import {useVoiceCallTracksAndLayout} from '@app/features/voice/components/useVoiceCallTracksAndLayout';
import {useVoiceEngineConnectionState} from '@app/features/voice/components/useVoiceEngineConnectionState';
import {VoiceCallCornerControls} from '@app/features/voice/components/VoiceCallCornerControls';
import {VoiceCallLayoutContent} from '@app/features/voice/components/VoiceCallLayoutContent';
import styles from '@app/features/voice/components/VoiceCallView.module.css';
import {VoiceControlBar} from '@app/features/voice/components/VoiceControlBar';
import {
	useVoiceParticipantAvatarEntries,
	VoiceParticipantWrappedAvatarList,
} from '@app/features/voice/components/VoiceParticipantAvatarList';
import {VoiceRegionTeleportOverlay} from '@app/features/voice/components/VoiceRegionTeleportOverlay';
import {VoiceDetailsPopout} from '@app/features/voice/components/voice_connection_status/VoiceDetailsPopout';
import {VoiceDebugStatsForwarder} from '@app/features/voice/diagnostics/VoiceDebugStatsForwarder';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import {
	asVoiceEngineConnectionState,
	VoiceEngineConnectionState,
} from '@app/features/voice/engine/VoiceConnectionStateMachine';
import {asVoiceTrackSource, VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import PopoutWindowManager, {
	getVoiceCallPopoutKey,
	isVoicePopoutSupported,
} from '@app/features/voice/state/PopoutWindowManager';
import StreamAudioPrefs from '@app/features/voice/state/StreamAudioPrefs';
import VoiceCallLayout from '@app/features/voice/state/VoiceCallLayout';
import {hasValidRoomForVoiceCallContext} from '@app/features/voice/utils/VoiceCallContext';
import {VOICE_CALL_DESCRIPTOR} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {parseVoiceParticipantIdentity} from '@app/features/voice/utils/VoiceParticipantIdentity';
import {VOICE_VOLUME_MAX_SLIDER_VOLUME} from '@app/features/voice/utils/VoiceVolumeUtils';
import {ME} from '@fluxer/constants/src/AppConstants';
import {msg, plural} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {
	ArrowLeftIcon,
	ChartBarIcon,
	CornersInIcon,
	CornersOutIcon,
	ListIcon,
	PhoneIcon,
	StarIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {forwardRef, useCallback, useContext, useEffect, useMemo, useRef, useState} from 'react';

const CONNECTING_DESCRIPTOR = msg({
	message: 'Connecting...',
	comment: 'Overlay status in the voice call view while joining. Trailing ellipsis indicates in-progress.',
});
const RECONNECTING_DESCRIPTOR = msg({
	message: 'Reconnecting...',
	comment:
		'Overlay status in the voice call view while reconnecting after a drop. Trailing ellipsis indicates in-progress.',
});
const DISCONNECTED_DESCRIPTOR = msg({
	message: 'Disconnected',
	comment: 'Overlay status in the voice call view when not connected.',
});
const EXIT_FULLSCREEN_DESCRIPTOR = msg({
	message: 'Exit fullscreen',
	comment: 'Tooltip / aria label on the fullscreen toggle in the voice call view (currently in fullscreen).',
});
const ENTER_FULLSCREEN_DESCRIPTOR = msg({
	message: 'Enter fullscreen',
	comment: 'Tooltip / aria label on the fullscreen toggle in the voice call view (not in fullscreen).',
});
const BACK_TO_VOICE_CHANNEL_DESCRIPTOR = msg({
	message: 'Back to voice channel',
	comment: 'Tooltip / aria label on the back button that returns from the call view to the voice channel view.',
});
const CONNECTION_STATS_DESCRIPTOR = msg({
	message: 'Connection stats',
	comment:
		'Tooltip / aria label on the connection-stats button in the voice call header. Opens the voice connection status popout.',
});
const VIEW_CALL_CONTROLS_DESCRIPTOR = msg({
	message: 'View call controls',
	comment: 'Aria label for the show-controls trigger when the call toolbar is auto-hidden.',
});
const VOICE_CALL_STATS_POPOUT_ID = 'voice-call-view-stats-popout';
const VOICE_HUD_IDLE_TIMEOUT_MS = 2500;
const VOICE_CHROME_PIN_RELEASE_DELAY_MS = 320;

interface VoiceCallViewProps {
	channel: Channel;
	fullscreenRequestNonce?: number;
	onCloseFullscreenView?: () => void;
	inPopout?: boolean;
}

function useConnectionStateText(connectionState: unknown) {
	const {i18n} = useLingui();
	return useMemo(() => {
		const normalizedConnectionState = asVoiceEngineConnectionState(connectionState);
		switch (normalizedConnectionState) {
			case VoiceEngineConnectionState.Connecting:
				return i18n._(CONNECTING_DESCRIPTOR);
			case VoiceEngineConnectionState.Reconnecting:
			case VoiceEngineConnectionState.SignalReconnecting:
				return i18n._(RECONNECTING_DESCRIPTOR);
			case VoiceEngineConnectionState.Disconnected:
				return i18n._(DISCONNECTED_DESCRIPTOR);
			default:
				return null;
		}
	}, [connectionState, i18n.locale]);
}

const VoiceCallViewInner = observer(
	({
		channel,
		fullscreenRequestNonce,
		onCloseFullscreenView,
		inPopout = false,
	}: {
		channel: Channel;
		fullscreenRequestNonce?: number;
		onCloseFullscreenView?: () => void;
		inPopout?: boolean;
	}) => {
		const {i18n} = useLingui();
		useMediaEngineVersion();
		const containerRef = useRef<HTMLDivElement>(null);
		const [portalHost, setPortalHost] = useState<HTMLElement | null>(null);
		const setPortalHostRef = useCallback((node: HTMLDivElement | null) => {
			setPortalHost(node);
		}, []);
		const hudPointerTimeoutRef = useRef<number | null>(null);
		const previousFullscreenRequestNonceRef = useRef<number | undefined>(undefined);
		const isMobile = MobileLayout.isMobileLayout();
		const {keyboardModeEnabled} = KeyboardMode;
		const [isMobileStatsSheetOpen, setIsMobileStatsSheetOpen] = useState(false);
		const {isOpen: isStatsPopoutOpen, openProps: statsPopoutProps} = usePopout(VOICE_CALL_STATS_POPOUT_ID);
		const isStatsOpen = isMobile ? isMobileStatsSheetOpen : isStatsPopoutOpen;
		const [isCallSheetOpen, setIsCallSheetOpen] = useState(false);
		const [isSpectatorsPopoutOpen, setIsSpectatorsPopoutOpen] = useState(false);
		const {
			isFullscreen: isVoiceCallAppFullscreen,
			supportsFullscreen: supportsVoiceCallAppFullscreen,
			enterFullscreen: enterVoiceCallAppFullscreen,
			exitFullscreen: exitVoiceCallAppFullscreen,
			toggleFullscreen: toggleVoiceCallAppFullscreen,
		} = useVoiceCallAppFullscreen({containerRef});
		const [isPointerHudActive, setIsPointerHudActive] = useState(false);
		const connectionState = useVoiceEngineConnectionState();
		const normalizedConnectionState = asVoiceEngineConnectionState(connectionState);
		const connectionStateText = useConnectionStateText(connectionState);
		const isInboxPopoutOpen = Popout.isOpen('inbox');
		const isFavorited = channel ? Boolean(Favorites.getChannel(channel.id)) : false;
		const isAnyContextMenuOpen = useMemo(() => {
			const cm = ContextMenu.contextMenu;
			const target = cm?.target?.target ?? null;
			const container = containerRef.current;
			if (!cm || !container || !isContextMenuNodeTarget(target)) return false;
			return Boolean(container.contains(target));
		}, [ContextMenu.contextMenu]);
		const isPoppedOut = !inPopout && PopoutWindowManager.isCallPopoutOpenForChannel(channel.id);
		const poppedOutTransition = usePoppedOutTransition(isPoppedOut);
		const wantsChromePinned =
			inPopout || isAnyContextMenuOpen || isInboxPopoutOpen || isStatsOpen || isSpectatorsPopoutOpen;
		const [isChromePinned, setIsChromePinned] = useState(wantsChromePinned);
		const [expandedUserIds, setExpandedUserIds] = useState<Set<string>>(() => new Set());
		const handleExpandUser = useCallback((userId: string) => {
			setExpandedUserIds((previous) => {
				const next = new Set(previous);
				if (next.has(userId)) next.delete(userId);
				else next.add(userId);
				return next;
			});
		}, []);
		const {
			layoutMode,
			pinnedParticipantIdentity,
			hasScreenShare,
			filteredCameraTracks,
			gridEntries,
			focusMainTrack,
			carouselTracks,
			pipTrack,
			participantCount,
		} = useVoiceCallTracksAndLayout({channel, expandedUserIds});
		const participantAvatarEntries = useVoiceParticipantAvatarEntries({
			guildId: channel.guildId ?? null,
			channelId: channel.id,
		});
		const hasRenderableTiles = hasScreenShare || filteredCameraTracks.length > 0;
		const [gridOverflow, setGridOverflow] = useState(false);
		const handleGridCapacityChange = useCallback(
			(info: {visibleTileCount: number; totalTileCount: number; overflow: boolean}) => {
				setGridOverflow(info.overflow);
			},
			[],
		);
		useEffect(() => {
			if (layoutMode !== 'grid') setGridOverflow(false);
		}, [layoutMode]);
		const effectiveLayoutMode = isMobile || (layoutMode === 'grid' && gridOverflow) ? 'focus' : layoutMode;
		const effectiveFocusMainTrack =
			effectiveLayoutMode === 'focus' ? (focusMainTrack ?? (isMobile ? pipTrack : null)) : null;
		const effectiveSecondaryFocusTrackCount =
			effectiveFocusMainTrack == null
				? carouselTracks.length
				: carouselTracks.filter(
						(trackRef) =>
							trackRef.participant.identity !== effectiveFocusMainTrack.participant.identity ||
							trackRef.source !== effectiveFocusMainTrack.source,
					).length;
		const mobileFocusMembersVisible =
			isMobile &&
			effectiveLayoutMode === 'focus' &&
			VoiceCallLayout.focusMembersRowVisible &&
			effectiveSecondaryFocusTrackCount > 0;
		const isFocusFullscreenLayout = effectiveLayoutMode === 'focus';
		const mainContentClassName = clsx(
			styles.mainContent,
			effectiveLayoutMode === 'grid' && styles.mainContentGrid,
			isFocusFullscreenLayout && styles.mainContentFocusFullscreen,
		);
		const isFocusedOnScreenShare =
			effectiveLayoutMode === 'focus' &&
			asVoiceTrackSource(effectiveFocusMainTrack?.source) === VoiceTrackSource.ScreenShare;
		const focusedStreamInfo = useMemo(() => {
			if (!isFocusedOnScreenShare || !effectiveFocusMainTrack) return null;
			const parsedIdentity = parseVoiceParticipantIdentity(effectiveFocusMainTrack.participant.identity);
			if (!parsedIdentity.userId || !parsedIdentity.connectionId) return null;
			return {userId: parsedIdentity.userId, connectionId: parsedIdentity.connectionId};
		}, [isFocusedOnScreenShare, effectiveFocusMainTrack]);
		const focusedStreamerUser = focusedStreamInfo ? Users.getUser(focusedStreamInfo.userId) : null;
		const focusedStreamKey = useMemo(() => {
			if (!focusedStreamInfo) return '';
			return getStreamKey(channel.guildId, channel.id, focusedStreamInfo.connectionId);
		}, [focusedStreamInfo, channel.guildId, channel.id]);
		const focusedStreamTrackInfo = useStreamTrackInfo(isFocusedOnScreenShare ? effectiveFocusMainTrack : null);
		const focusedStreamerDisplayName = useMemo(() => {
			if (!focusedStreamerUser) return '';
			return NicknameUtils.getNickname(focusedStreamerUser, channel.guildId, channel.id);
		}, [focusedStreamerUser, channel.guildId, channel.id]);
		const {viewerUsers: spectatorUsers, spectatorEntries} = useStreamSpectators(
			focusedStreamKey,
			focusedStreamInfo?.userId,
		);
		const focusedStreamVolume = StreamAudioPrefs.getVolume(focusedStreamKey);
		const isFocusedStreamMuted = StreamAudioPrefs.isMuted(focusedStreamKey);
		const handleFocusedStreamToggleMute = useCallback(() => {
			if (!focusedStreamKey || !focusedStreamInfo) return;
			StreamAudioPrefs.setMuted(focusedStreamKey, !isFocusedStreamMuted);
			MediaEngine.applyLocalAudioPreferencesForUser(focusedStreamInfo.userId);
		}, [focusedStreamKey, isFocusedStreamMuted, focusedStreamInfo]);
		const handleFocusedStreamVolumeChange = useCallback(
			(newVolume: number) => {
				if (!focusedStreamKey || !focusedStreamInfo) return;
				StreamAudioPrefs.setVolume(focusedStreamKey, Math.round(newVolume * 100));
				MediaEngine.applyLocalAudioPreferencesForUser(focusedStreamInfo.userId);
			},
			[focusedStreamKey, focusedStreamInfo],
		);
		const handleSpectatorsPopoutOpenChange = useCallback((open: boolean) => {
			setIsSpectatorsPopoutOpen(open);
		}, []);
		const handleOpenMobileStatsSheet = useCallback(() => setIsMobileStatsSheetOpen(true), []);
		const handleCloseMobileStatsSheet = useCallback(() => setIsMobileStatsSheetOpen(false), []);
		useEffect(() => {
			if (!isMobile && isMobileStatsSheetOpen) {
				setIsMobileStatsSheetOpen(false);
			}
		}, [isMobile, isMobileStatsSheetOpen]);
		useEffect(() => {
			if (!isMobile && isCallSheetOpen) {
				setIsCallSheetOpen(false);
			}
		}, [isCallSheetOpen, isMobile]);
		const handleBackClick = useCallback(() => {
			if (onCloseFullscreenView) {
				onCloseFullscreenView();
				return;
			}
			goBackOr('/');
		}, [onCloseFullscreenView]);
		const clearHudPointerTimeout = useCallback(() => {
			if (hudPointerTimeoutRef.current == null) return;
			window.clearTimeout(hudPointerTimeoutRef.current);
			hudPointerTimeoutRef.current = null;
		}, []);
		const scheduleHudIdleState = useCallback(() => {
			clearHudPointerTimeout();
			hudPointerTimeoutRef.current = window.setTimeout(() => {
				hudPointerTimeoutRef.current = null;
				setIsPointerHudActive(false);
			}, VOICE_HUD_IDLE_TIMEOUT_MS);
		}, [clearHudPointerTimeout]);
		useEffect(() => {
			if (wantsChromePinned) {
				setIsChromePinned(true);
				return;
			}
			const timeout = window.setTimeout(() => {
				if (containerRef.current?.matches(':hover')) {
					setIsPointerHudActive(true);
					scheduleHudIdleState();
				}
				setIsChromePinned(false);
			}, VOICE_CHROME_PIN_RELEASE_DELAY_MS);
			return () => window.clearTimeout(timeout);
		}, [scheduleHudIdleState, wantsChromePinned]);
		const handleVoiceRootPointerActivity = useCallback(
			(event: React.PointerEvent<HTMLDivElement>) => {
				if (event.pointerType === 'touch') return;
				setIsPointerHudActive(true);
				scheduleHudIdleState();
			},
			[scheduleHudIdleState],
		);
		const handleVoiceRootPointerLeave = useCallback(
			(event: React.PointerEvent<HTMLDivElement>) => {
				if (event.pointerType === 'touch') return;
				clearHudPointerTimeout();
				setIsPointerHudActive(false);
			},
			[clearHudPointerTimeout],
		);
		useEffect(() => {
			return () => {
				clearHudPointerTimeout();
			};
		}, [clearHudPointerTimeout]);
		const handleToggleFavorite = useCallback(() => {
			if (!channel) return;
			if (isFavorited) {
				Favorites.removeChannel(channel.id);
				ToastCommands.createToast({type: 'success', children: i18n._(REMOVED_FROM_FAVORITES_TOAST_DESCRIPTOR)});
				return;
			}
			Favorites.addChannel(channel.id, channel.guildId ?? ME);
			ToastCommands.createToast({type: 'success', children: i18n._(ADDED_TO_FAVORITES_TOAST_DESCRIPTOR)});
		}, [channel, isFavorited]);
		const handleOpenCallSheet = useCallback(() => setIsCallSheetOpen(true), []);
		const handleCloseCallSheet = useCallback(() => setIsCallSheetOpen(false), []);
		const handleToggleVoiceCallAppFullscreen = useCallback(() => {
			void toggleVoiceCallAppFullscreen();
		}, [toggleVoiceCallAppFullscreen]);
		const handlePopOutCall = useCallback(() => {
			void (async () => {
				if (isVoiceCallAppFullscreen) {
					await exitVoiceCallAppFullscreen();
				}
				PopoutWindowManager.openCallPopout({
					channelId: channel.id,
					guildId: channel.guildId ?? null,
					title: channel.name ?? i18n._(VOICE_CALL_DESCRIPTOR),
				});
			})();
		}, [channel.guildId, channel.id, channel.name, exitVoiceCallAppFullscreen, i18n, isVoiceCallAppFullscreen]);
		const fullscreenButtonLabel = isVoiceCallAppFullscreen
			? i18n._(EXIT_FULLSCREEN_DESCRIPTOR)
			: i18n._(ENTER_FULLSCREEN_DESCRIPTOR);
		const FullscreenButtonIcon = useMemo(() => {
			const BaseIcon = isVoiceCallAppFullscreen ? CornersInIcon : CornersOutIcon;
			const BoldIcon = forwardRef<SVGSVGElement, React.ComponentProps<typeof BaseIcon>>((props, ref) => (
				<BaseIcon ref={ref} weight="bold" data-flx="voice.voice-call-view.bold-icon.base-icon" {...props} />
			));
			BoldIcon.displayName = 'FullscreenButtonIcon';
			return BoldIcon;
		}, [isVoiceCallAppFullscreen]);
		useEffect(() => {
			if (fullscreenRequestNonce == null) return;
			if (previousFullscreenRequestNonceRef.current === fullscreenRequestNonce) return;
			previousFullscreenRequestNonceRef.current = fullscreenRequestNonce;
			void enterVoiceCallAppFullscreen();
		}, [enterVoiceCallAppFullscreen, fullscreenRequestNonce]);
		const inheritedPortalHost = useContext(PortalHostContext);
		const effectivePortalHost = isVoiceCallAppFullscreen || inPopout ? portalHost : null;
		useEffect(() => {
			if (!isVoiceCallAppFullscreen) return;
			if (!effectivePortalHost) return;
			setActivePortalHost(effectivePortalHost);
			return () => {
				setActivePortalHost(null);
			};
		}, [isVoiceCallAppFullscreen, effectivePortalHost]);
		const FavoriteIcon = useMemo(() => {
			const Icon = forwardRef<SVGSVGElement, React.ComponentProps<typeof StarIcon>>((props, ref) => (
				<StarIcon
					ref={ref}
					weight={isFavorited ? 'fill' : 'bold'}
					data-flx="voice.voice-call-view.icon.star-icon"
					{...props}
				/>
			));
			Icon.displayName = 'FavoriteIcon';
			return Icon;
		}, [isFavorited]);
		const statsButton = isMobile ? (
			<ChannelHeaderIcon
				icon={ChartBarIcon}
				label={i18n._(CONNECTION_STATS_DESCRIPTOR)}
				className={styles.voiceHeaderIconButton}
				isSelected={isStatsOpen}
				onClick={handleOpenMobileStatsSheet}
				aria-expanded={isStatsOpen}
				aria-haspopup="dialog"
				data-flx="voice.voice-call-view.voice-call-view-inner.channel-header-icon.stats-mobile"
			/>
		) : (
			<PopoverPopout
				{...statsPopoutProps}
				position="bottom-end"
				offsetMainAxis={8}
				render={({onClose}) => (
					<VoiceDetailsPopout
						onClose={onClose}
						data-flx="voice.voice-call-view.voice-call-view-inner.voice-details-popout"
					/>
				)}
				data-flx="voice.voice-call-view.voice-call-view-inner.popover-popout.stats"
			>
				<ChannelHeaderIcon
					icon={ChartBarIcon}
					label={i18n._(CONNECTION_STATS_DESCRIPTOR)}
					className={styles.voiceHeaderIconButton}
					isSelected={isStatsPopoutOpen}
					data-flx="voice.voice-call-view.voice-call-view-inner.channel-header-icon.stats-desktop"
				/>
			</PopoverPopout>
		);
		return (
			<PortalHostContext.Provider value={effectivePortalHost ?? inheritedPortalHost}>
				<div
					ref={containerRef}
					data-voice-call-root
					data-voice-call-popped-out={isPoppedOut ? 'true' : undefined}
					className={clsx(
						styles.root,
						styles.voiceRoot,
						isFocusFullscreenLayout && styles.rootFocused,
						isVoiceCallAppFullscreen && styles.voiceCallFullscreen,
						mobileFocusMembersVisible && styles.mobileFocusMembersVisible,
						isPointerHudActive && styles.pointerActive,
						isChromePinned && styles.contextMenuActive,
						keyboardModeEnabled && styles.keyboardModeActive,
					)}
					onPointerEnter={handleVoiceRootPointerActivity}
					onPointerMove={handleVoiceRootPointerActivity}
					onPointerDown={handleVoiceRootPointerActivity}
					onPointerLeave={handleVoiceRootPointerLeave}
					data-flx="voice.voice-call-view.voice-call-view-inner.root"
				>
					<VoiceDebugStatsForwarder data-flx="voice.voice-call-view.voice-call-view-inner.voice-debug-stats-forwarder" />
					<output
						className={styles.srOnly}
						aria-live="polite"
						aria-atomic="true"
						data-flx="voice.voice-call-view.voice-call-view-inner.sr-only"
					>
						{plural(
							{count: participantCount},
							{
								one: '# participant in call',
								other: '# participants in call',
							},
						)}
					</output>
					<NativeDragRegion
						className={clsx(
							channelHeaderStyles.headerContainer,
							styles.voiceChrome,
							styles.voiceHeader,
							styles.voiceEdgeFadeTop,
						)}
						data-flx="voice.voice-call-view.voice-call-view-inner.voice-chrome"
					>
						<div
							className={channelHeaderStyles.headerLeftSection}
							data-flx="voice.voice-call-view.voice-call-view-inner.div"
						>
							{isMobile ? (
								<FocusRing offset={-2} data-flx="voice.voice-call-view.voice-call-view-inner.focus-ring">
									<button
										type="button"
										className={clsx(channelHeaderStyles.backButton, styles.voiceHeaderBackButton)}
										onClick={handleBackClick}
										aria-label={i18n._(BACK_TO_VOICE_CHANNEL_DESCRIPTOR)}
										data-flx="voice.voice-call-view.voice-call-view-inner.button.back-click"
									>
										<ArrowLeftIcon
											className={channelHeaderStyles.backIconBold}
											weight="bold"
											data-flx="voice.voice-call-view.voice-call-view-inner.arrow-left-icon"
										/>
									</button>
								</FocusRing>
							) : (
								<FocusRing offset={-2} data-flx="voice.voice-call-view.voice-call-view-inner.focus-ring--2">
									<button
										type="button"
										className={clsx(channelHeaderStyles.backButtonDesktop, styles.voiceHeaderBackButton)}
										onClick={handleBackClick}
										aria-label={i18n._(BACK_TO_VOICE_CHANNEL_DESCRIPTOR)}
										data-flx="voice.voice-call-view.voice-call-view-inner.button.back-click--2"
									>
										<ListIcon
											className={channelHeaderStyles.backIcon}
											data-flx="voice.voice-call-view.voice-call-view-inner.list-icon"
										/>
									</button>
								</FocusRing>
							)}
							<div
								className={channelHeaderStyles.leftContentContainer}
								data-flx="voice.voice-call-view.voice-call-view-inner.div--2"
							>
								<div
									className={channelHeaderStyles.channelInfoContainer}
									data-flx="voice.voice-call-view.voice-call-view-inner.div--3"
								>
									{ChannelUtils.getIcon(channel, {className: channelHeaderStyles.channelIcon})}
									<span
										className={channelHeaderStyles.channelName}
										data-flx="voice.voice-call-view.voice-call-view-inner.span"
									>
										{channel.name ?? ''}
									</span>
								</div>
							</div>
						</div>
						<div
							className={channelHeaderStyles.headerRightSection}
							data-flx="voice.voice-call-view.voice-call-view-inner.div--4"
						>
							{isFocusedOnScreenShare && focusedStreamerUser && (
								<div
									className={channelHeaderStyles.voiceCallHeaderSupplement}
									data-flx="voice.voice-call-view.voice-call-view-inner.stream-focus-header-info-wrapper"
								>
									<StreamFocusHeaderInfo
										streamerUser={focusedStreamerUser}
										streamerDisplayName={focusedStreamerDisplayName}
										viewerUsers={spectatorUsers}
										spectatorEntries={spectatorEntries}
										trackInfo={focusedStreamTrackInfo}
										guildId={channel.guildId ?? undefined}
										channelId={channel.id}
										onOpenChange={handleSpectatorsPopoutOpenChange}
										data-flx="voice.voice-call-view.voice-call-view-inner.stream-focus-header-info"
									/>
								</div>
							)}
							{channel && !isMobile && Accessibility.showFavorites && (
								<ChannelHeaderIcon
									icon={FavoriteIcon}
									label={isFavorited ? i18n._(REMOVE_FROM_FAVORITES_DESCRIPTOR) : i18n._(ADD_TO_FAVORITES_DESCRIPTOR)}
									className={styles.voiceHeaderIconButton}
									isSelected={isFavorited}
									onClick={handleToggleFavorite}
									data-flx="voice.voice-call-view.voice-call-view-inner.channel-header-icon.toggle-favorite"
								/>
							)}
							{connectionStateText && (
								<div
									className={clsx(
										styles.connectionStatusContainer,
										normalizedConnectionState === VoiceEngineConnectionState.Connecting && styles.statusConnecting,
										(normalizedConnectionState === VoiceEngineConnectionState.Reconnecting ||
											normalizedConnectionState === VoiceEngineConnectionState.SignalReconnecting) &&
											styles.statusReconnecting,
										normalizedConnectionState === VoiceEngineConnectionState.Disconnected && styles.statusDisconnected,
										normalizedConnectionState === VoiceEngineConnectionState.Connected && styles.statusConnected,
									)}
									data-flx="voice.voice-call-view.voice-call-view-inner.connection-status-container"
								>
									<div
										className={styles.connectionStatusDot}
										data-flx="voice.voice-call-view.voice-call-view-inner.connection-status-dot"
									/>
									{connectionStateText}
								</div>
							)}
							{isFocusedOnScreenShare && focusedStreamKey && (
								<MediaVerticalVolumeControl
									volume={focusedStreamVolume / 100}
									isMuted={isFocusedStreamMuted}
									maxVolume={VOICE_VOLUME_MAX_SLIDER_VOLUME}
									onVolumeChange={handleFocusedStreamVolumeChange}
									onToggleMute={handleFocusedStreamToggleMute}
									iconSize={18}
									className={styles.voiceHeaderIconButton}
									position="below"
									data-flx="voice.voice-call-view.voice-call-view-inner.hud-stream-volume-control"
								/>
							)}
							{statsButton}
							{isMobile && (
								<ChannelHeaderIcon
									icon={PhoneIcon}
									label={i18n._(VIEW_CALL_CONTROLS_DESCRIPTOR)}
									className={styles.voiceHeaderIconButton}
									onClick={handleOpenCallSheet}
									data-flx="voice.voice-call-view.voice-call-view-inner.channel-header-icon.open-call-sheet"
								/>
							)}
							{!isMobile && (
								<StaffToolsButton
									className={styles.voiceHeaderIconButton}
									data-flx="voice.voice-call-view.voice-call-view-inner.staff-tools-button"
								/>
							)}
							{!isMobile && (
								<InboxButton
									className={styles.voiceHeaderIconButton}
									data-flx="voice.voice-call-view.voice-call-view-inner.inbox-button"
								/>
							)}
						</div>
					</NativeDragRegion>
					<div className={mainContentClassName} data-flx="voice.voice-call-view.voice-call-view-inner.div--5">
						{hasRenderableTiles ? (
							<VoiceCallLayoutContent
								channel={channel}
								layoutMode={effectiveLayoutMode}
								focusMainTrack={effectiveFocusMainTrack}
								carouselTracks={carouselTracks}
								filteredCameraTracks={filteredCameraTracks}
								gridEntries={gridEntries}
								hasScreenShare={hasScreenShare}
								pinnedParticipantIdentity={pinnedParticipantIdentity}
								isVoiceCallAppFullscreen={isMobile || isVoiceCallAppFullscreen}
								onGridCapacityChange={handleGridCapacityChange}
								onExpandUser={handleExpandUser}
								data-flx="voice.voice-call-view.voice-call-view-inner.voice-call-layout-content"
							/>
						) : (
							<div
								className={styles.audioAvatarFallback}
								data-flx="voice.voice-call-view.voice-call-view-inner.audio-avatar-fallback"
							>
								<VoiceParticipantWrappedAvatarList
									entries={participantAvatarEntries}
									guildId={channel.guildId}
									channelId={channel.id}
									className={styles.audioAvatarFallbackList}
									data-flx="voice.voice-call-view.voice-call-view-inner.audio-avatar-fallback-list"
								/>
							</div>
						)}
					</div>
					<div
						className={clsx(styles.controlBarContainer, styles.voiceChrome, styles.voiceEdgeFadeBottom)}
						data-flx="voice.voice-call-view.voice-call-view-inner.control-bar-container"
					>
						<VoiceControlBar data-flx="voice.voice-call-view.voice-call-view-inner.voice-control-bar" />
					</div>
					<VoiceCallCornerControls
						wrapClassName={clsx(styles.fullscreenButtonWrap, styles.voiceChrome)}
						buttonClassName={styles.voiceHeaderIconButton}
						showPopout={!inPopout && isVoicePopoutSupported()}
						onPopOut={handlePopOutCall}
						showFullscreen={!inPopout && supportsVoiceCallAppFullscreen}
						isFullscreen={isVoiceCallAppFullscreen}
						fullscreenLabel={fullscreenButtonLabel}
						fullscreenIcon={FullscreenButtonIcon}
						onToggleFullscreen={handleToggleVoiceCallAppFullscreen}
						data-flx="voice.voice-call-view.voice-call-view-inner.voice-call-corner-controls"
					/>
					{isMobile && (
						<BottomSheet
							isOpen={isMobileStatsSheetOpen}
							onClose={handleCloseMobileStatsSheet}
							title={i18n._(CONNECTION_STATS_DESCRIPTOR)}
							snapPoints={[0.3, 0.65, 0.9]}
							data-flx="voice.voice-call-view.voice-call-view-inner.bottom-sheet"
						>
							<VoiceDetailsPopout
								hideHeader
								onClose={handleCloseMobileStatsSheet}
								data-flx="voice.voice-call-view.voice-call-view-inner.voice-details-popout.mobile"
							/>
						</BottomSheet>
					)}
					{isMobile && (
						<BottomSheet
							isOpen={isCallSheetOpen}
							onClose={handleCloseCallSheet}
							title={channel.name ?? i18n._(VOICE_CALL_DESCRIPTOR)}
							snapPoints={[0.35, 0.65, 0.95]}
							disablePadding
							surface="primary"
							data-flx="voice.voice-call-view.voice-call-view-inner.bottom-sheet--2"
						>
							<div
								className={styles.voiceCallSheetContent}
								data-flx="voice.voice-call-view.voice-call-view-inner.voice-call-sheet-content"
							>
								<CompactVoiceCallView
									channel={channel}
									className={styles.voiceCallSheetCompact}
									data-flx="voice.voice-call-view.voice-call-view-inner.voice-call-sheet-compact"
								/>
							</div>
						</BottomSheet>
					)}
					<VoiceRegionTeleportOverlay data-flx="voice.voice-call-view.voice-call-view-inner.region-teleport-overlay" />
					<div
						ref={setPortalHostRef}
						data-voice-call-portal-host
						data-flx="voice.voice-call-view.voice-call-view-inner.portal-host"
					/>
					{shouldRenderPoppedOutOverlay(poppedOutTransition.snapshot) && (
						<PoppedOutOverlay
							popoutKey={getVoiceCallPopoutKey(channel.id)}
							variant="call"
							transition={selectPoppedOutOverlayTransition(poppedOutTransition.snapshot)}
							onTransitionEnd={poppedOutTransition.handleTransitionEnd}
							data-flx="voice.voice-call-view.voice-call-view-inner.popped-out-overlay"
						/>
					)}
				</div>
			</PortalHostContext.Provider>
		);
	},
);

function hasValidRoomForVoiceCallView(channel: Channel): boolean {
	return hasValidRoomForVoiceCallContext(channel.id, channel.guildId);
}

const VoiceCallPendingView = observer(function VoiceCallPendingView({
	channel,
	fullscreenRequestNonce,
	onCloseFullscreenView,
}: VoiceCallViewProps) {
	const {i18n} = useLingui();
	const containerRef = useRef<HTMLDivElement>(null);
	const previousFullscreenRequestNonceRef = useRef<number | undefined>(undefined);
	const isMobile = MobileLayout.isMobileLayout();
	const {
		isFullscreen: isVoiceCallAppFullscreen,
		supportsFullscreen: supportsVoiceCallAppFullscreen,
		enterFullscreen: enterVoiceCallAppFullscreen,
		toggleFullscreen: toggleVoiceCallAppFullscreen,
	} = useVoiceCallAppFullscreen({containerRef});
	const handleBackClick = useCallback(() => {
		if (onCloseFullscreenView) {
			onCloseFullscreenView();
			return;
		}
		goBackOr('/');
	}, [onCloseFullscreenView]);
	const handleToggleVoiceCallAppFullscreen = useCallback(() => {
		void toggleVoiceCallAppFullscreen();
	}, [toggleVoiceCallAppFullscreen]);
	const fullscreenButtonLabel = isVoiceCallAppFullscreen
		? i18n._(EXIT_FULLSCREEN_DESCRIPTOR)
		: i18n._(ENTER_FULLSCREEN_DESCRIPTOR);
	const FullscreenButtonIcon = useMemo(() => {
		const BaseIcon = isVoiceCallAppFullscreen ? CornersInIcon : CornersOutIcon;
		const BoldIcon = forwardRef<SVGSVGElement, React.ComponentProps<typeof BaseIcon>>((props, ref) => (
			<BaseIcon ref={ref} weight="bold" data-flx="voice.voice-call-view.pending.bold-icon.base-icon" {...props} />
		));
		BoldIcon.displayName = 'PendingFullscreenButtonIcon';
		return BoldIcon;
	}, [isVoiceCallAppFullscreen]);
	useEffect(() => {
		if (fullscreenRequestNonce == null) return;
		if (previousFullscreenRequestNonceRef.current === fullscreenRequestNonce) return;
		previousFullscreenRequestNonceRef.current = fullscreenRequestNonce;
		void enterVoiceCallAppFullscreen();
	}, [enterVoiceCallAppFullscreen, fullscreenRequestNonce]);
	return (
		<div
			ref={containerRef}
			data-voice-call-root
			className={clsx(
				styles.root,
				styles.voiceRoot,
				styles.forceHudVisible,
				isVoiceCallAppFullscreen && styles.voiceCallFullscreen,
			)}
			aria-busy="true"
			data-flx="voice.voice-call-view.pending.root"
		>
			<NativeDragRegion
				className={clsx(channelHeaderStyles.headerContainer, styles.voiceChrome, styles.voiceHeader)}
				data-flx="voice.voice-call-view.pending.voice-chrome"
			>
				<div className={channelHeaderStyles.headerLeftSection} data-flx="voice.voice-call-view.pending.header-left">
					<FocusRing offset={-2} data-flx="voice.voice-call-view.pending.focus-ring">
						<button
							type="button"
							className={clsx(
								isMobile ? channelHeaderStyles.backButton : channelHeaderStyles.backButtonDesktop,
								styles.voiceHeaderBackButton,
							)}
							onClick={handleBackClick}
							aria-label={i18n._(BACK_TO_VOICE_CHANNEL_DESCRIPTOR)}
							data-flx="voice.voice-call-view.pending.button.back-click"
						>
							{isMobile ? (
								<ArrowLeftIcon
									className={channelHeaderStyles.backIconBold}
									weight="bold"
									data-flx="voice.voice-call-view.pending.arrow-left-icon"
								/>
							) : (
								<ListIcon className={channelHeaderStyles.backIcon} data-flx="voice.voice-call-view.pending.list-icon" />
							)}
						</button>
					</FocusRing>
					<div
						className={channelHeaderStyles.leftContentContainer}
						data-flx="voice.voice-call-view.pending.left-content-container"
					>
						<div
							className={channelHeaderStyles.channelInfoContainer}
							data-flx="voice.voice-call-view.pending.channel-info-container"
						>
							{ChannelUtils.getIcon(channel, {className: channelHeaderStyles.channelIcon})}
							<span className={channelHeaderStyles.channelName} data-flx="voice.voice-call-view.pending.channel-name">
								{channel.name ?? ''}
							</span>
						</div>
					</div>
				</div>
			</NativeDragRegion>
			<div className={styles.voiceCallPendingMain} data-flx="voice.voice-call-view.pending.main" />
			<div
				className={clsx(styles.controlBarContainer, styles.voiceChrome)}
				data-flx="voice.voice-call-view.pending.footer"
			/>
			<div
				className={clsx(styles.fullscreenButtonWrap, styles.voiceChrome)}
				data-flx="voice.voice-call-view.pending.fullscreen-button-wrap"
			>
				{supportsVoiceCallAppFullscreen && (
					<ChannelHeaderIcon
						icon={FullscreenButtonIcon}
						label={fullscreenButtonLabel}
						className={styles.voiceHeaderIconButton}
						isSelected={isVoiceCallAppFullscreen}
						onClick={handleToggleVoiceCallAppFullscreen}
						data-flx="voice.voice-call-view.pending.channel-header-icon.toggle-voice-call-app-fullscreen"
					/>
				)}
			</div>
		</div>
	);
});
export const VoiceCallView = observer(
	({channel, fullscreenRequestNonce, onCloseFullscreenView, inPopout = false}: VoiceCallViewProps) => {
		useMediaEngineVersion();
		if (!hasValidRoomForVoiceCallView(channel)) {
			return (
				<VoiceCallPendingView
					channel={channel}
					fullscreenRequestNonce={fullscreenRequestNonce}
					onCloseFullscreenView={onCloseFullscreenView}
					data-flx="voice.voice-call-view.voice-call-pending-view"
				/>
			);
		}
		return (
			<VoiceCallViewInner
				channel={channel}
				fullscreenRequestNonce={fullscreenRequestNonce}
				onCloseFullscreenView={onCloseFullscreenView}
				inPopout={inPopout}
				data-flx="voice.voice-call-view.voice-call-view-inner"
			/>
		);
	},
);
