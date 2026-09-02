// SPDX-License-Identifier: AGPL-3.0-or-later

import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import ContextMenu, {isContextMenuNodeTarget} from '@app/features/ui/state/ContextMenu';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import styles from '@app/features/voice/components/CompactVoiceCallView.module.css';
import {CompactCallHeightToggleButton} from '@app/features/voice/components/compact_voice_call_view/CompactCallHeightToggleButton';
import {
	getCompactAudioAvatarLayoutStyle,
	resolveCompactAudioAvatarLayoutMetrics,
} from '@app/features/voice/components/compact_voice_call_view/CompactVoiceCallLayoutMetrics';
import {resolveCompactVoiceCallPresentationModel} from '@app/features/voice/components/compact_voice_call_view/CompactVoiceCallPresentationStateMachine';
import {
	COMPACT_HEIGHT_MIN,
	type CompactCallMetrics,
	type CompactVoiceCallContainerStyle,
	type CompactVoiceCallViewProps,
	ENTER_FULLSCREEN_DESCRIPTOR,
	EXIT_FULLSCREEN_DESCRIPTOR,
	getCompactHeightKey,
	hasCompactCallMetricsChanged,
	RESIZE_CALL_VIEW_DESCRIPTOR,
	toLayoutPx,
	VOICE_CALL_DESCRIPTOR,
	VOICE_HUD_IDLE_TIMEOUT_MS,
} from '@app/features/voice/components/compact_voice_call_view/shared';
import {useCompactVoiceCallResize} from '@app/features/voice/components/compact_voice_call_view/useCompactVoiceCallResize';
import {useConnectionLabel} from '@app/features/voice/components/compact_voice_call_view/useConnectionLabel';
import {PoppedOutOverlay} from '@app/features/voice/components/popout/PoppedOutOverlay';
import {
	selectPoppedOutOverlayTransition,
	shouldRenderPoppedOutOverlay,
} from '@app/features/voice/components/popout/PoppedOutSurfaceStateMachine';
import {usePoppedOutTransition} from '@app/features/voice/components/popout/usePoppedOutTransition';
import {useVoiceCallAppFullscreen} from '@app/features/voice/components/useVoiceCallAppFullscreen';
import {useVoiceCallTracksAndLayout} from '@app/features/voice/components/useVoiceCallTracksAndLayout';
import {useVoiceEngineConnectionState} from '@app/features/voice/components/useVoiceEngineConnectionState';
import {VoiceCallCornerControls} from '@app/features/voice/components/VoiceCallCornerControls';
import {VoiceCallLayoutContent} from '@app/features/voice/components/VoiceCallLayoutContent';
import voiceCallStyles from '@app/features/voice/components/VoiceCallView.module.css';
import {VoiceControlBar} from '@app/features/voice/components/VoiceControlBar';
import {
	useVoiceParticipantAvatarEntries,
	VoiceParticipantWrappedAvatarList,
} from '@app/features/voice/components/VoiceParticipantAvatarList';
import {VoiceDebugStatsForwarder} from '@app/features/voice/diagnostics/VoiceDebugStatsForwarder';
import {
	asVoiceEngineConnectionState,
	VoiceEngineConnectionState,
} from '@app/features/voice/engine/VoiceConnectionStateMachine';
import CallState from '@app/features/voice/state/CallState';
import PopoutWindowManager, {
	getVoiceCallPopoutKey,
	isVoicePopoutSupported,
} from '@app/features/voice/state/PopoutWindowManager';
import {VOICE_CALL_DESCRIPTOR as VOICE_CALL_TITLE_DESCRIPTOR} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {useLingui} from '@lingui/react/macro';
import {isTrackReference} from '@livekit/components-react';
import {CornersInIcon, CornersOutIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {forwardRef, useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';

const VOICE_CHROME_PIN_RELEASE_DELAY_MS = 320;
export const CompactVoiceCallViewInner: React.FC<CompactVoiceCallViewProps> = observer(
	function CompactVoiceCallViewInner({
		channel,
		className,
		hideHeader = false,
		hideControlBar = false,
		controlBar,
		avatarFallback,
		showAvatarFallback = true,
		audioOnly = false,
		onFullscreenRequest,
		fullscreenRequestNonce,
		fillHeight = false,
		reserveHeaderChrome = false,
		heightToggle,
		avatarFallbackFullBleed = false,
	}) {
		const {i18n} = useLingui();
		const containerRef = useRef<HTMLElement>(null);
		const hudPointerTimeoutRef = useRef<number | null>(null);
		const previousFullscreenRequestNonceRef = useRef<number | undefined>(undefined);
		const {keyboardModeEnabled} = KeyboardMode;
		const connectionState = useVoiceEngineConnectionState();
		const isMobile = MobileLayout.isMobileLayout();
		const [isPointerHudActive, setIsPointerHudActive] = useState(false);
		const isResizable = !isMobile && !fillHeight;
		const {
			isFullscreen: isVoiceCallAppFullscreen,
			supportsFullscreen: supportsVoiceCallAppFullscreen,
			enterFullscreen: enterVoiceCallAppFullscreen,
			exitFullscreen: exitVoiceCallAppFullscreen,
			toggleFullscreen: toggleVoiceCallAppFullscreen,
		} = useVoiceCallAppFullscreen({containerRef});
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
			cameraTracksAll,
			screenShareTracks,
			filteredCameraTracks,
			gridEntries,
			focusMainTrack,
			carouselTracks,
			participantCount,
		} = useVoiceCallTracksAndLayout({channel, expandedUserIds});
		const compactHeightMin = COMPACT_HEIGHT_MIN;
		const call = CallState.getCall(channel.id);
		const heightKey = useMemo(
			() => getCompactHeightKey(channel.id, call?.messageId ?? null),
			[channel.id, call?.messageId],
		);
		const callViewId = useMemo(() => `compact-call-view-${heightKey}`, [heightKey]);
		const contentAreaRef = useRef<HTMLDivElement | null>(null);
		const controlBarRef = useRef<HTMLElement | null>(null);
		const [compactCallMetrics, setCompactCallMetrics] = useState<CompactCallMetrics | null>(null);
		const {compactHeight, maxHeight, isResizing, handleResizePointerDown, handleResizeKeyDown} =
			useCompactVoiceCallResize({
				containerRef,
				heightKey,
				isResizable,
				compactHeightMin,
			});
		const participantAvatarEntries = useVoiceParticipantAvatarEntries({
			guildId: channel.guildId ?? null,
			channelId: channel.id,
		});
		const hasRenderableCallMedia = useMemo(
			() =>
				screenShareTracks.length > 0 ||
				cameraTracksAll.some(
					(track) => isTrackReference(track) && track.publication != null && !track.publication.isMuted,
				),
			[cameraTracksAll, screenShareTracks],
		);
		const {shouldRenderCallLayout, useFullHeightCallLayout, shouldShowAvatarFallback, shouldForceFloatingHudVisible} =
			useMemo(
				() =>
					resolveCompactVoiceCallPresentationModel({
						audioOnly,
						fillHeight,
						showAvatarFallback,
						hasRenderableCallMedia,
					}),
				[audioOnly, fillHeight, hasRenderableCallMedia, showAvatarFallback],
			);
		const isAnyContextMenuOpen = useMemo(() => {
			const contextMenu = ContextMenu.contextMenu;
			const target = contextMenu?.target?.target ?? null;
			const container = containerRef.current;
			if (!contextMenu || !container || !isContextMenuNodeTarget(target)) return false;
			return Boolean(container.contains(target));
		}, [ContextMenu.contextMenu]);
		const [isChromePinned, setIsChromePinned] = useState(isAnyContextMenuOpen);
		const normalizedConnectionState = asVoiceEngineConnectionState(connectionState);
		const statusText = useConnectionLabel(connectionState, participantCount);
		const ariaLabel = useMemo(() => {
			if (normalizedConnectionState !== VoiceEngineConnectionState.Connected) return statusText;
			return i18n._(VOICE_CALL_DESCRIPTOR, {statusText});
		}, [normalizedConnectionState, statusText, i18n.locale]);
		const isPoppedOut = PopoutWindowManager.isCallPopoutOpenForChannel(channel.id);
		const poppedOutTransition = usePoppedOutTransition(isPoppedOut);
		const isInteractionActive = isResizing;
		const containerClassName = clsx(
			styles.container,
			voiceCallStyles.voiceRoot,
			className,
			hideHeader && styles.containerNoHeader,
			hideControlBar && styles.containerNoControlBar,
			reserveHeaderChrome && styles.containerReserveHeaderChrome,
			fillHeight && styles.containerFillHeight,
			isResizing && styles.containerResizing,
			isInteractionActive && voiceCallStyles.interactionActive,
			shouldForceFloatingHudVisible && voiceCallStyles.forceHudVisible,
			isPointerHudActive && voiceCallStyles.pointerActive,
			isChromePinned && voiceCallStyles.contextMenuActive,
			keyboardModeEnabled && voiceCallStyles.keyboardModeActive,
		);
		const controlBarContent = hideControlBar
			? null
			: (controlBar ?? (
					<VoiceControlBar data-flx="voice.compact-voice-call-view.compact-voice-call-view-inner.voice-control-bar" />
				));
		const hasControlBar = controlBarContent != null;
		const handleToggleVoiceCallAppFullscreen = useCallback(() => {
			if (!isVoiceCallAppFullscreen && onFullscreenRequest) {
				onFullscreenRequest();
				return;
			}
			void toggleVoiceCallAppFullscreen();
		}, [isVoiceCallAppFullscreen, onFullscreenRequest, toggleVoiceCallAppFullscreen]);
		const fullscreenButtonLabel = isVoiceCallAppFullscreen
			? i18n._(EXIT_FULLSCREEN_DESCRIPTOR)
			: i18n._(ENTER_FULLSCREEN_DESCRIPTOR);
		const FullscreenButtonIcon = useMemo(() => {
			const BaseIcon = isVoiceCallAppFullscreen ? CornersInIcon : CornersOutIcon;
			const BoldIcon = forwardRef<SVGSVGElement, React.ComponentProps<typeof BaseIcon>>((props, ref) => (
				<BaseIcon ref={ref} weight="bold" data-flx="voice.compact-voice-call-view.bold-icon.base-icon--2" {...props} />
			));
			BoldIcon.displayName = 'FullscreenButtonIcon';
			return BoldIcon;
		}, [isVoiceCallAppFullscreen]);
		const handlePopOutCall = useCallback(() => {
			const didOpen = PopoutWindowManager.openCallPopout({
				channelId: channel.id,
				guildId: channel.guildId ?? null,
				title: channel.name ?? i18n._(VOICE_CALL_TITLE_DESCRIPTOR),
			});
			if (didOpen && isVoiceCallAppFullscreen) {
				void exitVoiceCallAppFullscreen();
			}
		}, [channel.guildId, channel.id, channel.name, exitVoiceCallAppFullscreen, i18n, isVoiceCallAppFullscreen]);
		useEffect(() => {
			if (fullscreenRequestNonce == null) return;
			if (previousFullscreenRequestNonceRef.current === fullscreenRequestNonce) return;
			previousFullscreenRequestNonceRef.current = fullscreenRequestNonce;
			void enterVoiceCallAppFullscreen();
		}, [enterVoiceCallAppFullscreen, fullscreenRequestNonce]);
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
			if (isAnyContextMenuOpen) {
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
		}, [isAnyContextMenuOpen, scheduleHudIdleState]);
		const handleVoiceRootPointerActivity = useCallback(
			(event: React.PointerEvent<HTMLElement>) => {
				if (event.pointerType === 'touch') return;
				setIsPointerHudActive(true);
				scheduleHudIdleState();
			},
			[scheduleHudIdleState],
		);
		const handleVoiceRootPointerLeave = useCallback(
			(event: React.PointerEvent<HTMLElement>) => {
				if (event.pointerType === 'touch') return;
				if (isInteractionActive) return;
				clearHudPointerTimeout();
				setIsPointerHudActive(false);
			},
			[clearHudPointerTimeout, isInteractionActive],
		);
		const recomputeCompactCallMetrics = useCallback(() => {
			const containerNode = containerRef.current;
			const contentAreaNode = contentAreaRef.current;
			if (!containerNode || !contentAreaNode) {
				return;
			}
			const containerRect = containerNode.getBoundingClientRect();
			const contentRect = contentAreaNode.getBoundingClientRect();
			const controlBarRect = controlBarRef.current?.getBoundingClientRect() ?? null;
			const nextMetrics: CompactCallMetrics = {
				width: toLayoutPx(containerRect.width),
				height: toLayoutPx(containerRect.height),
				contentHeight: toLayoutPx(contentRect.height),
				controlBarHeight: controlBarRect ? toLayoutPx(controlBarRect.height) : 0,
			};
			setCompactCallMetrics((previousMetrics) => {
				if (previousMetrics && !hasCompactCallMetricsChanged(previousMetrics, nextMetrics)) {
					return previousMetrics;
				}
				return nextMetrics;
			});
		}, []);
		useLayoutEffect(() => {
			recomputeCompactCallMetrics();
		}, [
			compactHeight,
			fillHeight,
			hasControlBar,
			heightKey,
			recomputeCompactCallMetrics,
			shouldRenderCallLayout,
			shouldShowAvatarFallback,
		]);
		useLayoutEffect(() => {
			const containerNode = containerRef.current;
			const contentAreaNode = contentAreaRef.current;
			if (!containerNode || !contentAreaNode) {
				return;
			}
			let rafId: number | null = null;
			const scheduleRecompute = () => {
				if (rafId != null) return;
				rafId = requestAnimationFrame(() => {
					rafId = null;
					recomputeCompactCallMetrics();
				});
			};
			if (typeof ResizeObserver === 'undefined') {
				window.addEventListener('resize', scheduleRecompute);
				return () => {
					if (rafId != null) {
						cancelAnimationFrame(rafId);
					}
					window.removeEventListener('resize', scheduleRecompute);
				};
			}
			const resizeObserver = new ResizeObserver(recomputeCompactCallMetrics);
			resizeObserver.observe(containerNode);
			resizeObserver.observe(contentAreaNode);
			if (controlBarRef.current) {
				resizeObserver.observe(controlBarRef.current);
			}
			return () => {
				if (rafId != null) {
					cancelAnimationFrame(rafId);
				}
				resizeObserver.disconnect();
			};
		}, [hasControlBar, recomputeCompactCallMetrics]);
		const containerStyle = useMemo(() => {
			const measuredWidth = compactCallMetrics?.width ?? 0;
			const measuredHeight = compactCallMetrics?.height ?? 0;
			const measuredContentHeight = compactCallMetrics?.contentHeight ?? 0;
			const measuredControlBarHeight = compactCallMetrics?.controlBarHeight ?? 0;
			const compactCallHeight = isResizable && compactHeight != null ? compactHeight : measuredHeight;
			const nextStyle: CompactVoiceCallContainerStyle = {
				'--compact-call-participant-count': `${participantCount}`,
			};
			if (measuredWidth > 0) {
				nextStyle['--compact-call-width'] = `${Math.round(measuredWidth)}px`;
			}
			if (compactCallHeight > 0) {
				nextStyle['--compact-call-height'] = `${Math.round(compactCallHeight)}px`;
			}
			if (measuredContentHeight > 0) {
				nextStyle['--compact-call-content-height'] = `${Math.round(measuredContentHeight)}px`;
			}
			if (measuredControlBarHeight > 0) {
				nextStyle['--compact-call-control-height'] = `${Math.round(measuredControlBarHeight)}px`;
			}
			Object.assign(
				nextStyle,
				getCompactAudioAvatarLayoutStyle(
					resolveCompactAudioAvatarLayoutMetrics({
						callHeight: compactCallHeight,
						controlBarHeight: measuredControlBarHeight,
						hasControlBar,
					}),
				),
			);
			if (isResizable && compactHeight != null) {
				nextStyle.height = compactHeight;
				nextStyle.minHeight = compactHeightMin;
				nextStyle.maxHeight = maxHeight;
			}
			return nextStyle;
		}, [
			compactCallMetrics?.contentHeight,
			compactCallMetrics?.controlBarHeight,
			compactCallMetrics?.height,
			compactCallMetrics?.width,
			compactHeight,
			compactHeightMin,
			hasControlBar,
			isResizable,
			maxHeight,
			participantCount,
		]);
		const layoutNode = useMemo(
			() => (
				<div
					className={clsx(
						styles.layoutHost,
						useFullHeightCallLayout && styles.layoutHostFullHeight,
						useFullHeightCallLayout && layoutMode === 'grid' && styles.layoutHostFullHeightGrid,
						useFullHeightCallLayout && layoutMode === 'focus' && styles.layoutHostFullHeightFocus,
					)}
					data-flx="voice.compact-voice-call-view.layout-node.layout-host"
				>
					<VoiceCallLayoutContent
						channel={channel}
						layoutMode={layoutMode}
						focusMainTrack={focusMainTrack}
						carouselTracks={carouselTracks}
						filteredCameraTracks={filteredCameraTracks}
						gridEntries={gridEntries}
						hasScreenShare={hasScreenShare}
						pinnedParticipantIdentity={pinnedParticipantIdentity}
						compact={!useFullHeightCallLayout}
						isVoiceCallAppFullscreen={isVoiceCallAppFullscreen}
						onExpandUser={handleExpandUser}
						data-flx="voice.compact-voice-call-view.layout-node.voice-call-layout-content"
					/>
				</div>
			),
			[
				carouselTracks,
				channel,
				filteredCameraTracks,
				focusMainTrack,
				gridEntries,
				handleExpandUser,
				hasScreenShare,
				layoutMode,
				pinnedParticipantIdentity,
				screenShareTracks,
				isVoiceCallAppFullscreen,
				useFullHeightCallLayout,
			],
		);
		const directCallAvatarLayoutNode = useMemo(
			() => (
				<VoiceParticipantWrappedAvatarList
					entries={participantAvatarEntries}
					guildId={channel.guildId}
					channelId={channel.id}
					className={styles.audioAvatarList}
					data-flx="voice.compact-voice-call-view.direct-call-avatar-layout-node.audio-avatar-list"
				/>
			),
			[channel.guildId, channel.id, participantAvatarEntries],
		);
		const avatarFallbackNode = useMemo(
			() => (
				<div
					className={clsx(styles.audioAvatarLayout, avatarFallbackFullBleed && styles.audioAvatarLayoutFullBleed)}
					data-flx="voice.compact-voice-call-view.avatar-fallback-node.audio-avatar-layout"
				>
					<div
						className={clsx(
							styles.audioAvatarLayoutInner,
							avatarFallbackFullBleed && styles.audioAvatarLayoutInnerFullBleed,
						)}
						data-flx="voice.compact-voice-call-view.avatar-fallback-node.audio-avatar-layout-inner"
					>
						{avatarFallback ?? directCallAvatarLayoutNode}
					</div>
				</div>
			),
			[avatarFallback, avatarFallbackFullBleed, directCallAvatarLayoutNode],
		);
		useEffect(() => {
			return () => {
				clearHudPointerTimeout();
			};
		}, [clearHudPointerTimeout]);
		const controlBarNode = hasControlBar ? (
			<footer
				ref={controlBarRef}
				className={clsx(styles.controlBarSection, voiceCallStyles.voiceChrome, voiceCallStyles.voiceEdgeFadeBottom)}
				data-flx="voice.compact-voice-call-view.compact-voice-call-view-inner.control-bar-section"
			>
				<div
					className={styles.controlBarInner}
					data-flx="voice.compact-voice-call-view.compact-voice-call-view-inner.control-bar-inner"
				>
					{controlBarContent}
				</div>
			</footer>
		) : null;
		return (
			<section
				id={callViewId}
				ref={containerRef}
				data-voice-call-root
				className={clsx(containerClassName, isVoiceCallAppFullscreen && voiceCallStyles.voiceCallFullscreen)}
				aria-label={ariaLabel}
				style={containerStyle}
				data-voice-call-popped-out={isPoppedOut ? 'true' : undefined}
				data-voice-call-avatar-mode={shouldShowAvatarFallback ? 'true' : undefined}
				onPointerEnter={handleVoiceRootPointerActivity}
				onPointerMove={handleVoiceRootPointerActivity}
				onPointerDown={handleVoiceRootPointerActivity}
				onPointerLeave={handleVoiceRootPointerLeave}
				data-flx="voice.compact-voice-call-view.compact-voice-call-view-inner.section.voice-root-pointer-activity"
			>
				<VoiceDebugStatsForwarder data-flx="voice.compact-voice-call-view.compact-voice-call-view-inner.voice-debug-stats-forwarder" />
				<div
					ref={contentAreaRef}
					className={clsx(styles.contentArea, shouldShowAvatarFallback && styles.contentAreaAudioOnly)}
					data-audio-only={shouldShowAvatarFallback ? 'true' : undefined}
					data-flx="voice.compact-voice-call-view.compact-voice-call-view-inner.content-area"
				>
					{shouldRenderCallLayout ? layoutNode : shouldShowAvatarFallback ? avatarFallbackNode : null}
				</div>
				{controlBarNode}
				<CompactCallHeightToggleButton
					heightToggle={heightToggle}
					callViewId={callViewId}
					data-flx="voice.compact-voice-call-view.compact-voice-call-view-inner.compact-call-height-toggle-button"
				/>
				<VoiceCallCornerControls
					wrapClassName={clsx(styles.fullscreenButtonWrap, voiceCallStyles.voiceChrome)}
					showPopout={isVoicePopoutSupported()}
					onPopOut={handlePopOutCall}
					showFullscreen={supportsVoiceCallAppFullscreen}
					isFullscreen={isVoiceCallAppFullscreen}
					fullscreenLabel={fullscreenButtonLabel}
					fullscreenIcon={FullscreenButtonIcon}
					onToggleFullscreen={handleToggleVoiceCallAppFullscreen}
					data-flx="voice.compact-voice-call-view.compact-voice-call-view-inner.voice-call-corner-controls"
				/>
				{shouldRenderPoppedOutOverlay(poppedOutTransition.snapshot) && (
					<PoppedOutOverlay
						popoutKey={getVoiceCallPopoutKey(channel.id)}
						variant="call"
						compact
						transition={selectPoppedOutOverlayTransition(poppedOutTransition.snapshot)}
						onTransitionEnd={poppedOutTransition.handleTransitionEnd}
						data-flx="voice.compact-voice-call-view.compact-voice-call-view-inner.popped-out-overlay"
					/>
				)}
				{isResizable && (
					<FocusRing offset={-2} data-flx="voice.compact-voice-call-view.compact-voice-call-view-inner.focus-ring">
						<div
							className={clsx(styles.resizeHandle, voiceCallStyles.voiceChrome)}
							onPointerDown={handleResizePointerDown}
							onKeyDown={handleResizeKeyDown}
							role="separator"
							aria-orientation="horizontal"
							aria-label={i18n._(RESIZE_CALL_VIEW_DESCRIPTOR)}
							aria-valuemin={compactHeightMin}
							aria-valuemax={maxHeight}
							aria-valuenow={compactHeight ?? compactHeightMin}
							tabIndex={0}
							data-flx="voice.compact-voice-call-view.compact-voice-call-view-inner.resize-handle.resize-key-down"
						>
							<div
								className={styles.resizePill}
								data-flx="voice.compact-voice-call-view.compact-voice-call-view-inner.resize-pill"
							/>
						</div>
					</FocusRing>
				)}
			</section>
		);
	},
);
