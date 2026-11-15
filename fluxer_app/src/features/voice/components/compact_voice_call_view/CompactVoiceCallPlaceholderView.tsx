// SPDX-License-Identifier: AGPL-3.0-or-later

import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import styles from '@app/features/voice/components/CompactVoiceCallView.module.css';
import {CompactCallHeightToggleButton} from '@app/features/voice/components/compact_voice_call_view/CompactCallHeightToggleButton';
import {COMPACT_HEIGHT_MIN} from '@app/features/voice/components/compact_voice_call_view/CompactVoiceCallHeightBounds';
import {
	getCompactAudioAvatarLayoutStyle,
	resolveCompactAudioAvatarLayoutMetrics,
} from '@app/features/voice/components/compact_voice_call_view/CompactVoiceCallLayoutMetrics';
import {
	type CompactVoiceCallContainerStyle,
	type CompactVoiceCallViewProps,
	RESIZE_CALL_VIEW_DESCRIPTOR,
	VOICE_CALL_DESCRIPTOR,
} from '@app/features/voice/components/compact_voice_call_view/shared';
import {useCompactVoiceCallResize} from '@app/features/voice/components/compact_voice_call_view/useCompactVoiceCallResize';
import voiceCallStyles from '@app/features/voice/components/VoiceCallView.module.css';
import {DISCONNECTED_DESCRIPTOR} from '@app/features/voice/utils/VoiceSharedMessageDescriptors';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo, useRef} from 'react';

export const CompactVoiceCallPlaceholderView: React.FC<CompactVoiceCallViewProps> = observer(
	function CompactVoiceCallPlaceholderView({
		channel,
		className,
		hideHeader = false,
		hideControlBar = false,
		controlBar,
		avatarFallback,
		showAvatarFallback = true,
		fillHeight = false,
		heightToggle,
		avatarFallbackFullBleed = false,
	}) {
		const {i18n} = useLingui();
		const containerRef = useRef<HTMLElement>(null);
		const {keyboardModeEnabled} = KeyboardMode;
		const isMobile = MobileLayout.isMobileLayout();
		const isResizable = !isMobile && !fillHeight;
		const heightKey = channel.id;
		const callViewId = useMemo(() => `compact-call-view-${heightKey}`, [heightKey]);
		const compactHeightMin = COMPACT_HEIGHT_MIN;
		const {compactHeight, maxHeight, isResizing, handleResizePointerDown, handleResizeKeyDown} =
			useCompactVoiceCallResize({
				containerRef,
				heightKey,
				isResizable,
				compactHeightMin,
			});
		const disconnectedStatusText = i18n._(DISCONNECTED_DESCRIPTOR);
		const ariaLabel = i18n._(VOICE_CALL_DESCRIPTOR, {statusText: disconnectedStatusText});
		const controlBarContent = hideControlBar ? null : controlBar;
		const hasControlBar = controlBarContent != null;
		const containerStyle = useMemo(() => {
			const nextStyle: CompactVoiceCallContainerStyle = {
				'--compact-call-participant-count': '0',
			};
			if (isResizable && compactHeight != null) {
				nextStyle.height = compactHeight;
				nextStyle.minHeight = compactHeightMin;
				nextStyle.maxHeight = maxHeight;
				nextStyle['--compact-call-height'] = `${Math.round(compactHeight)}px`;
			}
			Object.assign(
				nextStyle,
				getCompactAudioAvatarLayoutStyle(
					resolveCompactAudioAvatarLayoutMetrics({
						callHeight: compactHeight ?? undefined,
						hasControlBar,
					}),
				),
			);
			return nextStyle;
		}, [compactHeight, compactHeightMin, hasControlBar, isResizable, maxHeight]);
		return (
			<section
				id={callViewId}
				ref={containerRef}
				data-voice-call-root
				className={clsx(
					styles.container,
					voiceCallStyles.voiceRoot,
					className,
					hideHeader && styles.containerNoHeader,
					!hasControlBar && styles.containerNoControlBar,
					fillHeight && styles.containerFillHeight,
					isResizing && styles.containerResizing,
					voiceCallStyles.forceHudVisible,
					keyboardModeEnabled && voiceCallStyles.keyboardModeActive,
				)}
				aria-label={ariaLabel}
				style={containerStyle}
				data-flx="voice.compact-voice-call-view.compact-voice-call-placeholder-view.container"
			>
				<div
					className={clsx(styles.contentArea, showAvatarFallback && styles.contentAreaAudioOnly)}
					data-audio-only={showAvatarFallback ? 'true' : undefined}
					data-flx="voice.compact-voice-call-view.compact-voice-call-placeholder-view.content-area"
				>
					{showAvatarFallback && (
						<div
							className={clsx(styles.audioAvatarLayout, avatarFallbackFullBleed && styles.audioAvatarLayoutFullBleed)}
							data-flx="voice.compact-voice-call-view.compact-voice-call-placeholder-view.audio-avatar-layout"
						>
							<div
								className={clsx(
									styles.audioAvatarLayoutInner,
									avatarFallbackFullBleed && styles.audioAvatarLayoutInnerFullBleed,
								)}
								data-flx="voice.compact-voice-call-view.compact-voice-call-placeholder-view.audio-avatar-layout-inner"
							>
								{avatarFallback}
							</div>
						</div>
					)}
				</div>
				{hasControlBar && (
					<footer
						className={clsx(styles.controlBarSection, voiceCallStyles.voiceChrome, voiceCallStyles.voiceEdgeFadeBottom)}
						data-flx="voice.compact-voice-call-view.compact-voice-call-placeholder-view.control-bar-section"
					>
						<div
							className={styles.controlBarInner}
							data-flx="voice.compact-voice-call-view.compact-voice-call-placeholder-view.control-bar-inner"
						>
							{controlBarContent}
						</div>
					</footer>
				)}
				<CompactCallHeightToggleButton
					heightToggle={heightToggle}
					callViewId={callViewId}
					data-flx="voice.compact-voice-call-view.compact-voice-call-placeholder-view.compact-call-height-toggle-button"
				/>
				{isResizable && (
					<FocusRing
						offset={-2}
						data-flx="voice.compact-voice-call-view.compact-voice-call-placeholder-view.focus-ring"
					>
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
							data-flx="voice.compact-voice-call-view.compact-voice-call-placeholder-view.resize-handle.resize-key-down"
						>
							<div
								className={styles.resizePill}
								data-flx="voice.compact-voice-call-view.compact-voice-call-placeholder-view.resize-pill"
							/>
						</div>
					</FocusRing>
				)}
			</section>
		);
	},
);
