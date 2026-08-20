// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import styles from '@app/features/channel/components/VoiceMessageRecorder.module.css';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {usePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import {getReducedMotionProps} from '@app/features/ui/utils/ReducedMotionAnimation';
import {prepareVoiceMessageWav} from '@app/features/voice/utils/VoiceMessageRecordingUtils';
import {sendVoiceMessage} from '@app/features/voice/utils/VoiceMessageSendUtils';
import {autoUpdate, FloatingPortal, flip, offset, shift, useFloating} from '@floating-ui/react';
import {
	VOICE_MESSAGE_HOLD_TOOLTIP_DURATION_MS,
	VOICE_MESSAGE_LOCK_DRAG_MAX_HORIZONTAL_DELTA_PX,
	VOICE_MESSAGE_LOCK_DRAG_MIN_VERTICAL_DELTA_PX,
	VOICE_MESSAGE_MIN_SEND_DURATION_MS,
	VOICE_MESSAGE_RECORDING_TICK_MS,
	VOICE_MESSAGE_WAVEFORM_BAR_COUNT,
	VOICE_MESSAGE_WAVEFORM_UPDATE_INTERVAL_MS,
} from '@fluxer/constants/src/VoiceMessageConstants';
import {formatDuration} from '@fluxer/date_utils/src/DateDuration';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {
	CaretUpIcon,
	LockSimpleIcon,
	LockSimpleOpenIcon,
	MicrophoneIcon,
	PaperPlaneRightIcon,
	TrashIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const RECORDING_FAILED_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Recording failed. Try again.',
	comment: 'Error toast shown when the voice message recording fails (encoder error, etc).',
});
const VOICE_MESSAGE_DESCRIPTOR = msg({
	message: 'Voice message',
	comment: 'Filename label used for outgoing voice messages.',
});
const UNABLE_TO_SEND_VOICE_MESSAGE_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Unable to send voice message. Try again.',
	comment: 'Error toast shown when sending the recorded voice message fails.',
});
const VOICE_RECORDING_IS_NOT_SUPPORTED_IN_THIS_BROWSER_DESCRIPTOR = msg({
	message: 'Voice recording is not supported in this browser.',
	comment: 'Inline error shown when the browser does not support MediaRecorder for voice messages.',
});
const UNABLE_TO_START_RECORDING_PLEASE_ALLOW_MICROPHONE_ACCESS_DESCRIPTOR = msg({
	message: 'Unable to start recording. Allow microphone access.',
	comment: 'Inline error shown when the user denies microphone permission for voice messages.',
});
const HOLD_TO_RECORD_DRAG_UP_TO_LOCK_OR_DESCRIPTOR = msg({
	message: 'Hold to record. Drag up to lock, or release to send.',
	comment: 'Hint text shown next to the hold-to-record button explaining the gesture.',
});
const DISCARD_VOICE_MESSAGE_DESCRIPTOR = msg({
	message: 'Discard voice message',
	comment: 'Accessible label and tooltip for the discard button after recording a voice message.',
});
const SEND_VOICE_MESSAGE_DESCRIPTOR = msg({
	message: 'Send voice message',
	comment: 'Accessible label and tooltip for the send button after recording a voice message.',
});
const logger = new Logger('VoiceMessageRecorder');

type StopAction = 'send' | 'discard';

const tooltipMotion = {
	initial: {opacity: 0, y: 6},
	animate: {opacity: 1, y: 0},
	exit: {opacity: 0, y: 6},
	transition: {duration: 0.12},
};
const overlayMotion = {
	initial: {opacity: 0, y: 16},
	animate: {opacity: 1, y: 0},
	exit: {opacity: 0, y: 16},
	transition: {duration: 0.15},
};
const voiceButtonTransition = {duration: 0.12};

interface VoiceMessageRecorderProps {
	channelId: string;
	disabled: boolean;
	tooltipAnchorRef?: React.RefObject<HTMLElement | null>;
}

export default function VoiceMessageRecorder({channelId, disabled, tooltipAnchorRef}: VoiceMessageRecorderProps) {
	const {i18n} = useLingui();
	const portalHost = usePortalHost();
	const [isRecording, setIsRecording] = useState(false);
	const [isLocked, setIsLocked] = useState(false);
	const [isSending, setIsSending] = useState(false);
	const [lockPreview, setLockPreview] = useState(false);
	const [tooltipText, setTooltipText] = useState<string | null>(null);
	const [recordingDurationMs, setRecordingDurationMs] = useState(0);
	const [waveformBars, setWaveformBars] = useState(() => new Array(VOICE_MESSAGE_WAVEFORM_BAR_COUNT).fill(0));
	const {
		refs: tooltipRefs,
		floatingStyles: tooltipFloatingStyles,
		isPositioned: isTooltipPositioned,
	} = useFloating({
		open: tooltipText !== null,
		placement: 'top',
		middleware: [offset(8), flip(), shift({padding: 8})],
		whileElementsMounted: autoUpdate,
	});
	const {
		refs: lockIndicatorRefs,
		floatingStyles: lockIndicatorFloatingStyles,
		isPositioned: isLockIndicatorPositioned,
	} = useFloating({
		open: isRecording && !isSending,
		placement: 'top',
		middleware: [offset(8), flip(), shift({padding: 8})],
		whileElementsMounted: autoUpdate,
	});
	const maxRecordingSeconds = useMemo(
		() => LimitResolver.resolve({key: 'max_voice_message_duration', fallback: 1200}),
		[],
	);
	const maxRecordingMs = useMemo(() => maxRecordingSeconds * 1000, [maxRecordingSeconds]);
	const formattedDuration = useMemo(() => formatDuration(recordingDurationMs / 1000), [recordingDurationMs]);
	const reducedMotion = Accessibility.useReducedMotion;
	const voiceButtonAnimate = useMemo(() => (isRecording ? {scale: 1.05} : {scale: 1}), [isRecording]);
	const resolvedVoiceButtonTransition = useMemo(
		() => ({duration: reducedMotion ? 0 : voiceButtonTransition.duration}),
		[reducedMotion],
	);
	const resolvedTooltipMotion = useMemo(() => getReducedMotionProps(tooltipMotion, reducedMotion), [reducedMotion]);
	const resolvedOverlayMotion = useMemo(() => getReducedMotionProps(overlayMotion, reducedMotion), [reducedMotion]);
	const waveformStyles = useMemo(
		() => waveformBars.map((value) => ({height: `${Math.max(0.2, value) * 100}%`})),
		[waveformBars],
	);
	const showOverlay = isRecording || isSending;
	const showLockedIndicator = lockPreview || isLocked;
	const mediaRecorderRef = useRef<MediaRecorder | null>(null);
	const buttonWrapperRef = useRef<HTMLDivElement | null>(null);
	const streamRef = useRef<MediaStream | null>(null);
	const chunksRef = useRef<Array<Blob>>([]);
	const pointerIdRef = useRef<number | null>(null);
	const pointerStartRef = useRef<{x: number; y: number} | null>(null);
	const tooltipTimerRef = useRef<number | null>(null);
	const recordingTimerRef = useRef<number | null>(null);
	const recordingStartRef = useRef<number | null>(null);
	const recordingDurationRef = useRef(0);
	const stopActionRef = useRef<StopAction>('discard');
	const lockIndicatorRef = useRef<HTMLDivElement>(null);
	const lockPreviewRef = useRef(false);
	const audioContextRef = useRef<AudioContext | null>(null);
	const analyserRef = useRef<AnalyserNode | null>(null);
	const waveformDataRef = useRef<Uint8Array<ArrayBuffer> | null>(null);
	const waveformFrameRef = useRef<number | null>(null);
	const lastWaveformUpdateRef = useRef(0);
	const previousWaveformBarsRef = useRef<Array<number>>(new Array(VOICE_MESSAGE_WAVEFORM_BAR_COUNT).fill(0));
	const isMountedRef = useRef(true);
	const clearTooltipTimer = useCallback(() => {
		if (tooltipTimerRef.current) {
			clearTimeout(tooltipTimerRef.current);
			tooltipTimerRef.current = null;
		}
	}, []);
	const showTooltip = useCallback(
		(text: string) => {
			clearTooltipTimer();
			setTooltipText(text);
			tooltipTimerRef.current = window.setTimeout(() => {
				if (!isMountedRef.current) return;
				setTooltipText(null);
			}, VOICE_MESSAGE_HOLD_TOOLTIP_DURATION_MS);
		},
		[clearTooltipTimer],
	);
	const clearRecordingTimer = useCallback(() => {
		if (recordingTimerRef.current) {
			clearInterval(recordingTimerRef.current);
			recordingTimerRef.current = null;
		}
	}, []);
	const stopWaveformUpdates = useCallback(() => {
		if (waveformFrameRef.current) {
			cancelAnimationFrame(waveformFrameRef.current);
			waveformFrameRef.current = null;
		}
		if (audioContextRef.current) {
			audioContextRef.current.close().catch(() => {});
			audioContextRef.current = null;
		}
		analyserRef.current = null;
		waveformDataRef.current = null;
	}, []);
	const releaseStream = useCallback(() => {
		if (streamRef.current) {
			streamRef.current.getTracks().forEach((track) => track.stop());
			streamRef.current = null;
		}
	}, []);
	const resetState = useCallback(() => {
		setIsRecording(false);
		setIsLocked(false);
		setIsSending(false);
		setLockPreview(false);
		lockPreviewRef.current = false;
		pointerStartRef.current = null;
		setRecordingDurationMs(0);
		recordingDurationRef.current = 0;
		recordingStartRef.current = null;
		setWaveformBars(new Array(VOICE_MESSAGE_WAVEFORM_BAR_COUNT).fill(0));
		previousWaveformBarsRef.current = new Array(VOICE_MESSAGE_WAVEFORM_BAR_COUNT).fill(0);
	}, []);
	const cleanupResources = useCallback(() => {
		clearRecordingTimer();
		stopWaveformUpdates();
		releaseStream();
		mediaRecorderRef.current = null;
		chunksRef.current = [];
	}, [clearRecordingTimer, releaseStream, stopWaveformUpdates]);
	const updateWaveform = useCallback(() => {
		const analyser = analyserRef.current;
		const data = waveformDataRef.current;
		if (!analyser || !data) return;
		const now = performance.now();
		if (now - lastWaveformUpdateRef.current < VOICE_MESSAGE_WAVEFORM_UPDATE_INTERVAL_MS) {
			waveformFrameRef.current = requestAnimationFrame(updateWaveform);
			return;
		}
		lastWaveformUpdateRef.current = now;
		analyser.getByteTimeDomainData(data);
		const step = Math.max(1, Math.floor(data.length / VOICE_MESSAGE_WAVEFORM_BAR_COUNT));
		const bars = new Array(VOICE_MESSAGE_WAVEFORM_BAR_COUNT).fill(0);
		for (let i = 0; i < VOICE_MESSAGE_WAVEFORM_BAR_COUNT; i++) {
			const start = i * step;
			const end = Math.min(data.length, start + step);
			let peak = 0;
			for (let j = start; j < end; j++) {
				const value = Math.abs(data[j] - 128) / 128;
				if (value > peak) {
					peak = value;
				}
			}
			const shaped = Math.sqrt(peak);
			const previous = previousWaveformBarsRef.current[i] ?? 0;
			bars[i] = previous * 0.55 + shaped * 0.45;
		}
		previousWaveformBarsRef.current = bars;
		setWaveformBars(bars);
		waveformFrameRef.current = requestAnimationFrame(updateWaveform);
	}, []);
	const startWaveform = useCallback(
		(stream: MediaStream) => {
			const contextClass =
				window.AudioContext ||
				(window as typeof window & {webkitAudioContext?: typeof AudioContext}).webkitAudioContext;
			if (!contextClass) return;
			const audioContext = new contextClass();
			const source = audioContext.createMediaStreamSource(stream);
			const analyser = audioContext.createAnalyser();
			analyser.fftSize = 256;
			analyser.smoothingTimeConstant = 0.8;
			source.connect(analyser);
			audioContextRef.current = audioContext;
			analyserRef.current = analyser;
			waveformDataRef.current = new Uint8Array(analyser.frequencyBinCount);
			lastWaveformUpdateRef.current = 0;
			waveformFrameRef.current = requestAnimationFrame(updateWaveform);
		},
		[updateWaveform],
	);
	const handleDataAvailable = useCallback((event: BlobEvent) => {
		if (event.data && event.data.size > 0) {
			chunksRef.current = [...chunksRef.current, event.data];
		}
	}, []);
	const stopRecording = useCallback(
		(action: StopAction) => {
			const recorder = mediaRecorderRef.current;
			stopActionRef.current = action;
			clearRecordingTimer();
			stopWaveformUpdates();
			if (recorder && recorder.state !== 'inactive') {
				recorder.stop();
				return;
			}
			cleanupResources();
			resetState();
		},
		[clearRecordingTimer, cleanupResources, resetState, stopWaveformUpdates],
	);
	const updateRecordingDuration = useCallback(() => {
		const start = recordingStartRef.current;
		if (!start) return;
		const next = Math.min(maxRecordingMs, performance.now() - start);
		recordingDurationRef.current = next;
		setRecordingDurationMs(next);
		if (next >= maxRecordingMs) {
			stopRecording('send');
		}
	}, [maxRecordingMs, stopRecording]);
	const handleRecorderStop = useCallback(
		async (recorderMime: string) => {
			if (!isMountedRef.current) {
				cleanupResources();
				return;
			}
			const action = stopActionRef.current;
			stopActionRef.current = 'discard';
			const recordedChunks = chunksRef.current;
			chunksRef.current = [];
			cleanupResources();
			if (action === 'discard') {
				resetState();
				return;
			}
			if (recordedChunks.length === 0) {
				showTooltip(i18n._(RECORDING_FAILED_PLEASE_TRY_AGAIN_DESCRIPTOR));
				resetState();
				return;
			}
			const blob = new Blob(recordedChunks, {type: recorderMime || 'audio/ogg'});
			setIsSending(true);
			setIsRecording(false);
			setIsLocked(false);
			setLockPreview(false);
			try {
				const {file, duration, waveform} = await prepareVoiceMessageWav(blob, `voice-message-${Date.now()}.wav`);
				await sendVoiceMessage({
					channelId,
					file,
					waveform,
					duration,
					title: i18n._(VOICE_MESSAGE_DESCRIPTOR),
				});
			} catch (error) {
				logger.error('Failed to send voice message', error);
				showTooltip(i18n._(UNABLE_TO_SEND_VOICE_MESSAGE_PLEASE_TRY_AGAIN_DESCRIPTOR));
			}
			if (!isMountedRef.current) return;
			resetState();
		},
		[channelId, cleanupResources, resetState, showTooltip, i18n],
	);
	const startRecording = useCallback(async () => {
		if (isRecording || isSending || disabled) return;
		if (typeof navigator.mediaDevices === 'undefined' || typeof MediaRecorder === 'undefined') {
			showTooltip(i18n._(VOICE_RECORDING_IS_NOT_SUPPORTED_IN_THIS_BROWSER_DESCRIPTOR));
			return;
		}
		try {
			const stream = await navigator.mediaDevices.getUserMedia({audio: true});
			const preferredType = MediaRecorder.isTypeSupported('audio/ogg;codecs=opus')
				? 'audio/ogg;codecs=opus'
				: 'audio/webm;codecs=opus';
			const recorder = new MediaRecorder(stream, {mimeType: preferredType});
			chunksRef.current = [];
			recorder.addEventListener('dataavailable', handleDataAvailable);
			recorder.addEventListener('stop', () => {
				handleRecorderStop(recorder.mimeType);
			});
			recorder.start();
			mediaRecorderRef.current = recorder;
			streamRef.current = stream;
			setIsRecording(true);
			setIsLocked(false);
			setIsSending(false);
			setLockPreview(false);
			lockPreviewRef.current = false;
			recordingStartRef.current = performance.now();
			recordingDurationRef.current = 0;
			setRecordingDurationMs(0);
			previousWaveformBarsRef.current = new Array(VOICE_MESSAGE_WAVEFORM_BAR_COUNT).fill(0);
			clearRecordingTimer();
			recordingTimerRef.current = window.setInterval(updateRecordingDuration, VOICE_MESSAGE_RECORDING_TICK_MS);
			startWaveform(stream);
		} catch (error) {
			logger.error('Failed to start voice recording', error);
			showTooltip(i18n._(UNABLE_TO_START_RECORDING_PLEASE_ALLOW_MICROPHONE_ACCESS_DESCRIPTOR));
		}
	}, [
		clearRecordingTimer,
		disabled,
		handleDataAvailable,
		handleRecorderStop,
		isRecording,
		isSending,
		showTooltip,
		startWaveform,
		updateRecordingDuration,
		i18n,
	]);
	const getDurationMs = useCallback(() => {
		const start = recordingStartRef.current;
		if (!start) return 0;
		const next = Math.min(maxRecordingMs, performance.now() - start);
		recordingDurationRef.current = next;
		setRecordingDurationMs(next);
		return next;
	}, [maxRecordingMs]);
	const updateLockPreview = useCallback((clientX: number, clientY: number) => {
		const lockRect = lockIndicatorRef.current?.getBoundingClientRect();
		const pointerStart = pointerStartRef.current;
		const isInsideLockIndicator = lockRect
			? clientX >= lockRect.left && clientX <= lockRect.right && clientY >= lockRect.top && clientY <= lockRect.bottom
			: false;
		const passesGestureThreshold = pointerStart
			? pointerStart.y - clientY >= VOICE_MESSAGE_LOCK_DRAG_MIN_VERTICAL_DELTA_PX &&
				Math.abs(pointerStart.x - clientX) <= VOICE_MESSAGE_LOCK_DRAG_MAX_HORIZONTAL_DELTA_PX
			: false;
		const next = isInsideLockIndicator || passesGestureThreshold;
		if (lockPreviewRef.current === next) return;
		lockPreviewRef.current = next;
		setLockPreview(next);
	}, []);
	const setLockIndicatorRefs = useCallback(
		(element: HTMLDivElement | null) => {
			lockIndicatorRef.current = element;
			lockIndicatorRefs.setFloating(element);
		},
		[lockIndicatorRefs],
	);
	const handlePointerDown = useCallback(
		(event: React.PointerEvent<HTMLButtonElement>) => {
			if (disabled || isSending || isRecording) return;
			event.preventDefault();
			pointerIdRef.current = event.pointerId;
			pointerStartRef.current = {x: event.clientX, y: event.clientY};
			event.currentTarget.setPointerCapture(event.pointerId);
			showTooltip(i18n._(HOLD_TO_RECORD_DRAG_UP_TO_LOCK_OR_DESCRIPTOR));
			void startRecording();
		},
		[disabled, isRecording, isSending, showTooltip, startRecording, i18n],
	);
	const handlePointerMove = useCallback(
		(event: React.PointerEvent<HTMLButtonElement>) => {
			if (!isRecording || isLocked || pointerIdRef.current !== event.pointerId) return;
			updateLockPreview(event.clientX, event.clientY);
		},
		[isLocked, isRecording, updateLockPreview],
	);
	const handlePointerUp = useCallback(
		(event: React.PointerEvent<HTMLButtonElement>) => {
			if (pointerIdRef.current !== event.pointerId) return;
			event.preventDefault();
			try {
				event.currentTarget.releasePointerCapture(event.pointerId);
			} catch {}
			pointerIdRef.current = null;
			updateLockPreview(event.clientX, event.clientY);
			if (!isRecording || isSending) {
				pointerStartRef.current = null;
				return;
			}
			if (lockPreviewRef.current && !isLocked) {
				setIsLocked(true);
				setLockPreview(false);
				lockPreviewRef.current = false;
				pointerStartRef.current = null;
				return;
			}
			if (isLocked) {
				pointerStartRef.current = null;
				return;
			}
			const durationMs = getDurationMs();
			const shouldSend = durationMs >= VOICE_MESSAGE_MIN_SEND_DURATION_MS;
			stopRecording(shouldSend ? 'send' : 'discard');
		},
		[getDurationMs, isLocked, isRecording, isSending, stopRecording, updateLockPreview],
	);
	const handlePointerCancel = useCallback(
		(event: React.PointerEvent<HTMLButtonElement>) => {
			if (pointerIdRef.current !== event.pointerId) return;
			pointerIdRef.current = null;
			pointerStartRef.current = null;
			if (!isRecording || isSending || isLocked) return;
			const durationMs = getDurationMs();
			const shouldSend = durationMs >= VOICE_MESSAGE_MIN_SEND_DURATION_MS;
			stopRecording(shouldSend ? 'send' : 'discard');
		},
		[getDurationMs, isLocked, isRecording, isSending, stopRecording],
	);
	const handleDiscardClick = useCallback(() => {
		if (!isRecording || isSending) return;
		stopRecording('discard');
	}, [isRecording, isSending, stopRecording]);
	const handleSendClick = useCallback(() => {
		if (!isRecording || isSending) return;
		const durationMs = getDurationMs();
		const shouldSend = durationMs >= VOICE_MESSAGE_MIN_SEND_DURATION_MS;
		stopRecording(shouldSend ? 'send' : 'discard');
	}, [getDurationMs, isRecording, isSending, stopRecording]);
	useEffect(() => {
		isMountedRef.current = true;
		return () => {
			isMountedRef.current = false;
			clearTooltipTimer();
			clearRecordingTimer();
			stopWaveformUpdates();
			releaseStream();
			if (mediaRecorderRef.current && mediaRecorderRef.current.state !== 'inactive') {
				stopActionRef.current = 'discard';
				mediaRecorderRef.current.stop();
			}
			mediaRecorderRef.current = null;
		};
	}, [clearRecordingTimer, clearTooltipTimer, releaseStream, stopWaveformUpdates]);
	useEffect(() => {
		if (!disabled) return;
		if (isRecording && !isSending) {
			stopRecording('discard');
		}
	}, [disabled, isRecording, isSending, stopRecording]);
	useEffect(() => {
		const anchorElement = tooltipAnchorRef?.current ?? buttonWrapperRef.current;
		tooltipRefs.setReference(anchorElement);
	}, [tooltipAnchorRef, tooltipRefs]);
	return (
		<>
			<div
				className={styles.voiceButtonWrapper}
				ref={buttonWrapperRef}
				data-flx="channel.voice-message-recorder.voice-button-wrapper"
			>
				<motion.button
					type="button"
					className={styles.voiceButton}
					onPointerDown={handlePointerDown}
					onPointerUp={handlePointerUp}
					onPointerMove={handlePointerMove}
					onPointerCancel={handlePointerCancel}
					aria-label={i18n._(VOICE_MESSAGE_DESCRIPTOR)}
					disabled={disabled || isSending}
					animate={voiceButtonAnimate}
					transition={resolvedVoiceButtonTransition}
					data-flx="channel.voice-message-recorder.button.pointer-down"
				>
					<MicrophoneIcon
						className={styles.voiceButtonIcon}
						data-flx="channel.voice-message-recorder.microphone-icon"
					/>
				</motion.button>
				<FloatingPortal root={portalHost ?? undefined} data-flx="channel.voice-message-recorder.floating-portal">
					<AnimatePresence mode="wait" data-flx="channel.voice-message-recorder.animate-presence">
						{tooltipText && (
							<motion.div
								key={tooltipText}
								ref={tooltipRefs.setFloating}
								className={styles.holdTooltip}
								style={{
									...tooltipFloatingStyles,
									visibility: isTooltipPositioned ? 'visible' : 'hidden',
								}}
								initial={resolvedTooltipMotion.initial}
								animate={resolvedTooltipMotion.animate}
								exit={resolvedTooltipMotion.exit}
								transition={resolvedTooltipMotion.transition}
								data-flx="channel.voice-message-recorder.hold-tooltip"
							>
								{tooltipText}
							</motion.div>
						)}
					</AnimatePresence>
				</FloatingPortal>
			</div>
			<AnimatePresence data-flx="channel.voice-message-recorder.animate-presence--2">
				{showOverlay && (
					<motion.div
						className={styles.recordingOverlay}
						initial={resolvedOverlayMotion.initial}
						animate={resolvedOverlayMotion.animate}
						exit={resolvedOverlayMotion.exit}
						transition={resolvedOverlayMotion.transition}
						data-flx="channel.voice-message-recorder.recording-overlay"
					>
						<div className={styles.overlayInner} data-flx="channel.voice-message-recorder.overlay-inner">
							<button
								type="button"
								className={clsx(styles.iconButton, styles.trashButton)}
								onClick={handleDiscardClick}
								disabled={isSending}
								aria-label={i18n._(DISCARD_VOICE_MESSAGE_DESCRIPTOR)}
								data-flx="channel.voice-message-recorder.icon-button.discard-click"
							>
								<TrashIcon weight="fill" data-flx="channel.voice-message-recorder.trash-icon" />
							</button>
							<div className={styles.recordingControls} data-flx="channel.voice-message-recorder.recording-controls">
								<div className={styles.recordingBar} data-flx="channel.voice-message-recorder.recording-bar">
									<span className={styles.recordingDot} data-flx="channel.voice-message-recorder.recording-dot" />
									<span className={styles.timer} data-flx="channel.voice-message-recorder.timer">
										{formattedDuration}
									</span>
									<div className={styles.waveform} data-flx="channel.voice-message-recorder.waveform">
										{waveformStyles.map((style, index) => (
											<span
												key={`waveform-${index}`}
												className={styles.waveformBar}
												style={style}
												data-flx="channel.voice-message-recorder.waveform-bar"
											/>
										))}
									</div>
								</div>
								<div
									className={styles.lockIndicatorWrapper}
									data-flx="channel.voice-message-recorder.lock-indicator-wrapper"
								>
									<button
										ref={lockIndicatorRefs.setReference}
										type="button"
										className={clsx(styles.iconButton, styles.sendButton)}
										onClick={handleSendClick}
										disabled={isSending}
										aria-label={i18n._(SEND_VOICE_MESSAGE_DESCRIPTOR)}
										data-flx="channel.voice-message-recorder.icon-button.send-click"
									>
										<PaperPlaneRightIcon
											weight="fill"
											data-flx="channel.voice-message-recorder.paper-plane-right-icon"
										/>
									</button>
								</div>
							</div>
						</div>
					</motion.div>
				)}
			</AnimatePresence>
			<FloatingPortal root={portalHost ?? undefined} data-flx="channel.voice-message-recorder.floating-portal--2">
				{isRecording && !isSending && (
					<div
						ref={setLockIndicatorRefs}
						className={clsx(styles.lockIndicator, showLockedIndicator && styles.lockIndicatorLocked)}
						style={{
							...lockIndicatorFloatingStyles,
							visibility: isLockIndicatorPositioned ? 'visible' : 'hidden',
						}}
						data-flx="channel.voice-message-recorder.lock-indicator"
					>
						{showLockedIndicator ? (
							<LockSimpleIcon
								weight="fill"
								className={styles.lockIndicatorIcon}
								data-flx="channel.voice-message-recorder.lock-indicator-icon"
							/>
						) : (
							<>
								<LockSimpleOpenIcon
									weight="fill"
									className={styles.lockIndicatorIcon}
									data-flx="channel.voice-message-recorder.lock-indicator-icon--2"
								/>
								<CaretUpIcon
									weight="bold"
									className={styles.lockIndicatorCaret}
									data-flx="channel.voice-message-recorder.lock-indicator-caret"
								/>
							</>
						)}
					</div>
				)}
			</FloatingPortal>
		</>
	);
}
