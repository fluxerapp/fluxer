// SPDX-License-Identifier: AGPL-3.0-or-later

import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {SOMETHING_WENT_WRONG_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {AudioWaveform, computePeaks} from '@app/features/voice/components/AudioWaveform';
import {LiveRecordingWaveform} from '@app/features/voice/components/LiveRecordingWaveform';
import styles from '@app/features/voice/components/VoiceMessageComposerModal.module.css';
import {encodeAudioBufferSliceToWav} from '@app/features/voice/utils/AudioWavEncode';
import {computeVoiceWaveformFromAudioBuffer} from '@app/features/voice/utils/VoiceMessageRecordingUtils';
import {sendVoiceMessage} from '@app/features/voice/utils/VoiceMessageSendUtils';
import {VOICE_MESSAGE_MIN_SEND_DURATION_MS} from '@fluxer/constants/src/VoiceMessageConstants';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	ArrowCounterClockwiseIcon,
	MicrophoneIcon,
	PaperPlaneRightIcon,
	PauseIcon,
	PlayIcon,
	StopIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const COMPOSER_TITLE_DESCRIPTOR = msg({
	message: 'Voice message',
	comment: 'Title of the desktop voice message composer modal.',
});
const RECORDING_HINT_DESCRIPTOR = msg({
	message: 'Speak now. Press Stop when you are done — you can trim afterwards.',
	comment: 'Help text shown while recording a voice message on desktop.',
});
const REVIEW_HINT_DESCRIPTOR = msg({
	message: 'Drag the handles to trim, then press Send.',
	comment: 'Help text shown after recording while reviewing/trimming a voice message on desktop.',
});
const MIC_PERMISSION_DENIED_DESCRIPTOR = msg({
	message: 'Microphone access was denied. Enable it in your system settings and try again.',
	comment: 'Error shown when the user denies microphone access for voice message recording.',
});
const RECORDING_NOT_SUPPORTED_DESCRIPTOR = msg({
	message: 'Voice recording is not supported in this browser.',
	comment: 'Error shown when MediaRecorder is unavailable.',
});
const RECORDING_FAILED_DESCRIPTOR = msg({
	message: 'Recording failed. Try again.',
	comment: 'Toast shown when MediaRecorder produced no usable audio.',
});
const SEND_FAILED_DESCRIPTOR = msg({
	message: 'Unable to send voice message. Try again.',
	comment: 'Toast shown when the voice message send pipeline fails.',
});
const TOO_SHORT_DESCRIPTOR = msg({
	message: 'Selection must be at least {seconds}s.',
	comment: 'Error shown when the trimmed voice message is below the server minimum duration.',
});
const STOP_BUTTON_DESCRIPTOR = msg({
	message: 'Stop',
	comment: 'Stop-recording button label in the desktop voice message composer.',
});
const RECORD_BUTTON_DESCRIPTOR = msg({
	message: 'Start recording',
	comment: 'Button label shown before recording starts.',
});
const REDO_BUTTON_DESCRIPTOR = msg({
	message: 'Re-record',
	comment: 'Button label that discards the current recording and restarts.',
});
const SEND_BUTTON_DESCRIPTOR = msg({
	message: 'Send',
	comment: 'Send-message button label in the voice message composer modal.',
});
const CANCEL_DESCRIPTOR = msg({
	message: 'Cancel',
	comment: 'Cancel button label in the voice message composer modal.',
});
const PLAY_DESCRIPTOR = msg({
	message: 'Play',
	comment: 'Play-preview button label in the voice message composer modal.',
});
const PAUSE_DESCRIPTOR = msg({
	message: 'Pause',
	comment: 'Pause-preview button label in the voice message composer modal.',
});

const logger = new Logger('VoiceMessageComposerModal');

const MAX_LIVE_BARS = 600;
const ANALYSER_SAMPLE_INTERVAL_MS = 60;
const MIN_DURATION_SECONDS = VOICE_MESSAGE_MIN_SEND_DURATION_MS / 1000;
const PEAK_BIN_COUNT = 600;

type ModalStage = 'idle' | 'permission_error' | 'recording' | 'reviewing';

interface VoiceMessageComposerModalProps {
	channelId: string;
}

function pickRecorderMime(): string | undefined {
	if (typeof MediaRecorder === 'undefined') return undefined;
	const candidates = ['audio/webm;codecs=opus', 'audio/webm', 'audio/mp4'];
	for (const candidate of candidates) {
		if (MediaRecorder.isTypeSupported(candidate)) return candidate;
	}
	return undefined;
}

function formatSeconds(value: number): string {
	const safe = Math.max(0, value);
	return `${safe.toFixed(2)}s`;
}

function formatElapsedMs(value: number): string {
	const total = Math.max(0, Math.floor(value / 1000));
	const minutes = Math.floor(total / 60);
	const seconds = total % 60;
	const tenths = Math.floor((value % 1000) / 100);
	return `${minutes.toString().padStart(1, '0')}:${seconds.toString().padStart(2, '0')}.${tenths}`;
}

export const VoiceMessageComposerModal: React.FC<VoiceMessageComposerModalProps> = observer(({channelId}) => {
	const {i18n} = useLingui();
	const [stage, setStage] = useState<ModalStage>('idle');
	const [errorMessage, setErrorMessage] = useState<string | null>(null);
	const [amplitudes, setAmplitudes] = useState<Array<number>>([]);
	const [elapsedMs, setElapsedMs] = useState(0);
	const [audioBuffer, setAudioBuffer] = useState<AudioBuffer | null>(null);
	const [recordedBlob, setRecordedBlob] = useState<Blob | null>(null);
	const [startSeconds, setStartSeconds] = useState(0);
	const [endSeconds, setEndSeconds] = useState(0);
	const [playheadSeconds, setPlayheadSeconds] = useState<number | null>(null);
	const [isPlaying, setIsPlaying] = useState(false);

	const mediaStreamRef = useRef<MediaStream | null>(null);
	const mediaRecorderRef = useRef<MediaRecorder | null>(null);
	const recordedChunksRef = useRef<Array<Blob>>([]);
	const recordingStartedAtRef = useRef<number>(0);
	const analyserAudioCtxRef = useRef<AudioContext | null>(null);
	const analyserNodeRef = useRef<AnalyserNode | null>(null);
	const analyserBufferRef = useRef<Uint8Array<ArrayBuffer> | null>(null);
	const analyserLastSampleAtRef = useRef<number>(0);
	const recordingRafRef = useRef<number | null>(null);
	const playbackCtxRef = useRef<AudioContext | null>(null);
	const playbackSourceRef = useRef<AudioBufferSourceNode | null>(null);
	const playbackStartedAtRef = useRef<number>(0);
	const playbackOffsetRef = useRef<number>(0);
	const rafRef = useRef<number | null>(null);
	const blobResolversRef = useRef<{resolve: (blob: Blob) => void; reject: (err: unknown) => void} | null>(null);
	const sentRef = useRef(false);
	const initialRecordingStartedRef = useRef(false);

	const stopRecordingFrameLoop = useCallback(() => {
		if (recordingRafRef.current === null) return;
		cancelAnimationFrame(recordingRafRef.current);
		recordingRafRef.current = null;
	}, []);

	const stopAnalyser = useCallback(() => {
		try {
			analyserNodeRef.current?.disconnect();
		} catch {}
		analyserNodeRef.current = null;
		analyserBufferRef.current = null;
		analyserLastSampleAtRef.current = 0;
		const ctx = analyserAudioCtxRef.current;
		analyserAudioCtxRef.current = null;
		if (ctx) void ctx.close().catch(() => {});
	}, []);

	const stopMediaStream = useCallback(() => {
		const stream = mediaStreamRef.current;
		mediaStreamRef.current = null;
		if (stream) {
			for (const track of stream.getTracks()) {
				try {
					track.stop();
				} catch {}
			}
		}
	}, []);

	const stopPlayback = useCallback(() => {
		const node = playbackSourceRef.current;
		if (node) {
			try {
				node.stop();
			} catch {}
			try {
				node.disconnect();
			} catch {}
		}
		playbackSourceRef.current = null;
		if (rafRef.current != null) {
			cancelAnimationFrame(rafRef.current);
			rafRef.current = null;
		}
		setIsPlaying(false);
		setPlayheadSeconds(null);
	}, []);

	useEffect(() => {
		return () => {
			stopAnalyser();
			stopRecordingFrameLoop();
			stopMediaStream();
			stopPlayback();
			try {
				mediaRecorderRef.current?.stop();
			} catch {}
			mediaRecorderRef.current = null;
			const playCtx = playbackCtxRef.current;
			playbackCtxRef.current = null;
			if (playCtx) void playCtx.close().catch(() => {});
		};
	}, [stopAnalyser, stopMediaStream, stopPlayback, stopRecordingFrameLoop]);

	const close = useCallback(() => {
		ModalCommands.popByType(VoiceMessageComposerModal);
	}, []);
	const showComposerErrorModal = useCallback(
		(message: MessageDescriptor, dataFlx: string) => {
			showGenericErrorModal({
				title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
				message: () => i18n._(message),
				dataFlx,
			});
		},
		[i18n],
	);

	const updateLiveAmplitudes = useCallback(() => {
		const analyser = analyserNodeRef.current;
		const buffer = analyserBufferRef.current;
		if (!analyser || !buffer) return;
		analyser.getByteTimeDomainData(buffer);
		let sumSquares = 0;
		for (let i = 0; i < buffer.length; i++) {
			const normalised = (buffer[i]! - 128) / 128;
			sumSquares += normalised * normalised;
		}
		const rms = Math.sqrt(sumSquares / buffer.length);
		const amp = Math.min(1, rms * 2.5);
		setAmplitudes((prev) => {
			const next = prev.length >= MAX_LIVE_BARS ? prev.slice(-MAX_LIVE_BARS + 1) : prev.slice();
			next.push(amp);
			return next;
		});
	}, []);

	const runRecordingFrameLoop = useCallback(
		(now: number) => {
			recordingRafRef.current = null;
			setElapsedMs(now - recordingStartedAtRef.current);
			if (now - analyserLastSampleAtRef.current >= ANALYSER_SAMPLE_INTERVAL_MS) {
				analyserLastSampleAtRef.current = now;
				updateLiveAmplitudes();
			}
			recordingRafRef.current = requestAnimationFrame(runRecordingFrameLoop);
		},
		[updateLiveAmplitudes],
	);

	const startRecordingFrameLoop = useCallback(() => {
		stopRecordingFrameLoop();
		analyserLastSampleAtRef.current = 0;
		recordingRafRef.current = requestAnimationFrame(runRecordingFrameLoop);
	}, [runRecordingFrameLoop, stopRecordingFrameLoop]);

	const startRecording = useCallback(async () => {
		setErrorMessage(null);
		const mimeType = pickRecorderMime();
		if (!mimeType || typeof navigator === 'undefined' || !navigator.mediaDevices?.getUserMedia) {
			setStage('permission_error');
			setErrorMessage(i18n._(RECORDING_NOT_SUPPORTED_DESCRIPTOR));
			return;
		}
		try {
			const stream = await navigator.mediaDevices.getUserMedia({audio: true});
			mediaStreamRef.current = stream;
			const Ctor =
				window.AudioContext ||
				(window as typeof window & {webkitAudioContext?: typeof AudioContext}).webkitAudioContext;
			if (Ctor) {
				const audioContext = new Ctor();
				analyserAudioCtxRef.current = audioContext;
				const source = audioContext.createMediaStreamSource(stream);
				const analyser = audioContext.createAnalyser();
				analyser.fftSize = 1024;
				source.connect(analyser);
				analyserNodeRef.current = analyser;
				analyserBufferRef.current = new Uint8Array(new ArrayBuffer(analyser.fftSize));
			}
			const recorder = new MediaRecorder(stream, {mimeType});
			recordedChunksRef.current = [];
			recorder.ondataavailable = (event) => {
				if (event.data && event.data.size > 0) recordedChunksRef.current.push(event.data);
			};
			recorder.onstop = () => {
				const blob = new Blob(recordedChunksRef.current, {type: mimeType});
				recordedChunksRef.current = [];
				const resolver = blobResolversRef.current;
				blobResolversRef.current = null;
				resolver?.resolve(blob);
			};
			recorder.onerror = (event) => {
				const resolver = blobResolversRef.current;
				blobResolversRef.current = null;
				resolver?.reject(event);
			};
			mediaRecorderRef.current = recorder;
			recorder.start();
			recordingStartedAtRef.current = performance.now();
			setElapsedMs(0);
			setAmplitudes([]);
			startRecordingFrameLoop();
			setStage('recording');
		} catch (error) {
			logger.warn('Failed to start mic capture', {error});
			stopRecordingFrameLoop();
			stopAnalyser();
			stopMediaStream();
			setStage('permission_error');
			setErrorMessage(i18n._(MIC_PERMISSION_DENIED_DESCRIPTOR));
		}
	}, [i18n, startRecordingFrameLoop, stopAnalyser, stopMediaStream, stopRecordingFrameLoop]);

	const waitForRecording = useCallback((): Promise<Blob> => {
		return new Promise<Blob>((resolve, reject) => {
			blobResolversRef.current = {resolve, reject};
		});
	}, []);

	const decodeBlob = useCallback(async (blob: Blob): Promise<AudioBuffer | null> => {
		const Ctor =
			window.AudioContext || (window as typeof window & {webkitAudioContext?: typeof AudioContext}).webkitAudioContext;
		if (!Ctor) return null;
		const ctx = playbackCtxRef.current ?? new Ctor();
		playbackCtxRef.current = ctx;
		const arrayBuffer = await blob.arrayBuffer();
		try {
			return await ctx.decodeAudioData(arrayBuffer.slice(0));
		} catch (error) {
			logger.warn('Failed to decode recorded audio', {error});
			return null;
		}
	}, []);

	const stopRecording = useCallback(async () => {
		const recorder = mediaRecorderRef.current;
		if (!recorder) return;
		const blobPromise = waitForRecording();
		try {
			recorder.stop();
		} catch (error) {
			logger.warn('MediaRecorder.stop failed', {error});
			const resolver = blobResolversRef.current;
			blobResolversRef.current = null;
			resolver?.reject(error);
		}
		stopRecordingFrameLoop();
		stopAnalyser();
		try {
			const blob = await blobPromise;
			stopMediaStream();
			if (!blob || blob.size === 0) {
				showComposerErrorModal(
					RECORDING_FAILED_DESCRIPTOR,
					'voice.voice-message-composer-modal.empty-recording-error-modal',
				);
				setStage('idle');
				setAmplitudes([]);
				return;
			}
			const decoded = await decodeBlob(blob);
			if (!decoded) {
				showComposerErrorModal(
					RECORDING_FAILED_DESCRIPTOR,
					'voice.voice-message-composer-modal.decode-recording-error-modal',
				);
				setStage('idle');
				return;
			}
			setRecordedBlob(blob);
			setAudioBuffer(decoded);
			setStartSeconds(0);
			setEndSeconds(decoded.duration);
			setStage('reviewing');
		} catch (error) {
			logger.warn('MediaRecorder failed mid-stop', {error});
			stopMediaStream();
			showComposerErrorModal(
				RECORDING_FAILED_DESCRIPTOR,
				'voice.voice-message-composer-modal.stop-recording-error-modal',
			);
			setStage('idle');
		}
		mediaRecorderRef.current = null;
	}, [
		waitForRecording,
		stopRecordingFrameLoop,
		stopAnalyser,
		stopMediaStream,
		decodeBlob,
		i18n,
		showComposerErrorModal,
	]);

	const restartRecording = useCallback(async () => {
		stopPlayback();
		setRecordedBlob(null);
		setAudioBuffer(null);
		setAmplitudes([]);
		setElapsedMs(0);
		setStage('idle');
		await startRecording();
	}, [stopPlayback, startRecording]);

	const peaks = useMemo(() => (audioBuffer ? computePeaks(audioBuffer, PEAK_BIN_COUNT) : null), [audioBuffer]);
	const selectionDuration = Math.max(0, endSeconds - startSeconds);
	const totalDuration = audioBuffer?.duration ?? 0;

	const handleSelectionChange = useCallback(
		(next: {startSeconds: number; endSeconds: number}) => {
			stopPlayback();
			setStartSeconds(next.startSeconds);
			setEndSeconds(next.endSeconds);
			setErrorMessage(null);
		},
		[stopPlayback],
	);

	const tickPlayback = useCallback(() => {
		const ctx = playbackCtxRef.current;
		if (!ctx) return;
		const elapsed = ctx.currentTime - playbackStartedAtRef.current;
		const next = playbackOffsetRef.current + elapsed;
		if (next >= endSeconds) {
			stopPlayback();
			return;
		}
		setPlayheadSeconds(next);
		rafRef.current = requestAnimationFrame(tickPlayback);
	}, [endSeconds, stopPlayback]);

	const startPlayback = useCallback(() => {
		const ctx = playbackCtxRef.current;
		if (!ctx || !audioBuffer) return;
		stopPlayback();
		const node = ctx.createBufferSource();
		node.buffer = audioBuffer;
		node.connect(ctx.destination);
		playbackSourceRef.current = node;
		playbackOffsetRef.current = startSeconds;
		playbackStartedAtRef.current = ctx.currentTime;
		node.onended = () => {
			if (playbackSourceRef.current === node) stopPlayback();
		};
		try {
			node.start(0, startSeconds, Math.max(0.001, endSeconds - startSeconds));
			setIsPlaying(true);
			setPlayheadSeconds(startSeconds);
			rafRef.current = requestAnimationFrame(tickPlayback);
		} catch (error) {
			logger.warn('Failed to start playback', {error});
			stopPlayback();
		}
	}, [audioBuffer, startSeconds, endSeconds, stopPlayback, tickPlayback]);

	const togglePlayback = useCallback(() => {
		if (isPlaying) stopPlayback();
		else startPlayback();
	}, [isPlaying, startPlayback, stopPlayback]);

	const send = useCallback(() => {
		if (!audioBuffer || !recordedBlob || sentRef.current) return;
		if (selectionDuration < MIN_DURATION_SECONDS) {
			setErrorMessage(i18n._(TOO_SHORT_DESCRIPTOR, {seconds: MIN_DURATION_SECONDS.toFixed(1)}));
			return;
		}
		sentRef.current = true;
		stopPlayback();
		const outputBlob = encodeAudioBufferSliceToWav(audioBuffer, {startSeconds, endSeconds, downmixToMono: true});
		const outputFilename = 'voice-message.wav';
		const file = new File([outputBlob], outputFilename, {type: outputBlob.type});
		const {duration, waveform} = computeVoiceWaveformFromAudioBuffer(audioBuffer, startSeconds, endSeconds);
		void sendVoiceMessage({channelId, file, waveform, duration, title: outputFilename}).catch((error) => {
			logger.error({error}, 'Failed to send voice message');
			showComposerErrorModal(SEND_FAILED_DESCRIPTOR, 'voice.voice-message-composer-modal.send-error-modal');
		});
		close();
	}, [
		audioBuffer,
		recordedBlob,
		selectionDuration,
		startSeconds,
		endSeconds,
		channelId,
		close,
		i18n,
		stopPlayback,
		showComposerErrorModal,
	]);

	useEffect(() => {
		if (initialRecordingStartedRef.current) return;
		initialRecordingStartedRef.current = true;
		void startRecording();
	}, [startRecording]);

	return (
		<Modal.Root size="small" centered data-flx="voice.voice-message-composer-modal.modal-root">
			<Modal.Header
				title={i18n._(COMPOSER_TITLE_DESCRIPTOR)}
				onClose={close}
				data-flx="voice.voice-message-composer-modal.modal-header"
			/>
			<Modal.Content padding="default" data-flx="voice.voice-message-composer-modal.modal-content">
				<div className={styles.body} data-flx="voice.voice-message-composer-modal.body">
					<p className={styles.helpText} data-flx="voice.voice-message-composer-modal.help-text">
						{stage === 'reviewing' ? i18n._(REVIEW_HINT_DESCRIPTOR) : i18n._(RECORDING_HINT_DESCRIPTOR)}
					</p>
					{stage === 'permission_error' && errorMessage ? (
						<p className={styles.errorText} data-flx="voice.voice-message-composer-modal.permission-error">
							{errorMessage}
						</p>
					) : null}
					{stage === 'idle' || stage === 'recording' ? (
						<>
							<LiveRecordingWaveform
								amplitudes={amplitudes}
								data-flx="voice.voice-message-composer-modal.live-recording-waveform"
							/>
							{stage === 'recording' ? (
								<div className={styles.meta} data-flx="voice.voice-message-composer-modal.recording-meta">
									<span
										className={styles.recordingStatus}
										data-flx="voice.voice-message-composer-modal.recording-status"
									>
										<Trans>Recording</Trans>
									</span>
									<span data-flx="voice.voice-message-composer-modal.span">{formatElapsedMs(elapsedMs)}</span>
								</div>
							) : null}
						</>
					) : null}
					{stage === 'reviewing' && peaks && audioBuffer ? (
						<>
							<AudioWaveform
								peaks={peaks}
								durationSeconds={totalDuration}
								startSeconds={startSeconds}
								endSeconds={endSeconds}
								minSelectionSeconds={MIN_DURATION_SECONDS}
								maxSelectionSeconds={totalDuration}
								playheadSeconds={playheadSeconds}
								onSelectionChange={handleSelectionChange}
								data-flx="voice.voice-message-composer-modal.waveform"
							/>
							<div className={styles.meta} data-flx="voice.voice-message-composer-modal.meta">
								<span data-flx="voice.voice-message-composer-modal.span--2">
									<Trans>Selection: {formatSeconds(selectionDuration)}</Trans>
								</span>
								<span data-flx="voice.voice-message-composer-modal.span--3">
									{formatSeconds(startSeconds)} → {formatSeconds(endSeconds)}
								</span>
							</div>
							<div className={styles.controls} data-flx="voice.voice-message-composer-modal.controls">
								<Button
									variant="secondary"
									small
									onClick={togglePlayback}
									data-flx="voice.voice-message-composer-modal.play-button"
								>
									<span className={styles.buttonInner} data-flx="voice.voice-message-composer-modal.button-inner">
										{isPlaying ? (
											<PauseIcon
												size={remFromPx(14)}
												weight="fill"
												data-flx="voice.voice-message-composer-modal.pause-icon"
											/>
										) : (
											<PlayIcon
												size={remFromPx(14)}
												weight="fill"
												data-flx="voice.voice-message-composer-modal.play-icon"
											/>
										)}
										{isPlaying ? i18n._(PAUSE_DESCRIPTOR) : i18n._(PLAY_DESCRIPTOR)}
									</span>
								</Button>
								<Button
									variant="secondary"
									small
									onClick={() => void restartRecording()}
									data-flx="voice.voice-message-composer-modal.redo-button"
								>
									<span className={styles.buttonInner} data-flx="voice.voice-message-composer-modal.button-inner--2">
										<ArrowCounterClockwiseIcon
											size={remFromPx(14)}
											weight="bold"
											data-flx="voice.voice-message-composer-modal.arrow-counter-clockwise-icon"
										/>
										{i18n._(REDO_BUTTON_DESCRIPTOR)}
									</span>
								</Button>
							</div>
							{errorMessage ? (
								<p className={styles.errorText} data-flx="voice.voice-message-composer-modal.review-error">
									{errorMessage}
								</p>
							) : null}
						</>
					) : null}
				</div>
			</Modal.Content>
			<Modal.Footer data-flx="voice.voice-message-composer-modal.modal-footer">
				<Button variant="secondary" onClick={close} data-flx="voice.voice-message-composer-modal.cancel-button">
					{i18n._(CANCEL_DESCRIPTOR)}
				</Button>
				{stage === 'recording' ? (
					<Button
						variant="primary"
						onClick={() => void stopRecording()}
						data-flx="voice.voice-message-composer-modal.stop-button"
					>
						<span className={styles.buttonInner} data-flx="voice.voice-message-composer-modal.button-inner--3">
							<StopIcon size={remFromPx(14)} weight="fill" data-flx="voice.voice-message-composer-modal.stop-icon" />
							{i18n._(STOP_BUTTON_DESCRIPTOR)}
						</span>
					</Button>
				) : stage === 'reviewing' ? (
					<Button
						variant="primary"
						onClick={() => void send()}
						disabled={selectionDuration < MIN_DURATION_SECONDS}
						data-flx="voice.voice-message-composer-modal.send-button"
					>
						<span className={styles.buttonInner} data-flx="voice.voice-message-composer-modal.button-inner--4">
							<PaperPlaneRightIcon
								size={remFromPx(14)}
								weight="fill"
								data-flx="voice.voice-message-composer-modal.paper-plane-right-icon"
							/>
							{i18n._(SEND_BUTTON_DESCRIPTOR)}
						</span>
					</Button>
				) : stage === 'permission_error' ? (
					<Button
						variant="primary"
						onClick={() => void startRecording()}
						data-flx="voice.voice-message-composer-modal.retry-button"
					>
						<span className={styles.buttonInner} data-flx="voice.voice-message-composer-modal.button-inner--5">
							<MicrophoneIcon
								size={remFromPx(14)}
								weight="fill"
								data-flx="voice.voice-message-composer-modal.microphone-icon"
							/>
							{i18n._(RECORD_BUTTON_DESCRIPTOR)}
						</span>
					</Button>
				) : null}
			</Modal.Footer>
		</Modal.Root>
	);
});

VoiceMessageComposerModal.displayName = 'VoiceMessageComposerModal';

export function openVoiceMessageComposerModal(channelId: string): void {
	ModalCommands.push(
		ModalCommands.modal(() => (
			<VoiceMessageComposerModal
				channelId={channelId}
				data-flx="voice.voice-message-composer-modal.open-voice-message-composer-modal.voice-message-composer-modal"
			/>
		)),
	);
}
