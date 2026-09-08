// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Spinner} from '@app/features/ui/components/Spinner';
import {AudioWaveform, computePeaks} from '@app/features/voice/components/AudioWaveform';
import styles from '@app/features/voice/components/EntranceSoundTrimmerModal.module.css';
import {encodeAudioBufferSliceToWav} from '@app/features/voice/utils/AudioWavEncode';
import {
	ENTRANCE_SOUND_MAX_BYTES,
	ENTRANCE_SOUND_MAX_DURATION_MS,
	ENTRANCE_SOUND_MIN_DURATION_MS,
} from '@fluxer/constants/src/EntranceSoundConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {PauseIcon, PlayIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const TRIMMER_TITLE_DESCRIPTOR = msg({
	message: 'Trim entrance sound',
	comment: 'Title of the modal that lets the user crop a long audio file down to the entrance sound length cap.',
});
const TRIMMER_HELP_DESCRIPTOR = msg({
	message: 'Drag the handles to choose up to 5.2 seconds of audio. The selected region is what gets uploaded.',
	comment: 'Help text in the entrance sound trimmer modal explaining how the selection works.',
});
const CANCEL_DESCRIPTOR = msg({
	message: 'Cancel',
	comment: 'Cancel button label in the entrance sound trimmer modal.',
});
const USE_SELECTION_DESCRIPTOR = msg({
	message: 'Use selection',
	comment: 'Primary action button in the trimmer modal: accept the current selection and upload it.',
});
const PLAY_SELECTION_DESCRIPTOR = msg({
	message: 'Play selection',
	comment: 'Button label in the trimmer modal. Plays the currently selected portion.',
});
const PAUSE_SELECTION_DESCRIPTOR = msg({
	message: 'Pause',
	comment: 'Button label in the trimmer modal that pauses preview playback.',
});
const SELECTION_TOO_SHORT_DESCRIPTOR = msg({
	message: 'Selection must be at least {seconds} seconds.',
	comment: 'Error displayed when the trimmed entrance sound selection is below the minimum duration.',
});
const ENCODED_TOO_LARGE_DESCRIPTOR = msg({
	message: 'Trimmed clip is over the {limit} size limit. Try a shorter selection.',
	comment: 'Error displayed when the encoded trimmed audio exceeds the entrance sound size cap.',
});
const COULD_NOT_DECODE_DESCRIPTOR = msg({
	message: 'Could not decode this audio file.',
	comment: 'Error displayed when the browser fails to decode an audio file selected for entrance-sound upload.',
});

const logger = new Logger('EntranceSoundTrimmerModal');

const MAX_DURATION_SECONDS = ENTRANCE_SOUND_MAX_DURATION_MS / 1000;
const MIN_DURATION_SECONDS = ENTRANCE_SOUND_MIN_DURATION_MS / 1000;
const PEAK_BIN_COUNT = 600;

export interface TrimmedAudioResult {
	blob: Blob;
	startSeconds: number;
	endSeconds: number;
	durationMs: number;
	sampleRate: number;
}

interface EntranceSoundTrimmerModalProps {
	sourceFile: File;
	onConfirm: (result: TrimmedAudioResult) => Promise<void> | void;
}

function formatSeconds(value: number): string {
	const safe = Math.max(0, value);
	return `${safe.toFixed(2)}s`;
}

function formatBytes(bytes: number): string {
	if (bytes >= 1024 * 1024) {
		const mb = bytes / (1024 * 1024);
		return `${mb % 1 === 0 ? mb.toFixed(0) : mb.toFixed(1)}MB`;
	}
	return `${Math.floor(bytes / 1024)}KB`;
}

export const EntranceSoundTrimmerModal: React.FC<EntranceSoundTrimmerModalProps> = observer(
	({sourceFile, onConfirm}) => {
		const {i18n} = useLingui();
		const [audioBuffer, setAudioBuffer] = useState<AudioBuffer | null>(null);
		const [decodeError, setDecodeError] = useState<string | null>(null);
		const [startSeconds, setStartSeconds] = useState(0);
		const [endSeconds, setEndSeconds] = useState(MAX_DURATION_SECONDS);
		const [playheadSeconds, setPlayheadSeconds] = useState<number | null>(null);
		const [isPlaying, setIsPlaying] = useState(false);
		const [submitting, setSubmitting] = useState(false);
		const [error, setError] = useState<string | null>(null);
		const audioContextRef = useRef<AudioContext | null>(null);
		const sourceNodeRef = useRef<AudioBufferSourceNode | null>(null);
		const playbackStartedAtRef = useRef<number>(0);
		const playbackOffsetRef = useRef<number>(0);
		const rafRef = useRef<number | null>(null);

		useEffect(() => {
			let cancelled = false;
			let decodeContext: AudioContext | null = null;
			(async () => {
				try {
					const Ctor =
						typeof window === 'undefined'
							? null
							: window.AudioContext ||
								(window as typeof window & {webkitAudioContext?: typeof AudioContext}).webkitAudioContext;
					if (!Ctor) {
						setDecodeError(i18n._(COULD_NOT_DECODE_DESCRIPTOR));
						return;
					}
					const ctx = new Ctor();
					decodeContext = ctx;
					audioContextRef.current = ctx;
					const buf = await sourceFile.arrayBuffer();
					const decoded = await ctx.decodeAudioData(buf.slice(0));
					if (cancelled) return;
					setAudioBuffer(decoded);
					const initialEnd = Math.min(decoded.duration, MAX_DURATION_SECONDS);
					setStartSeconds(0);
					setEndSeconds(initialEnd);
				} catch (decodeErr) {
					logger.warn('Failed to decode entrance-sound source', {error: decodeErr});
					if (!cancelled) setDecodeError(i18n._(COULD_NOT_DECODE_DESCRIPTOR));
				}
			})();
			return () => {
				cancelled = true;
				if (decodeContext && audioContextRef.current === decodeContext) {
					audioContextRef.current = null;
					void decodeContext.close().catch(() => {});
				}
			};
		}, [i18n, sourceFile]);

		const peaks = useMemo(() => (audioBuffer ? computePeaks(audioBuffer, PEAK_BIN_COUNT) : null), [audioBuffer]);
		const totalDuration = audioBuffer?.duration ?? 0;
		const selectionDuration = Math.max(0, endSeconds - startSeconds);

		const stopRaf = useCallback(() => {
			if (rafRef.current != null) {
				cancelAnimationFrame(rafRef.current);
				rafRef.current = null;
			}
		}, []);

		const stopPlayback = useCallback(() => {
			const node = sourceNodeRef.current;
			if (node) {
				try {
					node.stop();
				} catch {}
				try {
					node.disconnect();
				} catch {}
			}
			sourceNodeRef.current = null;
			stopRaf();
			setIsPlaying(false);
			setPlayheadSeconds(null);
		}, [stopRaf]);

		useEffect(() => {
			return () => {
				stopPlayback();
				const ctx = audioContextRef.current;
				if (ctx) void ctx.close().catch(() => {});
				audioContextRef.current = null;
			};
		}, [stopPlayback]);

		const tickPlayhead = useCallback(() => {
			const ctx = audioContextRef.current;
			if (!ctx) return;
			const elapsed = ctx.currentTime - playbackStartedAtRef.current;
			const next = playbackOffsetRef.current + elapsed;
			if (next >= endSeconds) {
				stopPlayback();
				return;
			}
			setPlayheadSeconds(next);
			rafRef.current = requestAnimationFrame(tickPlayhead);
		}, [endSeconds, stopPlayback]);

		const startPlayback = useCallback(() => {
			const ctx = audioContextRef.current;
			if (!ctx || !audioBuffer) return;
			stopPlayback();
			const node = ctx.createBufferSource();
			node.buffer = audioBuffer;
			node.connect(ctx.destination);
			sourceNodeRef.current = node;
			playbackOffsetRef.current = startSeconds;
			playbackStartedAtRef.current = ctx.currentTime;
			node.onended = () => {
				if (sourceNodeRef.current === node) {
					stopPlayback();
				}
			};
			try {
				node.start(0, startSeconds, Math.max(0.001, endSeconds - startSeconds));
				setIsPlaying(true);
				setPlayheadSeconds(startSeconds);
				rafRef.current = requestAnimationFrame(tickPlayhead);
			} catch (playErr) {
				logger.warn('Failed to start preview playback', {error: playErr});
				stopPlayback();
			}
		}, [audioBuffer, startSeconds, endSeconds, stopPlayback, tickPlayhead]);

		const togglePlayback = useCallback(() => {
			if (isPlaying) {
				stopPlayback();
			} else {
				startPlayback();
			}
		}, [isPlaying, startPlayback, stopPlayback]);

		const handleSelectionChange = useCallback(
			(next: {startSeconds: number; endSeconds: number}) => {
				stopPlayback();
				setStartSeconds(next.startSeconds);
				setEndSeconds(next.endSeconds);
				setError(null);
			},
			[stopPlayback],
		);

		const close = useCallback(() => {
			stopPlayback();
			ModalCommands.popByType(EntranceSoundTrimmerModal);
		}, [stopPlayback]);

		const confirm = useCallback(async () => {
			if (!audioBuffer) return;
			if (selectionDuration < MIN_DURATION_SECONDS) {
				setError(i18n._(SELECTION_TOO_SHORT_DESCRIPTOR, {seconds: MIN_DURATION_SECONDS.toFixed(2)}));
				return;
			}
			setError(null);
			setSubmitting(true);
			try {
				const blob = encodeAudioBufferSliceToWav(audioBuffer, {
					startSeconds,
					endSeconds,
					downmixToMono: true,
				});
				if (blob.size > ENTRANCE_SOUND_MAX_BYTES) {
					setError(i18n._(ENCODED_TOO_LARGE_DESCRIPTOR, {limit: formatBytes(ENTRANCE_SOUND_MAX_BYTES)}));
					setSubmitting(false);
					return;
				}
				stopPlayback();
				await onConfirm({
					blob,
					startSeconds,
					endSeconds,
					durationMs: Math.round(selectionDuration * 1000),
					sampleRate: audioBuffer.sampleRate,
				});
				ModalCommands.popByType(EntranceSoundTrimmerModal);
			} catch (error) {
				logger.error('Failed to confirm trimmed entrance sound', error);
				setSubmitting(false);
			}
		}, [audioBuffer, selectionDuration, startSeconds, endSeconds, onConfirm, stopPlayback, i18n]);

		const selectionTooShort = selectionDuration < MIN_DURATION_SECONDS;
		const selectionTooLong = selectionDuration > MAX_DURATION_SECONDS + 0.001;

		return (
			<Modal.Root size="small" centered data-flx="voice.entrance-sound-trimmer-modal.modal-root">
				<Modal.Header
					title={i18n._(TRIMMER_TITLE_DESCRIPTOR)}
					onClose={close}
					data-flx="voice.entrance-sound-trimmer-modal.modal-header"
				/>
				<Modal.Content padding="default" data-flx="voice.entrance-sound-trimmer-modal.modal-content">
					<div className={styles.body} data-flx="voice.entrance-sound-trimmer-modal.body">
						<div className={styles.fileName} data-flx="voice.entrance-sound-trimmer-modal.file-name">
							{sourceFile.name}
						</div>
						<p className={styles.helpText} data-flx="voice.entrance-sound-trimmer-modal.help-text">
							{i18n._(TRIMMER_HELP_DESCRIPTOR)}
						</p>
						{decodeError ? (
							<p className={styles.errorText} data-flx="voice.entrance-sound-trimmer-modal.decode-error">
								{decodeError}
							</p>
						) : !audioBuffer || !peaks ? (
							<div className={styles.spinnerWrap} data-flx="voice.entrance-sound-trimmer-modal.spinner-wrap">
								<Spinner data-flx="voice.entrance-sound-trimmer-modal.spinner" />
							</div>
						) : (
							<>
								<AudioWaveform
									peaks={peaks}
									durationSeconds={totalDuration}
									startSeconds={startSeconds}
									endSeconds={endSeconds}
									minSelectionSeconds={MIN_DURATION_SECONDS}
									maxSelectionSeconds={MAX_DURATION_SECONDS}
									playheadSeconds={playheadSeconds}
									onSelectionChange={handleSelectionChange}
									data-flx="voice.entrance-sound-trimmer-modal.waveform"
								/>
								<div className={styles.meta} data-flx="voice.entrance-sound-trimmer-modal.meta">
									<span
										className={selectionTooShort || selectionTooLong ? styles.metaWarning : undefined}
										data-flx="voice.entrance-sound-trimmer-modal.selection-meta"
									>
										<Trans>
											Selection: {formatSeconds(selectionDuration)} (max {formatSeconds(MAX_DURATION_SECONDS)})
										</Trans>
									</span>
									<span data-flx="voice.entrance-sound-trimmer-modal.range-meta">
										{formatSeconds(startSeconds)} → {formatSeconds(endSeconds)}
									</span>
								</div>
								<div className={styles.controls} data-flx="voice.entrance-sound-trimmer-modal.controls">
									<Button
										variant="secondary"
										small
										onClick={togglePlayback}
										data-flx="voice.entrance-sound-trimmer-modal.play-button"
									>
										<span className={styles.playButton} data-flx="voice.entrance-sound-trimmer-modal.play-button">
											{isPlaying ? (
												<PauseIcon size={14} weight="fill" data-flx="voice.entrance-sound-trimmer-modal.pause-icon" />
											) : (
												<PlayIcon size={14} weight="fill" data-flx="voice.entrance-sound-trimmer-modal.play-icon" />
											)}
											<flx-i18n data-flx="voice.entrance-sound-trimmer-modal.flx-i18n">
												{isPlaying ? i18n._(PAUSE_SELECTION_DESCRIPTOR) : i18n._(PLAY_SELECTION_DESCRIPTOR)}
											</flx-i18n>
										</span>
									</Button>
								</div>
								{error ? (
									<p className={styles.errorText} data-flx="voice.entrance-sound-trimmer-modal.error">
										{error}
									</p>
								) : null}
							</>
						)}
					</div>
				</Modal.Content>
				<Modal.Footer data-flx="voice.entrance-sound-trimmer-modal.modal-footer">
					<Button variant="secondary" onClick={close} data-flx="voice.entrance-sound-trimmer-modal.cancel-button">
						{i18n._(CANCEL_DESCRIPTOR)}
					</Button>
					<Button
						variant="primary"
						onClick={confirm}
						disabled={!audioBuffer || selectionTooShort || selectionTooLong || submitting}
						submitting={submitting}
						data-flx="voice.entrance-sound-trimmer-modal.confirm-button"
					>
						{i18n._(USE_SELECTION_DESCRIPTOR)}
					</Button>
				</Modal.Footer>
			</Modal.Root>
		);
	},
);

EntranceSoundTrimmerModal.displayName = 'EntranceSoundTrimmerModal';

export function openEntranceSoundTrimmerModal(props: EntranceSoundTrimmerModalProps): void {
	ModalCommands.push(
		ModalCommands.modal(() => (
			<EntranceSoundTrimmerModal
				data-flx="voice.entrance-sound-trimmer-modal.open-entrance-sound-trimmer-modal.entrance-sound-trimmer-modal"
				{...props}
			/>
		)),
	);
}
