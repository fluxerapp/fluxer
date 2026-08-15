// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/voice/components/AudioWaveform.module.css';
import {resolveDevicePixelRatio} from '@app/features/voice/DevicePixelRatio';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const SELECTION_START_DESCRIPTOR = msg({
	message: 'Selection start',
	comment: 'Accessible label for the draggable start handle in an audio waveform trimmer.',
});
const SELECTION_END_DESCRIPTOR = msg({
	message: 'Selection end',
	comment: 'Accessible label for the draggable end handle in an audio waveform trimmer.',
});

interface AudioWaveformPeaks {
	readonly mins: ReadonlyArray<number>;
	readonly maxs: ReadonlyArray<number>;
}

export function computePeaks(buffer: AudioBuffer, targetBins: number): AudioWaveformPeaks {
	const bins = Math.max(1, Math.floor(targetBins));
	const mins = new Array<number>(bins);
	const maxs = new Array<number>(bins);
	const channelCount = buffer.numberOfChannels;
	const samplesPerBin = Math.max(1, Math.floor(buffer.length / bins));
	const channels: Array<Float32Array> = [];
	for (let c = 0; c < channelCount; c++) {
		channels.push(buffer.getChannelData(c));
	}
	for (let bin = 0; bin < bins; bin++) {
		const start = bin * samplesPerBin;
		const end = bin === bins - 1 ? buffer.length : Math.min(buffer.length, start + samplesPerBin);
		let lo = 1;
		let hi = -1;
		for (let i = start; i < end; i++) {
			let sum = 0;
			for (let c = 0; c < channelCount; c++) {
				sum += channels[c]![i]!;
			}
			const sample = sum / channelCount;
			if (sample < lo) lo = sample;
			if (sample > hi) hi = sample;
		}
		mins[bin] = lo === 1 ? 0 : lo;
		maxs[bin] = hi === -1 ? 0 : hi;
	}
	return {mins, maxs};
}

interface AudioWaveformProps {
	peaks: AudioWaveformPeaks | null;
	durationSeconds: number;
	startSeconds: number;
	endSeconds: number;
	minSelectionSeconds: number;
	maxSelectionSeconds: number;
	playheadSeconds?: number | null;
	onSelectionChange: (next: {startSeconds: number; endSeconds: number}) => void;
	'data-flx'?: string;
}

type DragMode = 'start' | 'end' | 'region';

interface DragState {
	mode: DragMode;
	pointerId: number;
	originX: number;
	originStart: number;
	originEnd: number;
}

function clamp(value: number, min: number, max: number): number {
	return value < min ? min : value > max ? max : value;
}

function computePeakNormalizer(peaks: AudioWaveformPeaks): number {
	let max = 0;
	for (let i = 0; i < peaks.maxs.length; i++) {
		const lo = Math.abs(peaks.mins[i] ?? 0);
		const hi = Math.abs(peaks.maxs[i] ?? 0);
		if (Number.isFinite(lo) && lo > max) max = lo;
		if (Number.isFinite(hi) && hi > max) max = hi;
	}
	return max > 0 ? max : 1;
}

export const AudioWaveform: React.FC<AudioWaveformProps> = ({
	peaks,
	durationSeconds,
	startSeconds,
	endSeconds,
	minSelectionSeconds,
	maxSelectionSeconds,
	playheadSeconds,
	onSelectionChange,
	'data-flx': dataFlx,
}) => {
	const {i18n} = useLingui();
	const rootRef = useRef<HTMLDivElement | null>(null);
	const canvasRef = useRef<HTMLCanvasElement | null>(null);
	const dragStateRef = useRef<DragState | null>(null);
	const [draggingRegion, setDraggingRegion] = useState(false);

	const safeDuration = durationSeconds > 0 ? durationSeconds : 1;
	const startPct = clamp(startSeconds / safeDuration, 0, 1) * 100;
	const endPct = clamp(endSeconds / safeDuration, 0, 1) * 100;
	const playheadPct =
		playheadSeconds != null && playheadSeconds >= 0 ? clamp(playheadSeconds / safeDuration, 0, 1) * 100 : null;

	const renderCanvas = useCallback(() => {
		const canvas = canvasRef.current;
		if (!canvas || !peaks) return;
		const dpr = resolveDevicePixelRatio(canvas.ownerDocument.defaultView);
		const rect = canvas.getBoundingClientRect();
		const widthPx = Math.max(1, Math.round(rect.width * dpr));
		const heightPx = Math.max(1, Math.round(rect.height * dpr));
		if (canvas.width !== widthPx) canvas.width = widthPx;
		if (canvas.height !== heightPx) canvas.height = heightPx;
		const ctx = canvas.getContext('2d');
		if (!ctx) return;
		ctx.clearRect(0, 0, widthPx, heightPx);
		const mid = heightPx / 2;
		const maxPaddingPx = Math.max(0, mid - 1);
		const verticalPaddingPx = Math.min(maxPaddingPx, Math.max(4 * dpr, Math.round(heightPx * 0.08)));
		const amplitudeHeightPx = Math.max(1, mid - verticalPaddingPx);
		const barWidth = Math.max(1, Math.floor(dpr));
		const computed = getComputedStyle(canvas);
		const peakStrokeStyle =
			computed.getPropertyValue('--text-tertiary').trim() ||
			computed.getPropertyValue('--text-secondary').trim() ||
			'#b5bac1';
		ctx.fillStyle = peakStrokeStyle;
		const binCount = peaks.maxs.length;
		const step = widthPx / Math.max(1, binCount);
		const normalizer = computePeakNormalizer(peaks);
		for (let i = 0; i < binCount; i++) {
			const x = Math.floor(i * step);
			const minVal = clamp((peaks.mins[i] ?? 0) / normalizer, -1, 1);
			const maxVal = clamp((peaks.maxs[i] ?? 0) / normalizer, -1, 1);
			const low = Math.min(minVal, maxVal);
			const high = Math.max(minVal, maxVal);
			const top = clamp(mid - high * amplitudeHeightPx, verticalPaddingPx, heightPx - verticalPaddingPx);
			const bottom = clamp(mid - low * amplitudeHeightPx, verticalPaddingPx, heightPx - verticalPaddingPx);
			ctx.fillRect(x, top, barWidth, Math.max(1, bottom - top));
		}
	}, [peaks]);

	useEffect(() => {
		renderCanvas();
	}, [renderCanvas]);

	useEffect(() => {
		const canvas = canvasRef.current;
		if (canvas == null) return;
		const ownerWindow = canvas.ownerDocument.defaultView;
		if (!ownerWindow) return;
		const handle = () => renderCanvas();
		ownerWindow.addEventListener('resize', handle);
		return () => ownerWindow.removeEventListener('resize', handle);
	}, [renderCanvas]);

	const beginDrag = useCallback(
		(mode: DragMode, event: React.PointerEvent<HTMLElement>) => {
			if (event.pointerType === 'mouse' && event.button !== 0) return;
			event.preventDefault();
			event.stopPropagation();
			const root = rootRef.current;
			if (!root) return;
			(event.target as Element).setPointerCapture?.(event.pointerId);
			dragStateRef.current = {
				mode,
				pointerId: event.pointerId,
				originX: event.clientX,
				originStart: startSeconds,
				originEnd: endSeconds,
			};
			if (mode === 'region') {
				setDraggingRegion(true);
			}
		},
		[startSeconds, endSeconds],
	);

	const handlePointerMove = useCallback(
		(event: React.PointerEvent<HTMLDivElement>) => {
			const drag = dragStateRef.current;
			if (!drag || drag.pointerId !== event.pointerId) return;
			const root = rootRef.current;
			if (!root) return;
			const rect = root.getBoundingClientRect();
			const widthPx = Math.max(1, rect.width);
			const deltaSeconds = ((event.clientX - drag.originX) / widthPx) * safeDuration;
			if (drag.mode === 'start') {
				const newStart = clamp(drag.originStart + deltaSeconds, 0, drag.originEnd - minSelectionSeconds);
				const span = drag.originEnd - newStart;
				const clampedStart = span > maxSelectionSeconds ? drag.originEnd - maxSelectionSeconds : newStart;
				onSelectionChange({startSeconds: clampedStart, endSeconds: drag.originEnd});
			} else if (drag.mode === 'end') {
				const newEnd = clamp(drag.originEnd + deltaSeconds, drag.originStart + minSelectionSeconds, safeDuration);
				const span = newEnd - drag.originStart;
				const clampedEnd = span > maxSelectionSeconds ? drag.originStart + maxSelectionSeconds : newEnd;
				onSelectionChange({startSeconds: drag.originStart, endSeconds: clampedEnd});
			} else {
				const span = drag.originEnd - drag.originStart;
				let newStart = drag.originStart + deltaSeconds;
				if (newStart < 0) newStart = 0;
				if (newStart + span > safeDuration) newStart = safeDuration - span;
				onSelectionChange({startSeconds: newStart, endSeconds: newStart + span});
			}
		},
		[safeDuration, minSelectionSeconds, maxSelectionSeconds, onSelectionChange],
	);

	const endDrag = useCallback((event: React.PointerEvent<HTMLDivElement>) => {
		const drag = dragStateRef.current;
		if (!drag || drag.pointerId !== event.pointerId) return;
		(event.target as Element).releasePointerCapture?.(event.pointerId);
		dragStateRef.current = null;
		setDraggingRegion(false);
	}, []);

	const handleStartPointerDown = useCallback(
		(event: React.PointerEvent<HTMLDivElement>) => beginDrag('start', event),
		[beginDrag],
	);
	const handleEndPointerDown = useCallback(
		(event: React.PointerEvent<HTMLDivElement>) => beginDrag('end', event),
		[beginDrag],
	);
	const handleRegionPointerDown = useCallback(
		(event: React.PointerEvent<HTMLDivElement>) => beginDrag('region', event),
		[beginDrag],
	);

	const selectionStyle = useMemo<React.CSSProperties>(
		() => ({left: `${startPct}%`, width: `${Math.max(0, endPct - startPct)}%`}),
		[startPct, endPct],
	);

	return (
		<div
			ref={rootRef}
			className={styles.root}
			onPointerMove={handlePointerMove}
			onPointerUp={endDrag}
			onPointerCancel={endDrag}
			data-flx={dataFlx}
		>
			<canvas ref={canvasRef} className={styles.canvas} aria-hidden data-flx="voice.audio-waveform.canvas" />
			<div
				className={clsx(styles.selection, draggingRegion && styles.selectionDragging)}
				style={selectionStyle}
				onPointerDown={handleRegionPointerDown}
				role="presentation"
				data-flx="voice.audio-waveform.selection.region-pointer-down"
			/>
			<div
				className={clsx(styles.handle, styles.handleStart)}
				style={{left: `${startPct}%`}}
				onPointerDown={handleStartPointerDown}
				role="slider"
				aria-label={i18n._(SELECTION_START_DESCRIPTOR)}
				aria-valuemin={0}
				aria-valuemax={safeDuration}
				aria-valuenow={startSeconds}
				tabIndex={0}
				data-flx="voice.audio-waveform.handle.start-pointer-down"
			/>
			<div
				className={clsx(styles.handle, styles.handleEnd)}
				style={{left: `${endPct}%`}}
				onPointerDown={handleEndPointerDown}
				role="slider"
				aria-label={i18n._(SELECTION_END_DESCRIPTOR)}
				aria-valuemin={0}
				aria-valuemax={safeDuration}
				aria-valuenow={endSeconds}
				tabIndex={0}
				data-flx="voice.audio-waveform.handle.end-pointer-down"
			/>
			{playheadPct != null && (
				<div
					className={styles.playhead}
					style={{left: `${playheadPct}%`}}
					aria-hidden
					data-flx="voice.audio-waveform.playhead"
				/>
			)}
		</div>
	);
};
