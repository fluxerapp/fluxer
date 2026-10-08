// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type AnimatedCropSourceFormat,
	type CropParams,
	CropPipelineError,
} from '@app/features/expressions/workers/AnimatedImageCropMessages';

export const MAX_SOURCE_CANVAS_PIXELS = 16_777_216;
export const MAX_FRAMES = 20_000;
export const MAX_OUTPUT_TOTAL_PIXELS = 1_073_741_824;
export const MAX_SOURCE_TOTAL_PIXELS = 1_073_741_824;
export const MAX_PLAYS = 65_535;
export const LOSSLESS_MAX_WORK_PIXELS = 16_777_216;

export const QUALITY_RUNGS: ReadonlyArray<number> = [75, 60, 45, 30];
export const SIZE_RATIO: ReadonlyArray<number> = [1, 0.889, 0.782, 0.595];

const LADDER_TARGET_FRACTION = 0.95;
const LADDER_GIVE_UP_FRACTION = 1.15;
const DEFAULT_AVIF_DELAY_MS = 100;

export interface EncodeRung {
	quality: number;
	method: number;
	lossless: boolean;
}

export const LOSSLESS_RUNG: EncodeRung = {quality: 75, method: 4, lossless: true};

export function chooseMethod(workPixels: number): number {
	if (workPixels <= 67_108_864) return 4;
	if (workPixels <= 268_435_456) return 2;
	return 0;
}

export function lossyRung(index: number, method: number): EncodeRung {
	return {quality: QUALITY_RUNGS[index], method, lossless: false};
}

export function nextRung(index: number, sizeBytes: number, capBytes: number): number | 'too_large' {
	const last = QUALITY_RUNGS.length - 1;
	if (index >= last) return 'too_large';
	const target = LADDER_TARGET_FRACTION * capBytes;
	for (let candidate = index + 1; candidate <= last; candidate++) {
		if ((sizeBytes * SIZE_RATIO[candidate]) / SIZE_RATIO[index] <= target) return candidate;
	}
	if ((sizeBytes * SIZE_RATIO[last]) / SIZE_RATIO[index] > LADDER_GIVE_UP_FRACTION * capBytes) return 'too_large';
	return last;
}

export function usesLosslessFirstRung(sourceLossless: boolean, workPixels: number): boolean {
	return sourceLossless && workPixels <= LOSSLESS_MAX_WORK_PIXELS;
}

export function assertSourceBudget(width: number, height: number, frameCount: number): void {
	if (!(width > 0 && height > 0 && frameCount > 0)) {
		throw new CropPipelineError(
			'decode_failed',
			`Invalid animation header ${width}x${height} with ${frameCount} frames`,
		);
	}
	if (width * height > MAX_SOURCE_CANVAS_PIXELS) {
		throw new CropPipelineError('budget_exceeded', `Canvas ${width}x${height} is larger than the decode budget`);
	}
	if (frameCount > MAX_FRAMES) {
		throw new CropPipelineError('budget_exceeded', `${frameCount} frames is more than ${MAX_FRAMES}`);
	}
	if (frameCount * width * height > MAX_SOURCE_TOTAL_PIXELS) {
		throw new CropPipelineError(
			'budget_exceeded',
			`${frameCount} frames at ${width}x${height} is too much decode work`,
		);
	}
}

export function assertOutputBudget(frameCount: number, outputWidth: number, outputHeight: number): void {
	if (!(outputWidth > 0 && outputHeight > 0)) {
		throw new CropPipelineError('internal', `Invalid output size ${outputWidth}x${outputHeight}`);
	}
	if (frameCount * outputWidth * outputHeight > MAX_OUTPUT_TOTAL_PIXELS) {
		throw new CropPipelineError(
			'budget_exceeded',
			`${frameCount} frames at ${outputWidth}x${outputHeight} is too much output`,
		);
	}
}

export function isNoopCrop(
	crop: CropParams,
	canvasWidth: number,
	canvasHeight: number,
	format: AnimatedCropSourceFormat,
	sourceBytes: number,
	capBytes: number,
): boolean {
	return (
		(format === 'gif' || format === 'webp') &&
		crop.x === 0 &&
		crop.y === 0 &&
		crop.width === canvasWidth &&
		crop.height === canvasHeight &&
		crop.rotation === 0 &&
		crop.outputWidth === canvasWidth &&
		crop.outputHeight === canvasHeight &&
		sourceBytes <= capBytes
	);
}

export function clampPlays(plays: number): number {
	return Math.min(Math.max(Math.trunc(plays), 0), MAX_PLAYS);
}

export function gifPlaysFromRepeat(repeat: number | null): number {
	if (repeat === null) return 1;
	if (repeat === 0) return 0;
	return clampPlays(repeat + 1);
}

export function gifRepeatFromPlays(plays: number): number {
	if (plays === 1) return -1;
	if (plays === 0) return 0;
	return clampPlays(plays - 1);
}

export function avifPlaysFromRepetitionCount(repetitionCount: number | undefined): number {
	if (repetitionCount === undefined || !Number.isFinite(repetitionCount)) return 0;
	return clampPlays(repetitionCount + 1);
}

export function apngDelayMs(numerator: number, denominator: number): number {
	return (numerator * 1000) / (denominator || 100);
}

export function avifDelayMs(durationMicros: number | null): number {
	return durationMicros == null ? DEFAULT_AVIF_DELAY_MS : durationMicros / 1000;
}
