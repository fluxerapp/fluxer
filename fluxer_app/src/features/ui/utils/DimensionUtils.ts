// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import type {CSSProperties} from 'react';

interface MediaDimensions {
	width: number;
	height: number;
}

interface DimensionOptions {
	maxWidth?: number;
	maxHeight?: number;
	preserve?: boolean;
	aspectRatio?: boolean;
	responsive?: boolean;
}

interface DimensionResult {
	style: CSSProperties;
	dimensions: MediaDimensions;
	scale: number;
}

export const MEDIA_MAX_WIDTH = 550;
export const MEDIA_MAX_HEIGHT = 350;

const DEFAULT_OPTIONS: Required<DimensionOptions> = {
	maxWidth: MEDIA_MAX_WIDTH,
	maxHeight: MEDIA_MAX_HEIGHT,
	preserve: false,
	aspectRatio: true,
	responsive: true,
};

export interface FitMediaInput extends MediaDimensions {
	maxWidth: number;
	maxHeight: number;
	minWidth?: number;
	minHeight?: number;
}

export function fitMediaWithinBounds({
	width,
	height,
	maxWidth,
	maxHeight,
	minWidth = 0,
	minHeight = 0,
}: FitMediaInput): MediaDimensions {
	if (width === maxWidth && height === maxHeight) {
		return {width, height};
	}
	const widthRatio = width > maxWidth ? maxWidth / width : 1;
	let fittedWidth = Math.max(Math.round(width * widthRatio), minWidth);
	let fittedHeight = Math.max(Math.round(height * widthRatio), minHeight);
	const heightRatio = fittedHeight > maxHeight ? maxHeight / fittedHeight : 1;
	fittedWidth = Math.max(Math.round(fittedWidth * heightRatio), minWidth);
	fittedHeight = Math.max(Math.round(fittedHeight * heightRatio), minHeight);
	return {width: fittedWidth, height: fittedHeight};
}

export function mediaContainRatio({width, height, maxWidth, maxHeight}: FitMediaInput): number {
	const widthRatio = width > maxWidth ? maxWidth / width : 1;
	const scaledHeight = Math.round(height * widthRatio);
	const heightRatio = scaledHeight > maxHeight ? maxHeight / scaledHeight : 1;
	return Math.min(widthRatio * heightRatio, 1);
}

export function mediaAspectRatioValue({width, height}: MediaDimensions): number {
	return width / height;
}

export class MediaDimensionCalculator {
	private options: Required<DimensionOptions>;

	constructor(options?: DimensionOptions) {
		this.options = {...DEFAULT_OPTIONS, ...options};
	}

	public calculate(dimensions: MediaDimensions, options?: DimensionOptions): DimensionResult {
		const config = {...this.options, ...options};
		const safeDimensions = this.normalizeDimensions(dimensions);
		if (config.preserve) {
			return this.preserveDimensions(safeDimensions, config);
		}
		return this.containDimensions(safeDimensions, config);
	}

	public calculateImage(dimensions: MediaDimensions, options?: DimensionOptions): DimensionResult {
		return this.calculate(dimensions, options);
	}

	public calculateVideo(dimensions: MediaDimensions, options?: Omit<DimensionOptions, 'preserve'>): DimensionResult {
		return this.calculate(dimensions, {...options, preserve: false});
	}

	private preserveDimensions(dimensions: MediaDimensions, options: Required<DimensionOptions>): DimensionResult {
		return {
			style: {
				width: remFromPx(dimensions.width),
				maxWidth: '100%',
				...(options.aspectRatio && {aspectRatio: mediaAspectRatioValue(dimensions)}),
			},
			dimensions: {...dimensions},
			scale: 1,
		};
	}

	private containDimensions(dimensions: MediaDimensions, options: Required<DimensionOptions>): DimensionResult {
		const maxWidth = this.normalizeLimit(options.maxWidth, DEFAULT_OPTIONS.maxWidth);
		const maxHeight = this.normalizeLimit(options.maxHeight, DEFAULT_OPTIONS.maxHeight);
		const fitted = fitMediaWithinBounds({...dimensions, maxWidth, maxHeight});
		const scale = mediaContainRatio({...dimensions, maxWidth, maxHeight});
		return {
			style: {
				maxWidth: `min(100%, ${remFromPx(fitted.width)})`,
				maxHeight: remFromPx(fitted.height),
				width: options.responsive ? '100%' : `min(100%, ${remFromPx(fitted.width)})`,
				display: 'block',
				...(options.aspectRatio && {aspectRatio: mediaAspectRatioValue(fitted)}),
			},
			dimensions: fitted,
			scale,
		};
	}

	private normalizeDimensions(dimensions: MediaDimensions): MediaDimensions {
		return {
			width: this.normalizeLimit(dimensions.width, 1),
			height: this.normalizeLimit(dimensions.height, 1),
		};
	}

	private normalizeLimit(value: number, fallback: number): number {
		if (!Number.isFinite(value) || value <= 0) {
			return fallback;
		}
		return value;
	}

	public static scale(
		width: number,
		height: number,
		maxWidth = DEFAULT_OPTIONS.maxWidth,
		maxHeight = DEFAULT_OPTIONS.maxHeight,
	): [number, number] {
		const fitted = fitMediaWithinBounds({width, height, maxWidth, maxHeight});
		return [fitted.width, fitted.height];
	}
}

export function createCalculator(options?: DimensionOptions): MediaDimensionCalculator {
	return new MediaDimensionCalculator(options);
}
