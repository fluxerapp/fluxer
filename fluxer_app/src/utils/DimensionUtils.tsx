/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {CSSProperties} from 'react';

interface MediaDimensions {
	width: number;
	height: number;
}

interface DimensionOptions {
	maxWidth?: number;
	maxHeight?: number;
	preserve?: boolean;
	forceScale?: boolean;
	aspectRatio?: boolean;
	responsive?: boolean;
}

interface DimensionResult {
	style: CSSProperties;
	dimensions: MediaDimensions;
	scale: number;
}

const DEFAULT_OPTIONS: Required<DimensionOptions> = {
	maxWidth: 550,
	maxHeight: 400,
	preserve: false,
	forceScale: false,
	aspectRatio: true,
	responsive: true,
};

export class MediaDimensionCalculator {
	private options: Required<DimensionOptions>;

	constructor(options?: DimensionOptions) {
		this.options = {...DEFAULT_OPTIONS, ...options};
	}

	public calculate(dimensions: MediaDimensions, options?: DimensionOptions): DimensionResult {
		const config = {...this.options, ...options};

		if (config.preserve) {
			return this.preserveDimensions(dimensions);
		}

		if (config.forceScale) {
			return this.forceScaleDimensions(dimensions, config);
		}

		return this.calculateResponsiveDimensions(dimensions, config);
	}

	public calculateImage(dimensions: MediaDimensions, options?: Omit<DimensionOptions, 'forceScale'>): DimensionResult {
		return this.calculate(dimensions, {...options, forceScale: false});
	}

	public calculateVideo(dimensions: MediaDimensions, options?: Omit<DimensionOptions, 'preserve'>): DimensionResult {
		return this.calculate(dimensions, {...options, preserve: false});
	}

	private preserveDimensions(dimensions: MediaDimensions): DimensionResult {
		return {
			style: {
				width: dimensions.width,
				aspectRatio: `${dimensions.width}/${dimensions.height}`,
			},
			dimensions: {...dimensions},
			scale: 1,
		};
	}

	private forceScaleDimensions(dimensions: MediaDimensions, options: Required<DimensionOptions>): DimensionResult {
		const scale = Math.min(1, options.maxWidth / dimensions.width, options.maxHeight / dimensions.height);

		const scaledDimensions = {
			width: Math.round(dimensions.width * scale),
			height: Math.round(dimensions.height * scale),
		};

		return {
			style: {
				width: scaledDimensions.width,
				height: scaledDimensions.height,
				maxWidth: '100%',
				maxHeight: '100%',
				...(options.aspectRatio && {
					aspectRatio: `${scaledDimensions.width}/${scaledDimensions.height}`,
				}),
			},
			dimensions: scaledDimensions,
			scale,
		};
	}

	private calculateResponsiveDimensions(
		dimensions: MediaDimensions,
		options: Required<DimensionOptions>,
	): DimensionResult {
		const isPortrait = dimensions.height > dimensions.width;
		let style: CSSProperties;
		let scaledDimensions: MediaDimensions;
		let scale: number;

		if (isPortrait) {
			const targetWidth = Math.round((options.maxHeight * dimensions.width) / dimensions.height);
			const maxAllowedWidth = Math.min(options.maxWidth, targetWidth);
			scale = maxAllowedWidth / dimensions.width;

			scaledDimensions = {
				width: maxAllowedWidth,
				height: Math.round(dimensions.height * scale),
			};

			style = {
				maxWidth: `${maxAllowedWidth}px`,
				width: options.responsive ? '100%' : maxAllowedWidth,
				...(options.aspectRatio && {
					aspectRatio: `${maxAllowedWidth}/${options.maxHeight}`,
				}),
			};
		} else {
			const scaleByWidth = options.maxWidth / dimensions.width;
			const scaleByHeight = options.maxHeight / dimensions.height;
			scale = Math.min(1, scaleByWidth, scaleByHeight);

			scaledDimensions = {
				width: Math.round(dimensions.width * scale),
				height: Math.round(dimensions.height * scale),
			};

			style = {
				maxWidth: `${options.maxWidth}px`,
				width: options.responsive ? '100%' : scaledDimensions.width,
				...(options.aspectRatio && {
					aspectRatio: `${scaledDimensions.width}/${scaledDimensions.height}`,
				}),
			};
		}

		return {style, dimensions: scaledDimensions, scale};
	}

	public static scale(
		width: number,
		height: number,
		maxWidth = DEFAULT_OPTIONS.maxWidth,
		maxHeight = DEFAULT_OPTIONS.maxHeight,
	): [number, number] {
		const calculator = new MediaDimensionCalculator();
		const result = calculator.calculate({width, height}, {maxWidth, maxHeight, forceScale: true});
		return [result.dimensions.width, result.dimensions.height];
	}
}

export const createCalculator = (options?: DimensionOptions): MediaDimensionCalculator => {
	return new MediaDimensionCalculator(options);
};
