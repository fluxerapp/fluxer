// SPDX-License-Identifier: AGPL-3.0-or-later

import {computeMasonryColumns} from '@app/features/channel/components/pickers/shared/ComputeColumns';
import {MASONRY_PADDING_PX} from '@app/features/channel/components/pickers/shared/PickerConstants';

export interface GifPickerLoadingSkeletonGifSpec {
	key: string;
	width: number;
	height: number;
	delaySeconds: number;
}

export interface GifPickerLoadingSkeletonLayoutItem extends GifPickerLoadingSkeletonGifSpec {
	index: number;
	column: number;
	left: number;
	top: number;
	renderedWidth: number;
	renderedHeight: number;
}

export interface GifPickerLoadingSkeletonLayoutInput {
	viewportWidth: number;
	viewportHeight: number;
	tileSpacing?: number;
	paddingPx?: number;
	overscanPx?: number;
}

export const GIF_PICKER_LOADING_SKELETON_GIF_SPECS: ReadonlyArray<GifPickerLoadingSkeletonGifSpec> = [
	{key: 'square-255', width: 255, height: 255, delaySeconds: 0},
	{key: 'wide-640-358', width: 640, height: 358, delaySeconds: 0.035},
	{key: 'wide-480-270', width: 480, height: 270, delaySeconds: 0.07},
	{key: 'near-square-640-584', width: 640, height: 584, delaySeconds: 0.105},
	{key: 'wide-640-360', width: 640, height: 360, delaySeconds: 0.025},
	{key: 'wide-640-356', width: 640, height: 356, delaySeconds: 0.06},
	{key: 'square-640', width: 640, height: 640, delaySeconds: 0.095},
	{key: 'landscape-244-160', width: 244, height: 160, delaySeconds: 0.13},
	{key: 'landscape-498-296', width: 498, height: 296, delaySeconds: 0.045},
	{key: 'square-400', width: 400, height: 400, delaySeconds: 0.08},
	{key: 'portrait-325-498', width: 325, height: 498, delaySeconds: 0.115},
	{key: 'portrait-400-498', width: 400, height: 498, delaySeconds: 0.15},
	{key: 'panorama-498-200', width: 498, height: 200, delaySeconds: 0.065},
	{key: 'landscape-300-170', width: 300, height: 170, delaySeconds: 0.1},
	{key: 'square-498', width: 498, height: 498, delaySeconds: 0.135},
	{key: 'half-wide-498-249', width: 498, height: 249, delaySeconds: 0.17},
	{key: 'landscape-360-216', width: 360, height: 216, delaySeconds: 0.09},
	{key: 'landscape-640-428', width: 640, height: 428, delaySeconds: 0.125},
	{key: 'near-square-244-240', width: 244, height: 240, delaySeconds: 0.16},
	{key: 'landscape-498-278', width: 498, height: 278, delaySeconds: 0.195},
	{key: 'portrait-640-602', width: 640, height: 602, delaySeconds: 0.145},
	{key: 'half-wide-400-200', width: 400, height: 200, delaySeconds: 0.18},
	{key: 'landscape-640-292', width: 640, height: 292, delaySeconds: 0.215},
	{key: 'portrait-498-407', width: 498, height: 407, delaySeconds: 0.25},
	{key: 'square-232', width: 232, height: 232, delaySeconds: 0.075},
	{key: 'landscape-640-352', width: 640, height: 352, delaySeconds: 0.11},
	{key: 'portrait-498-468', width: 498, height: 468, delaySeconds: 0.155},
	{key: 'panorama-365-182', width: 365, height: 182, delaySeconds: 0.205},
];

export function buildGifPickerLoadingSkeletonLayout({
	viewportWidth,
	viewportHeight,
	tileSpacing = 8,
	paddingPx = MASONRY_PADDING_PX,
	overscanPx = MASONRY_PADDING_PX * 2,
}: GifPickerLoadingSkeletonLayoutInput): Array<GifPickerLoadingSkeletonLayoutItem> {
	if (viewportWidth <= 0 || viewportHeight <= 0) {
		return [];
	}
	const columns = computeMasonryColumns(viewportWidth, tileSpacing, {minColumns: 2});
	const laneWidth = (viewportWidth - paddingPx * 2 - tileSpacing * Math.max(0, columns - 1)) / Math.max(1, columns);
	if (laneWidth <= 0) {
		return [];
	}
	const laneFill = Array.from({length: columns}, () => paddingPx);
	const visibleBottom = viewportHeight + overscanPx;
	const layout: Array<GifPickerLoadingSkeletonLayoutItem> = [];
	for (let index = 0; index < GIF_PICKER_LOADING_SKELETON_GIF_SPECS.length; index += 1) {
		const spec = GIF_PICKER_LOADING_SKELETON_GIF_SPECS[index];
		if (!spec) {
			continue;
		}
		const column = findShortestColumnIndex(laneFill);
		const left = paddingPx + column * (laneWidth + tileSpacing);
		const top = laneFill[column] ?? paddingPx;
		const renderedHeight = laneWidth * (spec.height / spec.width);
		laneFill[column] = top + renderedHeight + tileSpacing;
		if (top > visibleBottom) {
			continue;
		}
		layout.push({
			...spec,
			index,
			column,
			left,
			top,
			renderedWidth: laneWidth,
			renderedHeight,
		});
	}
	return layout;
}

function findShortestColumnIndex(laneFill: ReadonlyArray<number>): number {
	let shortestColumn = 0;
	let shortestHeight = laneFill[0] ?? 0;
	for (let column = 1; column < laneFill.length; column += 1) {
		const height = laneFill[column];
		if (height < shortestHeight) {
			shortestColumn = column;
			shortestHeight = height;
		}
	}
	return shortestColumn;
}
