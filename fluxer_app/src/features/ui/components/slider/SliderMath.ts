// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MarkerState} from '@app/features/ui/components/slider/SliderTypes';

interface BuildMarkerStateOptions {
	markers?: ReadonlyArray<number>;
	minValue: number;
	maxValue: number;
	value: number;
	equidistant: boolean;
}

export function clamp(value: number, min: number, max: number): number {
	return Math.max(min, Math.min(max, value));
}

export function findClosestMarkerIndex(value: number, markerArray: ReadonlyArray<number>): number {
	if (markerArray.length === 0) return 0;
	let previousDiff = 0;
	for (let i = 0; i < markerArray.length; i++) {
		const currentMarker = markerArray[i];
		if (value === currentMarker) {
			return i;
		}
		if (value < currentMarker) {
			if (previousDiff === 0) {
				return i;
			}
			if (currentMarker - value < previousDiff) {
				return i;
			}
			return i - 1;
		}
		previousDiff = value - currentMarker;
	}
	return markerArray.length - 1;
}

export function buildMarkerState({
	markers,
	minValue,
	maxValue,
	value,
	equidistant,
}: BuildMarkerStateOptions): MarkerState {
	if (!markers || markers.length === 0) {
		return {
			min: minValue,
			max: maxValue,
			range: maxValue - minValue,
			sortedMarkers: [],
			markerPositions: [],
		};
	}
	const sortedMarkers = [...markers].sort((a, b) => a - b);
	if (equidistant) {
		for (let index = 0; index < sortedMarkers.length; index++) {
			if (!Number.isFinite(sortedMarkers[index]) || (index > 0 && sortedMarkers[index] <= sortedMarkers[index - 1])) {
				throw new Error('Equidistant slider markers must be finite and strictly increasing.');
			}
		}
	}
	const closestMarkerIndex = findClosestMarkerIndex(value, sortedMarkers);
	const min = sortedMarkers[0];
	const max = sortedMarkers[sortedMarkers.length - 1];
	const range = max - min;
	const markerPositions =
		sortedMarkers.length === 1
			? [0]
			: equidistant
				? sortedMarkers.map((_, i) => i * (100 / (sortedMarkers.length - 1)))
				: sortedMarkers.map((marker) => scaleValue({min, range}, marker));
	return {
		min,
		max,
		range,
		sortedMarkers,
		markerPositions,
		closestMarkerIndex,
	};
}

export function scaleValue(markerState: Pick<MarkerState, 'min' | 'range'>, value: number): number {
	if (markerState.range === 0) return 0;
	return (100 * (value - markerState.min)) / markerState.range;
}

export function scaleValueEquidistant(markers: ReadonlyArray<number>, value: number): number {
	if (markers.length < 2) return 0;
	const boundedValue = clamp(value, markers[0], markers[markers.length - 1]);
	if (boundedValue === markers[markers.length - 1]) return 100;
	let upperIndex = 1;
	while (upperIndex < markers.length && boundedValue > markers[upperIndex]) {
		upperIndex++;
	}
	const lowerIndex = upperIndex - 1;
	const segmentRange = markers[upperIndex] - markers[lowerIndex];
	if (segmentRange <= 0) {
		throw new Error('Equidistant slider markers must be strictly increasing.');
	}
	const segmentProgress = (boundedValue - markers[lowerIndex]) / segmentRange;
	return (100 * (lowerIndex + segmentProgress)) / (markers.length - 1);
}

export function unscaleValueEquidistant(markers: ReadonlyArray<number>, percentage: number): number {
	if (markers.length < 2) return markers[0] ?? 0;
	const boundedPercentage = clamp(percentage, 0, 100);
	const segmentPosition = (boundedPercentage / 100) * (markers.length - 1);
	const lowerIndex = Math.min(Math.floor(segmentPosition), markers.length - 2);
	const segmentProgress = segmentPosition - lowerIndex;
	return markers[lowerIndex] + (markers[lowerIndex + 1] - markers[lowerIndex]) * segmentProgress;
}

export function snapValueToMarker(value: number, markerState: MarkerState): number {
	if (markerState.sortedMarkers.length === 0) {
		return value;
	}
	return markerState.sortedMarkers[findClosestMarkerIndex(value, markerState.sortedMarkers)];
}
