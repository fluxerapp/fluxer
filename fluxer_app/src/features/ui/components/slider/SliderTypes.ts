// SPDX-License-Identifier: AGPL-3.0-or-later

import type React from 'react';

export type SliderResetButtonPlacement = 'inline' | 'below';
export type SliderMarkerPosition = 'above' | 'below';
export type SliderOrientation = 'horizontal' | 'vertical';

export interface SliderProps {
	defaultValue: number;
	factoryDefaultValue: number;
	minValue?: number;
	maxValue?: number;
	disabled?: boolean;
	equidistant?: boolean;
	markers?: ReadonlyArray<number>;
	className?: string;
	ariaLabel?: string;
	ariaLabelledBy?: string;
	ariaValueText?: string;
	stickToMarkers?: boolean;
	mini?: boolean;
	markerPosition?: SliderMarkerPosition;
	orientation?: SliderOrientation;
	onValueChange?: (value: number) => void;
	onValueRender?: (value: number) => React.ReactNode;
	onMarkerRender?: (value: number) => React.ReactNode;
	asValueChanges?: (value: number) => void;
	onPointerInteractionChange?: (isInteracting: boolean) => void;
	stopEventPropagation?: boolean;
	barStyles?: React.CSSProperties;
	fillStyles?: React.CSSProperties;
	children?: React.ReactNode;
	value?: number;
	step?: number;
	showResetButton?: boolean;
	onReset?: () => void;
	resetTooltip?: string;
	resetButtonPlacement?: SliderResetButtonPlacement;
	resetLabel?: React.ReactNode;
	resetAccessory?: React.ReactNode;
}

export interface MarkerState {
	min: number;
	max: number;
	range: number;
	sortedMarkers: Array<number>;
	markerPositions: Array<number>;
	closestMarkerIndex?: number;
}
