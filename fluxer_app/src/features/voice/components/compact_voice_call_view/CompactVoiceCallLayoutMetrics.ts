// SPDX-License-Identifier: AGPL-3.0-or-later

const DEFAULT_COMPACT_CALL_HEIGHT = 320;
const CONTROL_BUTTON_SIZE = 56;
const CONTROL_PADDING_BOTTOM = 12;

function finitePositiveOr(value: number | undefined, fallback: number): number {
	if (value === undefined || !Number.isFinite(value) || value <= 0) {
		return fallback;
	}
	return value;
}

function clamp(min: number, value: number, max: number): number {
	return Math.min(max, Math.max(min, value));
}

function roundPx(value: number): string {
	return `${Math.round(value)}px`;
}

export interface CompactAudioAvatarLayoutMetricsInput {
	callHeight?: number;
	controlBarHeight?: number;
	hasControlBar: boolean;
	safeAreaBottom?: number;
}

export interface CompactAudioAvatarLayoutMetrics {
	callHeight: number;
	edgePadding: number;
	controlHeight: number;
	controlGap: number;
	gradientExtension: number;
	topPadding: number;
	bottomPadding: number;
}

export interface CompactAudioAvatarLayoutStyleVars {
	'--compact-call-audio-avatar-padding-top': string;
	'--compact-call-audio-avatar-padding-bottom': string;
	'--compact-call-edge-gradient-extension': string;
}

function resolveCompactCallEdgePadding(callHeight: number): number {
	return clamp(12, callHeight * 0.045, 24);
}

function resolveCompactControlPaddingTop(callHeight: number): number {
	return clamp(16, callHeight * 0.04, 24);
}

function resolveCompactControlGap(callHeight: number): number {
	return clamp(10, callHeight * 0.035, 18);
}

function resolveCompactEdgeGradientExtension(callHeight: number, hasControlBar: boolean): number {
	return hasControlBar ? clamp(14, callHeight * 0.052, 22) : clamp(12, callHeight * 0.045, 20);
}

export function resolveCompactAudioAvatarLayoutMetrics(
	input: CompactAudioAvatarLayoutMetricsInput,
): CompactAudioAvatarLayoutMetrics {
	const callHeight = finitePositiveOr(input.callHeight, DEFAULT_COMPACT_CALL_HEIGHT);
	const safeAreaBottom = Math.max(0, finitePositiveOr(input.safeAreaBottom, 0));
	const edgePadding = resolveCompactCallEdgePadding(callHeight);
	const fallbackControlHeight =
		CONTROL_BUTTON_SIZE + resolveCompactControlPaddingTop(callHeight) + CONTROL_PADDING_BOTTOM + safeAreaBottom;
	const controlHeight = input.hasControlBar ? finitePositiveOr(input.controlBarHeight, fallbackControlHeight) : 0;
	const controlGap = input.hasControlBar ? resolveCompactControlGap(callHeight) : 0;
	const gradientExtension = resolveCompactEdgeGradientExtension(callHeight, input.hasControlBar);
	const noControlPadding = clamp(48, callHeight * 0.12, 68);
	const bottomPadding = input.hasControlBar ? controlHeight + gradientExtension + controlGap : noControlPadding;
	const topTarget = clamp(18, callHeight * 0.065, 28);
	const topPadding = input.hasControlBar
		? Math.max(edgePadding, Math.min(topTarget, bottomPadding * 0.34))
		: noControlPadding;
	return {
		callHeight,
		edgePadding,
		controlHeight,
		controlGap,
		gradientExtension,
		topPadding,
		bottomPadding,
	};
}

export function getCompactAudioAvatarLayoutStyle(
	metrics: CompactAudioAvatarLayoutMetrics,
): CompactAudioAvatarLayoutStyleVars {
	return {
		'--compact-call-audio-avatar-padding-top': roundPx(metrics.topPadding),
		'--compact-call-audio-avatar-padding-bottom': roundPx(metrics.bottomPadding),
		'--compact-call-edge-gradient-extension': roundPx(metrics.gradientExtension),
	};
}
