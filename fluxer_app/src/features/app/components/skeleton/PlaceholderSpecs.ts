// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getRememberedSkeletonMessagePresentation,
	resolveDefaultSkeletonChatViewportHeightPx,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {createSkeletonRandomFromKey} from '@app/features/app/components/skeleton/SkeletonSeed';
import {MESSAGE_LAYOUT_SPEC} from '@app/features/theme/layout/MessageLayoutSpec';
import {REM_BASE_PX} from '@app/features/theme/layout/RemFromPx';
import {useMemo, useState} from 'react';

function pxFromRemLength(value: `${number}rem`): number {
	return Math.round(Number.parseFloat(value) * REM_BASE_PX);
}

export interface PlaceholderAttachmentSize {
	readonly width: number;
	readonly height: number;
}

export interface PlaceholderMessageGroup {
	readonly lineWidths: ReadonlyArray<number>;
	readonly usernameWidth: number;
	readonly timestampWidth: number;
	readonly attachment: PlaceholderAttachmentSize | null;
	readonly height: number;
}

interface MutablePlaceholderMessageGroup extends PlaceholderMessageGroup {
	attachment: PlaceholderAttachmentSize | null;
	height: number;
}

export interface PlaceholderSpecs {
	readonly compact: boolean;
	readonly compactAvatarsVisible: boolean;
	readonly groups: ReadonlyArray<PlaceholderMessageGroup>;
	readonly totalHeight: number;
	readonly groupSpacing: number;
}

export const FILLER_WRAPPER_PADDING_TOP = '1rem' as const;
export const FILLER_WRAPPER_PADDING_BOTTOM = '0.75rem' as const;
export const FILLER_WRAPPER_VERTICAL_PADDING =
	pxFromRemLength(FILLER_WRAPPER_PADDING_TOP) + pxFromRemLength(FILLER_WRAPPER_PADDING_BOTTOM);
const MESSAGE_LINE_HEIGHT = pxFromRemLength(MESSAGE_LAYOUT_SPEC.lineHeight);
const MESSAGE_ROW_VERTICAL_PADDING = pxFromRemLength(MESSAGE_LAYOUT_SPEC.spacingY) * 2;
const MESSAGE_HEIGHT_COMPACT = MESSAGE_LINE_HEIGHT + MESSAGE_ROW_VERTICAL_PADDING;
const COZY_LEAD_MESSAGE_HEIGHT = MESSAGE_LINE_HEIGHT * 2 + MESSAGE_ROW_VERTICAL_PADDING;
const COZY_GROUPED_MESSAGE_HEIGHT = MESSAGE_LINE_HEIGHT + MESSAGE_ROW_VERTICAL_PADDING;
const ATTACHMENT_MARGIN = 8;
const ATTACHMENT_WIDTH_MIN = 140;
const ATTACHMENT_WIDTH_MAX = 400;
const ATTACHMENT_HEIGHT_MIN = 100;
const ATTACHMENT_HEIGHT_MAX = 320;
const USERNAME_WIDTH_MIN = 48;
const USERNAME_WIDTH_RANGE = 36;
const TIMESTAMP_WIDTH_MIN = 8;
const TIMESTAMP_WIDTH_RANGE = 12;
const LINE_WIDTH_MIN = 75;
const LINE_WIDTH_RANGE = 18;
const MIN_MESSAGE_GROUPS = 10;
const MAX_MESSAGE_GROUPS = 30;
const GROUP_LINE_RANGE = 4;
const ATTACHMENT_GROUPS = 8;
const GROUP_COUNT_OVERSHOOT = 1.05;
const COZY_GROUP_COUNT_FACTOR = 0.87;
const MIN_ESTIMATED_VIEWPORT_HEIGHT = 400;
const MEAN_GROUP_LINE_COUNT = (1 + GROUP_LINE_RANGE) / 2;
const MESSAGE_LIST_FALLBACK_SEED_KEY = 'message-list-placeholder';

interface PlaceholderGenerationOptions {
	readonly compact: boolean;
	readonly compactAvatarsVisible: boolean;
	readonly messageGroups: number;
	readonly attachments: number;
	readonly groupSpacing: number;
	readonly random: () => number;
}

function randomInRange(random: () => number, min: number, max: number): number {
	return Math.floor(random() * (max - min + 1)) + min;
}

export function resolveAverageGroupHeight(compact: boolean): number {
	if (compact) {
		return MESSAGE_HEIGHT_COMPACT * MEAN_GROUP_LINE_COUNT;
	}
	return COZY_LEAD_MESSAGE_HEIGHT + COZY_GROUPED_MESSAGE_HEIGHT * (MEAN_GROUP_LINE_COUNT - 1);
}

export function resolvePlaceholderMessageGroups(compact: boolean, viewportHeightPx: number): number {
	const usableHeight = Math.max(viewportHeightPx, MIN_ESTIMATED_VIEWPORT_HEIGHT);
	const rows = Math.ceil(usableHeight / resolveAverageGroupHeight(compact));
	const clamped = Math.min(MAX_MESSAGE_GROUPS, Math.max(MIN_MESSAGE_GROUPS, Math.ceil(GROUP_COUNT_OVERSHOOT * rows)));
	return compact ? clamped : Math.round(COZY_GROUP_COUNT_FACTOR * clamped);
}

export function resolvePlaceholderAttachmentCount(messageGroups: number): number {
	return Math.max(1, Math.round((messageGroups / MAX_MESSAGE_GROUPS) * ATTACHMENT_GROUPS));
}

function resolvePlaceholderDensityKey(compact: boolean): string {
	if (compact) {
		return '1';
	}
	return '0';
}

function generatePlaceholderSpecs(options: PlaceholderGenerationOptions): PlaceholderSpecs {
	const {compact, compactAvatarsVisible, messageGroups, attachments, groupSpacing, random} = options;
	const groups: Array<MutablePlaceholderMessageGroup> = [];
	let totalHeight = FILLER_WRAPPER_VERTICAL_PADDING;
	for (let index = 0; index < messageGroups; index++) {
		const lineCount = Math.floor(random() * GROUP_LINE_RANGE) + 1;
		const lineWidths: Array<number> = [];
		for (let line = 0; line < lineCount; line++) {
			lineWidths.push(LINE_WIDTH_MIN + random() * LINE_WIDTH_RANGE);
		}
		const groupHeight = compact
			? MESSAGE_HEIGHT_COMPACT * lineCount
			: COZY_LEAD_MESSAGE_HEIGHT + COZY_GROUPED_MESSAGE_HEIGHT * (lineCount - 1);
		groups.push({
			lineWidths,
			usernameWidth: USERNAME_WIDTH_MIN + random() * USERNAME_WIDTH_RANGE,
			timestampWidth: TIMESTAMP_WIDTH_MIN + random() * TIMESTAMP_WIDTH_RANGE,
			attachment: null,
			height: groupHeight,
		});
		if (index > 0) {
			totalHeight += groupSpacing;
		}
		totalHeight += groupHeight;
	}
	const availableGroupIndices = Array.from(Array(groups.length).keys());
	for (let index = 0; index < attachments && availableGroupIndices.length > 0; index++) {
		const groupIndex = availableGroupIndices.splice(Math.floor(random() * availableGroupIndices.length), 1)[0];
		const attachment = {
			width: randomInRange(random, ATTACHMENT_WIDTH_MIN, ATTACHMENT_WIDTH_MAX),
			height: randomInRange(random, ATTACHMENT_HEIGHT_MIN, ATTACHMENT_HEIGHT_MAX),
		};
		groups[groupIndex].attachment = attachment;
		groups[groupIndex].height += attachment.height + ATTACHMENT_MARGIN;
		totalHeight += attachment.height + ATTACHMENT_MARGIN;
	}
	return {compact, compactAvatarsVisible, groups, totalHeight, groupSpacing};
}

interface PlaceholderSpecsOptions {
	readonly compact: boolean;
	readonly compactAvatarsVisible: boolean;
	readonly groupSpacing: number;
	readonly viewportHeightPx: number;
	readonly seedKey: string;
}

function usePlaceholderSpecs({
	compact,
	compactAvatarsVisible,
	groupSpacing,
	viewportHeightPx,
	seedKey,
}: PlaceholderSpecsOptions): PlaceholderSpecs {
	const messageGroups = resolvePlaceholderMessageGroups(compact, viewportHeightPx);
	return useMemo(
		() =>
			generatePlaceholderSpecs({
				compact,
				compactAvatarsVisible,
				messageGroups,
				attachments: Math.min(messageGroups, resolvePlaceholderAttachmentCount(messageGroups)),
				groupSpacing,
				random: createSkeletonRandomFromKey([seedKey, resolvePlaceholderDensityKey(compact), groupSpacing].join('|')),
			}),
		[compact, compactAvatarsVisible, groupSpacing, messageGroups, seedKey],
	);
}

export interface MessageListPlaceholderSpecsOptions {
	readonly channelId: string | null;
	readonly compact: boolean;
	readonly compactAvatarsVisible: boolean;
	readonly groupSpacing: number;
}

export function useMessageListPlaceholderSpecs({
	channelId,
	compact,
	compactAvatarsVisible,
	groupSpacing,
}: MessageListPlaceholderSpecsOptions): PlaceholderSpecs {
	const [viewportHeightPx] = useState(
		() => getRememberedSkeletonMessagePresentation()?.viewportHeightPx ?? resolveDefaultSkeletonChatViewportHeightPx(),
	);
	return usePlaceholderSpecs({
		compact,
		compactAvatarsVisible,
		groupSpacing,
		viewportHeightPx,
		seedKey: channelId ?? MESSAGE_LIST_FALLBACK_SEED_KEY,
	});
}
