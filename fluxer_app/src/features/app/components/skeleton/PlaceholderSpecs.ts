// SPDX-License-Identifier: AGPL-3.0-or-later

import {createSkeletonRandomFromKey} from '@app/features/app/components/skeleton/SkeletonSeed';
import {MESSAGE_LAYOUT_SPEC} from '@app/features/theme/layout/MessageLayoutSpec';
import {REM_BASE_PX} from '@app/features/theme/layout/RemFromPx';
import {useMemo} from 'react';

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

const MESSAGE_LINE_HEIGHT = pxFromRemLength(MESSAGE_LAYOUT_SPEC.lineHeight);
const MESSAGE_ROW_VERTICAL_PADDING = pxFromRemLength(MESSAGE_LAYOUT_SPEC.spacingY) * 2;
const MESSAGE_HEIGHT_COMPACT = MESSAGE_LINE_HEIGHT + MESSAGE_ROW_VERTICAL_PADDING;
const COZY_LEAD_MESSAGE_HEIGHT = MESSAGE_LINE_HEIGHT * 2 + MESSAGE_ROW_VERTICAL_PADDING;
const COZY_GROUPED_MESSAGE_HEIGHT = MESSAGE_LINE_HEIGHT + MESSAGE_ROW_VERTICAL_PADDING;
const SKELETON_PADDING_TOP = pxFromRemLength('1rem');
const SKELETON_PADDING_BOTTOM = pxFromRemLength('0.75rem');
const COZY_SKELETON_VERTICAL_PADDING = SKELETON_PADDING_TOP + SKELETON_PADDING_BOTTOM;
const ATTACHMENT_MARGIN = 8;
const ATTACHMENT_WIDTH_MIN = 140;
const ATTACHMENT_WIDTH_MAX = 400;
const ATTACHMENT_HEIGHT_MIN = 100;
const ATTACHMENT_HEIGHT_MAX = 250;
const USERNAME_WIDTH_MIN = 48;
const USERNAME_WIDTH_RANGE = 36;
const TIMESTAMP_WIDTH_MIN = 8;
const TIMESTAMP_WIDTH_RANGE = 12;
const LINE_WIDTH_MIN = 75;
const LINE_WIDTH_RANGE = 18;
const MIN_MESSAGE_GROUPS = 8;
const MAX_MESSAGE_GROUPS = 64;
const GROUP_LINE_RANGE = 4;
const ATTACHMENT_GROUPS = 8;

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

function resolveShortestGroupHeight(compact: boolean): number {
	if (compact) {
		return MESSAGE_HEIGHT_COMPACT;
	}
	return COZY_LEAD_MESSAGE_HEIGHT;
}

function resolvePlaceholderMessageGroups(compact: boolean, viewportHeightPx: number): number {
	const shortestGroupHeight = resolveShortestGroupHeight(compact);
	const groups = Math.ceil(Math.max(0, viewportHeightPx) / shortestGroupHeight);
	return Math.min(MAX_MESSAGE_GROUPS, Math.max(MIN_MESSAGE_GROUPS, groups));
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
	let totalHeight = compact ? 0 : COZY_SKELETON_VERTICAL_PADDING;
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

export interface PlaceholderSpecsOptions {
	readonly compact: boolean;
	readonly compactAvatarsVisible: boolean;
	readonly groupSpacing: number;
	readonly viewportHeightPx: number;
	readonly seedKey: string;
}

export function usePlaceholderSpecs({
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
				attachments: Math.min(ATTACHMENT_GROUPS, messageGroups),
				groupSpacing,
				random: createSkeletonRandomFromKey([seedKey, resolvePlaceholderDensityKey(compact), groupSpacing].join('|')),
			}),
		[compact, compactAvatarsVisible, groupSpacing, messageGroups, seedKey],
	);
}
