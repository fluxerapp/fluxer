// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {TYPING_BRIDGE_RIGHT_SHIFT_RATIO, TYPING_WIDTH_MULTIPLIER} from '../src/features/ui/constants/TypingConstants';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

type AvatarSize = 16 | 20 | 24 | 32 | 36 | 40 | 44 | 48 | 56 | 80 | 120;

interface StatusConfig {
	statusSize: number;
	cutoutRadius: number;
	cutoutCenter: number;
}

const LARGE_STATUS_CUTOUT_GUTTER_RATIO = 0.2;

const STATUS_CONFIG: Record<number, StatusConfig> = {
	16: {statusSize: 10, cutoutRadius: 5, cutoutCenter: 13},
	20: {statusSize: 10, cutoutRadius: 6, cutoutCenter: 17},
	24: {statusSize: 10, cutoutRadius: 7, cutoutCenter: 20},
	32: {statusSize: 10, cutoutRadius: 8, cutoutCenter: 27},
	36: {statusSize: 10, cutoutRadius: 8, cutoutCenter: 30},
	40: {statusSize: 12, cutoutRadius: 9, cutoutCenter: 34},
	44: {statusSize: 14, cutoutRadius: 10, cutoutCenter: 38},
	48: {statusSize: 14, cutoutRadius: 10, cutoutCenter: 42},
	56: {statusSize: 16, cutoutRadius: 11, cutoutCenter: 49},
	80: {statusSize: 16, cutoutRadius: 16 / 2 + 16 * LARGE_STATUS_CUTOUT_GUTTER_RATIO, cutoutCenter: 68},
	120: {statusSize: 24, cutoutRadius: 24 / 2 + 24 * LARGE_STATUS_CUTOUT_GUTTER_RATIO, cutoutCenter: 100},
};
const DESIGN_RULES = {
	mobileAspectRatio: 0.75,
	mobileCornerRadius: 0.12,
	mobileScreenWidth: 0.72,
	mobileScreenHeight: 0.7,
	mobileScreenY: 0.06,
	mobileWheelRadius: 0.13,
	mobileWheelY: 0.83,
	mobilePhoneExtraHeight: 2,
	mobileDisplayExtraHeight: 2,
	mobileDisplayExtraWidthPerSide: 2,
	idle: {
		cutoutRadiusRatio: 0.7,
		cutoutOffsetRatio: 0.35,
	},
	dnd: {
		barWidthRatio: 1.3,
		barHeightRatio: 0.4,
		minBarHeight: 2,
	},
	offline: {
		innerRingRatio: 0.6,
	},
} as const;
const MOBILE_SCREEN_WIDTH_TRIM_PX = 4;
const MOBILE_SCREEN_HEIGHT_TRIM_PX = 2;
const MOBILE_SCREEN_X_OFFSET_PX = 0;
const MOBILE_SCREEN_Y_OFFSET_PX = 3;

function getStatusConfig(avatarSize: number): StatusConfig {
	if (STATUS_CONFIG[avatarSize]) {
		return STATUS_CONFIG[avatarSize];
	}
	const sizes = Object.keys(STATUS_CONFIG)
		.map(Number)
		.sort((a, b) => a - b);
	const firstSize = sizes[0];
	const lastSize = sizes[sizes.length - 1];

	if (avatarSize <= firstSize) {
		return scaleStatusConfig(STATUS_CONFIG[firstSize], avatarSize / firstSize);
	}
	if (avatarSize >= lastSize) {
		return scaleStatusConfig(STATUS_CONFIG[lastSize], avatarSize / lastSize);
	}

	const upperSize = sizes.find((size) => size > avatarSize) ?? lastSize;
	const lowerSize = sizes[sizes.indexOf(upperSize) - 1] ?? firstSize;
	const progress = (avatarSize - lowerSize) / (upperSize - lowerSize);
	return interpolateStatusConfig(STATUS_CONFIG[lowerSize], STATUS_CONFIG[upperSize], progress);
}

function interpolateStatusConfig(from: StatusConfig, to: StatusConfig, progress: number): StatusConfig {
	return {
		statusSize: interpolate(from.statusSize, to.statusSize, progress),
		cutoutRadius: interpolate(from.cutoutRadius, to.cutoutRadius, progress),
		cutoutCenter: interpolate(from.cutoutCenter, to.cutoutCenter, progress),
	};
}

function scaleStatusConfig(config: StatusConfig, scale: number): StatusConfig {
	return {
		statusSize: config.statusSize * scale,
		cutoutRadius: config.cutoutRadius * scale,
		cutoutCenter: config.cutoutCenter * scale,
	};
}

function interpolate(from: number, to: number, progress: number): number {
	return from + (to - from) * progress;
}

function formatNumber(value: number): string {
	return Number(value.toFixed(6)).toString();
}

interface StatusGeometry {
	size: number;
	cx: number;
	cy: number;
	innerRadius: number;
	outerRadius: number;
	borderWidth: number;
}

interface MobileStatusGeometry extends StatusGeometry {
	phoneWidth: number;
	phoneHeight: number;
	phoneX: number;
	phoneY: number;
	phoneRx: number;
	bezelHeight: number;
}

function calculateStatusGeometry(avatarSize: number, isMobile: boolean = false): StatusGeometry | MobileStatusGeometry {
	const config = getStatusConfig(avatarSize);
	const statusSize = config.statusSize;
	const cutoutCenter = config.cutoutCenter;
	const cutoutRadius = config.cutoutRadius;
	const innerRadius = statusSize / 2;
	const outerRadius = cutoutRadius;
	const borderWidth = cutoutRadius - innerRadius;
	const baseGeometry = {
		size: statusSize,
		cx: cutoutCenter,
		cy: cutoutCenter,
		innerRadius,
		outerRadius,
		borderWidth,
	};
	if (!isMobile) {
		return baseGeometry;
	}
	const phoneWidth = statusSize;
	const phoneHeight = Math.round(phoneWidth / DESIGN_RULES.mobileAspectRatio) + DESIGN_RULES.mobilePhoneExtraHeight;
	const phoneRx = Math.round(phoneWidth * DESIGN_RULES.mobileCornerRadius);
	const bezelHeight = Math.max(1, Math.round(phoneHeight * 0.05));
	const phoneX = cutoutCenter - phoneWidth / 2;
	const phoneY = cutoutCenter - phoneHeight / 2;
	return {
		...baseGeometry,
		phoneWidth,
		phoneHeight,
		phoneX,
		phoneY,
		phoneRx,
		bezelHeight,
	};
}

function generateAvatarMaskDefault(size: number): string {
	const r = size / 2;
	return `<circle fill="white" cx="${r}" cy="${r}" r="${r}" />`;
}

function generateAvatarMaskStatusRound(size: number): string {
	const r = size / 2;
	const status = calculateStatusGeometry(size);
	return `(
				<>
					<circle fill="white" cx="${r}" cy="${r}" r="${r}" />
					<circle fill="black" cx="${status.cx}" cy="${status.cy}" r="${status.outerRadius}" />
				</>
			)`;
}

function generateAvatarMaskStatusTyping(size: number): string {
	const r = size / 2;
	const status = calculateStatusGeometry(size);
	const typingWidth = Math.round(status.size * TYPING_WIDTH_MULTIPLIER);
	const typingRx = status.outerRadius;
	const typingExtension = Math.max(0, typingWidth - status.size);
	const typingBridgeShift = typingExtension * TYPING_BRIDGE_RIGHT_SHIFT_RATIO;
	const leftCx = status.cx - typingExtension + typingBridgeShift;
	const rightCx = status.cx + typingBridgeShift;
	const y = status.cy - typingRx;
	const bridgeHeight = typingRx * 2;
	return `(
				<>
					<circle fill="white" cx="${r}" cy="${r}" r="${r}" />
					<circle fill="black" cx="${rightCx}" cy="${status.cy}" r="${typingRx}" />
					<rect fill="black" x="${leftCx}" y="${y}" width="${typingExtension}" height="${bridgeHeight}" />
					<circle fill="black" cx="${leftCx}" cy="${status.cy}" r="${typingRx}" />
				</>
			)`;
}

function generateMobilePhoneMask(mobileStatus: MobileStatusGeometry): string {
	const displayExtraHeight = DESIGN_RULES.mobileDisplayExtraHeight;
	const displayExtraWidthPerSide = DESIGN_RULES.mobileDisplayExtraWidthPerSide;
	const screenWidth =
		mobileStatus.phoneWidth * DESIGN_RULES.mobileScreenWidth +
		displayExtraWidthPerSide * 2 -
		MOBILE_SCREEN_WIDTH_TRIM_PX;
	const screenHeight =
		mobileStatus.phoneHeight * DESIGN_RULES.mobileScreenHeight + displayExtraHeight - MOBILE_SCREEN_HEIGHT_TRIM_PX;
	const screenX = mobileStatus.phoneX + (mobileStatus.phoneWidth - screenWidth) / 2 + MOBILE_SCREEN_X_OFFSET_PX;
	const screenY =
		mobileStatus.phoneY +
		mobileStatus.phoneHeight * DESIGN_RULES.mobileScreenY -
		displayExtraHeight / 2 +
		MOBILE_SCREEN_Y_OFFSET_PX;
	const screenRx = Math.min(screenWidth, screenHeight) * 0.1;
	const wheelRadius = mobileStatus.phoneWidth * DESIGN_RULES.mobileWheelRadius;
	const wheelCx = mobileStatus.phoneX + mobileStatus.phoneWidth / 2;
	const wheelCy = mobileStatus.phoneY + mobileStatus.phoneHeight * DESIGN_RULES.mobileWheelY;
	return `(
				<>
					<rect fill="white" x="${mobileStatus.phoneX}" y="${mobileStatus.phoneY}" width="${mobileStatus.phoneWidth}" height="${mobileStatus.phoneHeight}" rx="${mobileStatus.phoneRx}" ry="${mobileStatus.phoneRx}" />
					<rect fill="black" x="${screenX}" y="${screenY}" width="${screenWidth}" height="${screenHeight}" rx="${screenRx}" ry="${screenRx}" />
					<circle fill="black" cx="${wheelCx}" cy="${wheelCy}" r="${wheelRadius}" />
				</>
			)`;
}

function generateStatusOnline(size: number, isMobile: boolean = false): string {
	const status = calculateStatusGeometry(size, isMobile);
	if (!isMobile) {
		return `<circle fill="white" cx="${status.cx}" cy="${status.cy}" r="${status.outerRadius}" />`;
	}
	return generateMobilePhoneMask(status as MobileStatusGeometry);
}

function generateStatusIdle(size: number, isMobile: boolean = false): string {
	const status = calculateStatusGeometry(size, isMobile);
	if (!isMobile) {
		const cutoutRadius = Math.round(status.outerRadius * DESIGN_RULES.idle.cutoutRadiusRatio);
		const cutoutOffsetDistance = Math.round(status.outerRadius * DESIGN_RULES.idle.cutoutOffsetRatio);
		const cutoutCx = status.cx - cutoutOffsetDistance;
		const cutoutCy = status.cy - cutoutOffsetDistance;
		return `(
				<>
					<circle fill="white" cx="${status.cx}" cy="${status.cy}" r="${status.outerRadius}" />
					<circle fill="black" cx="${cutoutCx}" cy="${cutoutCy}" r="${cutoutRadius}" />
				</>
			)`;
	}
	return generateMobilePhoneMask(status as MobileStatusGeometry);
}

function generateStatusDnd(size: number, isMobile: boolean = false): string {
	const status = calculateStatusGeometry(size, isMobile);
	if (!isMobile) {
		const barWidth = Math.round(status.outerRadius * DESIGN_RULES.dnd.barWidthRatio);
		const rawBarHeight = status.outerRadius * DESIGN_RULES.dnd.barHeightRatio;
		const barHeight = Math.max(DESIGN_RULES.dnd.minBarHeight, Math.round(rawBarHeight));
		const barX = status.cx - barWidth / 2;
		const barY = status.cy - barHeight / 2;
		const barRx = barHeight / 2;
		return `(
				<>
					<circle fill="white" cx="${status.cx}" cy="${status.cy}" r="${status.outerRadius}" />
					<rect fill="black" x="${barX}" y="${barY}" width="${barWidth}" height="${barHeight}" rx="${barRx}" ry="${barRx}" />
				</>
			)`;
	}
	return generateMobilePhoneMask(status as MobileStatusGeometry);
}

function generateStatusOffline(size: number): string {
	const status = calculateStatusGeometry(size);
	const innerRadius = Math.round(status.innerRadius * DESIGN_RULES.offline.innerRingRatio);
	return `(
				<>
					<circle fill="white" cx="${status.cx}" cy="${status.cy}" r="${status.outerRadius}" />
					<circle fill="black" cx="${status.cx}" cy="${status.cy}" r="${innerRadius}" />
				</>
			)`;
}

function generateStatusTyping(size: number): string {
	const status = calculateStatusGeometry(size);
	const typingWidth = Math.round(status.size * TYPING_WIDTH_MULTIPLIER);
	const typingHeight = status.size;
	const rx = status.outerRadius;
	const typingExtension = Math.max(0, typingWidth - status.size);
	const typingBridgeShift = typingExtension * TYPING_BRIDGE_RIGHT_SHIFT_RATIO;
	const x = status.cx - status.size / 2 - typingExtension + typingBridgeShift;
	const y = status.cy - typingHeight / 2;
	return `<rect fill="white" x="${x}" y="${y}" width="${typingWidth}" height="${typingHeight}" rx="${rx}" ry="${rx}" />`;
}

const SIZES: Array<AvatarSize> = [16, 20, 24, 32, 36, 40, 44, 48, 56, 80, 120];
const GENERATED_HEADER = '// SPDX-License-Identifier: AGPL-3.0-or-later\n\n';

let output = `${GENERATED_HEADER}type AvatarSize = ${SIZES.join(' | ')};

interface MaskDefinition {
	viewBox: string;
	content: React.ReactElement;
}

interface MaskSet {
	avatarDefault: MaskDefinition;
	avatarStatusRound: MaskDefinition;
	avatarStatusTyping: MaskDefinition;
	statusOnline: MaskDefinition;
	statusOnlineMobile: MaskDefinition;
	statusIdle: MaskDefinition;
	statusIdleMobile: MaskDefinition;
	statusDnd: MaskDefinition;
	statusDndMobile: MaskDefinition;
	statusOffline: MaskDefinition;
	statusTyping: MaskDefinition;
}

export const AVATAR_MASKS: Record<AvatarSize, MaskSet> = {
`;

for (const size of SIZES) {
	output += `	${size}: {
		avatarDefault: {
			viewBox: '0 0 ${size} ${size}',
			content: ${generateAvatarMaskDefault(size)},
		},
		avatarStatusRound: {
			viewBox: '0 0 ${size} ${size}',
			content: ${generateAvatarMaskStatusRound(size)},
		},
		avatarStatusTyping: {
			viewBox: '0 0 ${size} ${size}',
			content: ${generateAvatarMaskStatusTyping(size)},
		},
		statusOnline: {
			viewBox: '0 0 ${size} ${size}',
			content: ${generateStatusOnline(size, false)},
		},
		statusOnlineMobile: {
			viewBox: '0 0 ${size} ${size}',
			content: ${generateStatusOnline(size, true)},
		},
		statusIdle: {
			viewBox: '0 0 ${size} ${size}',
			content: ${generateStatusIdle(size, false)},
		},
		statusIdleMobile: {
			viewBox: '0 0 ${size} ${size}',
			content: ${generateStatusIdle(size, true)},
		},
		statusDnd: {
			viewBox: '0 0 ${size} ${size}',
			content: ${generateStatusDnd(size, false)},
		},
		statusDndMobile: {
			viewBox: '0 0 ${size} ${size}',
			content: ${generateStatusDnd(size, true)},
		},
		statusOffline: {
			viewBox: '0 0 ${size} ${size}',
			content: ${generateStatusOffline(size)},
		},
		statusTyping: {
			viewBox: '0 0 ${size} ${size}',
			content: ${generateStatusTyping(size)},
		},
	},
`;
}

output += `} as const;

export const SVGMasks = () => (
	<svg
		viewBox="0 0 1 1"
		aria-hidden={true}
		style={{
			position: 'absolute',
			pointerEvents: 'none',
			top: '-1px',
			left: '-1px',
			width: 1,
			height: 1,
		}}
	>
		<defs>
`;

for (const size of SIZES) {
	const status = calculateStatusGeometry(size, false);
	const mobileStatus = calculateStatusGeometry(size, true) as MobileStatusGeometry;
	const cx = status.cx / size;
	const cy = status.cy / size;
	const r = status.outerRadius / size;
	const idleCutoutR = Math.round(status.outerRadius * DESIGN_RULES.idle.cutoutRadiusRatio) / size;
	const idleCutoutOffset = Math.round(status.outerRadius * DESIGN_RULES.idle.cutoutOffsetRatio) / size;
	const idleCutoutCx = cx - idleCutoutOffset;
	const idleCutoutCy = cy - idleCutoutOffset;
	const dndBarWidth = Math.round(status.outerRadius * DESIGN_RULES.dnd.barWidthRatio) / size;
	const dndBarHeight =
		Math.max(DESIGN_RULES.dnd.minBarHeight, Math.round(status.outerRadius * DESIGN_RULES.dnd.barHeightRatio)) / size;
	const dndBarX = cx - dndBarWidth / 2;
	const dndBarY = cy - dndBarHeight / 2;
	const dndBarRx = dndBarHeight / 2;
	const offlineInnerR = Math.round(status.innerRadius * DESIGN_RULES.offline.innerRingRatio) / size;
	const typingWidthPx = Math.round(status.size * TYPING_WIDTH_MULTIPLIER);
	const typingExtensionPx = Math.max(0, typingWidthPx - status.size);
	const typingWidth = typingWidthPx / size;
	const typingHeight = status.size / size;
	const typingX =
		(status.cx - status.size / 2 - typingExtensionPx + typingExtensionPx * TYPING_BRIDGE_RIGHT_SHIFT_RATIO) / size;
	const typingY = cy - typingHeight / 2;
	const typingRx = status.outerRadius / size;
	const typingCutoutLeftCx =
		(status.cx - typingExtensionPx + typingExtensionPx * TYPING_BRIDGE_RIGHT_SHIFT_RATIO) / size;
	const typingCutoutRightCx = (status.cx + typingExtensionPx * TYPING_BRIDGE_RIGHT_SHIFT_RATIO) / size;
	const typingCutoutY = (status.cy - status.outerRadius) / size;
	const typingCutoutHeight = (status.outerRadius * 2) / size;
	const cutoutPhoneWidth = (mobileStatus.phoneWidth + mobileStatus.borderWidth * 2) / size;
	const cutoutPhoneHeight = (mobileStatus.phoneHeight + mobileStatus.borderWidth * 2) / size;
	const cutoutPhoneX = (mobileStatus.phoneX - mobileStatus.borderWidth) / size;
	const cutoutPhoneY = (mobileStatus.phoneY - mobileStatus.borderWidth) / size;
	const cutoutPhoneRx = (mobileStatus.phoneRx + mobileStatus.borderWidth) / size;
	const displayExtraHeight = DESIGN_RULES.mobileDisplayExtraHeight;
	const displayExtraWidthPerSide = DESIGN_RULES.mobileDisplayExtraWidthPerSide;
	const screenWidthPx =
		mobileStatus.phoneWidth * DESIGN_RULES.mobileScreenWidth +
		displayExtraWidthPerSide * 2 -
		MOBILE_SCREEN_WIDTH_TRIM_PX;
	const screenHeightPx =
		mobileStatus.phoneHeight * DESIGN_RULES.mobileScreenHeight + displayExtraHeight - MOBILE_SCREEN_HEIGHT_TRIM_PX;
	const screenXpx = mobileStatus.phoneX + (mobileStatus.phoneWidth - screenWidthPx) / 2 + MOBILE_SCREEN_X_OFFSET_PX;
	const screenYpx =
		mobileStatus.phoneY +
		mobileStatus.phoneHeight * DESIGN_RULES.mobileScreenY -
		displayExtraHeight / 2 +
		MOBILE_SCREEN_Y_OFFSET_PX;
	const screenRxPx = Math.min(screenWidthPx, screenHeightPx) * 0.1;
	const mobileScreenX = ((screenXpx - mobileStatus.phoneX) / mobileStatus.phoneWidth).toFixed(4);
	const mobileScreenY = ((screenYpx - mobileStatus.phoneY) / mobileStatus.phoneHeight).toFixed(4);
	const mobileScreenWidth = (screenWidthPx / mobileStatus.phoneWidth).toFixed(4);
	const mobileScreenHeight = ((screenHeightPx / mobileStatus.phoneHeight) * DESIGN_RULES.mobileAspectRatio).toFixed(4);
	const mobileScreenRx = (screenRxPx / mobileStatus.phoneWidth).toFixed(4);
	const mobileScreenRy = ((screenRxPx / mobileStatus.phoneWidth) * DESIGN_RULES.mobileAspectRatio).toFixed(4);
	output += `			<mask id="svg-mask-avatar-default-${size}" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<circle fill="white" cx="0.5" cy="0.5" r="0.5" />
			</mask>
			<mask id="svg-mask-avatar-status-round-${size}" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<circle fill="white" cx="0.5" cy="0.5" r="0.5" />
				<circle fill="black" cx="${formatNumber(cx)}" cy="${formatNumber(cy)}" r="${formatNumber(r)}" />
			</mask>
			<mask id="svg-mask-avatar-status-mobile-${size}" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<circle fill="white" cx="0.5" cy="0.5" r="0.5" />
				<rect fill="black" x="${formatNumber(cutoutPhoneX)}" y="${formatNumber(cutoutPhoneY)}" width="${formatNumber(cutoutPhoneWidth)}" height="${formatNumber(cutoutPhoneHeight)}" rx="${formatNumber(cutoutPhoneRx)}" ry="${formatNumber(cutoutPhoneRx)}" />
			</mask>
			<mask id="svg-mask-avatar-status-typing-${size}" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<circle fill="white" cx="0.5" cy="0.5" r="0.5" />
				<circle fill="black" cx="${formatNumber(typingCutoutRightCx)}" cy="${formatNumber(cy)}" r="${formatNumber(typingRx)}" />
				<rect fill="black" x="${formatNumber(typingCutoutLeftCx)}" y="${formatNumber(typingCutoutY)}" width="${formatNumber(typingExtensionPx / size)}" height="${formatNumber(typingCutoutHeight)}" />
				<circle fill="black" cx="${formatNumber(typingCutoutLeftCx)}" cy="${formatNumber(cy)}" r="${formatNumber(typingRx)}" />
			</mask>
			<mask id="svg-mask-status-online-${size}" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<circle fill="white" cx="${formatNumber(cx)}" cy="${formatNumber(cy)}" r="${formatNumber(r)}" />
			</mask>
			<mask id="svg-mask-status-online-mobile-${size}" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<rect fill="white" x="0" y="0" width="1" height="1" rx="${DESIGN_RULES.mobileCornerRadius}" ry="${(DESIGN_RULES.mobileCornerRadius * DESIGN_RULES.mobileAspectRatio).toFixed(4)}" />
				<rect fill="black" x="${mobileScreenX}" y="${mobileScreenY}" width="${mobileScreenWidth}" height="${mobileScreenHeight}" rx="${mobileScreenRx}" ry="${mobileScreenRy}" />
				<ellipse fill="black" cx="0.5" cy="${DESIGN_RULES.mobileWheelY}" rx="${DESIGN_RULES.mobileWheelRadius}" ry="${(DESIGN_RULES.mobileWheelRadius * DESIGN_RULES.mobileAspectRatio).toFixed(4)}" />
			</mask>
			<mask id="svg-mask-status-idle-${size}" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<circle fill="white" cx="${formatNumber(cx)}" cy="${formatNumber(cy)}" r="${formatNumber(r)}" />
				<circle fill="black" cx="${formatNumber(idleCutoutCx)}" cy="${formatNumber(idleCutoutCy)}" r="${formatNumber(idleCutoutR)}" />
			</mask>
			<mask id="svg-mask-status-dnd-${size}" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<circle fill="white" cx="${formatNumber(cx)}" cy="${formatNumber(cy)}" r="${formatNumber(r)}" />
				<rect fill="black" x="${formatNumber(dndBarX)}" y="${formatNumber(dndBarY)}" width="${formatNumber(dndBarWidth)}" height="${formatNumber(dndBarHeight)}" rx="${formatNumber(dndBarRx)}" ry="${formatNumber(dndBarRx)}" />
			</mask>
			<mask id="svg-mask-status-offline-${size}" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<circle fill="white" cx="${formatNumber(cx)}" cy="${formatNumber(cy)}" r="${formatNumber(r)}" />
				<circle fill="black" cx="${formatNumber(cx)}" cy="${formatNumber(cy)}" r="${formatNumber(offlineInnerR)}" />
			</mask>
			<mask id="svg-mask-status-typing-${size}" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<rect fill="white" x="${formatNumber(typingX)}" y="${formatNumber(typingY)}" width="${formatNumber(typingWidth)}" height="${formatNumber(typingHeight)}" rx="${formatNumber(typingRx)}" ry="${formatNumber(typingRx)}" />
			</mask>

`;
}

output += `			<mask id="svg-mask-status-online" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<circle fill="white" cx="0.5" cy="0.5" r="0.5" />
			</mask>
			<mask id="svg-mask-status-idle" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<circle fill="white" cx="0.5" cy="0.5" r="0.5" />
				<circle fill="black" cx="0.25" cy="0.25" r="0.375" />
			</mask>
			<mask id="svg-mask-status-dnd" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<circle fill="white" cx="0.5" cy="0.5" r="0.5" />
				<rect fill="black" x="0.125" y="0.375" width="0.75" height="0.25" rx="0.125" ry="0.125" />
			</mask>
			<mask id="svg-mask-status-offline" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<circle fill="white" cx="0.5" cy="0.5" r="0.5" />
				<circle fill="black" cx="0.5" cy="0.5" r="0.25" />
			</mask>
			<mask id="svg-mask-status-typing" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<rect fill="white" x="0" y="0" width="1" height="1" rx="0.5" ry="0.5" />
			</mask>
			<mask id="svg-mask-status-online-mobile" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<rect fill="white" x="0" y="0" width="1" height="1" rx="${DESIGN_RULES.mobileCornerRadius}" ry="${(DESIGN_RULES.mobileCornerRadius * DESIGN_RULES.mobileAspectRatio).toFixed(2)}" />
				<rect fill="black" x="${((1 - DESIGN_RULES.mobileScreenWidth) / 2).toFixed(4)}" y="${DESIGN_RULES.mobileScreenY}" width="${DESIGN_RULES.mobileScreenWidth}" height="${(DESIGN_RULES.mobileScreenHeight * DESIGN_RULES.mobileAspectRatio).toFixed(4)}" rx="0.04" ry="${(0.04 * DESIGN_RULES.mobileAspectRatio).toFixed(2)}" />
				<ellipse fill="black" cx="0.5" cy="${DESIGN_RULES.mobileWheelY}" rx="${DESIGN_RULES.mobileWheelRadius}" ry="${(DESIGN_RULES.mobileWheelRadius * DESIGN_RULES.mobileAspectRatio).toFixed(3)}" />
			</mask>
			<mask id="svg-mask-avatar-default" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<circle fill="white" cx="0.5" cy="0.5" r="0.5" />
			</mask>
		</defs>
	</svg>
);
`;

const outputPath = path.join(__dirname, '../src/features/ui/components/SVGMasks.tsx');

fs.writeFileSync(outputPath, output);

console.log(`Generated ${outputPath}`);

const layoutOutput = `${GENERATED_HEADER}export interface StatusGeometry {
	size: number;
	cx: number;
	cy: number;
	radius: number;
	borderWidth: number;
	isMobile?: boolean;
	phoneWidth?: number;
	phoneHeight?: number;
}

const STATUS_GEOMETRY: Record<number, StatusGeometry> = {
${SIZES.map((size) => {
	const geom = calculateStatusGeometry(size, false);
	return `	${size}: {size: ${formatNumber(geom.size)}, cx: ${formatNumber(geom.cx)}, cy: ${formatNumber(geom.cy)}, radius: ${formatNumber(geom.outerRadius)}, borderWidth: ${formatNumber(geom.borderWidth)}, isMobile: false}`;
}).join(',\n')},
};

const STATUS_GEOMETRY_MOBILE: Record<number, StatusGeometry> = {
${SIZES.map((size) => {
	const geom = calculateStatusGeometry(size, true) as MobileStatusGeometry;
	return `	${size}: {size: ${formatNumber(geom.size)}, cx: ${formatNumber(geom.cx)}, cy: ${formatNumber(geom.cy)}, radius: ${formatNumber(geom.outerRadius)}, borderWidth: ${formatNumber(geom.borderWidth)}, isMobile: true, phoneWidth: ${formatNumber(geom.phoneWidth)}, phoneHeight: ${formatNumber(geom.phoneHeight)}}`;
}).join(',\n')},
};

export function getStatusGeometry(avatarSize: number, isMobile: boolean = false): StatusGeometry {
	const map = isMobile ? STATUS_GEOMETRY_MOBILE : STATUS_GEOMETRY;

	if (map[avatarSize]) {
		return map[avatarSize];
	}

	const sizes = Object.keys(map)
		.map(Number)
		.sort((a, b) => a - b);
	const firstSize = sizes[0];
	const lastSize = sizes[sizes.length - 1];

	if (avatarSize <= firstSize) {
		return scaleGeometry(map[firstSize], avatarSize / firstSize, isMobile);
	}
	if (avatarSize >= lastSize) {
		return scaleGeometry(map[lastSize], avatarSize / lastSize, isMobile);
	}

	const upperSize = sizes.find((size) => size > avatarSize) ?? lastSize;
	const lowerSize = sizes[sizes.indexOf(upperSize) - 1] ?? firstSize;
	const progress = (avatarSize - lowerSize) / (upperSize - lowerSize);
	return interpolateGeometry(map[lowerSize], map[upperSize], progress, isMobile);
}

function interpolateGeometry(from: StatusGeometry, to: StatusGeometry, progress: number, isMobile: boolean): StatusGeometry {
	return {
		size: interpolate(from.size, to.size, progress),
		cx: interpolate(from.cx, to.cx, progress),
		cy: interpolate(from.cy, to.cy, progress),
		radius: interpolate(from.radius, to.radius, progress),
		borderWidth: interpolate(from.borderWidth, to.borderWidth, progress),
		isMobile,
		phoneWidth:
			from.phoneWidth != null && to.phoneWidth != null ? interpolate(from.phoneWidth, to.phoneWidth, progress) : undefined,
		phoneHeight:
			from.phoneHeight != null && to.phoneHeight != null
				? interpolate(from.phoneHeight, to.phoneHeight, progress)
				: undefined,
	};
}

function scaleGeometry(geometry: StatusGeometry, scale: number, isMobile: boolean): StatusGeometry {
	return {
		size: geometry.size * scale,
		cx: geometry.cx * scale,
		cy: geometry.cy * scale,
		radius: geometry.radius * scale,
		borderWidth: geometry.borderWidth * scale,
		isMobile,
		phoneWidth: geometry.phoneWidth == null ? undefined : geometry.phoneWidth * scale,
		phoneHeight: geometry.phoneHeight == null ? undefined : geometry.phoneHeight * scale,
	};
}

function interpolate(from: number, to: number, progress: number): number {
	return from + (to - from) * progress;
}
`;
const layoutPath = path.join(__dirname, '../src/features/ui/constants/AvatarStatusGeometry.ts');

fs.writeFileSync(layoutPath, layoutOutput);
