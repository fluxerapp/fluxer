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

import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

type AvatarSize = 16 | 20 | 24 | 32 | 36 | 40 | 48 | 56 | 80 | 120;

interface StatusConfig {
	statusSize: number;
	cutoutRadius: number;
	cutoutCenter: number;
}

const STATUS_CONFIG: Record<number, StatusConfig> = {
	16: {statusSize: 10, cutoutRadius: 5, cutoutCenter: 13},
	20: {statusSize: 10, cutoutRadius: 5, cutoutCenter: 17},
	24: {statusSize: 10, cutoutRadius: 7, cutoutCenter: 20},
	32: {statusSize: 10, cutoutRadius: 8, cutoutCenter: 27},
	36: {statusSize: 10, cutoutRadius: 8, cutoutCenter: 30},
	40: {statusSize: 12, cutoutRadius: 9, cutoutCenter: 34},
	48: {statusSize: 14, cutoutRadius: 10, cutoutCenter: 42},
	56: {statusSize: 16, cutoutRadius: 11, cutoutCenter: 49},
	80: {statusSize: 16, cutoutRadius: 14, cutoutCenter: 68},
	120: {statusSize: 24, cutoutRadius: 20, cutoutCenter: 100},
};

const DESIGN_RULES = {
	mobileAspectRatio: 0.75,
	mobileCornerRadius: 0.12,
	mobileScreenWidth: 0.72,
	mobileScreenHeight: 0.7,
	mobileScreenY: 0.06,
	mobileWheelRadius: 0.13,
	mobileWheelY: 0.83,

	typingWidthMultiplier: 1.8,
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
	const closest = sizes.reduce((prev, curr) =>
		Math.abs(curr - avatarSize) < Math.abs(prev - avatarSize) ? curr : prev,
	);
	return STATUS_CONFIG[closest];
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

	const typingWidth = Math.round(status.size * DESIGN_RULES.typingWidthMultiplier);
	const typingHeight = status.size;
	const typingRx = status.outerRadius;

	const x = status.cx - typingWidth / 2;
	const y = status.cy - typingHeight / 2;

	return `(
				<>
					<circle fill="white" cx="${r}" cy="${r}" r="${r}" />
					<rect fill="black" x="${x}" y="${y}" width="${typingWidth}" height="${typingHeight}" rx="${typingRx}" ry="${typingRx}" />
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
	const typingWidth = Math.round(status.size * DESIGN_RULES.typingWidthMultiplier);
	const typingHeight = status.size;
	const rx = status.outerRadius;
	const x = status.cx - typingWidth / 2;
	const y = status.cy - typingHeight / 2;

	return `<rect fill="white" x="${x}" y="${y}" width="${typingWidth}" height="${typingHeight}" rx="${rx}" ry="${rx}" />`;
}

const SIZES: Array<AvatarSize> = [16, 20, 24, 32, 36, 40, 48, 56, 80, 120];

let output = `// @generated - DO NOT EDIT MANUALLY
// Run: pnpm generate:masks

type AvatarSize = ${SIZES.join(' | ')};

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

	const typingWidth = Math.round(status.size * DESIGN_RULES.typingWidthMultiplier) / size;
	const typingHeight = status.size / size;
	const typingX = cx - typingWidth / 2;
	const typingY = cy - typingHeight / 2;
	const typingRx = status.outerRadius / size;

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
				<circle fill="black" cx="${cx}" cy="${cy}" r="${r}" />
			</mask>
			<mask id="svg-mask-avatar-status-mobile-${size}" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<circle fill="white" cx="0.5" cy="0.5" r="0.5" />
				<rect fill="black" x="${cutoutPhoneX}" y="${cutoutPhoneY}" width="${cutoutPhoneWidth}" height="${cutoutPhoneHeight}" rx="${cutoutPhoneRx}" ry="${cutoutPhoneRx}" />
			</mask>
			<mask id="svg-mask-avatar-status-typing-${size}" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<circle fill="white" cx="0.5" cy="0.5" r="0.5" />
				<rect fill="black" x="${typingX}" y="${typingY}" width="${typingWidth}" height="${typingHeight}" rx="${typingRx}" ry="${typingRx}" />
			</mask>
			<mask id="svg-mask-status-online-${size}" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<circle fill="white" cx="${cx}" cy="${cy}" r="${r}" />
			</mask>
			<mask id="svg-mask-status-online-mobile-${size}" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<rect fill="white" x="0" y="0" width="1" height="1" rx="${DESIGN_RULES.mobileCornerRadius}" ry="${(DESIGN_RULES.mobileCornerRadius * DESIGN_RULES.mobileAspectRatio).toFixed(4)}" />
				<rect fill="black" x="${mobileScreenX}" y="${mobileScreenY}" width="${mobileScreenWidth}" height="${mobileScreenHeight}" rx="${mobileScreenRx}" ry="${mobileScreenRy}" />
				<ellipse fill="black" cx="0.5" cy="${DESIGN_RULES.mobileWheelY}" rx="${DESIGN_RULES.mobileWheelRadius}" ry="${(DESIGN_RULES.mobileWheelRadius * DESIGN_RULES.mobileAspectRatio).toFixed(4)}" />
			</mask>
			<mask id="svg-mask-status-idle-${size}" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<circle fill="white" cx="${cx}" cy="${cy}" r="${r}" />
				<circle fill="black" cx="${idleCutoutCx}" cy="${idleCutoutCy}" r="${idleCutoutR}" />
			</mask>
			<mask id="svg-mask-status-dnd-${size}" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<circle fill="white" cx="${cx}" cy="${cy}" r="${r}" />
				<rect fill="black" x="${dndBarX}" y="${dndBarY}" width="${dndBarWidth}" height="${dndBarHeight}" rx="${dndBarRx}" ry="${dndBarRx}" />
			</mask>
			<mask id="svg-mask-status-offline-${size}" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<circle fill="white" cx="${cx}" cy="${cy}" r="${r}" />
				<circle fill="black" cx="${cx}" cy="${cy}" r="${offlineInnerR}" />
			</mask>
			<mask id="svg-mask-status-typing-${size}" maskContentUnits="objectBoundingBox" viewBox="0 0 1 1">
				<rect fill="white" x="${typingX}" y="${typingY}" width="${typingWidth}" height="${typingHeight}" rx="${typingRx}" ry="${typingRx}" />
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

const outputPath = path.join(__dirname, '../src/components/uikit/SVGMasks.tsx');
fs.writeFileSync(outputPath, output);

console.log(`Generated ${outputPath}`);

const layoutOutput = `// @generated - DO NOT EDIT MANUALLY
// Run: pnpm generate:masks

export interface StatusGeometry {
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
	return `	${size}: {size: ${geom.size}, cx: ${geom.cx}, cy: ${geom.cy}, radius: ${geom.outerRadius}, borderWidth: ${geom.borderWidth}, isMobile: false}`;
}).join(',\n')},
};

const STATUS_GEOMETRY_MOBILE: Record<number, StatusGeometry> = {
${SIZES.map((size) => {
	const geom = calculateStatusGeometry(size, true) as MobileStatusGeometry;
	return `	${size}: {size: ${geom.size}, cx: ${geom.cx}, cy: ${geom.cy}, radius: ${geom.outerRadius}, borderWidth: ${geom.borderWidth}, isMobile: true, phoneWidth: ${geom.phoneWidth}, phoneHeight: ${geom.phoneHeight}}`;
}).join(',\n')},
};

export function getStatusGeometry(avatarSize: number, isMobile: boolean = false): StatusGeometry {
	const map = isMobile ? STATUS_GEOMETRY_MOBILE : STATUS_GEOMETRY;

	if (map[avatarSize]) {
		return map[avatarSize];
	}

	const closestSize = Object.keys(map)
		.map(Number)
		.reduce((prev, curr) => (Math.abs(curr - avatarSize) < Math.abs(prev - avatarSize) ? curr : prev));

	return map[closestSize];
}
`;

const layoutPath = path.join(__dirname, '../src/components/uikit/AvatarStatusGeometry.ts');
fs.writeFileSync(layoutPath, layoutOutput);
