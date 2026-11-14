// SPDX-License-Identifier: AGPL-3.0-or-later

import {getStatusGeometry} from '@app/features/ui/constants/AvatarStatusGeometry';
import {TYPING_BRIDGE_RIGHT_SHIFT_RATIO, TYPING_WIDTH_MULTIPLIER} from '@app/features/ui/constants/TypingConstants';

export interface AvatarStatusLayout {
	supportsStatus: boolean;
	innerStatusWidth: number;
	innerStatusHeight: number;
	innerTypingWidth: number;
	innerTypingHeight: number;
	innerStatusRight: number;
	innerStatusBottom: number;
	innerTypingRight: number;
	innerTypingBottom: number;
	cutoutCx: number;
	cutoutCy: number;
	cutoutRadius: number;
}

const LAYOUT_CACHE = new Map<string, AvatarStatusLayout>();

export function getAvatarStatusLayout(size: number, isMobile: boolean = false): AvatarStatusLayout {
	const cacheKey = `${isMobile ? 'mobile' : 'desktop'}:${size}`;
	const cached = LAYOUT_CACHE.get(cacheKey);
	if (cached) return cached;

	const supportsStatus = size > 16;
	const geom = getStatusGeometry(size, isMobile);
	const statusSize = geom.size;
	const statusHeight = isMobile && geom.phoneHeight ? geom.phoneHeight : statusSize;
	const typingWidth = Math.round(statusSize * TYPING_WIDTH_MULTIPLIER);
	const typingHeight = statusSize;
	const innerStatusWidth = statusSize;
	const innerStatusHeight = isMobile && geom.phoneHeight ? geom.phoneHeight : statusSize;
	const innerTypingWidth = typingWidth;
	const innerTypingHeight = typingHeight;
	const cutoutCx = geom.cx;
	const cutoutCy = geom.cy;
	const cutoutRadius = geom.radius;
	const typingExtension = Math.max(0, typingWidth - statusSize);
	const typingBridgeShift = typingExtension * TYPING_BRIDGE_RIGHT_SHIFT_RATIO;
	const innerStatusRight = size - cutoutCx - statusSize / 2;
	const innerStatusBottom = size - cutoutCy - statusHeight / 2;
	const innerTypingRight = innerStatusRight - typingBridgeShift;
	const innerTypingBottom = size - cutoutCy - typingHeight / 2;
	const layout = {
		supportsStatus,
		innerStatusWidth,
		innerStatusHeight,
		innerTypingWidth,
		innerTypingHeight,
		innerStatusRight,
		innerStatusBottom,
		innerTypingRight,
		innerTypingBottom,
		cutoutCx,
		cutoutCy,
		cutoutRadius,
	};
	LAYOUT_CACHE.set(cacheKey, layout);
	return layout;
}
