// SPDX-License-Identifier: AGPL-3.0-or-later

import {appZoomLayoutPx} from '@app/features/ui/utils/AppZoomUtils';
import {
	COMPACT_VOICE_CALL_HEIGHT_MAX,
	COMPACT_VOICE_CALL_HEIGHT_MIN,
} from '@app/features/voice/state/CompactVoiceCallHeight';

export const COMPACT_HEIGHT_MIN = COMPACT_VOICE_CALL_HEIGHT_MIN;
const COMPACT_HEIGHT_VIEWPORT_MARGIN = 32;
const COMPACT_HEIGHT_CHAT_AREA_RESERVATION = 220;
export const COMPACT_HEIGHT_MESSAGES_SLIVER = 56;

function resolveCompactHeightMax({
	compactHeightMin,
	viewportHeight,
	chatAreaReservation = COMPACT_HEIGHT_CHAT_AREA_RESERVATION,
	viewportMargin = COMPACT_HEIGHT_VIEWPORT_MARGIN,
	hardCap = COMPACT_VOICE_CALL_HEIGHT_MAX,
}: {
	compactHeightMin: number;
	viewportHeight: number;
	chatAreaReservation?: number;
	viewportMargin?: number;
	hardCap?: number;
}): number {
	const reservation = Math.max(viewportMargin, chatAreaReservation);
	const viewportLimitedMax = Math.round(viewportHeight - reservation);
	return Math.max(compactHeightMin, Math.min(viewportLimitedMax, hardCap));
}

export function getCompactHeightMax(compactHeightMin: number): number {
	return resolveCompactHeightMax({
		compactHeightMin,
		viewportHeight: appZoomLayoutPx(window.innerHeight),
	});
}

export function resolveCompactHeightMaxFromLayout({
	compactHeightMin,
	availableSpan,
	chatReservation,
	hardCap = COMPACT_VOICE_CALL_HEIGHT_MAX,
}: {
	compactHeightMin: number;
	availableSpan: number;
	chatReservation: number;
	hardCap?: number;
}): number {
	const limited = Math.round(availableSpan - chatReservation);
	return Math.max(compactHeightMin, Math.min(limited, hardCap));
}
