// SPDX-License-Identifier: AGPL-3.0-or-later

import {appZoomLayoutPx} from '@app/features/ui/utils/AppZoomUtils';
import {
	COMPACT_VOICE_CALL_HEIGHT_MAX,
	COMPACT_VOICE_CALL_HEIGHT_MIN,
} from '@app/features/voice/state/CompactVoiceCallHeight';

export const COMPACT_HEIGHT_MIN = COMPACT_VOICE_CALL_HEIGHT_MIN;
const COMPACT_HEIGHT_CHAT_AREA_RESERVATION = 220;
export const COMPACT_HEIGHT_MESSAGES_SLIVER = 56;

export function getCompactHeightMax(compactHeightMin: number): number {
	const viewportLimitedMax = Math.round(appZoomLayoutPx(window.innerHeight) - COMPACT_HEIGHT_CHAT_AREA_RESERVATION);
	return Math.max(compactHeightMin, Math.min(viewportLimitedMax, COMPACT_VOICE_CALL_HEIGHT_MAX));
}

export function resolveCompactHeightMaxFromLayout({
	compactHeightMin,
	availableSpan,
	chatReservation,
}: {
	compactHeightMin: number;
	availableSpan: number;
	chatReservation: number;
}): number {
	const limited = Math.round(availableSpan - chatReservation);
	return Math.max(compactHeightMin, Math.min(limited, COMPACT_VOICE_CALL_HEIGHT_MAX));
}
