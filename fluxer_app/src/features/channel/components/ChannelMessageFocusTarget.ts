// SPDX-License-Identifier: AGPL-3.0-or-later

export const MIN_VISIBLE_RATIO_FOR_FOCUS = 0.75;

export interface MessageFocusCandidate {
	messageId: string;
	top: number;
	bottom: number;
	height: number;
}

export function resolveBottommostFocusableMessageId(
	candidates: ReadonlyArray<MessageFocusCandidate>,
	viewportTop: number,
	viewportBottom: number,
): string | null {
	let bottommostVisibleId: string | null = null;
	let bottommostVisibleY = Number.NEGATIVE_INFINITY;
	let mostOverlappingId: string | null = null;
	let mostOverlappingHeight = 0;
	let mostOverlappingY = Number.NEGATIVE_INFINITY;
	for (const candidate of candidates) {
		if (candidate.height === 0) continue;
		const visibleTop = Math.max(candidate.top, viewportTop);
		const visibleBottom = Math.min(candidate.bottom, viewportBottom);
		const visibleHeight = Math.max(0, visibleBottom - visibleTop);
		if (
			visibleHeight > 0 &&
			(visibleHeight > mostOverlappingHeight ||
				(visibleHeight === mostOverlappingHeight && candidate.bottom > mostOverlappingY))
		) {
			mostOverlappingHeight = visibleHeight;
			mostOverlappingY = candidate.bottom;
			mostOverlappingId = candidate.messageId;
		}
		if (visibleHeight / candidate.height < MIN_VISIBLE_RATIO_FOR_FOCUS) continue;
		if (candidate.bottom > bottommostVisibleY) {
			bottommostVisibleY = candidate.bottom;
			bottommostVisibleId = candidate.messageId;
		}
	}
	if (bottommostVisibleId != null) {
		return bottommostVisibleId;
	}
	if (mostOverlappingId != null) {
		return mostOverlappingId;
	}
	return candidates[candidates.length - 1]?.messageId ?? null;
}
