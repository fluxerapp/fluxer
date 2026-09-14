// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	COMPACT_HEIGHT_CHAT_AREA_RESERVATION,
	COMPACT_HEIGHT_MIN,
	resolveCompactHeightMax,
	resolveCompactHeightMaxFromLayout,
} from '@app/features/voice/components/compact_voice_call_view/CompactVoiceCallHeightBounds';
import {describe, expect, it} from 'vitest';

describe('resolveCompactHeightMax', () => {
	it('reserves chat area space below the call view so the composer remains visible', () => {
		const viewportHeight = 900;
		const max = resolveCompactHeightMax({
			compactHeightMin: COMPACT_HEIGHT_MIN,
			viewportHeight,
		});
		expect(viewportHeight - max).toBeGreaterThanOrEqual(COMPACT_HEIGHT_CHAT_AREA_RESERVATION);
	});
	it('never goes below the minimum even in a tiny viewport', () => {
		const max = resolveCompactHeightMax({
			compactHeightMin: COMPACT_HEIGHT_MIN,
			viewportHeight: 100,
		});
		expect(max).toBe(COMPACT_HEIGHT_MIN);
	});
	it('honors the hard cap on tall viewports', () => {
		const hardCap = 600;
		const max = resolveCompactHeightMax({
			compactHeightMin: COMPACT_HEIGHT_MIN,
			viewportHeight: 4000,
			hardCap,
		});
		expect(max).toBe(hardCap);
	});
	it('takes the larger of viewport margin and chat reservation', () => {
		const viewportHeight = 800;
		const max = resolveCompactHeightMax({
			compactHeightMin: COMPACT_HEIGHT_MIN,
			viewportHeight,
			chatAreaReservation: 10,
			viewportMargin: 250,
		});
		expect(viewportHeight - max).toBe(250);
	});
});

describe('resolveCompactHeightMaxFromLayout', () => {
	it('reserves the measured chat height so the composer stays visible', () => {
		const availableSpan = 900;
		const chatReservation = 420;
		const max = resolveCompactHeightMaxFromLayout({
			compactHeightMin: COMPACT_HEIGHT_MIN,
			availableSpan,
			chatReservation,
		});
		expect(max).toBe(availableSpan - chatReservation);
		expect(availableSpan - max).toBeGreaterThanOrEqual(chatReservation);
	});
	it('never goes below the minimum when the chat reservation dominates', () => {
		const max = resolveCompactHeightMaxFromLayout({
			compactHeightMin: COMPACT_HEIGHT_MIN,
			availableSpan: 500,
			chatReservation: 480,
		});
		expect(max).toBe(COMPACT_HEIGHT_MIN);
	});
	it('honors the hard cap on tall layouts', () => {
		const hardCap = 600;
		const max = resolveCompactHeightMaxFromLayout({
			compactHeightMin: COMPACT_HEIGHT_MIN,
			availableSpan: 4000,
			chatReservation: 200,
			hardCap,
		});
		expect(max).toBe(hardCap);
	});
});
