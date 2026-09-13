// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getCompactAudioAvatarLayoutStyle,
	resolveCompactAudioAvatarLayoutMetrics,
	resolveCompactControlGap,
	resolveCompactEdgeGradientExtension,
} from '@app/features/voice/components/compact_voice_call_view/CompactVoiceCallLayoutMetrics';
import {describe, expect, it} from 'vitest';

describe('CompactVoiceCallLayoutMetrics', () => {
	it('keeps the avatar row out of the compact footer gradient when controls are visible', () => {
		const metrics = resolveCompactAudioAvatarLayoutMetrics({
			callHeight: 320,
			controlBarHeight: 84,
			hasControlBar: true,
		});
		expect(metrics.gradientExtension).toBeCloseTo(resolveCompactEdgeGradientExtension(320, true), 5);
		expect(metrics.controlGap).toBeCloseTo(resolveCompactControlGap(320), 5);
		expect(metrics.bottomPadding).toBeCloseTo(
			metrics.controlHeight + metrics.gradientExtension + metrics.controlGap,
			5,
		);
		expect(metrics.bottomPadding).toBeGreaterThan(metrics.topPadding);
		expect(metrics.topPadding).toBeLessThan(metrics.bottomPadding * 0.35);
	});
	it('uses a modest top inset instead of mirroring the full control reserve', () => {
		for (const callHeight of [220, 280, 320, 420, 560]) {
			const metrics = resolveCompactAudioAvatarLayoutMetrics({
				callHeight,
				controlBarHeight: 88,
				hasControlBar: true,
			});
			expect(metrics.topPadding).toBeGreaterThanOrEqual(metrics.edgePadding);
			expect(metrics.topPadding).toBeLessThanOrEqual(28);
			expect(metrics.topPadding).toBeLessThan(metrics.bottomPadding / 2);
		}
	});
	it('keeps the compact footer fade short across resizable heights', () => {
		const extensions = [180, 240, 320, 480, 720].map(
			(callHeight) =>
				resolveCompactAudioAvatarLayoutMetrics({
					callHeight,
					controlBarHeight: 84,
					hasControlBar: true,
				}).gradientExtension,
		);
		expect(extensions[0]).toBe(14);
		expect(Math.max(...extensions)).toBeLessThanOrEqual(22);
		expect(extensions).toEqual([...extensions].sort((a, b) => a - b));
	});
	it('centers avatar fallback padding when there is no control bar overlay', () => {
		for (const callHeight of [220, 320, 640]) {
			const metrics = resolveCompactAudioAvatarLayoutMetrics({callHeight, hasControlBar: false});
			expect(metrics.controlHeight).toBe(0);
			expect(metrics.topPadding).toBe(metrics.bottomPadding);
			expect(metrics.gradientExtension).toBeLessThanOrEqual(20);
		}
	});
	it('accounts for safe-area bottom insets without making the fade taller', () => {
		const plain = resolveCompactAudioAvatarLayoutMetrics({
			callHeight: 320,
			hasControlBar: true,
		});
		const inset = resolveCompactAudioAvatarLayoutMetrics({
			callHeight: 320,
			hasControlBar: true,
			safeAreaBottom: 28,
		});
		expect(inset.controlHeight - plain.controlHeight).toBe(28);
		expect(inset.bottomPadding - plain.bottomPadding).toBeCloseTo(28, 5);
		expect(inset.gradientExtension).toBe(plain.gradientExtension);
		expect(inset.topPadding).toBe(plain.topPadding);
	});
	it('serializes stable CSS variables for the compact call container', () => {
		const metrics = resolveCompactAudioAvatarLayoutMetrics({
			callHeight: 320,
			controlBarHeight: 84,
			hasControlBar: true,
		});
		expect(getCompactAudioAvatarLayoutStyle(metrics)).toEqual({
			'--compact-call-audio-avatar-padding-top': '21px',
			'--compact-call-audio-avatar-padding-bottom': '112px',
			'--compact-call-edge-gradient-extension': '17px',
		});
	});
});
