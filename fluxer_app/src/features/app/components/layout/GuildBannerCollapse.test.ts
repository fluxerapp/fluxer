// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	GUILD_BANNER_HEADER_GLASS_DISTANCE,
	GUILD_BANNER_HEADER_GLASS_MAX_OPACITY,
	isGuildBannerHeaderFrosted,
	resolveGuildBannerCollapseProgress,
	resolveGuildBannerGeometry,
	resolveGuildBannerHeaderGlassOpacity,
} from '@app/features/app/components/layout/GuildBannerCollapse';
import {describe, expect, it} from 'vitest';

describe('resolveGuildBannerGeometry', () => {
	it('sizes the banner from the sidebar width and the banner aspect ratio', () => {
		const geometry = resolveGuildBannerGeometry({
			containerWidth: 320,
			viewportHeight: 1000,
			headerHeight: 56,
			aspectRatio: 16 / 9,
		});
		expect(geometry.bannerHeight).toBeCloseTo(180);
		expect(geometry.collapseDistance).toBeCloseTo(124);
		expect(geometry.heightCapped).toBe(false);
	});

	it('caps the banner at a fraction of the viewport height', () => {
		const geometry = resolveGuildBannerGeometry({
			containerWidth: 480,
			viewportHeight: 500,
			headerHeight: 56,
			aspectRatio: 1,
		});
		expect(geometry.bannerHeight).toBeCloseTo(150);
		expect(geometry.collapseDistance).toBeCloseTo(94);
		expect(geometry.heightCapped).toBe(true);
	});

	it('never collapses below the header height', () => {
		const geometry = resolveGuildBannerGeometry({
			containerWidth: 200,
			viewportHeight: 120,
			headerHeight: 56,
			aspectRatio: 21 / 9,
		});
		expect(geometry.bannerHeight).toBe(56);
		expect(geometry.collapseDistance).toBe(0);
	});

	it('reports no banner height before the header is measured', () => {
		const geometry = resolveGuildBannerGeometry({
			containerWidth: 0,
			viewportHeight: 0,
			headerHeight: 56,
			aspectRatio: 16 / 9,
		});
		expect(geometry.bannerHeight).toBe(56);
		expect(geometry.collapseDistance).toBe(0);
	});
});

describe('resolveGuildBannerCollapseProgress', () => {
	it('tracks scroll position across the collapse distance', () => {
		expect(resolveGuildBannerCollapseProgress(0, 80)).toBe(0);
		expect(resolveGuildBannerCollapseProgress(40, 80)).toBe(0.5);
		expect(resolveGuildBannerCollapseProgress(80, 80)).toBe(1);
	});

	it('clamps overscroll and bounce past both ends', () => {
		expect(resolveGuildBannerCollapseProgress(-30, 80)).toBe(0);
		expect(resolveGuildBannerCollapseProgress(4000, 80)).toBe(1);
	});

	it('stays expanded when there is nothing to collapse', () => {
		expect(resolveGuildBannerCollapseProgress(200, 0)).toBe(0);
	});
});

describe('resolveGuildBannerHeaderGlassOpacity', () => {
	it('ramps in over a fixed scroll distance regardless of banner height', () => {
		expect(resolveGuildBannerHeaderGlassOpacity(0)).toBe(0);
		expect(resolveGuildBannerHeaderGlassOpacity(GUILD_BANNER_HEADER_GLASS_DISTANCE / 2)).toBeCloseTo(
			GUILD_BANNER_HEADER_GLASS_MAX_OPACITY / 2,
		);
		expect(resolveGuildBannerHeaderGlassOpacity(GUILD_BANNER_HEADER_GLASS_DISTANCE)).toBeCloseTo(
			GUILD_BANNER_HEADER_GLASS_MAX_OPACITY,
		);
	});

	it('never exceeds the opaque header background', () => {
		expect(resolveGuildBannerHeaderGlassOpacity(5000)).toBe(GUILD_BANNER_HEADER_GLASS_MAX_OPACITY);
	});

	it('frosts the header once the glass is fully ramped in', () => {
		expect(isGuildBannerHeaderFrosted(0)).toBe(false);
		expect(isGuildBannerHeaderFrosted(GUILD_BANNER_HEADER_GLASS_DISTANCE - 1)).toBe(false);
		expect(isGuildBannerHeaderFrosted(GUILD_BANNER_HEADER_GLASS_DISTANCE)).toBe(true);
	});
});
