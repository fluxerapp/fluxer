// SPDX-License-Identifier: AGPL-3.0-or-later

export const GUILD_BANNER_DEFAULT_ASPECT_RATIO = 16 / 9;
export const GUILD_BANNER_MAX_VIEWPORT_HEIGHT_FRACTION = 0.3;
export const GUILD_BANNER_HEADER_GLASS_MAX_OPACITY = 0.72;
export const GUILD_BANNER_HEADER_GLASS_DISTANCE = 16;

export interface GuildBannerMetrics {
	readonly containerWidth: number;
	readonly viewportHeight: number;
	readonly headerHeight: number;
	readonly aspectRatio: number;
}

export interface GuildBannerGeometry {
	readonly bannerHeight: number;
	readonly collapseDistance: number;
	readonly heightCapped: boolean;
}

export function resolveGuildBannerGeometry({
	containerWidth,
	viewportHeight,
	headerHeight,
	aspectRatio,
}: GuildBannerMetrics): GuildBannerGeometry {
	if (containerWidth <= 0 || aspectRatio <= 0) {
		return {bannerHeight: headerHeight, collapseDistance: 0, heightCapped: false};
	}
	const idealHeight = containerWidth / aspectRatio;
	const viewportCap =
		viewportHeight > 0 ? viewportHeight * GUILD_BANNER_MAX_VIEWPORT_HEIGHT_FRACTION : Number.POSITIVE_INFINITY;
	const bannerHeight = Math.max(headerHeight, Math.min(idealHeight, viewportCap));
	return {
		bannerHeight,
		collapseDistance: Math.max(bannerHeight - headerHeight, 0),
		heightCapped: idealHeight > viewportCap,
	};
}

export function resolveGuildBannerCollapseProgress(scrollTop: number, collapseDistance: number): number {
	if (collapseDistance <= 0) {
		return 0;
	}
	return Math.min(1, Math.max(0, scrollTop / collapseDistance));
}

export function resolveGuildBannerHeaderGlassOpacity(scrollTop: number): number {
	if (scrollTop <= 0) {
		return 0;
	}
	const ramp = (scrollTop / GUILD_BANNER_HEADER_GLASS_DISTANCE) * GUILD_BANNER_HEADER_GLASS_MAX_OPACITY;
	return Math.min(GUILD_BANNER_HEADER_GLASS_MAX_OPACITY, ramp);
}

export function isGuildBannerHeaderFrosted(scrollTop: number): boolean {
	return resolveGuildBannerHeaderGlassOpacity(scrollTop) >= GUILD_BANNER_HEADER_GLASS_MAX_OPACITY;
}
