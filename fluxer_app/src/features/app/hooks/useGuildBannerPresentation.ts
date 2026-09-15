// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	GUILD_BANNER_DEFAULT_ASPECT_RATIO,
	GUILD_BANNER_HEADER_GLASS_MAX_OPACITY,
	isGuildBannerHeaderFrosted,
	resolveGuildBannerCollapseProgress,
	resolveGuildBannerGeometry,
	resolveGuildBannerHeaderGlassOpacity,
} from '@app/features/app/components/layout/GuildBannerCollapse';
import {useAnimatedImageUrl} from '@app/features/app/hooks/useAnimatedImageUrl';
import GuildHeaderCollapseRollout from '@app/features/app/state/GuildHeaderCollapseRollout';
import {clampWideAssetAspectRatio} from '@app/features/expressions/utils/AssetImageGeometry';
import type {Guild} from '@app/features/guild/models/Guild';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {SIDEBAR_WIDTH_DEFAULT} from '@app/features/ui/state/SidebarWidth';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {type MotionValue, useMotionValue, useMotionValueEvent, useTransform} from 'framer-motion';
import type React from 'react';
import {useCallback, useLayoutEffect, useMemo, useRef, useState} from 'react';

const HEADER_FALLBACK_HEIGHT = 56;

interface GuildBannerMeasurements {
	readonly containerWidth: number;
	readonly headerHeight: number;
	readonly viewportHeight: number;
}

export interface GuildBannerPresentation {
	readonly headerRowRef: React.RefCallback<HTMLElement>;
	readonly hoverRef: React.RefCallback<HTMLElement>;
	readonly imageUrl: string | null;
	readonly hasBanner: boolean;
	readonly isDetached: boolean;
	readonly aspectRatio: number;
	readonly bannerHeight: number;
	readonly collapseDistance: number;
	readonly collapsible: boolean;
	readonly centerCrop: boolean;
	readonly frosted: boolean;
	readonly collapsed: boolean;
	readonly clipShift: MotionValue<number>;
	readonly imageShift: MotionValue<number>;
	readonly imageOpacity: MotionValue<number>;
	readonly glassOpacity: MotionValue<number>;
	readonly scrimOpacity: MotionValue<number>;
}

export function useGuildBannerPresentation({
	guild,
	scrollY,
}: {
	guild: Guild;
	scrollY: MotionValue<number>;
}): GuildBannerPresentation {
	const isMobile = MobileLayout.isMobileLayout();
	const isDetached = guild.features.has(GuildFeatures.DETACHED_BANNER);
	const [headerRowNode, setHeaderRowNode] = useState<HTMLElement | null>(null);
	const headerRowRef = useCallback((node: HTMLElement | null) => {
		setHeaderRowNode(node);
	}, []);
	const [measurements, setMeasurements] = useState<GuildBannerMeasurements>(() => ({
		containerWidth: isMobile && typeof window !== 'undefined' ? window.innerWidth : SIDEBAR_WIDTH_DEFAULT,
		headerHeight: HEADER_FALLBACK_HEIGHT,
		viewportHeight: typeof window !== 'undefined' ? window.innerHeight : 0,
	}));
	useLayoutEffect(() => {
		if (headerRowNode == null) return;
		const measure = () => {
			setMeasurements((previous) => {
				const next: GuildBannerMeasurements = {
					containerWidth: headerRowNode.offsetWidth || previous.containerWidth,
					headerHeight: headerRowNode.offsetHeight || previous.headerHeight,
					viewportHeight: window.innerHeight,
				};
				if (
					next.containerWidth === previous.containerWidth &&
					next.headerHeight === previous.headerHeight &&
					next.viewportHeight === previous.viewportHeight
				) {
					return previous;
				}
				return next;
			});
		};
		measure();
		window.addEventListener('resize', measure);
		const visualViewport = window.visualViewport;
		visualViewport?.addEventListener('resize', measure);
		const resizeObserver = typeof ResizeObserver !== 'undefined' ? new ResizeObserver(measure) : null;
		resizeObserver?.observe(headerRowNode);
		return () => {
			window.removeEventListener('resize', measure);
			visualViewport?.removeEventListener('resize', measure);
			resizeObserver?.disconnect();
		};
	}, [headerRowNode]);
	const aspectRatio = useMemo(() => {
		if (!guild.bannerWidth || !guild.bannerHeight) return GUILD_BANNER_DEFAULT_ASPECT_RATIO;
		return clampWideAssetAspectRatio(guild.bannerWidth / guild.bannerHeight) ?? GUILD_BANNER_DEFAULT_ASPECT_RATIO;
	}, [guild.bannerHeight, guild.bannerWidth]);
	const staticBannerUrl = useMemo(
		() => AvatarUtils.getGuildBannerURL({id: guild.id, banner: guild.banner}, false) || null,
		[guild.banner, guild.id],
	);
	const hasBanner = staticBannerUrl != null;
	const showsIntegratedBanner = hasBanner && !isDetached;
	const geometry = useMemo(() => {
		if (!showsIntegratedBanner) {
			return {bannerHeight: measurements.headerHeight, collapseDistance: 0, heightCapped: false};
		}
		return resolveGuildBannerGeometry({
			containerWidth: measurements.containerWidth,
			viewportHeight: measurements.viewportHeight,
			headerHeight: measurements.headerHeight,
			aspectRatio,
		});
	}, [aspectRatio, measurements, showsIntegratedBanner]);
	const collapseEnabled = showsIntegratedBanner && GuildHeaderCollapseRollout.collapsesOnScroll;
	const collapseDistance = collapseEnabled ? geometry.collapseDistance : 0;
	const progress = useMotionValue(0);
	const clipShift = useMotionValue(0);
	const glassOpacity = useMotionValue(0);
	const imageShift = useTransform(clipShift, (value) => -value);
	const imageOpacity = useTransform(progress, [0, 1], [1, 0]);
	const scrimOpacity = useTransform(glassOpacity, (value) => 1 - value / GUILD_BANNER_HEADER_GLASS_MAX_OPACITY);
	const [frosted, setFrosted] = useState(false);
	const [collapsed, setCollapsed] = useState(false);
	const frostedRef = useRef(false);
	const collapsedRef = useRef(false);
	const syncToScroll = useCallback(() => {
		const scrollTop = scrollY.get();
		const nextProgress = resolveGuildBannerCollapseProgress(scrollTop, collapseDistance);
		progress.set(nextProgress);
		clipShift.set(-nextProgress * collapseDistance);
		glassOpacity.set(collapseDistance > 0 ? resolveGuildBannerHeaderGlassOpacity(scrollTop) : 0);
		const nextFrosted = collapseDistance > 0 && isGuildBannerHeaderFrosted(scrollTop);
		if (frostedRef.current !== nextFrosted) {
			frostedRef.current = nextFrosted;
			setFrosted(nextFrosted);
		}
		const nextCollapsed = collapseDistance > 0 && nextProgress >= 1;
		if (collapsedRef.current !== nextCollapsed) {
			collapsedRef.current = nextCollapsed;
			setCollapsed(nextCollapsed);
		}
	}, [clipShift, collapseDistance, glassOpacity, progress, scrollY]);
	useMotionValueEvent(scrollY, 'change', syncToScroll);
	useLayoutEffect(() => {
		syncToScroll();
	}, [syncToScroll]);
	const animatedBannerUrl = useMemo(() => {
		if (!showsIntegratedBanner || collapsed) return null;
		return AvatarUtils.getGuildBannerURL({id: guild.id, banner: guild.banner}, true) || null;
	}, [collapsed, guild.banner, guild.id, showsIntegratedBanner]);
	const {hoverRef, imageUrl} = useAnimatedImageUrl({
		staticUrl: showsIntegratedBanner ? staticBannerUrl : null,
		animatedUrl: animatedBannerUrl,
		kind: 'gif',
	});
	return {
		headerRowRef,
		hoverRef,
		imageUrl,
		hasBanner,
		isDetached,
		aspectRatio,
		bannerHeight: geometry.bannerHeight,
		collapseDistance,
		collapsible: imageUrl != null && collapseDistance > 0,
		centerCrop: isMobile && geometry.heightCapped,
		frosted,
		collapsed,
		clipShift,
		imageShift,
		imageOpacity,
		glassOpacity,
		scrimOpacity,
	};
}
