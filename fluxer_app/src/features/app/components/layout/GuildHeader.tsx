// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/GuildHeader.module.css';
import {GuildHeaderShell} from '@app/features/app/components/layout/GuildHeaderShell';
import {NativeDragRegion} from '@app/features/app/components/layout/NativeDragRegion';
import {useAnimatedImageUrl} from '@app/features/app/hooks/useAnimatedImageUrl';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import {clampWideAssetAspectRatio} from '@app/features/expressions/utils/AssetImageGeometry';
import {GuildHeaderBottomSheet} from '@app/features/guild/components/bottomsheets/GuildHeaderBottomSheet';
import {GuildBadge} from '@app/features/guild/components/GuildBadge';
import {GuildHeaderPopout} from '@app/features/guild/components/popouts/GuildHeaderPopout';
import type {Guild} from '@app/features/guild/models/Guild';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {GuildContextMenu} from '@app/features/ui/action_menu/GuildContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import Popout from '@app/features/ui/state/Popout';
import {SIDEBAR_WIDTH_DEFAULT} from '@app/features/ui/state/SidebarWidth';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CaretDownIcon, DotsThreeIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useLayoutEffect, useMemo, useRef, useState} from 'react';

const OPEN_COMMUNITY_MENU_FOR_DESCRIPTOR = msg({
	message: 'Open community menu for {guildName}',
	comment: 'Short label in the app layout guild header. Preserve {guildName}; it is inserted by code.',
});
const HEADER_MIN_HEIGHT = 56;
const DEFAULT_BANNER_ASPECT_RATIO = 16 / 9;
const MAX_VIEWPORT_HEIGHT_FRACTION = 0.3;
export const GuildHeader = observer(({guild}: {guild: Guild}) => {
	const {i18n} = useLingui();
	const {popouts} = Popout;
	const isOpen = 'guild-header' in popouts;
	const isMobile = MobileLayout.isMobileLayout();
	const staticBannerURL = useMemo(
		() => AvatarUtils.getGuildBannerURL({id: guild.id, banner: guild.banner}, false) || null,
		[guild.banner, guild.id],
	);
	const animatedBannerURL = useMemo(
		() => AvatarUtils.getGuildBannerURL({id: guild.id, banner: guild.banner}, true) || null,
		[guild.banner, guild.id],
	);
	const {hoverRef: bannerHoverRef, imageUrl: bannerURL} = useAnimatedImageUrl({
		staticUrl: staticBannerURL,
		animatedUrl: animatedBannerURL,
		kind: 'gif',
	});
	const isDetachedBanner = guild.features.has(GuildFeatures.DETACHED_BANNER);
	const showIntegratedBanner = Boolean(bannerURL && !isDetachedBanner);
	const headerContainerRef = useRef<HTMLElement | null>(null);
	const mergedHeaderContainerRef = useMergeRefs([headerContainerRef, bannerHoverRef]);
	const [containerWidth, setContainerWidth] = useState<number>(() =>
		isMobile && typeof window !== 'undefined' ? window.innerWidth : SIDEBAR_WIDTH_DEFAULT,
	);
	const [viewportHeight, setViewportHeight] = useState<number>(() =>
		typeof window !== 'undefined' ? window.innerHeight : 0,
	);
	useLayoutEffect(() => {
		const measure = () => {
			const width = headerContainerRef.current?.clientWidth;
			if (width) setContainerWidth((prev) => (prev === width ? prev : width));
			setViewportHeight((prev) => (prev === window.innerHeight ? prev : window.innerHeight));
		};
		measure();
		window.addEventListener('resize', measure);
		const visualViewport = typeof window !== 'undefined' ? window.visualViewport : null;
		visualViewport?.addEventListener('resize', measure);
		const container = headerContainerRef.current;
		const resizeObserver = container && typeof ResizeObserver !== 'undefined' ? new ResizeObserver(measure) : null;
		if (container && resizeObserver) {
			resizeObserver.observe(container);
		}
		return () => {
			window.removeEventListener('resize', measure);
			visualViewport?.removeEventListener('resize', measure);
			resizeObserver?.disconnect();
		};
	}, []);
	const {bannerMaxHeight, centerCrop} = (() => {
		if (!showIntegratedBanner || !bannerURL || !containerWidth) {
			return {bannerMaxHeight: HEADER_MIN_HEIGHT, centerCrop: false};
		}
		const aspectRatio =
			guild.bannerWidth && guild.bannerHeight
				? (clampWideAssetAspectRatio(guild.bannerWidth / guild.bannerHeight) ?? DEFAULT_BANNER_ASPECT_RATIO)
				: DEFAULT_BANNER_ASPECT_RATIO;
		const idealHeight = containerWidth / aspectRatio;
		const viewportCap = viewportHeight * MAX_VIEWPORT_HEIGHT_FRACTION;
		const isCapped = idealHeight > viewportCap;
		return {
			bannerMaxHeight: Math.max(HEADER_MIN_HEIGHT, Math.min(idealHeight, viewportCap)),
			centerCrop: isMobile && isCapped,
		};
	})();
	const handleContextMenu = useCallback(
		(event: React.MouseEvent) => {
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<GuildContextMenu
					guild={guild}
					onClose={onClose}
					data-flx="app.guild-header.handle-context-menu.guild-context-menu"
				/>
			));
		},
		[guild],
	);
	const headerButtonRef = useRef<HTMLDivElement | null>(null);
	return (
		<div className={styles.headerWrapper} data-flx="app.guild-header.header-wrapper">
			<NativeDragRegion
				as={motion.div}
				ref={mergedHeaderContainerRef}
				onContextMenu={handleContextMenu}
				className={clsx(
					styles.headerContainer,
					!showIntegratedBanner && styles.headerContainerNoBanner,
					!showIntegratedBanner && isOpen && styles.headerContainerActive,
				)}
				style={{height: showIntegratedBanner ? bannerMaxHeight : remFromPx(HEADER_MIN_HEIGHT)}}
				data-flx="app.guild-header.header-container.context-menu"
			>
				{showIntegratedBanner && (
					<>
						<div
							className={clsx(styles.bannerBackground, centerCrop && styles.bannerBackgroundCentered)}
							style={{backgroundImage: `url(${bannerURL})`}}
							data-flx="app.guild-header.banner-background"
						/>
						<div className={styles.bannerGradient} data-flx="app.guild-header.banner-gradient" />
					</>
				)}
				<GuildHeaderShell
					popoutId="guild-header"
					renderPopout={() => <GuildHeaderPopout guild={guild} data-flx="app.guild-header.guild-header-popout" />}
					renderBottomSheet={({isOpen, onClose}) => (
						<GuildHeaderBottomSheet
							isOpen={isOpen}
							onClose={onClose}
							guild={guild}
							data-flx="app.guild-header.guild-header-bottom-sheet"
						/>
					)}
					onContextMenu={handleContextMenu}
					className={styles.headerContent}
					triggerRef={headerButtonRef}
					ariaLabel={i18n._(OPEN_COMMUNITY_MENU_FOR_DESCRIPTOR, {guildName: guild.name})}
					data-flx="app.guild-header.header-content.context-menu"
				>
					{(isOpen) => (
						<>
							<GuildBadge
								features={guild.features}
								variant={showIntegratedBanner ? 'banner' : 'default'}
								tooltipPosition="bottom"
								data-flx="app.guild-header.guild-badge"
							/>
							<span
								className={showIntegratedBanner ? styles.guildNameWithBanner : styles.guildNameDefault}
								data-flx="app.guild-header.guild-name"
							>
								{guild.name}
							</span>
							{isMobile ? (
								<DotsThreeIcon
									weight="bold"
									className={showIntegratedBanner ? styles.dotsIconWithBanner : styles.dotsIconDefault}
									data-flx="app.guild-header.dots-icon"
								/>
							) : (
								<CaretDownIcon
									weight="bold"
									className={clsx(
										showIntegratedBanner ? styles.caretIconWithBanner : styles.caretIconDefault,
										isOpen && styles.caretIconOpen,
									)}
									data-flx="app.guild-header.caret-icon"
								/>
							)}
						</>
					)}
				</GuildHeaderShell>
			</NativeDragRegion>
		</div>
	);
});
