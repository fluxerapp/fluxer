// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/GuildHeader.module.css';
import {GuildHeaderShell} from '@app/features/app/components/layout/GuildHeaderShell';
import {NativeDragRegion} from '@app/features/app/components/layout/NativeDragRegion';
import {
	reportSkeletonGuildPresentation,
	SkeletonGuildBannerPlacement,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import type {GuildBannerPresentation} from '@app/features/app/hooks/useGuildBannerPresentation';
import {
	measureSkeletonTextWidthPx,
	useSkeletonLayoutReport,
} from '@app/features/app/hooks/useSkeletonLayoutMemoryCapture';
import {GuildHeaderBottomSheet} from '@app/features/guild/components/bottomsheets/GuildHeaderBottomSheet';
import {GuildBadge} from '@app/features/guild/components/GuildBadge';
import {GuildHeaderPopout} from '@app/features/guild/components/popouts/GuildHeaderPopout';
import type {Guild} from '@app/features/guild/models/Guild';
import {GuildContextMenu} from '@app/features/ui/action_menu/GuildContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import Popout from '@app/features/ui/state/Popout';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CaretDownIcon, DotsThreeIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useRef} from 'react';

const OPEN_COMMUNITY_MENU_FOR_DESCRIPTOR = msg({
	message: 'Open community menu for {guildName}',
	comment: 'Short label in the app layout guild header. Preserve {guildName}; it is inserted by code.',
});
const GUILD_BADGE_FEATURES: ReadonlyArray<string> = [
	GuildFeatures.VERIFIED,
	GuildFeatures.PARTNERED,
	GuildFeatures.DISCOVERABLE,
];

function resolveGuildBadgeVisible(features: ReadonlySet<string>): boolean {
	return GUILD_BADGE_FEATURES.some((feature) => features.has(feature));
}

function resolveSkeletonBannerPlacement(hasBanner: boolean, isDetachedBanner: boolean): SkeletonGuildBannerPlacement {
	if (!hasBanner) {
		return SkeletonGuildBannerPlacement.NONE;
	}
	if (isDetachedBanner) {
		return SkeletonGuildBannerPlacement.DETACHED;
	}
	return SkeletonGuildBannerPlacement.INTEGRATED;
}
export const GuildHeader = observer(({guild, banner}: {guild: Guild; banner: GuildBannerPresentation}) => {
	const {i18n} = useLingui();
	const {popouts} = Popout;
	const isOpen = 'guild-header' in popouts;
	const isMobile = MobileLayout.isMobileLayout();
	const showIntegratedBanner = banner.imageUrl != null;
	const collapsesOnScroll = showIntegratedBanner && banner.collapsible;
	const onBanner = showIntegratedBanner && !banner.frosted;
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
	const guildNameRef = useRef<HTMLSpanElement | null>(null);
	const badgeVisible = resolveGuildBadgeVisible(guild.features);
	const bannerPlacement = resolveSkeletonBannerPlacement(banner.hasBanner, banner.isDetached);
	useSkeletonLayoutReport(() => {
		reportSkeletonGuildPresentation(guild.id, {
			headerNameWidthPx: measureSkeletonTextWidthPx(guildNameRef.current),
			badgeVisible,
			bannerPlacement,
			bannerAspectRatio: banner.aspectRatio,
		});
	}, `${guild.id}:${guild.name}:${badgeVisible}:${bannerPlacement}:${banner.aspectRatio}:${isMobile}`);
	return (
		<div className={styles.headerWrapper} data-flx="app.guild-header.header-wrapper">
			{showIntegratedBanner && (
				<motion.div
					className={styles.bannerClip}
					style={{height: banner.bannerHeight, y: banner.clipShift, opacity: banner.imageOpacity}}
					aria-hidden
					data-flx="app.guild-header.banner-clip"
				>
					<motion.div
						className={clsx(styles.bannerImage, banner.centerCrop && styles.bannerImageCentered)}
						style={{
							height: banner.bannerHeight,
							y: banner.imageShift,
							backgroundImage: `url(${banner.imageUrl})`,
						}}
						data-flx="app.guild-header.banner-image"
					/>
				</motion.div>
			)}
			<NativeDragRegion
				ref={collapsesOnScroll ? undefined : banner.hoverRef}
				onContextMenu={handleContextMenu}
				className={clsx(
					styles.headerContainer,
					showIntegratedBanner ? styles.headerContainerBanner : styles.headerContainerNoBanner,
					collapsesOnScroll && styles.headerContainerCollapsing,
					!showIntegratedBanner && isOpen && styles.headerContainerActive,
				)}
				data-flx="app.guild-header.header-container.context-menu"
			>
				{showIntegratedBanner && (
					<>
						{collapsesOnScroll && (
							<motion.div
								className={styles.headerGlass}
								style={{opacity: banner.glassOpacity}}
								data-flx="app.guild-header.header-glass"
							/>
						)}
						<motion.div
							className={styles.bannerGradient}
							style={{opacity: banner.scrimOpacity}}
							data-flx="app.guild-header.banner-gradient"
						/>
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
					triggerRef={banner.headerRowRef}
					ariaLabel={i18n._(OPEN_COMMUNITY_MENU_FOR_DESCRIPTOR, {guildName: guild.name})}
					data-flx="app.guild-header.header-content.context-menu"
				>
					{(isOpen) => (
						<>
							<GuildBadge
								features={guild.features}
								variant={onBanner ? 'banner' : 'default'}
								tooltipPosition="bottom"
								data-flx="app.guild-header.guild-badge"
							/>
							<span
								ref={guildNameRef}
								className={onBanner ? styles.guildNameWithBanner : styles.guildNameDefault}
								data-flx="app.guild-header.guild-name"
							>
								{guild.name}
							</span>
							{isMobile ? (
								<DotsThreeIcon
									weight="bold"
									className={onBanner ? styles.dotsIconWithBanner : styles.dotsIconDefault}
									data-flx="app.guild-header.dots-icon"
								/>
							) : (
								<CaretDownIcon
									weight="bold"
									className={clsx(
										onBanner ? styles.caretIconWithBanner : styles.caretIconDefault,
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
