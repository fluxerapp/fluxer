// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/skeleton/SidebarShellSkeleton.module.css';
import {SkeletonBlock} from '@app/features/app/components/skeleton/SkeletonBlock';
import {
	type RememberedSkeletonGuildPresentation,
	SKELETON_UNMEASURED_WIDTH_PX,
	SkeletonGuildBannerPlacement,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {SkeletonLine} from '@app/features/app/components/skeleton/SkeletonLine';
import {SkeletonEmphasis, SkeletonRadius} from '@app/features/app/components/skeleton/SkeletonStyle';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {flxElementClassName} from '@app/lib/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const HEADER_NAME_FALLBACK_WIDTH = '55%';
const HEADER_NAME_HEIGHT = '0.75rem';
const HEADER_BADGE_SIZE = '1.25rem';
const HEADER_CARET_SIZE = '1rem';
const HEADER_DOTS_SIZE = '1.5rem';

interface SidebarShellSkeletonProps {
	readonly children: React.ReactNode;
	readonly guildPresentation?: RememberedSkeletonGuildPresentation | null;
}

function resolveHeaderNameWidth(guildPresentation: RememberedSkeletonGuildPresentation | null | undefined): string {
	if (guildPresentation == null || guildPresentation.headerNameWidthPx === SKELETON_UNMEASURED_WIDTH_PX) {
		return HEADER_NAME_FALLBACK_WIDTH;
	}
	return remFromPx(guildPresentation.headerNameWidthPx);
}

export const SidebarShellSkeleton = observer(({children, guildPresentation}: SidebarShellSkeletonProps) => {
	const isMobile = MobileLayout.enabled;
	const showIntegratedBanner = guildPresentation?.bannerPlacement === SkeletonGuildBannerPlacement.INTEGRATED;
	let trailingGlyphSize = HEADER_CARET_SIZE;
	if (isMobile) {
		trailingGlyphSize = HEADER_DOTS_SIZE;
	}
	let headerStyle: React.CSSProperties | undefined;
	if (showIntegratedBanner && guildPresentation != null) {
		headerStyle = {aspectRatio: `${guildPresentation.bannerAspectRatio}`};
	}
	return (
		<flx-app-sidebar-shell-skeleton
			className={flxElementClassName(styles.container, isMobile && styles.containerMobile)}
			aria-hidden
			data-flx="app.skeleton.sidebar-shell-skeleton.container"
		>
			<flx-app-sidebar-shell-skeleton-header-frame
				className={flxElementClassName(styles.headerFrame, showIntegratedBanner && styles.headerFrameBanner)}
				style={headerStyle}
				data-flx="app.skeleton.sidebar-shell-skeleton.header-frame"
			>
				{showIntegratedBanner && (
					<SkeletonBlock
						width="100%"
						height="100%"
						radius={SkeletonRadius.SHARP}
						emphasis={SkeletonEmphasis.MUTED}
						className={styles.headerBanner}
						data-flx="app.skeleton.sidebar-shell-skeleton.header-banner"
					/>
				)}
				<flx-app-sidebar-shell-skeleton-header
					className={flxElementClassName(styles.header)}
					data-flx="app.skeleton.sidebar-shell-skeleton.header"
				>
					{guildPresentation?.badgeVisible === true && (
						<SkeletonBlock
							width={HEADER_BADGE_SIZE}
							height={HEADER_BADGE_SIZE}
							radius={SkeletonRadius.SMALL}
							emphasis={SkeletonEmphasis.MUTED}
							data-flx="app.skeleton.sidebar-shell-skeleton.skeleton-block"
						/>
					)}
					<SkeletonLine
						width={resolveHeaderNameWidth(guildPresentation)}
						height={HEADER_NAME_HEIGHT}
						emphasis={SkeletonEmphasis.STRONG}
						data-flx="app.skeleton.sidebar-shell-skeleton.skeleton-line"
					/>
					<SkeletonBlock
						width={trailingGlyphSize}
						height={trailingGlyphSize}
						radius={SkeletonRadius.SMALL}
						emphasis={SkeletonEmphasis.MUTED}
						className={styles.headerCaret}
						data-flx="app.skeleton.sidebar-shell-skeleton.header-caret"
					/>
				</flx-app-sidebar-shell-skeleton-header>
			</flx-app-sidebar-shell-skeleton-header-frame>
			<flx-app-sidebar-shell-skeleton-content
				className={flxElementClassName(styles.content)}
				data-flx="app.skeleton.sidebar-shell-skeleton.content"
			>
				{children}
			</flx-app-sidebar-shell-skeleton-content>
		</flx-app-sidebar-shell-skeleton>
	);
});
