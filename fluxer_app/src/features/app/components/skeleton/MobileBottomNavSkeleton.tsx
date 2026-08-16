// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/skeleton/MobileBottomNavSkeleton.module.css';
import {SkeletonBlock} from '@app/features/app/components/skeleton/SkeletonBlock';
import {SkeletonCircle} from '@app/features/app/components/skeleton/SkeletonCircle';
import {SkeletonLine} from '@app/features/app/components/skeleton/SkeletonLine';
import {SkeletonEmphasis, SkeletonRadius} from '@app/features/app/components/skeleton/SkeletonStyle';
import {flxElementClassName} from '@app/lib/react';
import type React from 'react';

const ICON_SIZE = '1.5rem';
const LABEL_HEIGHT = '0.5rem';

const MobileBottomNavSkeletonGlyph = Object.freeze({
	ICON: 'icon',
	AVATAR: 'avatar',
} as const);

type MobileBottomNavSkeletonGlyph = (typeof MobileBottomNavSkeletonGlyph)[keyof typeof MobileBottomNavSkeletonGlyph];

interface MobileBottomNavItemSpec {
	glyph: MobileBottomNavSkeletonGlyph;
	labelWidth: string;
}

const HOME_NAV_ITEM: MobileBottomNavItemSpec = {glyph: MobileBottomNavSkeletonGlyph.ICON, labelWidth: '2.25rem'};
const VOICE_NAV_ITEM: MobileBottomNavItemSpec = {glyph: MobileBottomNavSkeletonGlyph.ICON, labelWidth: '2.5rem'};
const NOTIFICATIONS_NAV_ITEM: MobileBottomNavItemSpec = {glyph: MobileBottomNavSkeletonGlyph.ICON, labelWidth: '4rem'};
const YOU_NAV_ITEM: MobileBottomNavItemSpec = {glyph: MobileBottomNavSkeletonGlyph.AVATAR, labelWidth: '1.5rem'};

function resolveNavItems(voiceItemVisible: boolean): ReadonlyArray<MobileBottomNavItemSpec> {
	if (voiceItemVisible) {
		return [HOME_NAV_ITEM, VOICE_NAV_ITEM, NOTIFICATIONS_NAV_ITEM, YOU_NAV_ITEM];
	}
	return [HOME_NAV_ITEM, NOTIFICATIONS_NAV_ITEM, YOU_NAV_ITEM];
}

function renderNavItem(item: MobileBottomNavItemSpec, index: number): React.ReactNode {
	let glyph: React.ReactNode;
	if (item.glyph === MobileBottomNavSkeletonGlyph.AVATAR) {
		glyph = (
			<SkeletonCircle
				size={ICON_SIZE}
				data-flx="app.skeleton.mobile-bottom-nav-skeleton.render-nav-item.skeleton-circle"
			/>
		);
	} else {
		glyph = (
			<SkeletonBlock
				width={ICON_SIZE}
				height={ICON_SIZE}
				radius={SkeletonRadius.SMALL}
				data-flx="app.skeleton.mobile-bottom-nav-skeleton.render-nav-item.skeleton-block"
			/>
		);
	}
	return (
		<flx-app-mobile-bottom-nav-skeleton-item
			key={`mobile-bottom-nav-item-${index}`}
			className={flxElementClassName(styles.navItem)}
			data-flx="app.skeleton.mobile-bottom-nav-skeleton.render-nav-item.nav-item"
		>
			{glyph}
			<flx-app-mobile-bottom-nav-skeleton-label
				className={flxElementClassName(styles.navItemLabel)}
				data-flx="app.skeleton.mobile-bottom-nav-skeleton.render-nav-item.nav-item-label"
			>
				<SkeletonLine
					width={item.labelWidth}
					height={LABEL_HEIGHT}
					emphasis={SkeletonEmphasis.MUTED}
					data-flx="app.skeleton.mobile-bottom-nav-skeleton.render-nav-item.skeleton-line"
				/>
			</flx-app-mobile-bottom-nav-skeleton-label>
		</flx-app-mobile-bottom-nav-skeleton-item>
	);
}

interface MobileBottomNavSkeletonProps {
	readonly voiceItemVisible: boolean;
}

export const MobileBottomNavSkeleton: React.FC<MobileBottomNavSkeletonProps> = ({voiceItemVisible}) => (
	<flx-app-mobile-bottom-nav-skeleton
		className={flxElementClassName(styles.container)}
		aria-hidden
		data-flx="app.skeleton.mobile-bottom-nav-skeleton.container"
	>
		{resolveNavItems(voiceItemVisible).map(renderNavItem)}
	</flx-app-mobile-bottom-nav-skeleton>
);
