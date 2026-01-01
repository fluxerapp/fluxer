/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {useLingui} from '@lingui/react/macro';
import {CaretDownIcon, DotsThreeIcon, SealCheckIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import {GuildFeatures} from '~/Constants';
import {GuildHeaderBottomSheet} from '~/components/bottomsheets/GuildHeaderBottomSheet';
import {GuildHeaderShell} from '~/components/layout/GuildHeaderShell';
import {NativeDragRegion} from '~/components/layout/NativeDragRegion';
import {GuildHeaderPopout} from '~/components/popouts/GuildHeaderPopout';
import {GuildContextMenu} from '~/components/uikit/ContextMenu/GuildContextMenu';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import type {GuildRecord} from '~/records/GuildRecord';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import PopoutStore from '~/stores/PopoutStore';
import * as AvatarUtils from '~/utils/AvatarUtils';
import styles from './GuildHeader.module.css';

const HEADER_MIN_HEIGHT = 56;
const DEFAULT_BANNER_ASPECT_RATIO = 16 / 9;
const MAX_VIEWPORT_HEIGHT_FRACTION = 0.3;

export const GuildHeader = observer(({guild}: {guild: GuildRecord}) => {
	const {t} = useLingui();
	const {popouts} = PopoutStore;
	const isOpen = 'guild-header' in popouts;
	const isMobile = MobileLayoutStore.isMobileLayout();

	const bannerURL = AvatarUtils.getGuildBannerURL({id: guild.id, banner: guild.banner}, true);
	const isDetachedBanner = guild.features.has(GuildFeatures.DETACHED_BANNER);
	const showIntegratedBanner = Boolean(bannerURL && !isDetachedBanner);

	const headerContainerRef = React.useRef<HTMLDivElement | null>(null);

	const calculateBannerLayout = React.useCallback(() => {
		if (!showIntegratedBanner || !bannerURL) {
			return {height: HEADER_MIN_HEIGHT, centerCrop: false};
		}

		const width = headerContainerRef.current?.clientWidth ?? window.innerWidth;
		if (!width) return {height: HEADER_MIN_HEIGHT, centerCrop: false};

		const aspectRatio =
			guild.bannerWidth && guild.bannerHeight ? guild.bannerWidth / guild.bannerHeight : DEFAULT_BANNER_ASPECT_RATIO;

		const idealHeight = width / aspectRatio;
		const viewportCap = window.innerHeight * MAX_VIEWPORT_HEIGHT_FRACTION;
		const isCapped = idealHeight > viewportCap;

		return {
			height: Math.max(HEADER_MIN_HEIGHT, Math.min(idealHeight, viewportCap)),
			centerCrop: isMobile && isCapped,
		};
	}, [showIntegratedBanner, bannerURL, guild.bannerWidth, guild.bannerHeight, isMobile]);

	const [{height: bannerMaxHeight, centerCrop}, setBannerLayout] = React.useState(() => calculateBannerLayout());

	React.useLayoutEffect(() => {
		const updateLayout = () => setBannerLayout(calculateBannerLayout());
		updateLayout();
		window.addEventListener('resize', updateLayout);
		return () => window.removeEventListener('resize', updateLayout);
	}, [calculateBannerLayout]);

	const handleContextMenu = React.useCallback(
		(event: React.MouseEvent) => {
			ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
				<GuildContextMenu guild={guild} onClose={onClose} />
			));
		},
		[guild],
	);

	const headerButtonRef = React.useRef<HTMLDivElement | null>(null);

	return (
		<div className={styles.headerWrapper}>
			<NativeDragRegion
				as={motion.div}
				ref={headerContainerRef}
				className={clsx(
					styles.headerContainer,
					!showIntegratedBanner && styles.headerContainerNoBanner,
					!showIntegratedBanner && isOpen && styles.headerContainerActive,
				)}
				style={{height: showIntegratedBanner ? bannerMaxHeight : HEADER_MIN_HEIGHT}}
			>
				{showIntegratedBanner && (
					<>
						<div
							className={clsx(styles.bannerBackground, centerCrop && styles.bannerBackgroundCentered)}
							style={{backgroundImage: `url(${bannerURL})`}}
						/>
						<div className={styles.bannerGradient} />
					</>
				)}

				<GuildHeaderShell
					popoutId="guild-header"
					renderPopout={() => <GuildHeaderPopout guild={guild} />}
					renderBottomSheet={({isOpen, onClose}) => (
						<GuildHeaderBottomSheet isOpen={isOpen} onClose={onClose} guild={guild} />
					)}
					onContextMenu={handleContextMenu}
					className={styles.headerContent}
					triggerRef={headerButtonRef}
				>
					{(isOpen) => (
						<>
							{guild.features.has(GuildFeatures.VERIFIED) && (
								<Tooltip text={t`Verified Community`} position="bottom">
									<SealCheckIcon
										className={showIntegratedBanner ? styles.verifiedIconWithBanner : styles.verifiedIconDefault}
									/>
								</Tooltip>
							)}
							<span className={showIntegratedBanner ? styles.guildNameWithBanner : styles.guildNameDefault}>
								{guild.name}
							</span>
							{isMobile ? (
								<DotsThreeIcon
									weight="bold"
									className={showIntegratedBanner ? styles.dotsIconWithBanner : styles.dotsIconDefault}
								/>
							) : (
								<CaretDownIcon
									weight="bold"
									className={clsx(
										showIntegratedBanner ? styles.caretIconWithBanner : styles.caretIconDefault,
										isOpen && styles.caretIconOpen,
									)}
								/>
							)}
						</>
					)}
				</GuildHeaderShell>
			</NativeDragRegion>
		</div>
	);
});
