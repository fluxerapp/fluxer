// SPDX-License-Identifier: AGPL-3.0-or-later

import {FavoritesGuildHeaderBottomSheet} from '@app/features/app/components/bottomsheets/FavoritesGuildHeaderBottomSheet';
import {FavoritesGuildHeaderPopout} from '@app/features/app/components/floating/FavoritesGuildHeaderPopout';
import styles from '@app/features/app/components/layout/FavoritesGuildHeader.module.css';
import guildHeaderStyles from '@app/features/app/components/layout/GuildHeader.module.css';
import {GuildHeaderShell} from '@app/features/app/components/layout/GuildHeaderShell';
import {FAVORITES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {FavoritesGuildContextMenu} from '@app/features/ui/action_menu/FavoritesGuildContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import Popout from '@app/features/ui/state/Popout';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CaretDownIcon, DotsThreeIcon, StarIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useRef} from 'react';

const OPEN_FAVORITES_MENU_DESCRIPTOR = msg({
	message: 'Open favorites menu',
	comment: 'Short label in the app layout favorites guild header.',
});
export const FavoritesGuildHeader = observer(() => {
	const {i18n} = useLingui();
	const {popouts} = Popout;
	const isOpen = 'favorites-guild-header' in popouts;
	const isMobile = MobileLayout.isMobileLayout();
	const mobileHeaderRef = useRef<HTMLDivElement | null>(null);
	const handleContextMenu = useCallback((event: React.MouseEvent) => {
		ContextMenuCommands.openFromEvent(event, ({onClose}) => (
			<FavoritesGuildContextMenu
				onClose={onClose}
				data-flx="app.favorites-guild-header.handle-context-menu.favorites-guild-context-menu"
			/>
		));
	}, []);
	return (
		<div
			className={clsx(
				guildHeaderStyles.headerContainer,
				guildHeaderStyles.headerContainerNoBanner,
				isOpen && guildHeaderStyles.headerContainerActive,
			)}
			style={{height: remFromPx(56)}}
			data-flx="app.favorites-guild-header.div"
		>
			<GuildHeaderShell
				popoutId="favorites-guild-header"
				renderPopout={() => (
					<FavoritesGuildHeaderPopout data-flx="app.favorites-guild-header.favorites-guild-header-popout" />
				)}
				renderBottomSheet={({isOpen, onClose}) => (
					<FavoritesGuildHeaderBottomSheet
						isOpen={isOpen}
						onClose={onClose}
						data-flx="app.favorites-guild-header.favorites-guild-header-bottom-sheet"
					/>
				)}
				onContextMenu={handleContextMenu}
				className={guildHeaderStyles.headerContent}
				triggerRef={mobileHeaderRef}
				ariaLabel={i18n._(OPEN_FAVORITES_MENU_DESCRIPTOR)}
				data-flx="app.favorites-guild-header.guild-header-shell.context-menu"
			>
				{(isOpen) => (
					<>
						<div className={styles.headerIconContainer} data-flx="app.favorites-guild-header.header-icon-container">
							<StarIcon
								weight="fill"
								className={clsx(guildHeaderStyles.verifiedIconDefault, styles.headerIcon)}
								data-flx="app.favorites-guild-header.header-icon"
							/>
							<span className={guildHeaderStyles.guildNameDefault} data-flx="app.favorites-guild-header.span">
								{i18n._(FAVORITES_DESCRIPTOR)}
							</span>
						</div>
						{isMobile ? (
							<DotsThreeIcon
								weight="bold"
								className={guildHeaderStyles.dotsIconDefault}
								data-flx="app.favorites-guild-header.dots-three-icon"
							/>
						) : (
							<CaretDownIcon
								weight="bold"
								className={clsx(guildHeaderStyles.caretIconDefault, isOpen && guildHeaderStyles.caretIconOpen)}
								data-flx="app.favorites-guild-header.caret-down-icon"
							/>
						)}
					</>
				)}
			</GuildHeaderShell>
		</div>
	);
});
