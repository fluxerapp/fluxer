// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import styles from '@app/features/app/components/layout/GuildsLayout.module.css';
import {useContextMenuHoverState} from '@app/features/app/hooks/useContextMenuHoverState';
import {useHover} from '@app/features/app/hooks/useHover';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import {FAVORITES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {FavoritesGuildContextMenu} from '@app/features/ui/action_menu/FavoritesGuildContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {getAppZoomFactor} from '@app/features/ui/utils/AppZoomUtils';
import {FAVORITES_GUILD_ID} from '@fluxer/constants/src/AppConstants';
import {useLingui} from '@lingui/react/macro';
import {StarIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useRef} from 'react';

interface FavoritesButtonProps {
	className?: string;
}

export const FavoritesButton = observer(({className}: FavoritesButtonProps = {}) => {
	const {i18n} = useLingui();
	const [hoverRef, isHovering] = useHover();
	const buttonRef = useRef<HTMLButtonElement | null>(null);
	const iconRef = useRef<HTMLDivElement | null>(null);
	const itemRef = useRef<HTMLElement | null>(null);
	const mergedButtonRef = useMergeRefs([hoverRef, buttonRef, itemRef]);
	const contextMenuOpen = useContextMenuHoverState(itemRef);
	const location = useLocation();
	const isSelected = location.pathname.startsWith(Routes.FAVORITES);
	const handleSelect = () => {
		const isMobile = MobileLayout.isMobileLayout();
		if (isMobile) {
			NavigationCommands.selectChannel(FAVORITES_GUILD_ID);
			return;
		}
		const validChannelId = SelectedChannel.getValidatedFavoritesChannel();
		if (validChannelId) {
			NavigationCommands.selectChannel(FAVORITES_GUILD_ID, validChannelId);
		} else {
			NavigationCommands.selectChannel(FAVORITES_GUILD_ID);
		}
	};
	const handleContextMenu = (event: React.MouseEvent) => {
		event.preventDefault();
		event.stopPropagation();
		ContextMenuCommands.openFromEvent(event, ({onClose}) => (
			<FavoritesGuildContextMenu
				onClose={onClose}
				data-flx="app.sidebar-nav.favorites-button.handle-context-menu.favorites-guild-context-menu"
			/>
		));
	};
	const shouldShowHoverState = isHovering || contextMenuOpen;
	const indicatorHeight =
		(() => {
			if (isSelected) return 40;
			if (shouldShowHoverState) return 20;
			return 8;
		})() * getAppZoomFactor();
	const isActive = shouldShowHoverState || isSelected;
	if (!Accessibility.showFavorites) {
		return null;
	}
	return (
		<Tooltip
			position="right"
			size="large"
			text={i18n._(FAVORITES_DESCRIPTOR)}
			data-flx="app.sidebar-nav.favorites-button.tooltip"
		>
			<FocusRing
				offset={-2}
				focusTarget={buttonRef}
				ringTarget={iconRef}
				data-flx="app.sidebar-nav.favorites-button.focus-ring"
			>
				<button
					type="button"
					className={clsx(styles.fluxerButton, contextMenuOpen && styles.contextMenuHover, className)}
					aria-label={i18n._(FAVORITES_DESCRIPTOR)}
					aria-current={isSelected ? 'page' : undefined}
					data-guild-list-focus-item="true"
					onClick={handleSelect}
					onContextMenu={handleContextMenu}
					ref={mergedButtonRef}
					data-flx="app.sidebar-nav.favorites-button.fluxer-button.select"
				>
					<AnimatePresence data-flx="app.sidebar-nav.favorites-button.animate-presence">
						{(isSelected || shouldShowHoverState) && (
							<div className={styles.guildIndicator} data-flx="app.sidebar-nav.favorites-button.guild-indicator">
								<motion.span
									className={styles.guildIndicatorBar}
									initial={false}
									animate={{opacity: 1, scale: 1, height: indicatorHeight}}
									exit={{opacity: 0, scale: 0}}
									transition={{duration: 0.2, ease: [0.25, 0.1, 0.25, 1]}}
									data-flx="app.sidebar-nav.favorites-button.guild-indicator-bar"
								/>
							</div>
						)}
					</AnimatePresence>
					<div className={styles.relative} data-flx="app.sidebar-nav.favorites-button.relative">
						<motion.div
							ref={iconRef}
							className={clsx(styles.fluxerButtonIcon, isSelected && styles.fluxerButtonIconSelected)}
							animate={{borderRadius: isActive ? '30%' : '50%'}}
							initial={false}
							transition={{duration: 0.07, ease: 'easeOut'}}
							data-flx="app.sidebar-nav.favorites-button.fluxer-button-icon"
						>
							<StarIcon
								weight="fill"
								className={styles.favoritesIcon}
								data-flx="app.sidebar-nav.favorites-button.favorites-icon"
							/>
						</motion.div>
					</div>
				</button>
			</FocusRing>
		</Tooltip>
	);
});
