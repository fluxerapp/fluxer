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
import {StarIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import {FavoritesGuildContextMenu} from '~/components/uikit/ContextMenu/FavoritesGuildContextMenu';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useHover} from '~/hooks/useHover';
import {useMergeRefs} from '~/hooks/useMergeRefs';
import {useLocation} from '~/lib/router';
import {Routes} from '~/Routes';
import AccessibilityStore from '~/stores/AccessibilityStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';
import * as RouterUtils from '~/utils/RouterUtils';
import styles from '../GuildsLayout.module.css';

interface FavoritesButtonProps {
	className?: string;
}

export const FavoritesButton = observer(({className}: FavoritesButtonProps = {}) => {
	const {t} = useLingui();
	const [hoverRef, isHovering] = useHover();
	const buttonRef = React.useRef<HTMLButtonElement | null>(null);
	const iconRef = React.useRef<HTMLDivElement | null>(null);
	const mergedButtonRef = useMergeRefs([hoverRef, buttonRef]);
	const location = useLocation();
	const isSelected = location.pathname.startsWith(Routes.FAVORITES);

	const handleSelect = () => {
		const isMobile = MobileLayoutStore.isMobileLayout();

		if (isMobile) {
			RouterUtils.transitionTo(Routes.FAVORITES);
			return;
		}

		const validChannelId = SelectedChannelStore.getValidatedFavoritesChannel();

		if (validChannelId) {
			RouterUtils.transitionTo(Routes.favoritesChannel(validChannelId));
		} else {
			RouterUtils.transitionTo(Routes.FAVORITES);
		}
	};

	const handleContextMenu = (event: React.MouseEvent) => {
		event.preventDefault();
		event.stopPropagation();
		ContextMenuActionCreators.openFromEvent(event, ({onClose}) => <FavoritesGuildContextMenu onClose={onClose} />);
	};

	const indicatorHeight = isSelected ? 40 : isHovering ? 20 : 8;
	const isActive = isHovering || isSelected;

	if (!AccessibilityStore.showFavorites) {
		return null;
	}

	return (
		<Tooltip position="right" size="large" text={t`Favorites`}>
			<FocusRing offset={-2} focusTarget={buttonRef} ringTarget={iconRef}>
				<button
					type="button"
					className={clsx(styles.fluxerButton, className)}
					aria-label={t`Favorites`}
					aria-pressed={isSelected}
					onClick={handleSelect}
					onContextMenu={handleContextMenu}
					ref={mergedButtonRef}
				>
					<AnimatePresence>
						{(isSelected || isHovering) && (
							<div className={styles.guildIndicator}>
								<motion.span
									className={styles.guildIndicatorBar}
									initial={false}
									animate={{opacity: 1, scale: 1, height: indicatorHeight}}
									exit={{opacity: 0, scale: 0}}
									transition={{duration: 0.2, ease: [0.25, 0.1, 0.25, 1]}}
								/>
							</div>
						)}
					</AnimatePresence>
					<div className={styles.relative}>
						<motion.div
							ref={iconRef}
							className={clsx(styles.fluxerButtonIcon, isSelected && styles.fluxerButtonIconSelected)}
							animate={{borderRadius: isActive ? '30%' : '50%'}}
							initial={false}
							transition={{duration: 0.07, ease: 'easeOut'}}
							whileHover={{borderRadius: '30%'}}
						>
							<StarIcon weight="fill" className={styles.favoritesIcon} />
						</motion.div>
					</div>
				</button>
			</FocusRing>
		</Tooltip>
	);
});
