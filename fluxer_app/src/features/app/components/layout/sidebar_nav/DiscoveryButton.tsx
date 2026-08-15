// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import guildStyles from '@app/features/app/components/layout/GuildsLayout.module.css';
import styles from '@app/features/app/components/layout/sidebar_nav/DiscoveryButton.module.css';
import {useHover} from '@app/features/app/hooks/useHover';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Navigation from '@app/features/navigation/state/Navigation';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CompassIcon} from '@phosphor-icons/react';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import {useCallback, useRef} from 'react';

const EXPLORE_COMMUNITIES_DESCRIPTOR = msg({
	message: 'Explore communities',
	comment: 'Short label in the sidebar navigation discovery button.',
});
export const DiscoveryButton = observer(() => {
	const {i18n} = useLingui();
	const [hoverRef, isHovering] = useHover();
	const buttonRef = useRef<HTMLButtonElement | null>(null);
	const iconRef = useRef<HTMLDivElement | null>(null);
	const mergedRef = useMergeRefs([hoverRef, buttonRef]);
	const location = useLocation();
	const isSelected = Routes.isDiscoverRoute(location.pathname);
	const handleClick = useCallback(() => {
		Navigation.navigateToDiscover();
	}, []);
	if (RuntimeConfig.singleCommunityEnabled) {
		return null;
	}
	return (
		<div className={guildStyles.createGuildButton} data-flx="app.sidebar-nav.discovery-button.div">
			<Tooltip
				position="right"
				size="large"
				text={i18n._(EXPLORE_COMMUNITIES_DESCRIPTOR)}
				data-flx="app.sidebar-nav.discovery-button.tooltip"
			>
				<FocusRing
					offset={-2}
					focusTarget={buttonRef}
					ringTarget={iconRef}
					data-flx="app.sidebar-nav.discovery-button.focus-ring"
				>
					<button
						type="button"
						aria-label={i18n._(EXPLORE_COMMUNITIES_DESCRIPTOR)}
						aria-current={isSelected ? 'page' : undefined}
						data-guild-list-focus-item="true"
						onClick={handleClick}
						className={styles.button}
						ref={mergedRef}
						data-flx="app.sidebar-nav.discovery-button.button.click"
					>
						<motion.div
							ref={iconRef}
							className={guildStyles.createGuildButtonIcon}
							animate={{borderRadius: isHovering || isSelected ? '30%' : '50%'}}
							initial={{borderRadius: isHovering || isSelected ? '30%' : '50%'}}
							transition={{duration: Accessibility.useReducedMotion ? 0 : 0.07, ease: 'easeOut'}}
							style={isSelected ? {backgroundColor: 'var(--brand-primary)', color: 'white'} : undefined}
							data-flx="app.sidebar-nav.discovery-button.div--2"
						>
							<CompassIcon
								weight="fill"
								className={isSelected ? styles.iconTextSelected : styles.iconText}
								data-flx="app.sidebar-nav.discovery-button.icon-text"
							/>
						</motion.div>
					</button>
				</FocusRing>
			</Tooltip>
		</div>
	);
});
