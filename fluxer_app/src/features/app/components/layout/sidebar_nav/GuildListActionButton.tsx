// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import guildStyles from '@app/features/app/components/layout/GuildsLayout.module.css';
import styles from '@app/features/app/components/layout/sidebar_nav/GuildListActionButton.module.css';
import {useContextMenuHoverState} from '@app/features/app/hooks/useContextMenuHoverState';
import {useHover} from '@app/features/app/hooks/useHover';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {clsx} from 'clsx';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useRef} from 'react';

interface GuildListActionButtonProps {
	label: string;
	tooltip: () => React.ReactNode;
	icon: React.ReactNode;
	onClick: () => void;
	onContextMenu: (event: React.MouseEvent) => void;
	hasDialogPopup?: boolean;
	buttonDataFlx: string;
	'data-flx': string;
}

export const GuildListActionButton = observer(
	({
		label,
		tooltip,
		icon,
		onClick,
		onContextMenu,
		hasDialogPopup = false,
		buttonDataFlx,
		'data-flx': dataFlx,
	}: GuildListActionButtonProps) => {
		const [hoverRef, isHovering] = useHover();
		const buttonRef = useRef<HTMLButtonElement | null>(null);
		const iconRef = useRef<HTMLDivElement | null>(null);
		const itemRef = useRef<HTMLElement | null>(null);
		const contextMenuOpen = useContextMenuHoverState(itemRef);
		const mergedButtonRef = useMergeRefs([hoverRef, buttonRef, itemRef]);
		const shouldShowHoverState = isHovering || contextMenuOpen;
		return (
			<div
				className={clsx(guildStyles.createGuildButton, contextMenuOpen && guildStyles.contextMenuHover)}
				data-flx={`${dataFlx}.div`}
			>
				<Tooltip position="right" size="large" text={tooltip} data-flx={`${dataFlx}.tooltip`}>
					<FocusRing offset={-2} focusTarget={buttonRef} ringTarget={iconRef} data-flx={`${dataFlx}.focus-ring`}>
						<button
							type="button"
							aria-label={label}
							aria-haspopup={hasDialogPopup ? 'dialog' : undefined}
							data-guild-list-focus-item="true"
							onClick={onClick}
							onContextMenu={onContextMenu}
							className={styles.button}
							ref={mergedButtonRef}
							data-flx={buttonDataFlx}
						>
							<motion.div
								ref={iconRef}
								className={guildStyles.createGuildButtonIcon}
								animate={{borderRadius: shouldShowHoverState ? '30%' : '50%'}}
								initial={{borderRadius: shouldShowHoverState ? '30%' : '50%'}}
								transition={{duration: Accessibility.useReducedMotion ? 0 : 0.07, ease: 'easeOut'}}
								data-flx={`${dataFlx}.div--2`}
							>
								{icon}
							</motion.div>
						</button>
					</FocusRing>
				</Tooltip>
			</div>
		);
	},
);
