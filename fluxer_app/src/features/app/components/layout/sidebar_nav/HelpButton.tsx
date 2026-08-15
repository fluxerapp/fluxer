// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import guildStyles from '@app/features/app/components/layout/GuildsLayout.module.css';
import styles from '@app/features/app/components/layout/sidebar_nav/HelpButton.module.css';
import {useContextMenuHoverState} from '@app/features/app/hooks/useContextMenuHoverState';
import {useHover} from '@app/features/app/hooks/useHover';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import HiddenGuildListButtons from '@app/features/guild/state/HiddenGuildListButtons';
import {openExternalUrlWithWarning} from '@app/features/messaging/utils/ExternalLinkUtils';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {TooltipWithKeybind} from '@app/features/ui/keybind_hint/KeybindHint';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {EyeSlashIcon, QuestionMarkIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useRef} from 'react';

const HELP_CENTER_DESCRIPTOR = msg({
	message: 'Help center',
	comment: 'Short label in the sidebar navigation help button.',
});
export const HelpButton = observer(() => {
	const {i18n} = useLingui();
	const [hoverRef, isHovering] = useHover();
	const buttonRef = useRef<HTMLButtonElement | null>(null);
	const iconRef = useRef<HTMLDivElement | null>(null);
	const itemRef = useRef<HTMLElement | null>(null);
	const contextMenuOpen = useContextMenuHoverState(itemRef);
	const mergedButtonRef = useMergeRefs([hoverRef, buttonRef, itemRef]);
	if (HiddenGuildListButtons.helpButtonHidden) {
		return null;
	}
	const handleHelp = () => {
		openExternalUrlWithWarning(Routes.help());
	};
	const handleContextMenu = (e: React.MouseEvent) => {
		e.preventDefault();
		e.stopPropagation();
		ContextMenuCommands.openFromEvent(e, ({onClose}) => (
			<MenuGroup data-flx="app.sidebar-nav.help-button.handle-context-menu.menu-group">
				<MenuItem
					icon={
						<EyeSlashIcon
							className={styles.menuIcon}
							data-flx="app.sidebar-nav.help-button.handle-context-menu.menu-icon"
						/>
					}
					onClick={() => {
						HiddenGuildListButtons.hideHelpButton();
						onClose();
					}}
					data-flx="app.sidebar-nav.help-button.handle-context-menu.menu-item.hide-help-button"
				>
					<Trans>Hide help center button</Trans>
				</MenuItem>
			</MenuGroup>
		));
	};
	const shouldShowHoverState = isHovering || contextMenuOpen;
	return (
		<div
			className={clsx(guildStyles.createGuildButton, contextMenuOpen && guildStyles.contextMenuHover)}
			data-flx="app.sidebar-nav.help-button.div"
		>
			<Tooltip
				position="right"
				size="large"
				text={() => (
					<TooltipWithKeybind
						label={i18n._(HELP_CENTER_DESCRIPTOR)}
						action="misc_help"
						data-flx="app.sidebar-nav.help-button.tooltip-with-keybind"
					/>
				)}
				data-flx="app.sidebar-nav.help-button.tooltip"
			>
				<FocusRing
					offset={-2}
					focusTarget={buttonRef}
					ringTarget={iconRef}
					data-flx="app.sidebar-nav.help-button.focus-ring"
				>
					<button
						type="button"
						aria-label={i18n._(HELP_CENTER_DESCRIPTOR)}
						data-guild-list-focus-item="true"
						onClick={handleHelp}
						onContextMenu={handleContextMenu}
						className={styles.button}
						ref={mergedButtonRef}
						data-flx="app.sidebar-nav.help-button.button.help"
					>
						<motion.div
							ref={iconRef}
							className={guildStyles.createGuildButtonIcon}
							animate={{borderRadius: shouldShowHoverState ? '30%' : '50%'}}
							initial={{borderRadius: shouldShowHoverState ? '30%' : '50%'}}
							transition={{duration: Accessibility.useReducedMotion ? 0 : 0.07, ease: 'easeOut'}}
							data-flx="app.sidebar-nav.help-button.div--2"
						>
							<QuestionMarkIcon
								weight="bold"
								className={styles.iconText}
								data-flx="app.sidebar-nav.help-button.icon-text"
							/>
						</motion.div>
					</button>
				</FocusRing>
			</Tooltip>
		</div>
	);
});
