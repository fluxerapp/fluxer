// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import guildStyles from '@app/features/app/components/layout/GuildsLayout.module.css';
import styles from '@app/features/app/components/layout/sidebar_nav/AddGuildButton.module.css';
import {useContextMenuHoverState} from '@app/features/app/hooks/useContextMenuHoverState';
import {useHover} from '@app/features/app/hooks/useHover';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {AddGuildModal, type AddGuildModalView} from '@app/features/guild/components/modals/AddGuildModal';
import {
	CREATE_COMMUNITY_DESCRIPTOR,
	JOIN_COMMUNITY_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {TooltipWithKeybind} from '@app/features/ui/keybind_hint/KeybindHint';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {HouseIcon, LinkIcon, PlusIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useRef} from 'react';

const CREATE_OR_JOIN_A_COMMUNITY_DESCRIPTOR = msg({
	message: 'Create or join a community',
	comment: 'Short label in the sidebar navigation add guild button.',
});
export const AddGuildButton = observer(() => {
	const {i18n} = useLingui();
	const [hoverRef, isHovering] = useHover();
	const buttonRef = useRef<HTMLButtonElement | null>(null);
	const iconRef = useRef<HTMLDivElement | null>(null);
	const itemRef = useRef<HTMLElement | null>(null);
	const contextMenuOpen = useContextMenuHoverState(itemRef);
	const mergedButtonRef = useMergeRefs([hoverRef, buttonRef, itemRef]);
	const buttonLabel = i18n._(CREATE_OR_JOIN_A_COMMUNITY_DESCRIPTOR);
	const handleAddGuild = (view?: AddGuildModalView) => {
		ModalCommands.push(
			modal(() => (
				<AddGuildModal
					initialView={view}
					data-flx="app.sidebar-nav.add-guild-button.handle-add-guild.add-guild-modal"
				/>
			)),
		);
	};
	const handleContextMenu = (e: React.MouseEvent) => {
		e.preventDefault();
		e.stopPropagation();
		ContextMenuCommands.openFromEvent(e, ({onClose}) => (
			<MenuGroup data-flx="app.sidebar-nav.add-guild-button.handle-context-menu.menu-group">
				<MenuItem
					icon={
						<HouseIcon
							className={styles.menuIcon}
							data-flx="app.sidebar-nav.add-guild-button.handle-context-menu.menu-icon"
						/>
					}
					onClick={() => {
						handleAddGuild('create_guild');
						onClose();
					}}
					data-flx="app.sidebar-nav.add-guild-button.handle-context-menu.menu-item.add-guild"
				>
					{i18n._(CREATE_COMMUNITY_DESCRIPTOR)}
				</MenuItem>
				<MenuItem
					icon={
						<LinkIcon
							className={styles.menuIcon}
							weight="bold"
							data-flx="app.sidebar-nav.add-guild-button.handle-context-menu.menu-icon--2"
						/>
					}
					onClick={() => {
						handleAddGuild('join_guild');
						onClose();
					}}
					data-flx="app.sidebar-nav.add-guild-button.handle-context-menu.menu-item.add-guild--2"
				>
					{i18n._(JOIN_COMMUNITY_DESCRIPTOR)}
				</MenuItem>
			</MenuGroup>
		));
	};
	const shouldShowHoverState = isHovering || contextMenuOpen;
	if (RuntimeConfig.singleCommunityEnabled) {
		return null;
	}
	return (
		<div
			className={clsx(guildStyles.createGuildButton, contextMenuOpen && guildStyles.contextMenuHover)}
			data-flx="app.sidebar-nav.add-guild-button.div"
		>
			<Tooltip
				position="right"
				size="large"
				text={() => (
					<TooltipWithKeybind
						label={buttonLabel}
						action="nav_add_guild"
						data-flx="app.sidebar-nav.add-guild-button.tooltip-with-keybind"
					/>
				)}
				data-flx="app.sidebar-nav.add-guild-button.tooltip"
			>
				<FocusRing
					offset={-2}
					focusTarget={buttonRef}
					ringTarget={iconRef}
					data-flx="app.sidebar-nav.add-guild-button.focus-ring"
				>
					<button
						type="button"
						aria-label={buttonLabel}
						aria-haspopup="dialog"
						data-guild-list-focus-item="true"
						onClick={() => handleAddGuild()}
						onContextMenu={handleContextMenu}
						className={styles.button}
						ref={mergedButtonRef}
						data-flx="app.sidebar-nav.add-guild-button.button.add-guild"
					>
						<motion.div
							ref={iconRef}
							className={guildStyles.createGuildButtonIcon}
							animate={{borderRadius: shouldShowHoverState ? '30%' : '50%'}}
							initial={{borderRadius: shouldShowHoverState ? '30%' : '50%'}}
							transition={{duration: Accessibility.useReducedMotion ? 0 : 0.07, ease: 'easeOut'}}
							data-flx="app.sidebar-nav.add-guild-button.div--2"
						>
							<PlusIcon
								weight="bold"
								className={styles.iconText}
								data-flx="app.sidebar-nav.add-guild-button.icon-text"
							/>
						</motion.div>
					</button>
				</FocusRing>
			</Tooltip>
		</div>
	);
});
