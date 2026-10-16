// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildListActionButton} from '@app/features/app/components/layout/sidebar_nav/GuildListActionButton';
import styles from '@app/features/app/components/layout/sidebar_nav/GuildListActionButton.module.css';
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
import {TooltipWithKeybind} from '@app/features/ui/keybind_hint/KeybindHint';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {HouseIcon, LinkIcon, PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const CREATE_OR_JOIN_A_COMMUNITY_DESCRIPTOR = msg({
	message: 'Create or join a community',
	comment: 'Short label in the sidebar navigation add guild button.',
});
export const AddGuildButton = observer(() => {
	const {i18n} = useLingui();
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
	if (RuntimeConfig.singleCommunityEnabled) {
		return null;
	}
	return (
		<GuildListActionButton
			label={buttonLabel}
			tooltip={() => (
				<TooltipWithKeybind
					label={buttonLabel}
					action="nav_add_guild"
					data-flx="app.sidebar-nav.add-guild-button.tooltip-with-keybind"
				/>
			)}
			icon={
				<PlusIcon weight="bold" className={styles.iconText} data-flx="app.sidebar-nav.add-guild-button.icon-text" />
			}
			onClick={() => handleAddGuild()}
			onContextMenu={handleContextMenu}
			hasDialogPopup
			buttonDataFlx="app.sidebar-nav.add-guild-button.button.add-guild"
			data-flx="app.sidebar-nav.add-guild-button"
		/>
	);
});
