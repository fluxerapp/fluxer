// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {GuildListActionButton} from '@app/features/app/components/layout/sidebar_nav/GuildListActionButton';
import styles from '@app/features/app/components/layout/sidebar_nav/GuildListActionButton.module.css';
import HiddenGuildListButtons from '@app/features/guild/state/HiddenGuildListButtons';
import {openExternalUrlWithWarning} from '@app/features/messaging/utils/ExternalLinkUtils';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {TooltipWithKeybind} from '@app/features/ui/keybind_hint/KeybindHint';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {EyeSlashIcon, QuestionMarkIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const HELP_CENTER_DESCRIPTOR = msg({
	message: 'Help center',
	comment: 'Short label in the sidebar navigation help button.',
});
export const HelpButton = observer(() => {
	const {i18n} = useLingui();
	const helpUrl = Routes.help();
	if (HiddenGuildListButtons.helpButtonHidden || !helpUrl) {
		return null;
	}
	const handleHelp = () => {
		openExternalUrlWithWarning(helpUrl);
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
	return (
		<GuildListActionButton
			label={i18n._(HELP_CENTER_DESCRIPTOR)}
			tooltip={() => (
				<TooltipWithKeybind
					label={i18n._(HELP_CENTER_DESCRIPTOR)}
					action="misc_help"
					data-flx="app.sidebar-nav.help-button.tooltip-with-keybind"
				/>
			)}
			icon={
				<QuestionMarkIcon weight="bold" className={styles.iconText} data-flx="app.sidebar-nav.help-button.icon-text" />
			}
			onClick={handleHelp}
			onContextMenu={handleContextMenu}
			buttonDataFlx="app.sidebar-nav.help-button.button.help"
			data-flx="app.sidebar-nav.help-button"
		/>
	);
});
