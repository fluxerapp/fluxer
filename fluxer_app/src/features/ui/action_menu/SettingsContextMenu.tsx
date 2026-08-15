// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import {
	getSettingsTabs,
	getSubtabsForTab,
	type SettingsSubtab,
	type SettingsTab,
} from '@app/features/user/components/settings_utils/SettingsConstants';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

interface SettingsContextMenuProps {
	onClose: () => void;
}

export const SettingsContextMenu: React.FC<SettingsContextMenuProps> = observer(({onClose}) => {
	const {i18n} = useLingui();
	const hasExpressionPackAccess = Users.getCurrentUser()?.isStaff() ?? false;
	const handleOpenSettings = useCallback(
		(tab: SettingsTab, subtab?: SettingsSubtab) => {
			ModalCommands.push(
				modal(() => (
					<UserSettingsModal
						initialTab={tab.type}
						initialSubtab={subtab?.type}
						data-flx="ui.action-menu.settings-context-menu.handle-open-settings.user-settings-modal"
					/>
				)),
			);
			onClose();
		},
		[onClose],
	);
	const renderSettingsMenuItem = useCallback(
		(tab: SettingsTab) => {
			const subtabs = getSubtabsForTab(tab.type, i18n);
			if (subtabs.length === 0) {
				const IconComponent = tab.icon;
				return (
					<MenuItem
						key={tab.type}
						icon={
							<IconComponent
								size={remFromPx(16)}
								weight={tab.iconWeight ?? 'fill'}
								data-flx="ui.action-menu.settings-context-menu.render-settings-menu-item.icon-component"
							/>
						}
						onClick={() => handleOpenSettings(tab)}
						data-flx="ui.action-menu.settings-context-menu.render-settings-menu-item.menu-item.open-settings"
					>
						{tab.label}
					</MenuItem>
				);
			}
			return (
				<MenuItemSubmenu
					key={tab.type}
					label={tab.label}
					onTriggerSelect={() => handleOpenSettings(tab)}
					render={() => (
						<>
							{subtabs.map((subtab) => (
								<MenuItem
									key={subtab.type}
									onClick={() => handleOpenSettings(tab, subtab)}
									data-flx="ui.action-menu.settings-context-menu.render-settings-menu-item.menu-item.open-settings--2"
								>
									{subtab.label}
								</MenuItem>
							))}
						</>
					)}
					data-flx="ui.action-menu.settings-context-menu.render-settings-menu-item.menu-item-submenu"
				/>
			);
		},
		[handleOpenSettings],
	);
	const isDeveloperModeEnabled = UserSettings.developerMode;
	const accessibleTabs = useMemo(() => {
		const allTabs = getSettingsTabs(i18n);
		return allTabs.filter((tab) => {
			if (!hasExpressionPackAccess && tab.type === 'expression_packs') {
				return false;
			}
			if (!isDeveloperModeEnabled && (tab.type === 'embed_debugger' || tab.type === 'component_gallery')) {
				return false;
			}
			return true;
		});
	}, [hasExpressionPackAccess, i18n.locale, isDeveloperModeEnabled]);
	const userSettingsTabs = accessibleTabs.filter((tab) => tab.category === 'user_settings');
	const billingTabs = accessibleTabs.filter((tab) => tab.category === 'billing');
	const appSettingsTabs = accessibleTabs.filter((tab) => tab.category === 'app_settings');
	const developerTabs = accessibleTabs.filter((tab) => tab.category === 'developer');
	return (
		<>
			{userSettingsTabs.length > 0 && (
				<MenuGroup data-flx="ui.action-menu.settings-context-menu.menu-group">
					{userSettingsTabs.map(renderSettingsMenuItem)}
				</MenuGroup>
			)}
			{billingTabs.length > 0 && (
				<MenuGroup data-flx="ui.action-menu.settings-context-menu.menu-group--billing">
					{billingTabs.map(renderSettingsMenuItem)}
				</MenuGroup>
			)}
			{appSettingsTabs.length > 0 && (
				<MenuGroup data-flx="ui.action-menu.settings-context-menu.menu-group--2">
					{appSettingsTabs.map(renderSettingsMenuItem)}
				</MenuGroup>
			)}
			{developerTabs.length > 0 && (
				<MenuGroup data-flx="ui.action-menu.settings-context-menu.menu-group--3">
					{developerTabs.map(renderSettingsMenuItem)}
				</MenuGroup>
			)}
		</>
	);
});
