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
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {UserSettingsModal} from '~/components/modals/UserSettingsModal';
import {
	getSettingsTabs,
	getSubtabsForTab,
	type SettingsSubtab,
	type SettingsTab,
} from '~/components/modals/utils/settingsConstants';
import DeveloperModeStore from '~/stores/DeveloperModeStore';
import FeatureFlagStore from '~/stores/FeatureFlagStore';
import SelectedGuildStore from '~/stores/SelectedGuildStore';
import {MenuGroup} from './MenuGroup';
import {MenuItem} from './MenuItem';
import {MenuItemSubmenu} from './MenuItemSubmenu';

interface SettingsContextMenuProps {
	onClose: () => void;
}

export const SettingsContextMenu: React.FC<SettingsContextMenuProps> = observer(({onClose}) => {
	const {t} = useLingui();
	const isDeveloper = DeveloperModeStore.isDeveloper;
	const selectedGuildId = SelectedGuildStore.selectedGuildId;
	const hasExpressionPackAccess = FeatureFlagStore.isExpressionPacksEnabled(selectedGuildId ?? undefined);

	const handleOpenSettings = React.useCallback(
		(tab: SettingsTab, subtab?: SettingsSubtab) => {
			ModalActionCreators.push(modal(() => <UserSettingsModal initialTab={tab.type} initialSubtab={subtab?.type} />));
			onClose();
		},
		[onClose],
	);

	const renderSettingsMenuItem = React.useCallback(
		(tab: SettingsTab) => {
			const subtabs = getSubtabsForTab(tab.type, t);

			if (subtabs.length === 0) {
				const IconComponent = tab.icon;
				return (
					<MenuItem
						key={tab.type}
						icon={<IconComponent size={16} weight={tab.iconWeight ?? 'fill'} />}
						onClick={() => handleOpenSettings(tab)}
					>
						{tab.label}
					</MenuItem>
				);
			}

			const IconComponent = tab.icon;
			return (
				<MenuItemSubmenu
					key={tab.type}
					label={tab.label}
					icon={<IconComponent size={16} weight={tab.iconWeight ?? 'fill'} />}
					onTriggerSelect={() => handleOpenSettings(tab)}
					render={() => (
						<>
							{subtabs.map((subtab) => (
								<MenuItem key={subtab.type} onClick={() => handleOpenSettings(tab, subtab)}>
									{subtab.label}
								</MenuItem>
							))}
						</>
					)}
				/>
			);
		},
		[handleOpenSettings],
	);

	const accessibleTabs = React.useMemo(() => {
		const allTabs = getSettingsTabs(t);
		return allTabs.filter((tab) => {
			if (!isDeveloper && tab.category === 'staff_only') {
				return false;
			}
			if (!hasExpressionPackAccess && tab.type === 'expression_packs') {
				return false;
			}
			return true;
		});
	}, [isDeveloper, hasExpressionPackAccess, t]);

	const userSettingsTabs = accessibleTabs.filter((tab) => tab.category === 'user_settings');
	const appSettingsTabs = accessibleTabs.filter((tab) => tab.category === 'app_settings');
	const developerTabs = accessibleTabs.filter((tab) => tab.category === 'developer');
	const staffOnlyTabs = accessibleTabs.filter((tab) => tab.category === 'staff_only');

	return (
		<>
			{userSettingsTabs.length > 0 && <MenuGroup>{userSettingsTabs.map(renderSettingsMenuItem)}</MenuGroup>}
			{appSettingsTabs.length > 0 && <MenuGroup>{appSettingsTabs.map(renderSettingsMenuItem)}</MenuGroup>}
			{developerTabs.length > 0 && <MenuGroup>{developerTabs.map(renderSettingsMenuItem)}</MenuGroup>}
			{staffOnlyTabs.length > 0 && <MenuGroup>{staffOnlyTabs.map(renderSettingsMenuItem)}</MenuGroup>}
		</>
	);
});
