// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import * as ThemeProfileCommands from '@app/features/theme/commands/ThemeProfileCommands';
import AccountTheme from '@app/features/theme/state/AccountTheme';
import ThemeEditTarget from '@app/features/theme/state/ThemeEditTarget';
import ThemeProfile from '@app/features/theme/state/ThemeProfile';
import {ThemeSelector} from '@app/features/theme_settings/components/ThemeSelector';
import styles from '@app/features/theme_settings/ThemesTab.module.css';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {BASE_THEME_SECTION_DESCRIPTOR} from '@app/features/user/components/settings_utils/section_registry/ThemesSections';
import type {ThemeType} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const SYNC_THEME_ACROSS_DEVICES_DESCRIPTOR = msg({
	message: 'Sync theme across devices',
	comment: 'Label in the theme tab content.',
});
const WHEN_ENABLED_THEME_CHANGES_WILL_SYNC_TO_ALL_DESCRIPTOR = msg({
	message: 'Theme changes sync to all your devices. Off keeps this device on its own theme.',
	comment: 'Label in the theme tab content.',
});
const DESKTOP_BASE_THEME_NOTE_DESCRIPTOR = msg({
	message: 'Every account that uses the desktop app theme gets this base theme.',
	comment: 'Shown under the base theme picker in Settings > Themes while editing the desktop app theme.',
});

export const BaseThemeSection: React.FC = observer(() => {
	const {i18n} = useLingui();
	const target = ThemeEditTarget.target;
	const selectedTheme = ThemeProfile.themePreferenceFor(target);
	const handleThemeChange = useCallback(
		(theme: ThemeType) => {
			if (theme === selectedTheme) return;
			ThemeProfileCommands.setThemePreference(theme, target);
		},
		[selectedTheme, target],
	);
	return (
		<SettingsSection
			id="base-theme"
			title={i18n._(BASE_THEME_SECTION_DESCRIPTOR)}
			data-flx="theme-settings.base-theme-section.base-theme"
		>
			<ThemeSelector
				value={selectedTheme}
				onChange={handleThemeChange}
				data-flx="theme-settings.base-theme-section.theme-selector.theme-change"
			/>
			{target === 'account' ? (
				<Switch
					label={i18n._(SYNC_THEME_ACROSS_DEVICES_DESCRIPTOR)}
					description={i18n._(WHEN_ENABLED_THEME_CHANGES_WILL_SYNC_TO_ALL_DESCRIPTOR)}
					value={AccountTheme.syncAcrossDevices}
					onChange={(value) => ThemeProfileCommands.setSyncAcrossDevices(value)}
					data-flx="theme-settings.base-theme-section.switch.set-sync-across-devices"
				/>
			) : (
				<p className={styles.note} data-flx="theme-settings.base-theme-section.note">
					{i18n._(DESKTOP_BASE_THEME_NOTE_DESCRIPTOR)}
				</p>
			)}
		</SettingsSection>
	);
});
