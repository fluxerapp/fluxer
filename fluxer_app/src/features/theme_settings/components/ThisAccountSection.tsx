// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import * as ThemeProfileCommands from '@app/features/theme/commands/ThemeProfileCommands';
import AccountThemeMode from '@app/features/theme/state/AccountThemeMode';
import ThemeEditTarget, {type ThemeTarget} from '@app/features/theme/state/ThemeEditTarget';
import ThemeProfile from '@app/features/theme/state/ThemeProfile';
import {useCurrentAccountLabels} from '@app/features/theme_settings/hooks/useCurrentAccountLabels';
import styles from '@app/features/theme_settings/ThemesTab.module.css';
import {USE_DESKTOP_APP_THEME_DESCRIPTOR} from '@app/features/theme_settings/utils/ThemeSettingsMessageDescriptors';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {RadioGroup, type RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {THIS_ACCOUNT_SECTION_DESCRIPTOR} from '@app/features/user/components/settings_utils/section_registry/ThemesSections';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

const USE_DESKTOP_APP_THEME_DESCRIPTION_DESCRIPTOR = msg({
	message:
		"Every account you add to this desktop app uses the same theme, on any instance. Turn this off to use this account's own theme. Its own theme stays saved either way.",
	comment: 'Description under the "Use the desktop app theme for this account" switch in Settings > Themes.',
});
const THEME_TO_EDIT_DESCRIPTOR = msg({
	message: 'Theme to edit',
	comment:
		"Label for a choice in Settings > Themes on the desktop app between editing the theme shared by every account and editing the current account's own theme.",
});
const DESKTOP_APP_THEME_DESCRIPTOR = msg({
	message: 'Desktop app theme',
	comment: 'Option under "Theme to edit". The theme shared by every account in this desktop app.',
});
const ACCOUNT_ON_INSTANCE_DESCRIPTOR = msg({
	message: '{username} on {instanceName}',
	comment:
		'Option under "Theme to edit" for the current account\'s own theme. {username} is the account\'s username and {instanceName} is the instance it is on, like fluxer.app.',
});
const EDITING_OWN_THEME_WHILE_FOLLOWING_DESCRIPTOR = msg({
	message:
		'{username} on {instanceName} uses the desktop app theme right now. Changes here show when you turn off "{switchLabel}".',
	comment:
		'Shown in Settings > Themes while editing an account\'s own theme that the account does not use right now. {switchLabel} is the label of the "Use the desktop app theme for this account" switch.',
});
const EDITING_DESKTOP_THEME_WHILE_OWN_DESCRIPTOR = msg({
	message:
		'{username} on {instanceName} uses its own theme right now. Changes here show on accounts that use the desktop app theme.',
	comment: 'Shown in Settings > Themes while editing the desktop app theme from an account that uses its own theme.',
});

export const EditTargetNote: React.FC = observer(() => {
	const {i18n} = useLingui();
	const {username, instanceName} = useCurrentAccountLabels();
	const target = ThemeEditTarget.target;
	if (target === ThemeProfile.activeTarget) {
		return null;
	}
	return (
		<p className={styles.note} data-flx="theme-settings.this-account-section.edit-target-note.note">
			{target === 'account'
				? i18n._(EDITING_OWN_THEME_WHILE_FOLLOWING_DESCRIPTOR, {
						username,
						instanceName,
						switchLabel: i18n._(USE_DESKTOP_APP_THEME_DESCRIPTOR),
					})
				: i18n._(EDITING_DESKTOP_THEME_WHILE_OWN_DESCRIPTOR, {username, instanceName})}
		</p>
	);
});

export const ThisAccountSection: React.FC = observer(() => {
	const {i18n} = useLingui();
	const {username, instanceName} = useCurrentAccountLabels();
	const targetOptions = useMemo(
		(): ReadonlyArray<RadioOption<ThemeTarget>> => [
			{value: 'desktop', name: i18n._(DESKTOP_APP_THEME_DESCRIPTOR)},
			{value: 'account', name: i18n._(ACCOUNT_ON_INSTANCE_DESCRIPTOR, {username, instanceName})},
		],
		[i18n.locale, username, instanceName],
	);
	return (
		<SettingsSection
			id="this-account"
			title={i18n._(THIS_ACCOUNT_SECTION_DESCRIPTOR)}
			data-flx="theme-settings.this-account-section.this-account"
		>
			<Switch
				label={i18n._(USE_DESKTOP_APP_THEME_DESCRIPTOR)}
				description={i18n._(USE_DESKTOP_APP_THEME_DESCRIPTION_DESCRIPTOR)}
				value={AccountThemeMode.followDesktopTheme}
				onChange={(value) => ThemeProfileCommands.setFollowDesktopTheme(value)}
				data-flx="theme-settings.this-account-section.switch.set-follow-desktop-theme"
			/>
			<div className={styles.stack} data-flx="theme-settings.this-account-section.stack">
				<p className={styles.fieldLabel} data-flx="theme-settings.this-account-section.field-label">
					{i18n._(THEME_TO_EDIT_DESCRIPTOR)}
				</p>
				<RadioGroup
					options={targetOptions}
					value={ThemeEditTarget.target}
					onChange={(value) => ThemeProfileCommands.pickEditTarget(value)}
					aria-label={i18n._(THEME_TO_EDIT_DESCRIPTOR)}
					data-flx="theme-settings.this-account-section.radio-group.pick-edit-target"
				/>
				<EditTargetNote data-flx="theme-settings.this-account-section.edit-target-note" />
			</div>
		</SettingsSection>
	);
});
