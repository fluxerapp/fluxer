// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Keybind from '@app/features/input/state/InputKeybind';
import {formatKeyCombo} from '@app/features/input/utils/KeybindUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import ThemeEditTarget from '@app/features/theme/state/ThemeEditTarget';
import ThemeProfile from '@app/features/theme/state/ThemeProfile';
import * as ThemeSettingsCommands from '@app/features/theme_settings/commands/ThemeSettingsCommands';
import styles from '@app/features/theme_settings/ThemesTab.module.css';
import {
	OPEN_QUICK_CSS_EDITOR_DESCRIPTOR,
	SYNC_CUSTOM_CSS_ACROSS_DEVICES_DESCRIPTOR,
} from '@app/features/theme_settings/utils/ThemeSettingsMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {QUICK_CSS_SECTION_DESCRIPTOR} from '@app/features/user/components/settings_utils/section_registry/ThemesSections';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {FileCssIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const QUICK_CSS_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Write your own CSS to change anything in the app. The editor stays open while you use the app, and your CSS never applies to the editor itself, so you can always fix CSS that breaks the app.',
	comment: 'Description of the "Quick CSS" section in Settings > Themes.',
});
const QUICK_CSS_SIZE_DESCRIPTOR = msg({
	message: '{lines, plural, one {# line} other {# lines}} of quick CSS.',
	comment: 'Shown in the "Quick CSS" section in Settings > Themes. How long the quick CSS of the edited theme is.',
});
const QUICK_CSS_EMPTY_DESCRIPTOR = msg({
	message: "You haven't written any quick CSS yet.",
	comment: 'Shown in the "Quick CSS" section in Settings > Themes when the edited theme has no quick CSS.',
});
const OPEN_ON_A_COMPUTER_DESCRIPTOR = msg({
	message: 'Open this page on a computer to edit quick CSS.',
	comment: 'Shown in the "Quick CSS" section in Settings > Themes on phones, where the editor window is not available.',
});
const TURN_OFF_HINT_DESCRIPTOR = msg({
	message: 'If your CSS breaks the app, press {shortcut} to turn it off.',
	comment:
		'Shown in the "Quick CSS" section in Settings > Themes. {shortcut} is a keyboard shortcut such as Ctrl+Alt+Shift+K that stops applying quick CSS and saved themes in this tab.',
});
const OPEN_HINT_DESCRIPTOR = msg({
	message: 'Press {shortcut} anywhere in the app to open the editor.',
	comment:
		'Shown in the "Quick CSS" section in Settings > Themes. {shortcut} is a keyboard shortcut such as Ctrl+Alt+Shift+T.',
});
const SYNC_CUSTOM_CSS_ACROSS_DEVICES_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Only the CSS text is synced. Uploaded assets and local file references stay on this device.',
	comment: 'Description of the "Sync custom CSS across devices" switch in Settings > Themes.',
});
const SYNC_CUSTOM_CSS_DESCRIPTOR = msg({
	message: 'Sync custom CSS?',
	comment: 'Confirmation title before turning on custom CSS sync in Settings > Themes.',
});
const SYNC_CUSTOM_CSS_CONFIRM_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Only the CSS text is synced across devices. Uploaded assets, desktop file references, fonts, and media stay on this device.',
	comment: 'Confirmation text before turning on custom CSS sync in Settings > Themes.',
});
const SYNC_CSS_TEXT_DESCRIPTOR = msg({
	message: 'Sync CSS text',
	comment: 'Confirm button that turns on custom CSS sync in Settings > Themes.',
});

function countLines(css: string | null): number {
	if (!css || css.trim().length === 0) return 0;
	return css.split('\n').length;
}

export const QuickCssSection: React.FC = observer(() => {
	const {i18n} = useLingui();
	const target = ThemeEditTarget.target;
	const lines = countLines(ThemeProfile.customThemeCssFor(target));
	const [turnOffCombo] = Keybind.getActiveCombosForAction('system_toggle_custom_css');
	const [openCombo] = Keybind.getActiveCombosForAction('system_open_theme_studio_popout');
	const handleSyncChange = (value: boolean) => {
		if (!value) {
			Accessibility.updateSettings({customThemeCssSyncAcrossDevices: false});
			return;
		}
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(SYNC_CUSTOM_CSS_DESCRIPTOR)}
					description={i18n._(SYNC_CUSTOM_CSS_CONFIRM_DESCRIPTION_DESCRIPTOR)}
					primaryText={i18n._(SYNC_CSS_TEXT_DESCRIPTOR)}
					secondaryText={i18n._(CANCEL_DESCRIPTOR)}
					onPrimary={() => Accessibility.updateSettings({customThemeCssSyncAcrossDevices: true})}
					data-flx="theme-settings.quick-css-section.handle-sync-change.confirm-modal"
				/>
			)),
		);
	};
	return (
		<SettingsSection
			id="quick-css"
			title={i18n._(QUICK_CSS_SECTION_DESCRIPTOR)}
			description={i18n._(QUICK_CSS_DESCRIPTION_DESCRIPTOR)}
			data-flx="theme-settings.quick-css-section.quick-css"
		>
			<div className={styles.stack} data-flx="theme-settings.quick-css-section.stack">
				<p className={styles.note} data-flx="theme-settings.quick-css-section.note">
					{lines > 0 ? i18n._(QUICK_CSS_SIZE_DESCRIPTOR, {lines}) : i18n._(QUICK_CSS_EMPTY_DESCRIPTOR)}
				</p>
				{openCombo && !MobileLayout.enabled ? (
					<p className={styles.note} data-flx="theme-settings.quick-css-section.open-hint">
						{i18n._(OPEN_HINT_DESCRIPTOR, {shortcut: formatKeyCombo(i18n, openCombo)})}
					</p>
				) : null}
				{turnOffCombo && !MobileLayout.enabled ? (
					<p className={styles.note} data-flx="theme-settings.quick-css-section.turn-off-hint">
						{i18n._(TURN_OFF_HINT_DESCRIPTOR, {shortcut: formatKeyCombo(i18n, turnOffCombo)})}
					</p>
				) : null}
				{MobileLayout.enabled ? (
					<p className={styles.note} data-flx="theme-settings.quick-css-section.note--2">
						{i18n._(OPEN_ON_A_COMPUTER_DESCRIPTOR)}
					</p>
				) : (
					<div data-flx="theme-settings.quick-css-section.div">
						<Button
							fitContent
							leftIcon={<FileCssIcon size={remFromPx(18)} data-flx="theme-settings.quick-css-section.file-css-icon" />}
							onClick={() => ThemeSettingsCommands.openQuickCssEditor()}
							data-flx="theme-settings.quick-css-section.button.open-quick-css-editor"
						>
							{i18n._(OPEN_QUICK_CSS_EDITOR_DESCRIPTOR)}
						</Button>
					</div>
				)}
			</div>
			{target === 'account' ? (
				<Switch
					label={i18n._(SYNC_CUSTOM_CSS_ACROSS_DEVICES_DESCRIPTOR)}
					description={i18n._(SYNC_CUSTOM_CSS_ACROSS_DEVICES_DESCRIPTION_DESCRIPTOR)}
					value={Accessibility.customThemeCssSyncAcrossDevices}
					onChange={handleSyncChange}
					data-flx="theme-settings.quick-css-section.switch.sync-change"
				/>
			) : null}
		</SettingsSection>
	);
});
