// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import GlobalShortcuts from '@app/features/input/state/GlobalShortcuts';
import Keybind, {type KeybindCommand} from '@app/features/input/state/InputKeybind';
import {
	getGnomeHoldWarning,
	HOLD_PORTAL_ACTIONS,
	isGnomeHoldTriggerRisky,
	SYSTEM_WIDE_SHORTCUTS_DESCRIPTOR,
	triggerHasModifiers,
} from '@app/features/user/components/modals/tabs/components/SystemShortcutsSection';
import styles from '@app/features/user/components/modals/tabs/KeybindsTab.module.css';
import {getUserSettingsTabLabel} from '@app/features/user/components/settings_utils/SettingsConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const WORKS_WHILE_FOCUSED_DESCRIPTOR = msg({
	message: 'Works while {productName} is focused',
	comment:
		'Hint under a shortcut on Linux when the desktop owns system-wide shortcuts. The key shown here only works inside the app. {productName} is the app name.',
});
const IN_APP_KEY_DESCRIPTOR = msg({
	message:
		'This key works while {productName} is focused. Your desktop sets the system-wide key, under {settingsTabName} > {sectionName}.',
	comment:
		'Hint under the push-to-talk key recorder in voice settings on Linux when the desktop owns system-wide shortcuts. {settingsTabName} is the name of the settings tab with keyboard shortcuts and {sectionName} the title of its system-wide shortcuts section, both filled in by the app. {productName} is the app name.',
});
const SYSTEM_WIDE_TRIGGER_DESCRIPTOR = msg({
	message: 'System-wide: {trigger}',
	comment:
		'Hint under a shortcut on Linux showing the key the desktop assigned. {trigger} is the key, as named by the desktop.',
});
const SYSTEM_WIDE_NOT_ASSIGNED_DESCRIPTOR = msg({
	message: 'System-wide: Not assigned',
	comment: 'Hint under a shortcut on Linux when the desktop has no key assigned for it system-wide.',
});
const EXTRA_MODIFIERS_DESCRIPTOR = msg({
	message: 'Holding Shift, Ctrl or Alt blocks this shortcut. Add alternates in your system settings.',
	comment:
		'Hint under a system-wide hold shortcut such as push-to-talk on Linux. The desktop ignores the key while other modifier keys are held.',
});

export const SystemShortcutRowHint = observer(
	({
		action,
		variant = 'row',
		'data-flx': dataFlx = 'user.keybinds-tab.system-shortcut-row-hint',
	}: {
		action: KeybindCommand;
		variant?: 'row' | 'voice-tab';
		'data-flx'?: string;
	}) => {
		const {i18n} = useLingui();
		if (!GlobalShortcuts.isPortalBackend || !Keybind.isActionGlobalCapable(action)) return null;
		const bound = GlobalShortcuts.portal?.state === 'bound';
		const showTrigger = bound && GlobalShortcuts.linux?.desktop !== 'hyprland';
		const trigger = GlobalShortcuts.getPortalTrigger(action);
		return (
			<div className={styles.customHint} data-flx={dataFlx}>
				<div data-flx={`${dataFlx}.focused`}>
					{variant === 'voice-tab'
						? i18n._(IN_APP_KEY_DESCRIPTOR, {
								productName: PRODUCT_NAME,
								settingsTabName: getUserSettingsTabLabel(i18n, 'keybinds'),
								sectionName: i18n._(SYSTEM_WIDE_SHORTCUTS_DESCRIPTOR),
							})
						: i18n._(WORKS_WHILE_FOCUSED_DESCRIPTOR, {productName: PRODUCT_NAME})}
				</div>
				{showTrigger ? (
					<div data-flx={`${dataFlx}.trigger`}>
						{trigger === null
							? i18n._(SYSTEM_WIDE_NOT_ASSIGNED_DESCRIPTOR)
							: i18n._(SYSTEM_WIDE_TRIGGER_DESCRIPTOR, {trigger})}
					</div>
				) : null}
				{showTrigger && trigger !== null && HOLD_PORTAL_ACTIONS.has(action) && !triggerHasModifiers(trigger) ? (
					<div data-flx={`${dataFlx}.extra-modifiers`}>{i18n._(EXTRA_MODIFIERS_DESCRIPTOR)}</div>
				) : null}
				{showTrigger && variant === 'row' && isGnomeHoldTriggerRisky(action, trigger) ? (
					<div data-flx={`${dataFlx}.gnome-hold`}>{getGnomeHoldWarning(i18n, action)}</div>
				) : null}
			</div>
		);
	},
);
