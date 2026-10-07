// SPDX-License-Identifier: AGPL-3.0-or-later

import {CANARY_DESKTOP_ENTRY_NAME, DESKTOP_ENTRY_NAME} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import GlobalShortcuts from '@app/features/input/state/GlobalShortcuts';
import Keybind from '@app/features/input/state/InputKeybind';
import {Button} from '@app/features/ui/button/Button';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {Spinner} from '@app/features/ui/components/Spinner';
import {isCanaryDesktop} from '@app/features/ui/utils/NativeUtils';
import {WarningAlert} from '@app/features/ui/warning_alert/WarningAlert';
import styles from '@app/features/user/components/modals/tabs/KeybindsTab.module.css';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

export const SYSTEM_WIDE_SHORTCUTS_DESCRIPTOR = msg({
	message: 'System-wide shortcuts',
	comment: 'Settings subsection title for keyboard and mouse shortcuts that work outside the app.',
});
const NOT_SET_UP_DESCRIPTOR = msg({
	message:
		'{productName} can ask your desktop to run its shortcuts while {productName} is not focused. Your desktop asks you to confirm and lets you pick the keys.',
	comment:
		'Linux settings text before system-wide shortcuts are set up. The desktop environment owns the keys. {productName} is the app name.',
});
const HYPRLAND_NOT_SET_UP_DESCRIPTOR = msg({
	message:
		'{productName} can register its shortcuts with Hyprland so they run while {productName} is not focused. You then bind the keys in your Hyprland config.',
	comment:
		'Linux settings text before system-wide shortcuts are set up on the Hyprland desktop. Hyprland shows no dialog. The config is hyprland.lua on Hyprland 0.55 and later, hyprland.conf before. {productName} is the app name.',
});
const SET_UP_DESCRIPTOR = msg({
	message: 'Set up',
	comment: 'Button that asks the Linux desktop to register system-wide shortcuts for the app. Keep it short.',
});
const WAITING_FOR_DESKTOP_DESCRIPTOR = msg({
	message: 'Waiting for your desktop…',
	comment: 'Status text while the Linux desktop shows its own dialog for confirming system-wide shortcuts.',
});
const RECONNECTING_DESCRIPTOR = msg({
	message: 'Reconnecting to your desktop…',
	comment:
		'Status text on Linux while the app reconnects to the desktop service that runs system-wide shortcuts, for example after it restarted.',
});
const BOUND_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Your desktop runs these shortcuts even while {productName} is not focused.',
	comment:
		'Subtitle above the list of system-wide shortcuts assigned by the Linux desktop. {productName} is the app name.',
});
const HYPRLAND_BOUND_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'{productName} registered its shortcuts with Hyprland. Bind them in your Hyprland config to use them while {productName} is not focused.',
	comment:
		'Subtitle for system-wide shortcuts on the Hyprland desktop after setup. Hyprland shows no list of assigned keys. {productName} is the app name.',
});
const KEYS_RESERVED_DESCRIPTOR = msg({
	message: "Your desktop keeps these keys for {productName}, so other apps can't use them.",
	comment:
		'Linux settings note under the list of system-wide shortcuts the desktop runs (KDE, GNOME). A key assigned here stops reaching every other app. {productName} is the app name.',
});
const KEYS_RESERVED_DIRECT_INPUT_DESCRIPTOR = msg({
	message:
		"Your desktop keeps these keys for {productName}, so other apps can't use them. To use the same key in other apps, turn on direct input device access below.",
	comment:
		'Linux settings note under the list of system-wide shortcuts the desktop runs (KDE, GNOME), shown when the "Use direct input device access" switch is offered below it. A key assigned here stops reaching every other app. {productName} is the app name.',
});
const ONE_ACTION_PER_KEY_DESCRIPTOR = msg({
	message: 'Each key can run only one system-wide shortcut.',
	comment: 'Note under the list of system-wide shortcuts on Linux. The desktop cannot assign one key to two actions.',
});
const NOT_ASSIGNED_DESCRIPTOR = msg({
	message: 'Not assigned',
	comment: 'Shown instead of a key when the desktop has no key assigned to a system-wide shortcut.',
});
const CHANGE_IN_SYSTEM_SETTINGS_DESCRIPTOR = msg({
	message: 'Change in system settings',
	comment: 'Button that opens the Linux desktop dialog for changing the keys of system-wide shortcuts.',
});
const KDE_INSTRUCTIONS_DESCRIPTOR = msg({
	message: 'To change the keys, go to System Settings > Keyboard > Shortcuts > {productName}.',
	comment:
		'Instructions for KDE Plasma. Keep "System Settings", "Keyboard" and "Shortcuts" as the KDE menu names in your language. {productName} is the app name.',
});
const GNOME_INSTRUCTIONS_DESCRIPTOR = msg({
	message: 'To change the keys, go to Settings > Apps > {productName}.',
	comment:
		'Instructions for GNOME. Keep "Settings" and "Apps" as the GNOME menu names in your language. {productName} is the app name.',
});
const HYPRLAND_INSTRUCTIONS_DESCRIPTOR = msg({
	message:
		'Hyprland has no shortcut dialog. Add the lines for your Hyprland version to your Hyprland config and replace KEY with the key you want.',
	comment:
		'Instructions for the Hyprland desktop, shown above config lines for hyprland.lua (Hyprland 0.55 and later) and for hyprland.conf (before 0.55). Keep "KEY" exactly as written.',
});
const HYPRLAND_NO_APP_ID_DESCRIPTOR = msg({
	message: 'Hyprland has no shortcut dialog. Bind the keys in your Hyprland config.',
	comment:
		'Instructions for the Hyprland desktop when the app id is unknown. The config is hyprland.lua on Hyprland 0.55 and later, hyprland.conf before.',
});
const OTHER_DESKTOP_INSTRUCTIONS_DESCRIPTOR = msg({
	message: "To change the keys, open your desktop's keyboard shortcut settings.",
	comment: 'Instructions for changing system-wide shortcut keys on an unrecognized Linux desktop.',
});
const HYPRLAND_LUA_LINES_DESCRIPTOR = msg({
	message: 'hyprland.lua (Hyprland 0.55 and later)',
	comment: 'Label above Hyprland config lines in the Lua format. Keep "hyprland.lua" exactly as written.',
});
const HYPRLAND_CONF_LINES_DESCRIPTOR = msg({
	message: 'hyprland.conf (before Hyprland 0.55)',
	comment: 'Label above Hyprland config lines in the older hyprlang format. Keep "hyprland.conf" exactly as written.',
});
const COPY_LINES_DESCRIPTOR = msg({
	message: 'Copy lines',
	comment: 'Button that copies Hyprland config lines to the clipboard.',
});
const COPY_LUA_LINES_DESCRIPTOR = msg({
	message: 'Copy lines for hyprland.lua',
	comment:
		'Screen reader name of the button that copies Hyprland config lines in the Lua format. Starts with the visible button text "Copy lines". Keep "hyprland.lua" exactly as written.',
});
const COPY_CONF_LINES_DESCRIPTOR = msg({
	message: 'Copy lines for hyprland.conf',
	comment:
		'Screen reader name of the button that copies Hyprland config lines in the older hyprlang format. Starts with the visible button text "Copy lines". Keep "hyprland.conf" exactly as written.',
});
const DECLINED_DESCRIPTOR = msg({
	message: "Your desktop didn't set up system-wide shortcuts.",
	comment:
		'Linux settings text when the desktop dialog for system-wide shortcuts was dismissed or failed. Neutral wording, the user may not have declined anything.',
});
const CHECK_AGAIN_DESCRIPTOR = msg({
	message: 'Check again',
	comment:
		'Button on Linux that checks again whether the desktop offers system-wide shortcuts, for example after the desktop service started. Keep it short.',
});
const UNSUPPORTED_DESCRIPTOR = msg({
	message: "This desktop doesn't offer system-wide shortcuts. Shortcuts still work while {productName} is focused.",
	comment: 'Linux settings text when the desktop has no system-wide shortcut support. {productName} is the app name.',
});
const ERROR_DESCRIPTOR = msg({
	message: 'System-wide shortcuts could not be set up.',
	comment: 'Linux settings text when setting up system-wide shortcuts failed.',
});
const UPDATE_DESCRIPTOR = msg({
	message: 'Update {productName} to use system-wide shortcuts on Wayland.',
	comment:
		'Linux settings text when the installed desktop app is too old for system-wide shortcuts. {productName} is the app name.',
});
const DIRECT_INPUT_LABEL_DESCRIPTOR = msg({
	message: 'Use direct input device access',
	comment:
		'Switch label on Linux. When on, the app reads keyboard devices directly for system-wide shortcuts. Not related to the "Direct input" voice processing profile.',
});
const DIRECT_INPUT_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Lets {productName} read your keyboard directly. Your system already allows this.',
	comment: 'Description for the Linux direct input device access switch. {productName} is the app name.',
});
const DIRECT_INPUT_ENABLED_DESKTOP_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Lets {productName} read your keyboard directly. Turn this off to set up shortcuts through your desktop instead.',
	comment:
		'Description for the Linux direct input device access switch while it is on, on a desktop that can also run system-wide shortcuts itself (KDE, GNOME, Hyprland). {productName} is the app name.',
});
const DIRECT_INPUT_UNAVAILABLE_DESCRIPTION_DESCRIPTOR = msg({
	message: "{productName} can no longer read your keyboard, so your desktop's shortcuts are used instead.",
	comment:
		'Description for the Linux direct input device access switch when it is on but the keyboard can no longer be read, for example after a restart removed the access. The app already falls back to the shortcuts the desktop runs. {productName} is the app name.',
});
const DIRECT_INPUT_UNAVAILABLE_X11_DESCRIPTION_DESCRIPTOR = msg({
	message: '{productName} can no longer read your keyboard, so it listens for shortcuts through X11 instead.',
	comment:
		'Description for the Linux direct input device access switch when it is on but the keyboard can no longer be read, on an X11 session. The app now listens for shortcuts through the X11 display server itself. Keep "X11" as written. {productName} is the app name.',
});
const DIRECT_INPUT_UNAVAILABLE_FOCUSED_DESCRIPTION_DESCRIPTOR = msg({
	message: '{productName} can no longer read your keyboard. Shortcuts only work while {productName} is focused.',
	comment:
		'Description for the Linux direct input device access switch when it is on but the keyboard can no longer be read and nothing else runs system-wide shortcuts. {productName} is the app name.',
});
const DIRECT_INPUT_PUSH_TO_TALK_WARNING_DESCRIPTOR = msg({
	message:
		'Push-to-talk will use the key you set in {productName}. Set one first, or your microphone will use voice activity.',
	comment:
		'Linux warning next to the direct input device access switch when push-to-talk only has a key assigned by the desktop. With direct input on, only the push-to-talk key set in the app works, and without one the microphone sends whenever you speak. {productName} is the app name.',
});
const DIRECT_INPUT_ERROR_DESCRIPTOR = msg({
	message: "Keyboard access couldn't start. Shortcuts only work while {productName} is focused.",
	comment:
		'Linux warning when direct input device access is on but reading the keyboard failed. Not related to the "Direct input" voice processing profile. {productName} is the app name.',
});
const GNOME_HOLD_WARNING_PUSH_TO_TALK_DESCRIPTOR = msg({
	message:
		'For push-to-talk, use a single key without Ctrl, Alt or Shift. With a combination, let go of the main key first or the microphone can stay on.',
	comment:
		'Warning on GNOME when the push-to-talk shortcut uses a key combination. Letting go of a modifier key such as Ctrl before the main key can leave the microphone on.',
});
const GNOME_HOLD_WARNING_PUSH_TO_MUTE_DESCRIPTOR = msg({
	message:
		'For push-to-mute, use a single key without Ctrl, Alt or Shift. With a combination, let go of the main key first or the microphone can stay muted.',
	comment:
		'Warning on GNOME when the push-to-mute shortcut uses a key combination. Letting go of a modifier key such as Ctrl before the main key can leave the microphone muted.',
});
const GNOME_HOLD_WARNING_PRIORITY_DESCRIPTOR = msg({
	message:
		'For voice activity priority, use a single key without Ctrl, Alt or Shift. With a combination, let go of the main key first or priority can stay on.',
	comment:
		'Warning on GNOME when the voice activity priority shortcut uses a key combination. Letting go of a modifier key such as Ctrl before the main key can leave priority speaking on. "Voice activity priority" is the shortcut name.',
});
const GNOME_HOLD_WARNING_GENERIC_DESCRIPTOR = msg({
	message:
		'For this shortcut, use a single key without Ctrl, Alt or Shift. With a combination, let go of the main key first or the shortcut can stay active.',
	comment:
		'Warning on GNOME when a hold shortcut uses a key combination. Letting go of a modifier key such as Ctrl before the main key can leave the shortcut active.',
});
const PTT_NOT_SET_UP_DESCRIPTOR = msg({
	message: 'Push-to-talk only works while {productName} is focused until you set up system-wide shortcuts.',
	comment: 'Voice settings warning on Linux before system-wide shortcuts are set up. {productName} is the app name.',
});
const PTT_DECLINED_DESCRIPTOR = msg({
	message:
		"Push-to-talk only works while {productName} is focused because your desktop didn't set up system-wide shortcuts.",
	comment:
		'Voice settings warning on Linux when the desktop dialog for system-wide shortcuts was dismissed or failed. {productName} is the app name.',
});
const PTT_UNSUPPORTED_DESCRIPTOR = msg({
	message: "This desktop doesn't offer system-wide shortcuts. Push-to-talk only works while {productName} is focused.",
	comment:
		'Voice settings warning on Linux desktops without system-wide shortcut support. {productName} is the app name.',
});
const PTT_UNASSIGNED_KDE_DESCRIPTOR = msg({
	message:
		'Push-to-talk has no system-wide key yet. To set one, go to System Settings > Keyboard > Shortcuts > {productName}.',
	comment:
		'Voice settings warning on KDE Plasma when the desktop has no key assigned to push-to-talk. Keep "System Settings", "Keyboard" and "Shortcuts" as the KDE menu names in your language. {productName} is the app name.',
});
const PTT_UNASSIGNED_GNOME_DESCRIPTOR = msg({
	message: 'Push-to-talk has no system-wide key yet. To set one, go to Settings > Apps > {productName}.',
	comment:
		'Voice settings warning on GNOME when the desktop has no key assigned to push-to-talk. Keep "Settings" and "Apps" as the GNOME menu names in your language. {productName} is the app name.',
});
const PTT_UNASSIGNED_OTHER_DESCRIPTOR = msg({
	message: "Push-to-talk has no system-wide key yet. To set one, open your desktop's keyboard shortcut settings.",
	comment:
		'Voice settings warning on an unrecognized Linux desktop when the desktop has no key assigned to push-to-talk.',
});
const PTT_HYPRLAND_DESCRIPTOR = msg({
	message:
		'Push-to-talk works outside {productName} only after you bind it in your Hyprland config. Add the line for your Hyprland version and replace KEY with the key you want.',
	comment:
		'Voice settings note on the Hyprland desktop, shown above config lines for hyprland.lua (Hyprland 0.55 and later) and for hyprland.conf (before 0.55). Keep "KEY" exactly as written. {productName} is the app name.',
});
const PTT_HYPRLAND_NO_APP_ID_DESCRIPTOR = msg({
	message: 'Push-to-talk works outside {productName} only after you bind it in your Hyprland config.',
	comment:
		'Voice settings note on the Hyprland desktop when the app id is unknown. The config is hyprland.lua on Hyprland 0.55 and later, hyprland.conf before. {productName} is the app name.',
});

export const HOLD_PORTAL_ACTIONS: ReadonlySet<string> = new Set([
	'voice_push_to_talk',
	'voice_push_to_talk_priority',
	'voice_push_to_mute',
	'voice_priority_vad',
]);

export function triggerHasModifiers(triggerDescription: string): boolean {
	return triggerDescription.length > 1 && triggerDescription.includes('+');
}

export function isGnomeHoldTriggerRisky(action: string, triggerDescription: string | null): boolean {
	if (GlobalShortcuts.linux?.desktop !== 'gnome') return false;
	if (!HOLD_PORTAL_ACTIONS.has(action)) return false;
	return triggerDescription !== null && triggerHasModifiers(triggerDescription);
}

export function getGnomeHoldWarning(i18n: I18n, action: string): string {
	switch (action) {
		case 'voice_push_to_talk':
		case 'voice_push_to_talk_priority':
			return i18n._(GNOME_HOLD_WARNING_PUSH_TO_TALK_DESCRIPTOR);
		case 'voice_push_to_mute':
			return i18n._(GNOME_HOLD_WARNING_PUSH_TO_MUTE_DESCRIPTOR);
		case 'voice_priority_vad':
			return i18n._(GNOME_HOLD_WARNING_PRIORITY_DESCRIPTOR);
		default:
			return i18n._(GNOME_HOLD_WARNING_GENERIC_DESCRIPTOR);
	}
}

function getPortalActions(): Array<{action: string; label: string; triggerDescription: string | null}> {
	return Keybind.getDefaults()
		.filter((config) => config.allowGlobal)
		.map((config) => ({
			action: config.action,
			label: config.label,
			triggerDescription: GlobalShortcuts.getPortalTrigger(config.action),
		}));
}

function getHyprlandLuaBindLines(portalAppId: string, actions: ReadonlyArray<string>): string {
	return actions.map((action) => `hl.bind("KEY", hl.dsp.global("${portalAppId}:${action}"))`).join('\n');
}

function getHyprlandConfBindLines(portalAppId: string, actions: ReadonlyArray<string>): string {
	return actions.map((action) => `bind = , KEY, global, ${portalAppId}:${action}`).join('\n');
}

function getDesktopEntryName(): string {
	return isCanaryDesktop() ? CANARY_DESKTOP_ENTRY_NAME : DESKTOP_ENTRY_NAME;
}

function getPushToTalkUnassignedText(i18n: I18n): string {
	switch (GlobalShortcuts.linux?.desktop) {
		case 'kde':
			return i18n._(PTT_UNASSIGNED_KDE_DESCRIPTOR, {productName: getDesktopEntryName()});
		case 'gnome':
			return i18n._(PTT_UNASSIGNED_GNOME_DESCRIPTOR, {productName: getDesktopEntryName()});
		default:
			return i18n._(PTT_UNASSIGNED_OTHER_DESCRIPTOR);
	}
}

function getChangeInstructions(i18n: I18n): string {
	switch (GlobalShortcuts.linux?.desktop) {
		case 'kde':
			return i18n._(KDE_INSTRUCTIONS_DESCRIPTOR, {productName: getDesktopEntryName()});
		case 'gnome':
			return i18n._(GNOME_INSTRUCTIONS_DESCRIPTOR, {productName: getDesktopEntryName()});
		case 'hyprland':
			return GlobalShortcuts.portal?.portalAppId
				? i18n._(HYPRLAND_INSTRUCTIONS_DESCRIPTOR)
				: i18n._(HYPRLAND_NO_APP_ID_DESCRIPTOR);
		default:
			return i18n._(OTHER_DESKTOP_INSTRUCTIONS_DESCRIPTOR);
	}
}

const PUSH_TO_TALK_ACTIONS = ['voice_push_to_talk', 'voice_push_to_talk_priority'] as const;

function isPushToTalkDesktopOnly(): boolean {
	if (!Keybind.isPushToTalkEnabled()) return false;
	if (!PUSH_TO_TALK_ACTIONS.some((action) => GlobalShortcuts.isPortalActionAssigned(action))) return false;
	return !PUSH_TO_TALK_ACTIONS.some((action) =>
		Keybind.getActiveCombosForAction(action).some((combo) => combo.global === true),
	);
}

const SetUpButton: React.FC<{label: string; 'data-flx': string}> = observer(({label, 'data-flx': dataFlx}) => (
	<Button
		variant="primary"
		small={true}
		onClick={() => void GlobalShortcuts.setUp()}
		submitting={GlobalShortcuts.pendingAction === 'set-up'}
		data-flx={dataFlx}
	>
		{label}
	</Button>
));

const ConfigureButton: React.FC<{'data-flx': string}> = observer(({'data-flx': dataFlx}) => {
	const {i18n} = useLingui();
	return (
		<Button
			variant="secondary"
			small={true}
			onClick={() => void GlobalShortcuts.configure()}
			submitting={GlobalShortcuts.pendingAction === 'configure'}
			disabled={GlobalShortcuts.portalRecovering}
			data-flx={dataFlx}
		>
			{i18n._(CHANGE_IN_SYSTEM_SETTINGS_DESCRIPTOR)}
		</Button>
	);
});

const CheckAgainButton: React.FC<{'data-flx': string}> = observer(({'data-flx': dataFlx}) => {
	const {i18n} = useLingui();
	return (
		<Button
			variant="secondary"
			small={true}
			onClick={() => void GlobalShortcuts.recheck()}
			submitting={GlobalShortcuts.pendingAction === 'recheck'}
			data-flx={dataFlx}
		>
			{i18n._(CHECK_AGAIN_DESCRIPTOR)}
		</Button>
	);
});

const ReconnectingSection: React.FC<{'data-flx': string}> = observer(({'data-flx': dataFlx}) => {
	const {i18n} = useLingui();
	return (
		<div className={styles.customSection} data-flx={dataFlx}>
			<div className={styles.customHeader} data-flx="user.system-shortcuts-section.reconnecting.header">
				<div className={styles.customHeaderText} data-flx="user.system-shortcuts-section.reconnecting.header-text">
					<h3 className={styles.customTitle} data-flx="user.system-shortcuts-section.reconnecting.title">
						{i18n._(SYSTEM_WIDE_SHORTCUTS_DESCRIPTOR)}
					</h3>
					<p className={styles.customSubtitle} data-flx="user.system-shortcuts-section.reconnecting.subtitle">
						{i18n._(RECONNECTING_DESCRIPTOR)}
					</p>
				</div>
				<Spinner size="small" data-flx="user.system-shortcuts-section.reconnecting.spinner" />
			</div>
		</div>
	);
});

const HyprlandBindBlock: React.FC<{label: string; copyLabel: string; lines: string; 'data-flx': string}> = observer(
	({label, copyLabel, lines, 'data-flx': dataFlx}) => {
		const {i18n} = useLingui();
		return (
			<div className={styles.shortcutOptions} data-flx={dataFlx}>
				<p className={styles.permissionSectionHelper} data-flx={`${dataFlx}.label`}>
					{label}
				</p>
				<pre className={styles.systemShortcutsCommands} data-flx={`${dataFlx}.code`}>
					{lines}
				</pre>
				<div className={styles.customHeaderActions} data-flx={`${dataFlx}.actions`}>
					<Button
						variant="secondary"
						small={true}
						onClick={() => void TextCopyCommands.copy(i18n, lines)}
						aria-label={copyLabel}
						data-flx={`${dataFlx}.button.copy`}
					>
						{i18n._(COPY_LINES_DESCRIPTOR)}
					</Button>
				</div>
			</div>
		);
	},
);

const HyprlandBindLines: React.FC<{portalAppId: string; actions: ReadonlyArray<string>; 'data-flx': string}> = observer(
	({portalAppId, actions, 'data-flx': dataFlx}) => {
		const {i18n} = useLingui();
		return (
			<>
				<HyprlandBindBlock
					label={i18n._(HYPRLAND_LUA_LINES_DESCRIPTOR)}
					copyLabel={i18n._(COPY_LUA_LINES_DESCRIPTOR)}
					lines={getHyprlandLuaBindLines(portalAppId, actions)}
					data-flx={`${dataFlx}.lua`}
				/>
				<HyprlandBindBlock
					label={i18n._(HYPRLAND_CONF_LINES_DESCRIPTOR)}
					copyLabel={i18n._(COPY_CONF_LINES_DESCRIPTOR)}
					lines={getHyprlandConfBindLines(portalAppId, actions)}
					data-flx={`${dataFlx}.conf`}
				/>
			</>
		);
	},
);

const BoundPortalShortcuts: React.FC<{'data-flx': string}> = observer(({'data-flx': dataFlx}) => {
	const {i18n} = useLingui();
	const portal = GlobalShortcuts.portal;
	if (!portal) return null;
	const desktop = GlobalShortcuts.linux?.desktop;
	const actions = getPortalActions();
	return (
		<div className={styles.customSection} data-flx={dataFlx}>
			<div className={styles.customHeader} data-flx="user.system-shortcuts-section.bound.header">
				<div className={styles.customHeaderText} data-flx="user.system-shortcuts-section.bound.header-text">
					<h3 className={styles.customTitle} data-flx="user.system-shortcuts-section.bound.title">
						{i18n._(SYSTEM_WIDE_SHORTCUTS_DESCRIPTOR)}
					</h3>
					<p className={styles.customSubtitle} data-flx="user.system-shortcuts-section.bound.subtitle">
						{i18n._(desktop === 'hyprland' ? HYPRLAND_BOUND_DESCRIPTION_DESCRIPTOR : BOUND_DESCRIPTION_DESCRIPTOR, {
							productName: RuntimeConfig.productName,
						})}
					</p>
					{GlobalShortcuts.portalRecovering ? (
						<p className={styles.customHint} data-flx="user.system-shortcuts-section.bound.reconnecting">
							{i18n._(RECONNECTING_DESCRIPTOR)}
						</p>
					) : null}
				</div>
				{portal.canConfigure ? (
					<div className={styles.customHeaderActions} data-flx="user.system-shortcuts-section.bound.header-actions">
						<ConfigureButton data-flx="user.system-shortcuts-section.bound.button.configure" />
					</div>
				) : null}
			</div>
			{desktop === 'hyprland' ? null : (
				<div className={styles.defaultsSectionRows} data-flx="user.system-shortcuts-section.bound.rows">
					{actions.map(({action, label, triggerDescription}) => (
						<div key={action} className={styles.defaultRow} data-flx="user.system-shortcuts-section.bound.row">
							<div className={styles.defaultLabel} data-flx="user.system-shortcuts-section.bound.row.label">
								{label}
								{isGnomeHoldTriggerRisky(action, triggerDescription) ? (
									<div className={styles.customHint} data-flx="user.system-shortcuts-section.bound.row.gnome-hold-hint">
										{getGnomeHoldWarning(i18n, action)}
									</div>
								) : null}
							</div>
							{triggerDescription ? (
								<span className={styles.defaultChip} data-flx="user.system-shortcuts-section.bound.row.trigger">
									{triggerDescription}
								</span>
							) : (
								<span
									className={styles.defaultChipsEmpty}
									data-flx="user.system-shortcuts-section.bound.row.not-assigned"
								>
									{i18n._(NOT_ASSIGNED_DESCRIPTOR)}
								</span>
							)}
						</div>
					))}
				</div>
			)}
			<p className={styles.permissionSectionHelper} data-flx="user.system-shortcuts-section.bound.instructions">
				{getChangeInstructions(i18n)}
			</p>
			{desktop === 'hyprland' ? null : (
				<p className={styles.permissionSectionHelper} data-flx="user.system-shortcuts-section.bound.one-action-per-key">
					{i18n._(ONE_ACTION_PER_KEY_DESCRIPTOR)}
				</p>
			)}
			{desktop === 'hyprland' ? null : (
				<p className={styles.permissionSectionHelper} data-flx="user.system-shortcuts-section.bound.keys-reserved">
					{i18n._(shouldShowDirectInputSwitch() ? KEYS_RESERVED_DIRECT_INPUT_DESCRIPTOR : KEYS_RESERVED_DESCRIPTOR, {
						productName: RuntimeConfig.productName,
					})}
				</p>
			)}
			{desktop === 'hyprland' && portal.portalAppId ? (
				<HyprlandBindLines
					portalAppId={portal.portalAppId}
					actions={actions.map(({action}) => action)}
					data-flx="user.system-shortcuts-section.hyprland-lines"
				/>
			) : null}
		</div>
	);
});

const PortalStateSection: React.FC<{'data-flx': string}> = observer(({'data-flx': dataFlx}) => {
	const {i18n} = useLingui();
	const portal = GlobalShortcuts.portal;
	if (!portal) return null;
	if (GlobalShortcuts.portalRecovering && portal.state !== 'bound') {
		return <ReconnectingSection data-flx={`${dataFlx}.reconnecting`} />;
	}
	const title = i18n._(SYSTEM_WIDE_SHORTCUTS_DESCRIPTOR);
	switch (portal.state) {
		case 'not-set-up':
			return (
				<WarningAlert
					title={title}
					actions={
						<SetUpButton label={i18n._(SET_UP_DESCRIPTOR)} data-flx="user.system-shortcuts-section.button.set-up" />
					}
					data-flx={`${dataFlx}.not-set-up`}
				>
					{i18n._(
						GlobalShortcuts.linux?.desktop === 'hyprland' ? HYPRLAND_NOT_SET_UP_DESCRIPTOR : NOT_SET_UP_DESCRIPTOR,
						{productName: RuntimeConfig.productName},
					)}
				</WarningAlert>
			);
		case 'binding':
			return (
				<div className={styles.customSection} data-flx={`${dataFlx}.binding`}>
					<div className={styles.customHeader} data-flx="user.system-shortcuts-section.binding.header">
						<div className={styles.customHeaderText} data-flx="user.system-shortcuts-section.binding.header-text">
							<h3 className={styles.customTitle} data-flx="user.system-shortcuts-section.binding.title">
								{title}
							</h3>
							<p className={styles.customSubtitle} data-flx="user.system-shortcuts-section.binding.subtitle">
								{i18n._(WAITING_FOR_DESKTOP_DESCRIPTOR)}
							</p>
						</div>
						<Spinner size="small" data-flx="user.system-shortcuts-section.binding.spinner" />
					</div>
				</div>
			);
		case 'bound':
			return <BoundPortalShortcuts data-flx={`${dataFlx}.bound`} />;
		case 'declined':
			return (
				<WarningAlert
					title={title}
					actions={
						<SetUpButton
							label={i18n._(TRY_AGAIN_DESCRIPTOR)}
							data-flx="user.system-shortcuts-section.button.retry-declined"
						/>
					}
					data-flx={`${dataFlx}.declined`}
				>
					{i18n._(DECLINED_DESCRIPTOR)}
				</WarningAlert>
			);
		case 'unsupported':
			return (
				<WarningAlert
					title={title}
					actions={
						portal.canRecheck ? (
							<CheckAgainButton data-flx="user.system-shortcuts-section.button.check-again" />
						) : undefined
					}
					data-flx={`${dataFlx}.unsupported`}
				>
					{i18n._(UNSUPPORTED_DESCRIPTOR, {productName: RuntimeConfig.productName})}
				</WarningAlert>
			);
		case 'error':
			return (
				<WarningAlert
					title={title}
					actions={
						<SetUpButton
							label={i18n._(TRY_AGAIN_DESCRIPTOR)}
							data-flx="user.system-shortcuts-section.button.retry-error"
						/>
					}
					data-flx={`${dataFlx}.error`}
				>
					{i18n._(ERROR_DESCRIPTOR)}
				</WarningAlert>
			);
		default:
			return null;
	}
});

function shouldShowDirectInputSwitch(): boolean {
	const directInput = GlobalShortcuts.linux?.directInput;
	if (!directInput || directInput.locked === true) return false;
	return directInput.available || directInput.enabled;
}

function canDesktopRunShortcuts(): boolean {
	const linux = GlobalShortcuts.linux;
	if (linux?.session !== 'wayland') return false;
	return linux.desktop === 'kde' || linux.desktop === 'gnome' || linux.desktop === 'hyprland';
}

function getDirectInputDescription(available: boolean): MessageDescriptor {
	if (available) {
		return GlobalShortcuts.backend === 'evdev' && canDesktopRunShortcuts()
			? DIRECT_INPUT_ENABLED_DESKTOP_DESCRIPTION_DESCRIPTOR
			: DIRECT_INPUT_DESCRIPTION_DESCRIPTOR;
	}
	const backend = GlobalShortcuts.backend;
	if (backend === 'x11') return DIRECT_INPUT_UNAVAILABLE_X11_DESCRIPTION_DESCRIPTOR;
	if (backend === 'portal' && GlobalShortcuts.portal?.state === 'bound') {
		return DIRECT_INPUT_UNAVAILABLE_DESCRIPTION_DESCRIPTOR;
	}
	return DIRECT_INPUT_UNAVAILABLE_FOCUSED_DESCRIPTION_DESCRIPTOR;
}

const DirectInputSwitch: React.FC<{'data-flx': string}> = observer(({'data-flx': dataFlx}) => {
	const {i18n} = useLingui();
	const directInput = GlobalShortcuts.linux?.directInput;
	if (!directInput || !shouldShowDirectInputSwitch()) return null;
	return (
		<>
			{!directInput.enabled && isPushToTalkDesktopOnly() ? (
				<WarningAlert data-flx={`${dataFlx}.push-to-talk-warning`}>
					{i18n._(DIRECT_INPUT_PUSH_TO_TALK_WARNING_DESCRIPTOR, {productName: RuntimeConfig.productName})}
				</WarningAlert>
			) : null}
			<Switch
				label={i18n._(DIRECT_INPUT_LABEL_DESCRIPTOR)}
				description={i18n._(getDirectInputDescription(directInput.available), {productName: RuntimeConfig.productName})}
				value={directInput.enabled}
				onChange={(value) => void GlobalShortcuts.setDirectInputEnabled(value)}
				disabled={GlobalShortcuts.pendingAction !== null}
				ariaLabel={i18n._(DIRECT_INPUT_LABEL_DESCRIPTOR)}
				data-flx={dataFlx}
			/>
		</>
	);
});

function isPortalStatusBackend(): boolean {
	const backend = GlobalShortcuts.backend;
	return backend === 'portal' || backend === 'none';
}

function hasDirectInputError(): boolean {
	return GlobalShortcuts.backend === 'evdev' && GlobalShortcuts.hookError !== null;
}

export const SystemShortcutsSection: React.FC<{'data-flx'?: string}> = observer(
	({'data-flx': dataFlx = 'user.system-shortcuts-section'}) => {
		const {i18n} = useLingui();
		if (GlobalShortcuts.legacyWaylandNeedsUpdate) {
			return (
				<WarningAlert title={i18n._(SYSTEM_WIDE_SHORTCUTS_DESCRIPTOR)} data-flx={`${dataFlx}.update`}>
					{i18n._(UPDATE_DESCRIPTOR, {productName: RuntimeConfig.productName})}
				</WarningAlert>
			);
		}
		const linux = GlobalShortcuts.linux;
		if (!linux || GlobalShortcuts.backend === null) return null;
		const portalState = GlobalShortcuts.portal?.state;
		const showPortal =
			isPortalStatusBackend() && portalState !== undefined && portalState !== 'unknown' && portalState !== 'probing';
		const showDirectInputError = hasDirectInputError();
		if (!showPortal && !showDirectInputError && !shouldShowDirectInputSwitch()) return null;
		return (
			<div className={styles.customSection} data-flx={dataFlx}>
				{showPortal ? <PortalStateSection data-flx={dataFlx} /> : null}
				<DirectInputSwitch data-flx={`${dataFlx}.switch.direct-input`} />
				{showDirectInputError ? (
					<WarningAlert data-flx={`${dataFlx}.direct-input-error`}>
						{i18n._(DIRECT_INPUT_ERROR_DESCRIPTOR, {productName: RuntimeConfig.productName})}
					</WarningAlert>
				) : null}
			</div>
		);
	},
);

const HyprlandPushToTalkCard: React.FC<{portalAppId: string | null; 'data-flx': string}> = observer(
	({portalAppId, 'data-flx': dataFlx}) => {
		const {i18n} = useLingui();
		return (
			<div className={styles.customSection} data-flx={dataFlx}>
				<p className={styles.permissionSectionHelper} data-flx={`${dataFlx}.text`}>
					{portalAppId
						? i18n._(PTT_HYPRLAND_DESCRIPTOR, {productName: RuntimeConfig.productName})
						: i18n._(PTT_HYPRLAND_NO_APP_ID_DESCRIPTOR, {productName: RuntimeConfig.productName})}
				</p>
				{portalAppId ? (
					<HyprlandBindLines portalAppId={portalAppId} actions={['voice_push_to_talk']} data-flx={`${dataFlx}.lines`} />
				) : null}
			</div>
		);
	},
);

export const SystemShortcutsPushToTalkAlert: React.FC<{'data-flx'?: string}> = observer(
	({'data-flx': dataFlx = 'user.system-shortcuts-ptt-alert'}) => {
		const {i18n} = useLingui();
		if (GlobalShortcuts.legacyWaylandNeedsUpdate) {
			return (
				<WarningAlert data-flx={`${dataFlx}.update`}>
					{i18n._(UPDATE_DESCRIPTOR, {productName: RuntimeConfig.productName})}
				</WarningAlert>
			);
		}
		const linux = GlobalShortcuts.linux;
		if (!linux) return null;
		if (hasDirectInputError()) {
			return (
				<WarningAlert data-flx={`${dataFlx}.direct-input-error`}>
					{i18n._(DIRECT_INPUT_ERROR_DESCRIPTOR, {productName: RuntimeConfig.productName})}
				</WarningAlert>
			);
		}
		const portal = GlobalShortcuts.portal;
		if (!portal || !isPortalStatusBackend()) return null;
		if (GlobalShortcuts.portalRecovering && portal.state !== 'bound') {
			return <WarningAlert data-flx={`${dataFlx}.reconnecting`}>{i18n._(RECONNECTING_DESCRIPTOR)}</WarningAlert>;
		}
		switch (portal.state) {
			case 'not-set-up':
				return (
					<WarningAlert
						actions={<SetUpButton label={i18n._(SET_UP_DESCRIPTOR)} data-flx={`${dataFlx}.button.set-up`} />}
						data-flx={`${dataFlx}.not-set-up`}
					>
						{i18n._(PTT_NOT_SET_UP_DESCRIPTOR, {productName: RuntimeConfig.productName})}
					</WarningAlert>
				);
			case 'binding':
				return <WarningAlert data-flx={`${dataFlx}.binding`}>{i18n._(WAITING_FOR_DESKTOP_DESCRIPTOR)}</WarningAlert>;
			case 'declined':
				return (
					<WarningAlert
						actions={<SetUpButton label={i18n._(TRY_AGAIN_DESCRIPTOR)} data-flx={`${dataFlx}.button.retry-declined`} />}
						data-flx={`${dataFlx}.declined`}
					>
						{i18n._(PTT_DECLINED_DESCRIPTOR, {productName: RuntimeConfig.productName})}
					</WarningAlert>
				);
			case 'error':
				return (
					<WarningAlert
						actions={<SetUpButton label={i18n._(TRY_AGAIN_DESCRIPTOR)} data-flx={`${dataFlx}.button.retry-error`} />}
						data-flx={`${dataFlx}.error`}
					>
						{i18n._(ERROR_DESCRIPTOR)}
					</WarningAlert>
				);
			case 'unsupported':
				return (
					<WarningAlert
						actions={portal.canRecheck ? <CheckAgainButton data-flx={`${dataFlx}.button.check-again`} /> : undefined}
						data-flx={`${dataFlx}.unsupported`}
					>
						{i18n._(PTT_UNSUPPORTED_DESCRIPTOR, {productName: RuntimeConfig.productName})}
					</WarningAlert>
				);
			case 'bound': {
				if (linux.desktop === 'hyprland')
					return <HyprlandPushToTalkCard portalAppId={portal.portalAppId} data-flx={`${dataFlx}.hyprland`} />;
				const trigger = GlobalShortcuts.getPortalTrigger('voice_push_to_talk');
				if (trigger !== null) {
					if (!isGnomeHoldTriggerRisky('voice_push_to_talk', trigger)) return null;
					return (
						<WarningAlert data-flx={`${dataFlx}.gnome-hold`}>
							{getGnomeHoldWarning(i18n, 'voice_push_to_talk')}
						</WarningAlert>
					);
				}
				return (
					<WarningAlert
						actions={portal.canConfigure ? <ConfigureButton data-flx={`${dataFlx}.button.configure`} /> : undefined}
						data-flx={`${dataFlx}.unassigned`}
					>
						{getPushToTalkUnassignedText(i18n)}
					</WarningAlert>
				);
			}
			default:
				return null;
		}
	},
);
