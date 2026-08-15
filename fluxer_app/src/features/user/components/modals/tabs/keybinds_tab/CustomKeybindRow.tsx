// SPDX-License-Identifier: AGPL-3.0-or-later

import {KeybindRecorder} from '@app/features/input/components/KeybindRecorder';
import Keybind, {
	type CustomKeybindEntry,
	type KeybindCommand,
	type KeyCombo,
} from '@app/features/input/state/InputKeybind';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {MoreOptionsVerticalIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import styles from '@app/features/user/components/modals/tabs/KeybindsTab.module.css';
import {ActionDropdown} from '@app/features/user/components/modals/tabs/keybinds_tab/ActionDropdown';
import {useAssignableActionOptions} from '@app/features/user/components/modals/tabs/keybinds_tab/useAssignableActionOptions';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {TrashIcon, WarningIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const DISABLE_SHORTCUT_DESCRIPTOR = msg({
	message: 'Disable shortcut',
	comment: 'Button or menu action label in the keybinds tab. Keep it concise.',
});
const ENABLE_SHORTCUT_DESCRIPTOR = msg({
	message: 'Enable shortcut',
	comment: 'Button or menu action label in the keybinds tab. Keep it concise.',
});
const DELETE_SHORTCUT_DESCRIPTOR = msg({
	message: 'Delete shortcut',
	comment: 'Button or menu action label in the keybinds tab. Keep it concise. Keep the tone plain and specific.',
});
const CUSTOM_SHORTCUT_OPTIONS_DESCRIPTOR = msg({
	message: 'Custom shortcut options',
	comment: 'Short label in the keybinds tab. Keep it concise.',
});
const CUSTOM_SHORTCUT_ACTION_DESCRIPTOR = msg({
	message: 'Custom shortcut action',
	comment: 'Short label in the keybinds tab. Keep it concise.',
});
export const CustomKeybindRow = observer(
	({
		entry,
		onRemove,
		conflictLabel,
	}: {
		entry: CustomKeybindEntry;
		onRemove: () => void;
		conflictLabel: string | null;
	}) => {
		const {i18n} = useLingui();
		const options = useAssignableActionOptions(entry.action);
		const handleAction = (action: KeybindCommand | null) => {
			Keybind.setCustomKeybindAction(entry.id, action);
		};
		const handleCombo = (combo: KeyCombo) => {
			Keybind.updateCustomKeybindCombo(entry.id, {...combo, global: combo.global ?? entry.combo.global ?? true});
		};
		const handleClear = () => {
			Keybind.updateCustomKeybindCombo(entry.id, {key: '', code: '', global: entry.combo.global, enabled: true});
		};
		const handleEnabled = (value: boolean) => {
			Keybind.setCustomKeybindEnabled(entry.id, value);
		};
		const openRowMenu = (event: React.MouseEvent<HTMLButtonElement>) => {
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<MenuGroup data-flx="user.keybinds-tab.open-row-menu.menu-group">
					<MenuItem
						icon={<TrashIcon size={remFromPx(16)} data-flx="user.keybinds-tab.open-row-menu.trash-icon" />}
						danger
						onClick={() => {
							onClose();
							onRemove();
						}}
						data-flx="user.keybinds-tab.open-row-menu.menu-item.close--2"
					>
						{i18n._(DELETE_SHORTCUT_DESCRIPTOR)}
					</MenuItem>
				</MenuGroup>
			));
		};
		return (
			<div
				className={clsx(styles.customRow, !entry.enabled && styles.customRowDisabled)}
				data-keybind-id={entry.id}
				data-flx="user.keybinds-tab.custom-keybind-row.custom-row"
			>
				<div className={styles.customRowMain} data-flx="user.keybinds-tab.custom-keybind-row.custom-row-main">
					<div className={styles.customField} data-flx="user.keybinds-tab.custom-keybind-row.custom-field">
						<div className={styles.customFieldLabel} data-flx="user.keybinds-tab.custom-keybind-row.custom-field-label">
							<Trans>Action</Trans>
						</div>
						<ActionDropdown
							value={entry.action}
							options={options}
							onChange={handleAction}
							ariaLabel={i18n._(CUSTOM_SHORTCUT_ACTION_DESCRIPTOR)}
							data-flx="user.keybinds-tab.custom-keybind-row.action-dropdown"
						/>
					</div>
					<KeybindRecorder
						label={<Trans>Shortcut</Trans>}
						action={entry.action ?? 'voice_push_to_talk'}
						value={entry.combo}
						onChange={handleCombo}
						onClear={handleClear}
						data-flx="user.keybinds-tab.custom-keybind-row.keybind-recorder.combo"
					/>
					<div className={styles.customControls} data-flx="user.keybinds-tab.custom-keybind-row.custom-controls">
						<Switch
							value={entry.enabled}
							onChange={handleEnabled}
							compact
							ariaLabel={entry.enabled ? i18n._(DISABLE_SHORTCUT_DESCRIPTOR) : i18n._(ENABLE_SHORTCUT_DESCRIPTOR)}
							data-flx="user.keybinds-tab.custom-keybind-row.switch.handle-enabled"
						/>
						<button
							type="button"
							className={styles.dotsButton}
							onClick={openRowMenu}
							aria-label={i18n._(CUSTOM_SHORTCUT_OPTIONS_DESCRIPTOR)}
							data-flx="user.keybinds-tab.custom-keybind-row.dots-button.open-row-menu"
						>
							<MoreOptionsVerticalIcon
								size={18}
								data-flx="user.keybinds-tab.custom-keybind-row.more-options-vertical-icon"
							/>
						</button>
					</div>
				</div>
				{!entry.action ? (
					<div className={styles.customHint} data-flx="user.keybinds-tab.custom-keybind-row.custom-hint">
						<Trans>Choose an action.</Trans>
					</div>
				) : null}
				{conflictLabel ? (
					<div
						className={styles.conflictWarning}
						role="alert"
						data-flx="user.keybinds-tab.custom-keybind-row.conflict-warning"
					>
						<WarningIcon
							size={remFromPx(14)}
							weight="fill"
							className={styles.conflictIcon}
							aria-hidden
							data-flx="user.keybinds-tab.custom-keybind-row.conflict-icon"
						/>
						<span data-flx="user.keybinds-tab.custom-keybind-row.span">
							<Trans>Conflicts with {conflictLabel}</Trans>
						</span>
					</div>
				) : null}
			</div>
		);
	},
);
