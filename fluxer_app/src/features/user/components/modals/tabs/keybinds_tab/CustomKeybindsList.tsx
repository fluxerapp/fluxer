// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import Keybind from '@app/features/input/state/InputKeybind';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {RetryIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import styles from '@app/features/user/components/modals/tabs/KeybindsTab.module.css';
import {getCustomKeybindActionLabelMap} from '@app/features/user/components/modals/tabs/keybinds_tab/AssignableActionOptions';
import {CustomKeybindRow} from '@app/features/user/components/modals/tabs/keybinds_tab/CustomKeybindRow';
import {
	comboMatchesQuery,
	combosLooseEqual,
	normalizeQuery,
} from '@app/features/user/components/modals/tabs/keybinds_tab/shared';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

const UNASSIGNED_2_DESCRIPTOR = msg({
	message: '(unassigned)',
	comment: 'Short label in the keybinds tab. Keep it concise.',
});
const RESET_ALL_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Reset all shortcuts?',
	comment: 'Confirmation prompt in the keybinds tab.',
});
const THIS_REMOVES_EVERY_CUSTOM_SHORTCUT_AND_RE_ENABLES_DESCRIPTOR = msg({
	message: 'This removes every custom shortcut and re-enables all built-in shortcuts. This cannot be undone.',
	comment: 'Error message in the keybinds tab. Keep the tone plain and specific.',
});
const RESET_TO_DEFAULTS_DESCRIPTOR = msg({
	message: 'Reset to defaults',
	comment: 'Button or menu action label in the keybinds tab. Keep it concise.',
});
const DISABLE_BUILT_IN_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Disable built-ins',
	comment: 'Button or menu action label in the keybinds tab. Keep it concise.',
});
const SYNC_SHORTCUTS_ACROSS_DEVICES_DESCRIPTOR = msg({
	message: 'Sync shortcuts',
	comment: 'Label in the keybinds tab.',
});
const CUSTOM_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Custom shortcuts',
	comment: 'Short heading in the keybinds tab.',
});
const RECORDING_PAUSES_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Recording pauses shortcuts.',
	comment: 'Short helper text in the keybinds tab.',
});
export const CustomKeybindsList: React.FC<{searchQuery: string}> = observer(({searchQuery}) => {
	const {i18n} = useLingui();
	const customKeybinds = Keybind.getCustomKeybinds();
	const syncAcrossDevices = Keybind.getSyncAcrossDevices();
	const disableBuiltinKeybinds = Keybind.getDisableBuiltinKeybinds();
	const labelByAction = getCustomKeybindActionLabelMap(i18n, Keybind.getDefaults());
	const normalized = normalizeQuery(searchQuery);
	const filteredKeybinds = useMemo(() => {
		if (!normalized) return customKeybinds;
		return customKeybinds.filter((entry) => {
			const label = entry.action ? (labelByAction.get(entry.action) ?? entry.action) : '';
			if (label.toLowerCase().includes(normalized)) return true;
			return comboMatchesQuery(entry.combo, normalized);
		});
	}, [customKeybinds, labelByAction, normalized]);
	const conflictLabels = useMemo(() => {
		const result = new Map<string, string>();
		for (let i = 0; i < customKeybinds.length; i++) {
			const target = customKeybinds[i];
			if (!target.enabled || !target.combo.key) continue;
			for (let j = 0; j < customKeybinds.length; j++) {
				if (i === j) continue;
				const other = customKeybinds[j];
				if (!other.enabled || !other.combo.key) continue;
				if (combosLooseEqual(target.combo, other.combo)) {
					const otherLabel = other.action
						? (labelByAction.get(other.action) ?? other.action)
						: i18n._(UNASSIGNED_2_DESCRIPTOR);
					result.set(target.id, otherLabel);
					break;
				}
			}
		}
		return result;
	}, [customKeybinds, labelByAction, i18n.locale]);
	const handleAdd = () => {
		Keybind.addCustomKeybind();
	};
	const handleResetAll = () => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(RESET_ALL_SHORTCUTS_DESCRIPTOR)}
					description={i18n._(THIS_REMOVES_EVERY_CUSTOM_SHORTCUT_AND_RE_ENABLES_DESCRIPTOR)}
					primaryText={i18n._(RESET_TO_DEFAULTS_DESCRIPTOR)}
					primaryVariant="danger"
					onPrimary={() => {
						Keybind.resetCustomKeybinds();
						Keybind.setDisableBuiltinKeybinds(false);
					}}
					data-flx="user.keybinds-tab.handle-reset-all.confirm-modal"
				/>
			)),
		);
	};
	const hasAnyOverrides = customKeybinds.length > 0 || disableBuiltinKeybinds;
	return (
		<div className={styles.customSection} data-flx="user.keybinds-tab.custom-keybinds-list.custom-section">
			<div className={styles.customHeader} data-flx="user.keybinds-tab.custom-keybinds-list.custom-header">
				<div className={styles.customHeaderText} data-flx="user.keybinds-tab.custom-keybinds-list.custom-header-text">
					<h3 className={styles.customTitle} data-flx="user.keybinds-tab.custom-keybinds-list.custom-title">
						{i18n._(CUSTOM_SHORTCUTS_DESCRIPTOR)}
					</h3>
					<p className={styles.customSubtitle} data-flx="user.keybinds-tab.custom-keybinds-list.custom-subtitle">
						{i18n._(RECORDING_PAUSES_SHORTCUTS_DESCRIPTOR)}
					</p>
				</div>
				<div
					className={styles.customHeaderActions}
					data-flx="user.keybinds-tab.custom-keybinds-list.custom-header-actions"
				>
					{hasAnyOverrides ? (
						<Button
							variant="secondary"
							small
							type="button"
							onClick={handleResetAll}
							leftIcon={<RetryIcon size={14} data-flx="user.keybinds-tab.custom-keybinds-list.retry-icon" />}
							data-flx="user.keybinds-tab.custom-keybinds-list.button.reset-all"
						>
							<Trans>Reset to defaults</Trans>
						</Button>
					) : null}
					<Button
						variant="primary"
						small
						type="button"
						onClick={handleAdd}
						leftIcon={
							<PlusIcon
								size={remFromPx(14)}
								weight="bold"
								data-flx="user.keybinds-tab.custom-keybinds-list.plus-icon"
							/>
						}
						data-flx="user.keybinds-tab.custom-keybinds-list.button.add"
					>
						<Trans>Add shortcut</Trans>
					</Button>
				</div>
			</div>
			<div className={styles.shortcutOptions} data-flx="user.keybinds-tab.custom-keybinds-list.shortcut-options">
				<Switch
					className={styles.shortcutOption}
					label={i18n._(DISABLE_BUILT_IN_SHORTCUTS_DESCRIPTOR)}
					value={disableBuiltinKeybinds}
					onChange={(value) => Keybind.setDisableBuiltinKeybinds(value)}
					compact
					data-flx="user.keybinds-tab.custom-keybinds-list.switch.set-disable-builtin-keybinds"
				/>
				<Switch
					className={styles.shortcutOption}
					label={i18n._(SYNC_SHORTCUTS_ACROSS_DEVICES_DESCRIPTOR)}
					value={syncAcrossDevices}
					onChange={(value) => Keybind.setSyncAcrossDevices(value)}
					compact
					data-flx="user.keybinds-tab.custom-keybinds-list.switch.set-sync-across-devices"
				/>
			</div>
			<div className={styles.customList} data-flx="user.keybinds-tab.custom-keybinds-list.custom-list">
				{customKeybinds.length === 0 ? (
					<div className={styles.emptyState} data-flx="user.keybinds-tab.custom-keybinds-list.empty-state">
						<Trans>No custom shortcuts yet.</Trans>
					</div>
				) : filteredKeybinds.length === 0 ? (
					<div className={styles.emptyState} data-flx="user.keybinds-tab.custom-keybinds-list.empty-state--2">
						<Trans>No custom shortcuts match that search.</Trans>
					</div>
				) : (
					filteredKeybinds.map((entry) => (
						<CustomKeybindRow
							key={entry.id}
							entry={entry}
							conflictLabel={conflictLabels.get(entry.id) ?? null}
							onRemove={() => Keybind.removeCustomKeybind(entry.id)}
							data-flx="user.keybinds-tab.custom-keybinds-list.custom-keybind-row"
						/>
					))
				)}
			</div>
		</div>
	);
});
