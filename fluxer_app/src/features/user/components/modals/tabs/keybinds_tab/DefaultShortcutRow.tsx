// SPDX-License-Identifier: AGPL-3.0-or-later

import GlobalShortcuts from '@app/features/input/state/GlobalShortcuts';
import Keybind, {type KeybindCommand} from '@app/features/input/state/InputKeybind';
import {isBuiltinDisableMarker} from '@app/features/input/state/KeybindResolution';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import {
	EditSimpleIcon,
	HideIcon,
	MoreOptionsVerticalIcon,
	RetryIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import styles from '@app/features/user/components/modals/tabs/KeybindsTab.module.css';
import {getCustomKeybindActionLabel} from '@app/features/user/components/modals/tabs/keybinds_tab/AssignableActionOptions';
import {DefaultShortcutChipList} from '@app/features/user/components/modals/tabs/keybinds_tab/DefaultShortcutChipList';
import {SystemShortcutRowHint} from '@app/features/user/components/modals/tabs/keybinds_tab/SystemShortcutRowHint';
import {
	chipsForDefaultEntry,
	getRowActions,
	isShortcutMergePair,
	type ShortcutRowModel,
} from '@app/features/user/components/modals/tabs/keybinds_tab/shared';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useCallback} from 'react';

const SET_CUSTOM_SHORTCUT_DESCRIPTOR = msg({
	message: 'Set custom shortcut',
	comment: 'Short label in the keybinds tab. Keep it concise.',
});
const DISABLE_BUILT_IN_SHORTCUT_DESCRIPTOR = msg({
	message: 'Disable built-in shortcut',
	comment: 'Button or menu action label in the keybinds tab. Keep it concise.',
});
const RESET_TO_BUILT_IN_DESCRIPTOR = msg({
	message: 'Reset to built-in',
	comment: 'Button or menu action label in the keybinds tab. Keep it concise.',
});
const ADD_ANOTHER_CUSTOM_SHORTCUT_DESCRIPTOR = msg({
	message: 'Add another custom shortcut',
	comment: 'Button or menu action label in the keybinds tab. Keep it concise.',
});
const RESET_TO_BUILT_IN_2_DESCRIPTOR = msg({
	message: 'Reset "{label}" to built-in',
	comment:
		'Button or menu action label in the keybinds tab. Keep it concise. Preserve {label}; it is inserted by code.',
});
const RE_ENABLE_BUILT_IN_SHORTCUT_DESCRIPTOR = msg({
	message: 'Re-enable built-in shortcut',
	comment:
		'Button or menu action label in the keybinds tab. Turns a disabled built-in shortcut back on. Keep it concise.',
});
const SET_CUSTOM_SHORTCUT_FOR_DESCRIPTOR = msg({
	message: 'Set custom shortcut for "{label}"',
	comment: 'Label in the keybinds tab. Preserve {label}; it is inserted by code.',
});
const MORE_SHORTCUT_OPTIONS_DESCRIPTOR = msg({
	message: 'More shortcut options',
	comment: 'Short label in the keybinds tab. Keep it concise.',
});
const GLOBAL_SHORTCUT_DESCRIPTOR = msg({
	message: 'Global shortcut',
	comment: 'Toggle in the keybinds tab. When on, the shortcut works even when Fluxer is not focused. Keep it concise.',
});
export const DefaultShortcutRow: React.FC<{
	row: ShortcutRowModel;
	overriddenActions: ReadonlySet<KeybindCommand>;
	disabledActions: ReadonlySet<KeybindCommand>;
}> = ({row, overriddenActions, disabledActions}) => {
	const {i18n} = useLingui();
	const actions = getRowActions(row);
	const openRowMenu = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			ContextMenuCommands.openFromEvent(event, ({onClose}) => {
				const perAction = actions.map((action) => ({
					action,
					label: getCustomKeybindActionLabel(i18n, action.action, action.label),
					hasCustomBinding: Keybind.getCustomKeybinds().some(
						(entry) => entry.action === action.action && !isBuiltinDisableMarker(entry),
					),
					builtinDisabled: Keybind.isBuiltinDisabled(action.action),
				}));
				const noneHaveCustom = perAction.every((p) => !p.hasCustomBinding);
				const anyBuiltinDisabled = perAction.some((p) => p.builtinDisabled);
				const allBuiltinDisabled = perAction.every((p) => p.builtinDisabled);
				const globalAction =
					actions.length === 1 &&
					!anyBuiltinDisabled &&
					!GlobalShortcuts.isPortalBackend &&
					Keybind.isActionGlobalCapable(actions[0].action)
						? actions[0].action
						: null;
				const reEnableGroup = anyBuiltinDisabled ? (
					<MenuGroup data-flx="user.keybinds-tab.open-row-menu.re-enable-menu-group">
						<MenuItem
							icon={<RetryIcon size={16} data-flx="user.keybinds-tab.open-row-menu.retry-icon--3" />}
							onClick={() => {
								onClose();
								for (const {action} of perAction) {
									Keybind.enableBuiltinForAction(action.action);
								}
							}}
							data-flx="user.keybinds-tab.open-row-menu.menu-item.re-enable-built-in"
						>
							{i18n._(RE_ENABLE_BUILT_IN_SHORTCUT_DESCRIPTOR)}
						</MenuItem>
					</MenuGroup>
				) : null;
				const globalGroup = globalAction ? (
					<MenuGroup data-flx="user.keybinds-tab.open-row-menu.global-menu-group">
						<CheckboxItem
							checked={Keybind.isActionGlobal(globalAction)}
							onCheckedChange={(value) => Keybind.setActionGlobal(globalAction, value)}
							closeOnChange
							data-flx="user.keybinds-tab.open-row-menu.checkbox-item.global"
						>
							{i18n._(GLOBAL_SHORTCUT_DESCRIPTOR)}
						</CheckboxItem>
					</MenuGroup>
				) : null;
				const renderBody = () => {
					if (noneHaveCustom) {
						return (
							<MenuGroup data-flx="user.keybinds-tab.open-row-menu.menu-group--2">
								<MenuItem
									icon={<EditSimpleIcon size={16} data-flx="user.keybinds-tab.open-row-menu.edit-simple-icon--2" />}
									onClick={() => {
										onClose();
										for (const {action} of perAction) {
											Keybind.addCustomKeybindForAction(action.action);
										}
									}}
									data-flx="user.keybinds-tab.open-row-menu.menu-item.close--3"
								>
									{i18n._(SET_CUSTOM_SHORTCUT_DESCRIPTOR)}
								</MenuItem>
								{allBuiltinDisabled ? null : (
									<MenuItem
										icon={<HideIcon size={16} data-flx="user.keybinds-tab.open-row-menu.hide-icon--2" />}
										onClick={() => {
											onClose();
											for (const {action} of perAction) {
												Keybind.disableBuiltinForAction(action.action);
											}
										}}
										data-flx="user.keybinds-tab.open-row-menu.menu-item.close--4"
									>
										{i18n._(DISABLE_BUILT_IN_SHORTCUT_DESCRIPTOR)}
									</MenuItem>
								)}
							</MenuGroup>
						);
					}
					if (perAction.length === 1) {
						const {action} = perAction[0];
						return (
							<MenuGroup data-flx="user.keybinds-tab.open-row-menu.menu-group--3">
								<MenuItem
									icon={<RetryIcon size={16} data-flx="user.keybinds-tab.open-row-menu.retry-icon" />}
									danger
									onClick={() => {
										onClose();
										Keybind.removeCustomKeybindsForAction(action.action);
									}}
									data-flx="user.keybinds-tab.open-row-menu.menu-item.close--5"
								>
									{i18n._(RESET_TO_BUILT_IN_DESCRIPTOR)}
								</MenuItem>
								<MenuItem
									icon={<EditSimpleIcon size={16} data-flx="user.keybinds-tab.open-row-menu.edit-simple-icon--3" />}
									onClick={() => {
										onClose();
										Keybind.addCustomKeybindForAction(action.action);
									}}
									data-flx="user.keybinds-tab.open-row-menu.menu-item.close--6"
								>
									{i18n._(ADD_ANOTHER_CUSTOM_SHORTCUT_DESCRIPTOR)}
								</MenuItem>
							</MenuGroup>
						);
					}
					return (
						<>
							{perAction.map(({action, label, hasCustomBinding}) => (
								<MenuGroup key={action.action} data-flx="user.keybinds-tab.open-row-menu.menu-group--4">
									{hasCustomBinding ? (
										<MenuItem
											icon={<RetryIcon size={16} data-flx="user.keybinds-tab.open-row-menu.retry-icon--2" />}
											danger
											onClick={() => {
												onClose();
												Keybind.removeCustomKeybindsForAction(action.action);
											}}
											data-flx="user.keybinds-tab.open-row-menu.menu-item.close--7"
										>
											{i18n._(RESET_TO_BUILT_IN_2_DESCRIPTOR, {label})}
										</MenuItem>
									) : (
										<MenuItem
											icon={<EditSimpleIcon size={16} data-flx="user.keybinds-tab.open-row-menu.edit-simple-icon--4" />}
											onClick={() => {
												onClose();
												Keybind.addCustomKeybindForAction(action.action);
											}}
											data-flx="user.keybinds-tab.open-row-menu.menu-item.close--8"
										>
											{i18n._(SET_CUSTOM_SHORTCUT_FOR_DESCRIPTOR, {label})}
										</MenuItem>
									)}
								</MenuGroup>
							))}
						</>
					);
				};
				return (
					<>
						{globalGroup}
						{reEnableGroup}
						{renderBody()}
					</>
				);
			});
		},
		[actions, i18n],
	);
	if (isShortcutMergePair(row)) {
		const [a, b] = row;
		return (
			<div className={styles.defaultRow} data-flx="user.keybinds-tab.default-shortcut-row.default-row">
				<div className={styles.defaultLabel} data-flx="user.keybinds-tab.default-shortcut-row.default-label">
					{a.label}
				</div>
				<div className={styles.defaultRowActions} data-flx="user.keybinds-tab.default-shortcut-row.default-row-actions">
					<div
						className={styles.defaultChipsMerged}
						data-flx="user.keybinds-tab.default-shortcut-row.default-chips-merged"
					>
						<DefaultShortcutChipList
							chips={chipsForDefaultEntry(i18n, a)}
							overridden={overriddenActions.has(a.action)}
							disabled={disabledActions.has(a.action)}
							data-flx="user.keybinds-tab.default-shortcut-row.default-shortcut-chip-list"
						/>
						<DefaultShortcutChipList
							chips={chipsForDefaultEntry(i18n, b)}
							overridden={overriddenActions.has(b.action)}
							disabled={disabledActions.has(b.action)}
							data-flx="user.keybinds-tab.default-shortcut-row.default-shortcut-chip-list--2"
						/>
					</div>
					<button
						type="button"
						className={styles.dotsButton}
						onClick={openRowMenu}
						aria-label={i18n._(MORE_SHORTCUT_OPTIONS_DESCRIPTOR)}
						data-flx="user.keybinds-tab.default-shortcut-row.dots-button.open-row-menu"
					>
						<MoreOptionsVerticalIcon
							size={18}
							data-flx="user.keybinds-tab.default-shortcut-row.more-options-vertical-icon"
						/>
					</button>
				</div>
			</div>
		);
	}
	const entry = row;
	return (
		<div className={styles.defaultRow} data-flx="user.keybinds-tab.default-shortcut-row.default-row--2">
			<div className={styles.defaultLabel} data-flx="user.keybinds-tab.default-shortcut-row.default-label--2">
				{entry.label}
				<SystemShortcutRowHint
					action={entry.action}
					data-flx="user.keybinds-tab.default-shortcut-row.system-shortcut-row-hint"
				/>
			</div>
			<div
				className={styles.defaultRowActions}
				data-flx="user.keybinds-tab.default-shortcut-row.default-row-actions--2"
			>
				<DefaultShortcutChipList
					chips={chipsForDefaultEntry(i18n, entry)}
					overridden={overriddenActions.has(entry.action)}
					disabled={disabledActions.has(entry.action)}
					data-flx="user.keybinds-tab.default-shortcut-row.default-shortcut-chip-list--3"
				/>
				<button
					type="button"
					className={styles.dotsButton}
					onClick={openRowMenu}
					aria-label={i18n._(MORE_SHORTCUT_OPTIONS_DESCRIPTOR)}
					data-flx="user.keybinds-tab.default-shortcut-row.dots-button.open-row-menu--2"
				>
					<MoreOptionsVerticalIcon
						size={18}
						data-flx="user.keybinds-tab.default-shortcut-row.more-options-vertical-icon--2"
					/>
				</button>
			</div>
		</div>
	);
};
