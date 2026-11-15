// SPDX-License-Identifier: AGPL-3.0-or-later

import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {isApplePlatform as isMacPlatform} from '@app/features/platform/utils/PlatformHelpers';
import styles from '@app/features/user/components/modals/tabs/components/SelectableSettingsList.module.css';
import {CheckIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

export type SelectionToggleHandler = (id: string, index: number, shiftKey: boolean) => void;

interface SelectionCheckboxProps extends React.HTMLAttributes<HTMLDivElement> {
	checked: boolean;
}

interface SelectableItemOptions {
	enabled: boolean;
	id: string;
	index?: number;
	selected?: boolean;
	onSelect?: SelectionToggleHandler;
}

interface SelectableSettingsListOptions<TItem> {
	items: ReadonlyArray<TItem>;
	getId: (item: TItem) => string;
}

interface SelectableSettingsListState<TItem> {
	selectedIds: Set<string>;
	selectedIdList: Array<string>;
	selectedItems: Array<TItem>;
	itemIds: Array<string>;
	selectionMode: boolean;
	selectAllShortcutLabel: string;
	clearSelection: () => void;
	toggleSelection: SelectionToggleHandler;
}

export const SelectionCheckbox = ({checked, className, ...props}: SelectionCheckboxProps) => (
	<div
		className={clsx(styles.checkbox, checked ? styles.checkboxChecked : styles.checkboxUnchecked, className)}
		data-flx="user.selectable-settings-list.selection-checkbox.checkbox"
		{...props}
	>
		{checked && (
			<CheckIcon
				weight="regular"
				className={styles.checkIcon}
				data-flx="user.selectable-settings-list.selection-checkbox.check-icon"
			/>
		)}
	</div>
);

function getSelectAllShortcutLabel(): string {
	return isMacPlatform() ? 'Cmd+A' : 'Ctrl+A';
}

function isEditableTarget(target: EventTarget | null): boolean {
	if (!(target instanceof HTMLElement)) return false;
	if (target.isContentEditable) return true;
	return (
		target instanceof HTMLInputElement || target instanceof HTMLTextAreaElement || target instanceof HTMLSelectElement
	);
}

function isSelectAllShortcut(event: KeyboardEvent): boolean {
	if (event.key.toLowerCase() !== 'a' || event.altKey || event.shiftKey) return false;
	return isMacPlatform() ? event.metaKey && !event.ctrlKey : event.ctrlKey && !event.metaKey;
}

export function getSelectableItemProps({
	enabled,
	id,
	index,
	selected,
	onSelect,
}: SelectableItemOptions): React.HTMLAttributes<HTMLElement> {
	if (!enabled || !onSelect || index === undefined) {
		return {};
	}
	return {
		onClick: (event) => onSelect(id, index, event.shiftKey),
		onKeyDown: (event) => {
			if (!isKeyboardActivationKey(event.key)) return;
			event.preventDefault();
			onSelect(id, index, event.shiftKey);
		},
		role: 'checkbox',
		'aria-checked': Boolean(selected),
		tabIndex: 0,
	};
}

export function selectOnShiftClick(
	event: React.MouseEvent<HTMLElement>,
	id: string,
	index: number | undefined,
	onSelect: SelectionToggleHandler | undefined,
): boolean {
	if (!event.shiftKey || !onSelect || index === undefined) {
		return false;
	}
	event.preventDefault();
	onSelect(id, index, false);
	return true;
}

export function selectOnShiftActivation(
	event: React.KeyboardEvent<HTMLElement>,
	id: string,
	index: number | undefined,
	onSelect: SelectionToggleHandler | undefined,
): boolean {
	if (!event.shiftKey || !isKeyboardActivationKey(event.key) || !onSelect || index === undefined) {
		return false;
	}
	event.preventDefault();
	onSelect(id, index, false);
	return true;
}

export function useSelectableSettingsList<TItem>({
	items,
	getId,
}: SelectableSettingsListOptions<TItem>): SelectableSettingsListState<TItem> {
	const [selectedIds, setSelectedIds] = useState<Set<string>>(new Set());
	const [lastToggledIndex, setLastToggledIndex] = useState(-1);
	const itemIds = useMemo(() => items.map(getId), [getId, items]);
	const selectedIdList = useMemo(() => Array.from(selectedIds), [selectedIds]);
	const selectedItems = useMemo(
		() => items.filter((item) => selectedIds.has(getId(item))),
		[getId, items, selectedIds],
	);
	const selectAllShortcutLabel = useMemo(() => getSelectAllShortcutLabel(), []);
	const clearSelection = useCallback(() => {
		setSelectedIds(new Set());
		setLastToggledIndex(-1);
	}, []);
	const toggleSelection = useCallback<SelectionToggleHandler>(
		(id, index, isShiftSelect) => {
			setSelectedIds((currentSelection) => {
				const nextSelection = new Set(currentSelection);
				if (isShiftSelect && lastToggledIndex !== -1) {
					const start = Math.min(lastToggledIndex, index);
					const end = Math.max(lastToggledIndex, index);
					const shouldAdd = !currentSelection.has(id);
					itemIds.slice(start, end + 1).forEach((itemId) => {
						if (shouldAdd) {
							nextSelection.add(itemId);
						} else {
							nextSelection.delete(itemId);
						}
					});
				} else if (currentSelection.has(id)) {
					nextSelection.delete(id);
				} else {
					nextSelection.add(id);
				}
				return nextSelection;
			});
			setLastToggledIndex(index);
		},
		[itemIds, lastToggledIndex],
	);
	useEffect(() => {
		const availableIds = new Set(itemIds);
		setSelectedIds((currentSelection) => {
			if ([...currentSelection].every((id) => availableIds.has(id))) {
				return currentSelection;
			}
			const nextSelection = new Set<string>();
			currentSelection.forEach((id) => {
				if (availableIds.has(id)) nextSelection.add(id);
			});
			return nextSelection;
		});
	}, [itemIds]);
	useEffect(() => {
		if (selectedIds.size > 0) return;
		setLastToggledIndex(-1);
	}, [selectedIds.size]);
	useEffect(() => {
		const handleKeyDown = (event: KeyboardEvent) => {
			if (itemIds.length === 0 || !isSelectAllShortcut(event) || isEditableTarget(event.target)) return;
			event.preventDefault();
			setSelectedIds(new Set(itemIds));
			setLastToggledIndex(itemIds.length - 1);
		};
		window.addEventListener('keydown', handleKeyDown);
		return () => window.removeEventListener('keydown', handleKeyDown);
	}, [itemIds]);
	return {
		selectedIds,
		selectedIdList,
		selectedItems,
		itemIds,
		selectionMode: selectedIds.size > 0,
		selectAllShortcutLabel,
		clearSelection,
		toggleSelection,
	};
}
