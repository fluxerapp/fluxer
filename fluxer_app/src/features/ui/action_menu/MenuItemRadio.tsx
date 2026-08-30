// SPDX-License-Identifier: AGPL-3.0-or-later

import {SelectableMenuItem, useContextMenuClose} from '@app/features/ui/action_menu/ContextMenu';
import styles from '@app/features/ui/action_menu/ContextMenu.module.css';
import radioStyles from '@app/features/ui/action_menu/MenuItemRadio.module.css';
import React, {useCallback} from 'react';

interface MenuItemRadioProps {
	label?: string;
	children?: React.ReactNode;
	icon?: React.ReactNode;
	selected: boolean;
	disabled?: boolean;
	onSelect?: () => void;
	closeOnSelect?: boolean;
}

const textValueFromNode = (node: React.ReactNode): string => {
	if (typeof node === 'string') return node;
	if (typeof node === 'number') return String(node);
	if (Array.isArray(node)) return node.map(textValueFromNode).join('');
	if (React.isValidElement(node)) {
		return textValueFromNode((node.props as {children?: React.ReactNode}).children);
	}
	return '';
};
export const MenuItemRadio = React.forwardRef<HTMLDivElement, MenuItemRadioProps>(
	({label, children, icon, selected, disabled = false, onSelect, closeOnSelect = false}, forwardedRef) => {
		const closeMenu = useContextMenuClose();
		const handleAction = useCallback(() => {
			if (disabled) return;
			onSelect?.();
			if (closeOnSelect) {
				closeMenu();
			}
		}, [closeMenu, closeOnSelect, disabled, onSelect]);
		const visibleLabel = label ?? children;
		return (
			<SelectableMenuItem
				ref={forwardedRef}
				selectionMode="single"
				selected={selected}
				onAction={handleAction}
				isDisabled={disabled}
				shouldCloseOnSelect={closeOnSelect}
				className={`${styles.item} ${styles.checkboxItem} ${disabled ? styles.disabled : ''}`.trim()}
				textValue={label ?? textValueFromNode(children)}
				data-flx="ui.action-menu.menu-item-radio.selectable-menu-item"
			>
				<div className={styles.itemLabel} data-flx="ui.action-menu.menu-item-radio.item-label">
					<div className={radioStyles.labelContent} data-flx="ui.action-menu.menu-item-radio.div">
						{icon && (
							<span className={radioStyles.icon} data-flx="ui.action-menu.menu-item-radio.span">
								{icon}
							</span>
						)}
						<span className={radioStyles.labelText} data-flx="ui.action-menu.menu-item-radio.span--2">
							{visibleLabel}
						</span>
					</div>
				</div>
				<div
					className={styles.checkboxIndicator}
					aria-hidden="true"
					data-flx="ui.action-menu.menu-item-radio.checkbox-indicator"
				>
					<div
						className={`${radioStyles.radioButton} ${selected ? radioStyles.radioButtonSelected : radioStyles.radioButtonUnselected}`}
						data-flx="ui.action-menu.menu-item-radio.div"
					>
						{selected && (
							<div className={radioStyles.radioIndicator} data-flx="ui.action-menu.menu-item-radio.div--2" />
						)}
					</div>
				</div>
			</SelectableMenuItem>
		);
	},
);

MenuItemRadio.displayName = 'MenuItemRadio';
