// SPDX-License-Identifier: AGPL-3.0-or-later

import {getRoleSwatchColor} from '@app/features/app/components/dialogs/shared/PermissionComponents';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import itemStyles from '@app/features/ui/action_menu/items/MenuItems.module.css';
import type {MenuCheckboxType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {clsx} from 'clsx';
import type React from 'react';

interface RoleMenuItemLabelProps {
	readonly role: GuildRole;
	readonly disabled?: boolean;
	readonly onContextMenu?: (event: React.MouseEvent) => void;
	readonly 'data-flx'?: string;
}

export function RoleMenuItemLabel({
	role,
	disabled = false,
	onContextMenu,
	'data-flx': dataFlx,
}: RoleMenuItemLabelProps): React.ReactElement {
	return (
		// biome-ignore lint/a11y/noStaticElementInteractions: context-menu affordance inside a menu item.
		<div
			className={itemStyles.roleContainer}
			onContextMenu={onContextMenu}
			data-flx={dataFlx ?? 'ui.action-menu.items.role-menu-items.role-menu-item-label.role-container'}
		>
			<div
				className={itemStyles.roleIcon}
				style={{backgroundColor: getRoleSwatchColor(role)}}
				data-flx="ui.action-menu.items.role-menu-items.role-menu-item-label.swatch"
			/>
			<span
				className={clsx(itemStyles.roleName, disabled && itemStyles.roleDisabled)}
				data-flx="ui.action-menu.items.role-menu-items.role-menu-item-label.name"
			>
				{role.name}
			</span>
		</div>
	);
}

interface RoleCheckboxMenuItemProps {
	readonly role: GuildRole;
	readonly checked: boolean;
	readonly disabled?: boolean;
	readonly onToggle: () => void;
	readonly onRoleContextMenu?: (event: React.MouseEvent) => void;
	readonly 'data-flx'?: string;
}

export function RoleCheckboxMenuItem({
	role,
	checked,
	disabled = false,
	onToggle,
	onRoleContextMenu,
	'data-flx': dataFlx,
}: RoleCheckboxMenuItemProps): React.ReactElement {
	return (
		<CheckboxItem
			checked={checked}
			disabled={disabled}
			closeOnChange={false}
			onCheckedChange={onToggle}
			data-flx={dataFlx ?? 'ui.action-menu.items.role-menu-items.role-checkbox-menu-item.checkbox-item'}
		>
			<RoleMenuItemLabel
				role={role}
				disabled={disabled}
				onContextMenu={onRoleContextMenu}
				data-flx="ui.action-menu.items.role-menu-items.role-checkbox-menu-item.role-menu-item-label"
			/>
		</CheckboxItem>
	);
}

export function roleCheckboxSheetItem({
	role,
	checked,
	disabled,
	onChange,
}: {
	readonly role: GuildRole;
	readonly checked: boolean;
	readonly disabled?: boolean;
	readonly onChange: (checked: boolean) => void;
}): MenuCheckboxType {
	return {
		label: role.name,
		checked,
		disabled,
		onChange,
		icon: (
			<div
				className={itemStyles.roleIcon}
				style={{backgroundColor: getRoleSwatchColor(role)}}
				data-flx="ui.action-menu.items.role-menu-items.role-checkbox-sheet-item.swatch"
			/>
		),
	};
}
