// SPDX-License-Identifier: AGPL-3.0-or-later

import {CopyIcon, DeleteIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {CopyRoleIdMenuItem} from '@app/features/ui/action_menu/items/CopyMenuItems';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';

const DUPLICATE_ROLE_DESCRIPTOR = msg({
	message: 'Duplicate role',
	comment:
		'Context menu action in the community roles settings tab that creates a new role copying the settings of the one right-clicked. Keep it short.',
});
const DELETE_ROLE_DESCRIPTOR = msg({
	message: 'Delete role',
	comment:
		'Destructive context menu action in the community roles settings tab that immediately removes the right-clicked role from every member. Keep it short.',
});

export interface RoleContextMenuActions {
	canDuplicate?: boolean;
	onDuplicate?: () => void;
	canDelete?: boolean;
	onDelete?: () => void;
}

interface RoleContextMenuProps extends RoleContextMenuActions {
	roleId: string;
	onClose: () => void;
}

export const RoleContextMenu: React.FC<RoleContextMenuProps> = ({
	roleId,
	onClose,
	canDuplicate,
	onDuplicate,
	canDelete,
	onDelete,
}) => {
	const {i18n} = useLingui();
	return (
		<>
			<MenuGroup data-flx="ui.action-menu.role-context-menu.menu-group">
				<CopyRoleIdMenuItem
					roleId={roleId}
					onClose={onClose}
					data-flx="ui.action-menu.role-context-menu.copy-role-id-menu-item"
				/>
				{canDuplicate && onDuplicate && (
					<MenuItem
						icon={<CopyIcon data-flx="ui.action-menu.role-context-menu.duplicate-role-menu-item.copy-icon" />}
						onClick={() => {
							onClose();
							onDuplicate();
						}}
						data-flx="ui.action-menu.role-context-menu.duplicate-role-menu-item"
					>
						{i18n._(DUPLICATE_ROLE_DESCRIPTOR)}
					</MenuItem>
				)}
			</MenuGroup>
			{canDelete && onDelete && (
				<MenuGroup data-flx="ui.action-menu.role-context-menu.menu-group--danger">
					<MenuItem
						danger
						icon={<DeleteIcon data-flx="ui.action-menu.role-context-menu.delete-role-menu-item.delete-icon" />}
						onClick={() => {
							onClose();
							onDelete();
						}}
						data-flx="ui.action-menu.role-context-menu.delete-role-menu-item"
					>
						{i18n._(DELETE_ROLE_DESCRIPTOR)}
					</MenuItem>
				</MenuGroup>
			)}
		</>
	);
};

export function openRoleContextMenu(
	event: React.MouseEvent | MouseEvent,
	roleId: string,
	actions?: RoleContextMenuActions,
): void {
	ContextMenuCommands.openFromEvent(event, ({onClose}) => (
		<RoleContextMenu
			roleId={roleId}
			onClose={onClose}
			{...actions}
			data-flx="ui.action-menu.role-context-menu.open-role-context-menu.role-context-menu"
		/>
	));
}

export function openRoleContextMenuForElement(element: HTMLElement, roleId: string): void {
	ContextMenuCommands.openForElement(element, ({onClose}) => (
		<RoleContextMenu
			roleId={roleId}
			onClose={onClose}
			data-flx="ui.action-menu.role-context-menu.open-role-context-menu-for-element.role-context-menu"
		/>
	));
}
