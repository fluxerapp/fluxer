// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PermissionOverwrite} from '@app/features/channel/components/modals/channel_tabs/channel_permissions_tab/shared';
import {DeleteIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {CopyRoleIdMenuItem, CopyUserIdMenuItem} from '@app/features/ui/action_menu/items/CopyMenuItems';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import type {User} from '@app/features/user/models/User';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const DELETE_ROLE_DESCRIPTOR = msg({
	message: 'Delete role',
	comment:
		'Danger action in the channel permissions overwrite context menu that removes a role permission override from the channel.',
});
const DELETE_USER_DESCRIPTOR = msg({
	message: 'Delete user',
	comment:
		'Danger action in the channel permissions overwrite context menu that removes a member permission override from the channel.',
});

interface OverwriteContextMenuProps {
	overwrite: PermissionOverwrite;
	roleId: string | null;
	user: User | null;
	canDelete: boolean;
	onDelete: () => void;
	onClose: () => void;
}

export const OverwriteContextMenu: React.FC<OverwriteContextMenuProps> = observer(
	({overwrite, roleId, user, canDelete, onDelete, onClose}) => {
		const {i18n} = useLingui();
		return (
			<>
				<MenuGroup data-flx="channel.channel-tabs.channel-permissions-tab.overwrite-context-menu.menu-group">
					{overwrite.type === 0 && roleId ? (
						<CopyRoleIdMenuItem
							roleId={roleId}
							onClose={onClose}
							data-flx="channel.channel-tabs.channel-permissions-tab.overwrite-context-menu.copy-role-id-menu-item"
						/>
					) : user ? (
						<CopyUserIdMenuItem
							user={user}
							onClose={onClose}
							data-flx="channel.channel-tabs.channel-permissions-tab.overwrite-context-menu.copy-user-id-menu-item"
						/>
					) : null}
				</MenuGroup>
				{canDelete && (
					<MenuGroup data-flx="channel.channel-tabs.channel-permissions-tab.overwrite-context-menu.menu-group--2">
						<MenuItem
							icon={
								<DeleteIcon data-flx="channel.channel-tabs.channel-permissions-tab.overwrite-context-menu.delete-icon" />
							}
							danger
							onClick={onDelete}
							data-flx="channel.channel-tabs.channel-permissions-tab.overwrite-context-menu.menu-item.delete"
						>
							{overwrite.type === 0 ? i18n._(DELETE_ROLE_DESCRIPTOR) : i18n._(DELETE_USER_DESCRIPTOR)}
						</MenuItem>
					</MenuGroup>
				)}
			</>
		);
	},
);
