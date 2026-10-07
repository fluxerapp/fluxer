// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import Guilds from '@app/features/guild/state/Guilds';
import {CHANGE_NICKNAME_DESCRIPTOR, ROLES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as GuildMemberCommands from '@app/features/member/commands/GuildMemberCommands';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {useRoleHierarchy} from '@app/features/permissions/hooks/useRoleHierarchy';
import Permission from '@app/features/permissions/state/Permission';
import * as ColorUtils from '@app/features/theme/utils/ColorUtils';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import {ChangeNicknameIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import itemStyles from '@app/features/ui/action_menu/items/MenuItems.module.css';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import {openRoleContextMenu} from '@app/features/ui/action_menu/RoleContextMenu';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {ChangeNicknameModal} from '@app/features/user/components/modals/ChangeNicknameModal';
import type {User} from '@app/features/user/models/User';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

interface ManageRolesMenuItemProps {
	guildId: string;
	member: GuildMember;
}

export const ManageRolesMenuItem: React.FC<ManageRolesMenuItemProps> = observer(function ManageRolesMenuItem({
	guildId,
	member,
}) {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(guildId);
	const currentMember = GuildMembers.getMember(guildId, member.user.id);
	const {canManageRole} = useRoleHierarchy(guild);
	const canManageRoles = Permission.can(Permissions.MANAGE_ROLES, {guildId});
	const allRoles = useMemo(() => {
		if (!guild) return [];
		return Object.values(guild.roles)
			.filter((role) => !role.isEveryone)
			.sort((a, b) => b.position - a.position)
			.map((role) => ({
				role,
				canManage: canManageRole({id: role.id, position: role.position, permissions: role.permissions}),
			}));
	}, [guild, canManageRole]);
	const visibleRoles = useMemo(() => {
		if (canManageRoles) return allRoles;
		const memberRoles = currentMember?.roles;
		if (!memberRoles) return [];
		return allRoles.filter(({role}) => memberRoles.has(role.id));
	}, [allRoles, canManageRoles, currentMember]);
	const handleToggleRole = useCallback(
		async (roleId: string, hasRole: boolean, canToggle: boolean) => {
			if (!canToggle) return;
			if (hasRole) {
				await GuildMemberCommands.removeRole(guildId, member.user.id, roleId);
			} else {
				await GuildMemberCommands.addRole(guildId, member.user.id, roleId);
			}
		},
		[guildId, member.user.id],
	);
	if (visibleRoles.length === 0) return null;
	return (
		<MenuItemSubmenu
			label={i18n._(ROLES_DESCRIPTOR)}
			render={() => (
				<MenuGroup data-flx="ui.action-menu.items.guild-member-menu-items.manage-roles-menu-item.menu-group">
					{canManageRoles
						? visibleRoles.map(({role, canManage}) => {
								const hasRole = currentMember?.roles.has(role.id) ?? false;
								const canToggle = canManageRoles && canManage;
								return (
									<CheckboxItem
										key={role.id}
										checked={hasRole}
										disabled={!canToggle}
										onCheckedChange={() => handleToggleRole(role.id, hasRole, canToggle)}
										closeOnChange={false}
										data-flx="ui.action-menu.items.guild-member-menu-items.manage-roles-menu-item.checkbox-item"
									>
										{/* biome-ignore lint/a11y/noStaticElementInteractions: context-menu affordance inside a CheckboxItem. */}
										<div
											className={itemStyles.roleContainer}
											onContextMenu={(event) => openRoleContextMenu(event, role.id)}
											data-flx="ui.action-menu.items.guild-member-menu-items.manage-roles-menu-item.role-container.open-role-context-menu"
										>
											<div
												className={itemStyles.roleIcon}
												style={{backgroundColor: ColorUtils.int2rgb(role.color)}}
												data-flx="ui.action-menu.items.guild-member-menu-items.manage-roles-menu-item.div"
											/>
											<span
												className={!canToggle ? itemStyles.roleDisabled : undefined}
												data-flx="ui.action-menu.items.guild-member-menu-items.manage-roles-menu-item.span"
											>
												{role.name}
											</span>
										</div>
									</CheckboxItem>
								);
							})
						: visibleRoles.map(({role}) => (
								<MenuItem
									key={role.id}
									closeOnSelect={false}
									data-flx="ui.action-menu.items.guild-member-menu-items.manage-roles-menu-item.menu-item"
								>
									<div
										role="group"
										className={itemStyles.readonlyRoleItem}
										onContextMenu={(event) => openRoleContextMenu(event, role.id)}
										data-flx="ui.action-menu.items.guild-member-menu-items.manage-roles-menu-item.group.open-role-context-menu--2"
									>
										<div
											className={itemStyles.roleContainer}
											data-flx="ui.action-menu.items.guild-member-menu-items.manage-roles-menu-item.div--2"
										>
											<div
												className={itemStyles.roleIcon}
												style={{backgroundColor: ColorUtils.int2rgb(role.color)}}
												data-flx="ui.action-menu.items.guild-member-menu-items.manage-roles-menu-item.div--3"
											/>
											<span
												className={itemStyles.roleName}
												data-flx="ui.action-menu.items.guild-member-menu-items.manage-roles-menu-item.span--2"
											>
												{role.name}
											</span>
										</div>
										<div
											className={itemStyles.readonlyRoleSpacer}
											data-flx="ui.action-menu.items.guild-member-menu-items.manage-roles-menu-item.div--4"
										/>
									</div>
								</MenuItem>
							))}
				</MenuGroup>
			)}
			data-flx="ui.action-menu.items.guild-member-menu-items.manage-roles-menu-item.menu-item-submenu"
		/>
	);
});

interface ChangeNicknameMenuItemProps {
	guildId: string;
	user: User;
	member: GuildMember;
	onClose: () => void;
}

export const ChangeNicknameMenuItem: React.FC<ChangeNicknameMenuItemProps> = observer(function ChangeNicknameMenuItem({
	guildId,
	user,
	member,
	onClose,
}) {
	const {i18n} = useLingui();
	const currentUserId = Authentication.currentUserId;
	const isCurrentUser = user.id === currentUserId;
	const guild = Guilds.getGuild(guildId);
	const {canManageTarget} = useRoleHierarchy(guild);
	const hasChangeNicknamePermission = Permission.can(Permissions.CHANGE_NICKNAME, {guildId});
	const hasManageNicknamesPermission = Permission.can(Permissions.MANAGE_NICKNAMES, {guildId});
	const canManageNicknames =
		(isCurrentUser && hasChangeNicknamePermission && !member.isTimedOut()) ||
		(hasManageNicknamesPermission && canManageTarget(user.id));
	const handleChangeNickname = useCallback(() => {
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<ChangeNicknameModal
					guildId={guildId}
					user={user}
					member={member}
					data-flx="ui.action-menu.items.guild-member-menu-items.handle-change-nickname.change-nickname-modal"
				/>
			)),
		);
	}, [guildId, user, member, onClose]);
	if (!canManageNicknames) return null;
	return (
		<MenuItem
			icon={
				<ChangeNicknameIcon
					size={16}
					data-flx="ui.action-menu.items.guild-member-menu-items.change-nickname-menu-item.change-nickname-icon"
				/>
			}
			onClick={handleChangeNickname}
			data-flx="ui.action-menu.items.guild-member-menu-items.change-nickname-menu-item.menu-item.change-nickname"
		>
			{isCurrentUser ? i18n._(CHANGE_NICKNAME_DESCRIPTOR) : i18n._(CHANGE_NICKNAME_DESCRIPTOR)}
		</MenuItem>
	);
});
