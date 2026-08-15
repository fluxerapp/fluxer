// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {GuildSettingsModal} from '@app/features/guild/components/modals/GuildSettingsModal';
import styles from '@app/features/guild/components/RoleManagement.module.css';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import GuildSettingsModalState from '@app/features/guild/state/GuildSettingsModal';
import Guilds from '@app/features/guild/state/Guilds';
import * as GuildMemberCommands from '@app/features/member/commands/GuildMemberCommands';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {useRoleHierarchy} from '@app/features/permissions/hooks/useRoleHierarchy';
import * as PermissionUtils from '@app/features/permissions/utils/PermissionUtils';
import * as ColorUtils from '@app/features/theme/utils/ColorUtils';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import itemStyles from '@app/features/ui/action_menu/items/MenuItems.module.css';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {openRoleContextMenu} from '@app/features/ui/action_menu/RoleContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {MenuBottomSheet, type MenuGroupType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import profileStyles from '@app/features/user/components/popouts/UserProfilePopout.module.css';
import {formatGuildSettingsPath} from '@app/features/user/components/settings_utils/GuildSettingsConstants';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {PlusIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useState} from 'react';

const REMOVE_ROLE_DESCRIPTOR = msg({
	message: 'Remove role {roleName}',
	comment:
		'Button or menu action label in the community role management. Keep it concise. Preserve {roleName}; it is inserted by code. Keep the tone plain and specific.',
});
const NO_ROLES_YET_ADD_ROLES_IN_DESCRIPTOR = msg({
	message: 'No roles yet. Add roles in {rolesSettingsPath}',
	comment: 'Empty-state text in the community role management. Preserve {rolesSettingsPath}; it is inserted by code.',
});
const ADD_ROLE_DESCRIPTOR = msg({
	message: 'Add role',
	comment: 'Button or menu action label in the community role management. Keep it concise.',
});
const RoleBadge: React.FC<{
	role: GuildRole;
	canRemove: boolean;
	guildId: string;
	userId: string;
}> = observer(function RoleBadge({role, canRemove, guildId, userId}) {
	const {i18n} = useLingui();
	const roleColor = ColorUtils.int2rgb(role.color);
	const iconColor = ColorUtils.getBestContrastColor(role.color);
	const removeRoleLabel = i18n._(REMOVE_ROLE_DESCRIPTOR, {roleName: role.name});
	const handleContextMenu = useCallback(
		(event: React.MouseEvent<HTMLDivElement>) => {
			openRoleContextMenu(event, role.id);
		},
		[role.id],
	);
	const handleRemoveRole = () => {
		GuildMemberCommands.removeRole(guildId, userId, role.id);
	};
	const roleIndicator = (
		<span
			className={styles.roleIndicator}
			style={{backgroundColor: roleColor}}
			data-flx="guild.role-management.role-badge.role-indicator"
		/>
	);
	return (
		// biome-ignore lint/a11y/noStaticElementInteractions: context-menu affordance on role badge.
		<div
			key={role.id}
			className={clsx(styles.roleBadge, profileStyles.role)}
			onContextMenu={handleContextMenu}
			data-flx="guild.role-management.role-badge.role-badge.context-menu"
		>
			{canRemove ? (
				<Tooltip text={removeRoleLabel} position="top" data-flx="guild.role-management.role-badge.tooltip">
					<FocusRing offset={-2} data-flx="guild.role-management.role-badge.focus-ring">
						<button
							type="button"
							className={styles.roleRemoveButton}
							onClick={handleRemoveRole}
							aria-label={removeRoleLabel}
							data-flx="guild.role-management.role-badge.role-remove-button.remove-role"
						>
							<XIcon
								weight="bold"
								className={clsx(styles.roleRemoveIconContainer, profileStyles.roleRemoveIcon)}
								style={{color: iconColor}}
								data-flx="guild.role-management.role-badge.role-remove-icon-container"
							/>
							{roleIndicator}
						</button>
					</FocusRing>
				</Tooltip>
			) : (
				<div
					className={styles.roleRemoveButtonContainer}
					data-flx="guild.role-management.role-badge.role-remove-button-container"
				>
					{roleIndicator}
				</div>
			)}
			<div className={styles.roleName} data-flx="guild.role-management.role-badge.role-name">
				{role.name}
			</div>
		</div>
	);
});
export const RoleList: React.FC<{
	guildId: string;
	userId: string;
	roles: Array<GuildRole>;
	canManage: boolean;
}> = observer(function RoleList({guildId, userId, roles, canManage}) {
	const {canManageRole} = useRoleHierarchy(Guilds.getGuild(guildId));
	if (roles.length === 0 && !canManage) {
		return null;
	}
	return (
		<div className={styles.roleListContainer} data-flx="guild.role-management.role-list.role-list-container">
			{roles.map((role) => {
				const canRemoveRole =
					canManage &&
					canManageRole({
						id: role.id,
						position: role.position,
						permissions: role.permissions,
					});
				return (
					<RoleBadge
						key={role.id}
						role={role}
						canRemove={canRemoveRole}
						guildId={guildId}
						userId={userId}
						data-flx="guild.role-management.role-list.role-badge"
					/>
				);
			})}
		</div>
	);
});

interface ManageRolesMenuContentProps {
	guildId: string;
	userId: string;
	onClose: () => void;
}

const ManageRolesMenuContent: React.FC<ManageRolesMenuContentProps> = observer(function ManageRolesMenuContent({
	guildId,
	userId,
	onClose,
}) {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(guildId);
	const currentMember = GuildMembers.getMember(guildId, userId);
	const {canManageRole} = useRoleHierarchy(guild);
	const rolesSettingsPath = useMemo(() => formatGuildSettingsPath(i18n, 'roles'), [i18n.locale]);
	const handleOpenGuildSettings = useCallback(() => {
		if (GuildSettingsModalState.navigateToTab(guildId, 'roles')) {
			onClose();
			return;
		}
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<GuildSettingsModal
					guildId={guildId}
					initialTab="roles"
					data-flx="guild.role-management.handle-open-guild-settings.guild-settings-modal"
				/>
			)),
		);
	}, [guildId, onClose]);
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
	const handleToggleRole = useCallback(
		async (roleId: string, hasRole: boolean, canManage: boolean) => {
			if (!canManage) return;
			if (hasRole) {
				await GuildMemberCommands.removeRole(guildId, userId, roleId);
			} else {
				await GuildMemberCommands.addRole(guildId, userId, roleId);
			}
		},
		[guildId, userId],
	);
	if (allRoles.length === 0) {
		return (
			<MenuGroup data-flx="guild.role-management.manage-roles-menu-content.menu-group">
				<MenuItem
					onClick={handleOpenGuildSettings}
					data-flx="guild.role-management.manage-roles-menu-content.menu-item.open-guild-settings"
				>
					<Trans>No roles yet. Add roles in {rolesSettingsPath}</Trans>
				</MenuItem>
			</MenuGroup>
		);
	}
	return (
		<MenuGroup data-flx="guild.role-management.manage-roles-menu-content.menu-group--2">
			{allRoles.map(({role, canManage}) => {
				const hasRole = currentMember?.roles.has(role.id) ?? false;
				return (
					<CheckboxItem
						key={role.id}
						checked={hasRole}
						disabled={!canManage}
						onCheckedChange={() => handleToggleRole(role.id, hasRole, canManage)}
						closeOnChange={false}
						data-flx="guild.role-management.manage-roles-menu-content.checkbox-item"
					>
						{/* biome-ignore lint/a11y/noStaticElementInteractions: context-menu affordance inside a CheckboxItem. */}
						<div
							className={itemStyles.roleContainer}
							onContextMenu={(event) => openRoleContextMenu(event, role.id)}
							data-flx="guild.role-management.manage-roles-menu-content.div.open-role-context-menu"
						>
							<div
								className={itemStyles.roleIcon}
								style={{backgroundColor: ColorUtils.int2rgb(role.color)}}
								data-flx="guild.role-management.manage-roles-menu-content.div"
							/>
							<span
								className={clsx(itemStyles.roleName, !canManage && itemStyles.roleDisabled)}
								data-flx="guild.role-management.manage-roles-menu-content.span"
							>
								{role.name}
							</span>
						</div>
					</CheckboxItem>
				);
			})}
		</MenuGroup>
	);
});

interface ManageRolesBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	guildId: string;
	userId: string;
}

export const ManageRolesBottomSheet: React.FC<ManageRolesBottomSheetProps> = observer(function ManageRolesBottomSheet({
	isOpen,
	onClose,
	guildId,
	userId,
}) {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(guildId);
	const currentMember = GuildMembers.getMember(guildId, userId);
	const {canManageRole} = useRoleHierarchy(guild);
	const rolesSettingsPath = useMemo(() => formatGuildSettingsPath(i18n, 'roles'), [i18n.locale]);
	const manageRolesLabel = PermissionUtils.formatPermissionLabel(i18n, Permissions.MANAGE_ROLES);
	const handleOpenGuildSettings = useCallback(() => {
		if (GuildSettingsModalState.navigateToTab(guildId, 'roles')) {
			onClose();
			return;
		}
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<GuildSettingsModal
					guildId={guildId}
					initialTab="roles"
					data-flx="guild.role-management.handle-open-guild-settings.guild-settings-modal--2"
				/>
			)),
		);
	}, [guildId, onClose]);
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
	const menuGroups: Array<MenuGroupType> = useMemo(() => {
		if (allRoles.length === 0) {
			return [
				{
					items: [
						{
							icon: (
								<PlusIcon
									weight="bold"
									className={styles.iconSize}
									data-flx="guild.role-management.menu-groups.icon-size"
								/>
							),
							label: i18n._(NO_ROLES_YET_ADD_ROLES_IN_DESCRIPTOR, {rolesSettingsPath}),
							onClick: handleOpenGuildSettings,
						},
					],
				},
			];
		}
		return [
			{
				items: allRoles.map(({role, canManage}) => {
					const hasRole = currentMember?.roles.has(role.id) ?? false;
					return {
						label: role.name,
						checked: hasRole,
						disabled: !canManage,
						onChange: async () => {
							if (!canManage) return;
							if (hasRole) {
								await GuildMemberCommands.removeRole(guildId, userId, role.id);
							} else {
								await GuildMemberCommands.addRole(guildId, userId, role.id);
							}
						},
						icon: (
							<div
								className={styles.roleColorIndicator}
								style={{backgroundColor: ColorUtils.int2rgb(role.color)}}
								data-flx="guild.role-management.menu-groups.role-color-indicator"
							/>
						),
					};
				}),
			},
		];
	}, [allRoles, currentMember, guildId, userId, handleOpenGuildSettings, rolesSettingsPath, i18n.locale]);
	return (
		<MenuBottomSheet
			isOpen={isOpen}
			onClose={onClose}
			title={manageRolesLabel}
			groups={menuGroups}
			data-flx="guild.role-management.manage-roles-bottom-sheet.menu-bottom-sheet"
		/>
	);
});

function openNoRolesModal(guildId: string) {
	function NoRolesModalDescription() {
		const {i18n} = useLingui();
		const rolesSettingsPath = useMemo(() => formatGuildSettingsPath(i18n, 'roles'), [i18n.locale]);
		const handleOpenRolesSettings = useCallback(() => {
			ModalCommands.pop();
			if (GuildSettingsModalState.navigateToTab(guildId, 'roles')) {
				return;
			}
			ModalCommands.push(
				modal(() => (
					<GuildSettingsModal
						guildId={guildId}
						initialTab="roles"
						data-flx="guild.role-management.handle-open-roles-settings.guild-settings-modal"
					/>
				)),
			);
		}, []);
		return (
			<Trans>
				There are no roles to assign in this community at this time, but you can create a new role in{' '}
				<button
					type="button"
					className={styles.noRolesLink}
					onClick={handleOpenRolesSettings}
					data-flx="guild.role-management.no-roles-modal-description.no-roles-link.open-roles-settings.button"
				>
					{rolesSettingsPath}
				</button>
				.
			</Trans>
		);
	}
	ModalCommands.push(
		modal(() => (
			<ConfirmModal
				title={<Trans>No roles available</Trans>}
				description={
					<NoRolesModalDescription data-flx="guild.role-management.open-no-roles-modal.no-roles-modal-description" />
				}
				primaryText={<Trans>OK</Trans>}
				primaryVariant="primary"
				secondaryText={false}
				onPrimary={() => {}}
				data-flx="guild.role-management.open-no-roles-modal.confirm-modal"
			/>
		)),
	);
}

export const AddRoleButton: React.FC<{
	guildId: string;
	userId: string;
}> = observer(function AddRoleButton({guildId, userId}) {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(guildId);
	const member = GuildMembers.getMember(guildId, userId);
	const isMobile = MobileLayout.enabled;
	const [showBottomSheet, setShowBottomSheet] = useState(false);
	const hasRoles = useMemo(() => {
		if (!guild) return false;
		return Object.values(guild.roles).some((r) => !r.isEveryone);
	}, [guild]);
	const handleClick = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			if (!hasRoles) {
				openNoRolesModal(guildId);
				return;
			}
			if (isMobile) {
				setShowBottomSheet(true);
			} else {
				ContextMenuCommands.openFromEvent(event, ({onClose}) => (
					<ManageRolesMenuContent
						guildId={guildId}
						userId={userId}
						onClose={onClose}
						data-flx="guild.role-management.handle-click.manage-roles-menu-content"
					/>
				));
			}
		},
		[guildId, userId, isMobile, hasRoles],
	);
	if (!guild || !member) return null;
	return (
		<>
			<Tooltip text={i18n._(ADD_ROLE_DESCRIPTOR)} data-flx="guild.role-management.add-role-button.tooltip">
				<FocusRing offset={-2} data-flx="guild.role-management.add-role-button.focus-ring">
					<button
						type="button"
						className={clsx(styles.assignRoleButton, styles.assignRoleButtonIcon)}
						onClick={handleClick}
						aria-label={i18n._(ADD_ROLE_DESCRIPTOR)}
						data-flx="guild.role-management.add-role-button.add-role-button.click"
					>
						<PlusIcon
							weight="bold"
							className={styles.iconSize}
							data-flx="guild.role-management.add-role-button.icon-size"
						/>
					</button>
				</FocusRing>
			</Tooltip>
			{isMobile && (
				<ManageRolesBottomSheet
					isOpen={showBottomSheet}
					onClose={() => setShowBottomSheet(false)}
					guildId={guildId}
					userId={userId}
					data-flx="guild.role-management.add-role-button.manage-roles-bottom-sheet"
				/>
			)}
		</>
	);
});
