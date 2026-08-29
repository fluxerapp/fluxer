// SPDX-License-Identifier: AGPL-3.0-or-later

import {RoleCreateFailedModal} from '@app/features/app/components/alerts/RoleCreateFailedModal';
import {RoleDeleteFailedModal} from '@app/features/app/components/alerts/RoleDeleteFailedModal';
import {RoleNameBlankModal} from '@app/features/app/components/alerts/RoleNameBlankModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {sortRolesByPosition} from '@app/features/app/components/dialogs/shared/PermissionComponents';
import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import {showGuildErrorModal} from '@app/features/guild/components/alerts/GuildErrorModalUtils';
import {MobileRoleList} from '@app/features/guild/components/modals/guild_tabs/guild_roles_tab/MobileRoleList';
import {RoleEditor} from '@app/features/guild/components/modals/guild_tabs/guild_roles_tab/RoleEditor';
import {RoleSidebar} from '@app/features/guild/components/modals/guild_tabs/guild_roles_tab/RoleSidebar';
import {
	applyRoleUpdate,
	GUILD_ROLES_TAB_ID,
	type RoleUpdate,
} from '@app/features/guild/components/modals/guild_tabs/guild_roles_tab/shared';
import {
	createRoleMovePreview,
	type RoleMovePreview,
} from '@app/features/guild/components/modals/guild_tabs/RoleMoveOperation';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import Guilds from '@app/features/guild/state/Guilds';
import {createSubmittableRoleOrderIds} from '@app/features/guild/utils/GuildRoleOrderUtils';
import {CANCEL_DESCRIPTOR, TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {RoleUpdateFailedModal} from '@app/features/moderation/components/alerts/RoleUpdateFailedModal';
import Permission from '@app/features/permissions/state/Permission';
import PermissionLayout from '@app/features/permissions/state/PermissionLayout';
import * as PermissionUtils from '@app/features/permissions/utils/PermissionUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import * as UnsavedChangesCommands from '@app/features/ui/commands/UnsavedChangesCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import SettingsSidebar from '@app/features/ui/state/SettingsSidebar';
import Users from '@app/features/user/state/Users';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {matchSorter} from 'match-sorter';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';

const logger = new Logger('GuildRolesTab');
const NEW_ROLE_DESCRIPTOR = msg({
	message: 'New role',
	comment: 'Default name for a newly created role in the community roles settings tab. Short label.',
});
const ROLE_COPY_NAME_DESCRIPTOR = msg({
	message: '{name} copy',
	comment:
		'Default name for a role created by duplicating another role in the community roles settings tab. {name} is the source role name; translate the whole label so "copy" can move or inflect as the language needs.',
});
const DELETE_ROLE_DESCRIPTOR = msg({
	message: 'Delete role',
	comment:
		'Destructive button in the role editor that removes the selected role. Removes the role from every member; keep tone plain.',
});
const COULDN_T_RESET_HOIST_ORDER_DESCRIPTOR = msg({
	message: "Couldn't reset hoist order",
	comment: 'Error modal title shown when resetting the community role hoist order fails.',
});
const YOU_NEED_THE_PERMISSION_TO_EDIT_THESE_PERMISSIONS_DESCRIPTOR = msg({
	message: 'You need the "{manageRolesPermissionLabel}" permission to edit these permissions',
	comment:
		'Tooltip on the disabled permission list in the role editor. {manageRolesPermissionLabel} is the localized name of the Manage Roles permission and should match its label exactly.',
});
const YOU_CANNOT_EDIT_A_ROLE_AT_OR_ABOVE_DESCRIPTOR = msg({
	message: 'You cannot edit a role at or above your highest role',
	comment: 'Tooltip on the disabled permission list in the role editor when role hierarchy blocks editing.',
});
const YOU_CANNOT_GRANT_A_PERMISSION_YOU_DON_T_DESCRIPTOR = msg({
	message: "You cannot grant a permission you don't have",
	comment:
		'Tooltip on a permission toggle in the role editor that the current user cannot grant because they do not hold that permission themselves.',
});
const YOU_CANNOT_REMOVE_THIS_PERMISSION_BECAUSE_IT_WOULD_DESCRIPTOR = msg({
	message: 'You cannot remove this permission because it would remove it from yourself',
	comment:
		'Tooltip on a permission toggle in the role editor when revoking that permission would leave the current user without it.',
});
const GuildRolesTab: React.FC<{guildId: string}> = observer(({guildId}) => {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(guildId);
	const currentUser = Users.currentUser;
	const isMobile = MobileLayout.enabled;
	const overrideOwnerId = useMemo(() => `guild-roles-${guildId}`, [guildId]);
	const [selectedRoleId, setSelectedRoleId] = useState<string | null>(null);
	const [mobileShowEditor, setMobileShowEditor] = useState(false);
	const [roleUpdates, setRoleUpdates] = useState<Map<string, RoleUpdate>>(new Map());
	const [pendingRoleOrder, setPendingRoleOrder] = useState<Array<string> | null>(null);
	const [optimisticRoleOrder, setOptimisticRoleOrder] = useState<Array<string> | null>(null);
	const [hoistOrderMode, setHoistOrderMode] = useState(false);
	const [pendingHoistOrder, setPendingHoistOrder] = useState<Array<string> | null>(null);
	const [permissionSearchQuery, setPermissionSearchQuery] = useState('');
	const pendingRoleCreationRef = useRef(false);
	const previousRoleIdsRef = useRef<Array<string>>([]);
	const canManageRoles = Permission.can(Permissions.MANAGE_ROLES, {guildId});
	const manageRolesPermissionLabel = PermissionUtils.formatPermissionLabel(i18n, Permissions.MANAGE_ROLES);
	const mentionEveryonePermissionLabel = PermissionUtils.formatPermissionLabel(i18n, Permissions.MENTION_EVERYONE);
	const isGuildOwner = guild && currentUser ? guild.isOwner(currentUser.id) : false;
	const roles = useMemo(() => {
		if (!guild) return [];
		const rolesList = Object.values(guild.roles);
		const sorted = sortRolesByPosition(rolesList);
		const order = pendingRoleOrder ?? optimisticRoleOrder;
		if (order) {
			const mapped = order
				.map((id: string) => sorted.find((r: GuildRole) => r.id === id))
				.filter((r): r is GuildRole => r != null);
			const missing = sorted.filter((r: GuildRole) => !order.includes(r.id));
			return [...mapped, ...missing];
		}
		return sorted;
	}, [guild, pendingRoleOrder, optimisticRoleOrder]);
	useEffect(() => {
		if (!pendingRoleOrder || !guild) return;
		const sortedIds = sortRolesByPosition(Object.values(guild.roles)).map((role: GuildRole) => role.id);
		if (pendingRoleOrder.length !== sortedIds.length) return;
		const matches = pendingRoleOrder.every((id, index) => id === sortedIds[index]);
		if (matches) setPendingRoleOrder(null);
	}, [pendingRoleOrder, guild]);
	useEffect(() => {
		if (!optimisticRoleOrder || !guild) return;
		const sortedIds = sortRolesByPosition(Object.values(guild.roles)).map((role: GuildRole) => role.id);
		if (optimisticRoleOrder.length !== sortedIds.length) return;
		const matches = optimisticRoleOrder.every((id, index) => id === sortedIds[index]);
		if (matches) setOptimisticRoleOrder(null);
	}, [optimisticRoleOrder, guild]);
	const hoistedRoles = useMemo(() => {
		if (!guild) return [];
		const rolesList = Object.values(guild.roles).filter((role) => role.hoist && !role.isEveryone);
		const sorted = [...rolesList].sort((a, b) => {
			const aPos = a.effectiveHoistPosition;
			const bPos = b.effectiveHoistPosition;
			if (bPos !== aPos) return bPos - aPos;
			return BigInt(a.id) < BigInt(b.id) ? -1 : 1;
		});
		if (pendingHoistOrder) {
			return pendingHoistOrder.map((id) => sorted.find((r) => r.id === id)).filter((r): r is GuildRole => r != null);
		}
		return sorted;
	}, [guild, pendingHoistOrder]);
	const hasCustomHoistOrder = useMemo(() => {
		if (!guild) return false;
		return Object.values(guild.roles).some((role) => role.hoistPosition !== null);
	}, [guild]);
	const currentUserMember = useMemo(() => {
		if (!guild || !currentUser) return null;
		return GuildMembers.getMember(guild.id, currentUser.id);
	}, [guild, currentUser]);
	const currentUserHighestRole = useMemo(() => {
		if (!guild || !currentUserMember) return null;
		const memberRoles = Object.values(guild.roles).filter((role) => currentUserMember.roles.has(role.id));
		const sortedMemberRoles = sortRolesByPosition([...memberRoles]);
		const highestRole = sortedMemberRoles[0];
		if (!highestRole) return null;
		return {
			id: highestRole.id,
			position: highestRole.position,
			permissions: highestRole.permissions,
		};
	}, [guild, currentUserMember]);
	const currentUserPermissions = useMemo(() => {
		if (!guild || !currentUser) return 0n;
		return PermissionUtils.computePermissions(currentUser.id, guild.toJSON());
	}, [guild, currentUser]);
	const wouldRemoveOwnPermission = useCallback(
		(permission: bigint, roleId: string): boolean => {
			if (!guild || !currentUser || !currentUserMember) return false;
			if (guild.isOwner(currentUser.id)) return false;
			const userHasRole = currentUserMember.roles.has(roleId) || roleId === guild.id;
			if (!userHasRole) return false;
			const role = guild.roles[roleId];
			if (!role) return false;
			const roleHasPermission = (role.permissions & permission) === permission;
			if (!roleHasPermission) return false;
			let permissionsWithoutThisRole = guild.roles[guild.id]?.permissions ?? 0n;
			for (const memberRoleId of currentUserMember.roles) {
				if (memberRoleId === roleId) continue;
				const memberRole = guild.roles[memberRoleId];
				if (memberRole) {
					permissionsWithoutThisRole |= memberRole.permissions;
				}
			}
			if ((permissionsWithoutThisRole & Permissions.ADMINISTRATOR) === Permissions.ADMINISTRATOR) {
				return false;
			}
			return (permissionsWithoutThisRole & permission) !== permission;
		},
		[guild, currentUser, currentUserMember],
	);
	const isRoleLocked = useCallback(
		(role: GuildRole): boolean => {
			if (!guild || !currentUser) return true;
			if (guild.isOwner(currentUser.id)) return false;
			if (role.isEveryone) return false;
			if (!canManageRoles) return true;
			if (!currentUserHighestRole) return true;
			if (currentUserHighestRole.position > role.position) return false;
			if (currentUserHighestRole.position < role.position) return true;
			return BigInt(currentUserHighestRole.id) >= BigInt(role.id);
		},
		[guild, currentUser, currentUserHighestRole, canManageRoles],
	);
	const selectedRole = useMemo(() => {
		if (!selectedRoleId || !guild) return null;
		return guild.roles[selectedRoleId] ?? null;
	}, [selectedRoleId, guild]);
	const selectedRoleLocked = useMemo(
		() => (selectedRole ? isRoleLocked(selectedRole) : false),
		[selectedRole, isRoleLocked],
	);
	const selectedRoleWithUpdates = useMemo(() => {
		if (!selectedRole) return null;
		return applyRoleUpdate(selectedRole, roleUpdates.get(selectedRole.id));
	}, [selectedRole, roleUpdates]);
	const permissionSpecs = useMemo(() => PermissionUtils.generatePermissionSpec(i18n), [i18n.locale]);
	const filteredPermissionSpecs = useMemo(() => {
		if (!permissionSearchQuery) return permissionSpecs;
		return permissionSpecs
			.map((spec) => {
				const filteredPermissions = matchSorter(spec.permissions, permissionSearchQuery, {
					keys: ['title', 'description'],
				});
				if (filteredPermissions.length === 0) return null;
				return {...spec, permissions: filteredPermissions};
			})
			.filter((spec): spec is PermissionUtils.PermissionSpec => spec !== null);
	}, [permissionSpecs, permissionSearchQuery]);
	useLayoutEffect(() => {
		if (!isMobile && roles.length > 0 && !selectedRoleId) {
			setSelectedRoleId(roles[0].id);
		}
	}, [roles, selectedRoleId, isMobile]);
	const hasUnsavedChanges = roleUpdates.size > 0 || pendingRoleOrder !== null || pendingHoistOrder !== null;
	useEffect(() => {
		UnsavedChangesCommands.setUnsavedChanges(GUILD_ROLES_TAB_ID, hasUnsavedChanges);
	}, [hasUnsavedChanges]);
	const handleSave = useCallback(async () => {
		if (!guild || !canManageRoles) return;
		for (const [_roleId, updates] of roleUpdates.entries()) {
			if (updates.name !== undefined && updates.name.trim() === '') {
				ModalCommands.push(
					modal(() => (
						<RoleNameBlankModal data-flx="guild.guild-tabs.guild-roles-tab.handle-save.role-name-blank-modal" />
					)),
				);
				return;
			}
		}
		try {
			if (pendingRoleOrder) {
				const submittableRoleOrder = createSubmittableRoleOrderIds({
					guildId: guild.id,
					orderedRoleIds: pendingRoleOrder,
					isRoleLocked: (roleId) => {
						const role = guild.roles[roleId];
						if (!role) return true;
						return isRoleLocked(role);
					},
				});
				if (submittableRoleOrder.length > 0) {
					await GuildCommands.setRoleOrder(guild.id, submittableRoleOrder);
				}
			}
			if (pendingHoistOrder) {
				const submittableHoistOrder = createSubmittableRoleOrderIds({
					guildId: guild.id,
					orderedRoleIds: pendingHoistOrder,
					isRoleLocked: (roleId) => {
						const role = guild.roles[roleId];
						if (!role) return true;
						return isRoleLocked(role);
					},
				});
				if (submittableHoistOrder.length > 0) {
					await GuildCommands.setRoleHoistOrder(guild.id, submittableHoistOrder);
				}
			}
			for (const [roleId, updates] of roleUpdates.entries()) {
				const updateData: Record<string, unknown> = {};
				if (updates.name !== undefined) updateData.name = updates.name;
				if (updates.color !== undefined) updateData.color = updates.color;
				if (updates.hoist !== undefined) updateData.hoist = updates.hoist;
				if (updates.mentionable !== undefined) updateData.mentionable = updates.mentionable;
				if (updates.permissions !== undefined) updateData.permissions = updates.permissions.toString();
				await GuildCommands.updateRole(guild.id, roleId, updateData);
			}
			setRoleUpdates(new Map());
			setPendingRoleOrder(null);
			setPendingHoistOrder(null);
			ToastCommands.createToast({type: 'success', children: <Trans>Roles updated successfully</Trans>});
		} catch (_error) {
			ModalCommands.push(
				modal(() => (
					<RoleUpdateFailedModal data-flx="guild.guild-tabs.guild-roles-tab.handle-save.role-update-failed-modal" />
				)),
			);
		}
	}, [guild, canManageRoles, roleUpdates, pendingRoleOrder, pendingHoistOrder, isRoleLocked]);
	const handleReset = useCallback(() => {
		setRoleUpdates(new Map());
		setPendingRoleOrder(null);
		setPendingHoistOrder(null);
		setHoistOrderMode(false);
	}, []);
	useEffect(() => {
		UnsavedChangesCommands.setTabData(GUILD_ROLES_TAB_ID, {
			onReset: handleReset,
			onSave: handleSave,
			isSubmitting: false,
		});
	}, [handleReset, handleSave]);
	useEffect(() => {
		return () => {
			UnsavedChangesCommands.clearUnsavedChanges(GUILD_ROLES_TAB_ID);
		};
	}, []);
	const handleRoleUpdate = useCallback(
		(roleId: string, updates: Partial<RoleUpdate>) => {
			setRoleUpdates((prev) => {
				const newMap = new Map(prev);
				const existing = newMap.get(roleId) || {id: roleId};
				const merged = {...existing, ...updates};
				const originalRole = guild?.roles[roleId];
				if (!originalRole) {
					newMap.set(roleId, merged);
					return newMap;
				}
				const hasChanges =
					(merged.name !== undefined && merged.name !== originalRole.name) ||
					(merged.color !== undefined && merged.color !== originalRole.color) ||
					(merged.hoist !== undefined && merged.hoist !== originalRole.hoist) ||
					(merged.mentionable !== undefined && merged.mentionable !== originalRole.mentionable) ||
					(merged.permissions !== undefined && merged.permissions !== originalRole.permissions);
				if (hasChanges) newMap.set(roleId, merged);
				else newMap.delete(roleId);
				return newMap;
			});
		},
		[guild],
	);
	const handleCreateRole = useCallback(async () => {
		if (!guild) return;
		pendingRoleCreationRef.current = true;
		try {
			await GuildCommands.createRole(guild.id, i18n._(NEW_ROLE_DESCRIPTOR));
			ToastCommands.createToast({type: 'success', children: <Trans>Role created successfully</Trans>});
		} catch (_error) {
			pendingRoleCreationRef.current = false;
			ModalCommands.push(
				modal(() => (
					<RoleCreateFailedModal data-flx="guild.guild-tabs.guild-roles-tab.handle-create-role.role-create-failed-modal" />
				)),
			);
		}
	}, [guild]);
	const handleDuplicateRole = useCallback(
		async (sourceRoleId: string) => {
			if (!guild || !canManageRoles) return;
			const sourceRole = guild.roles[sourceRoleId];
			if (!sourceRole) return;
			const copyName = i18n._(ROLE_COPY_NAME_DESCRIPTOR, {name: sourceRole.name}).slice(0, 100);
			const grantablePermissions = sourceRole.permissions & currentUserPermissions;
			pendingRoleCreationRef.current = true;
			try {
				const createdRole = await GuildCommands.createRole(guild.id, copyName, {
					color: sourceRole.color,
					permissions: grantablePermissions,
				});
				const orderedRoleIds = roles.map((role: GuildRole) => role.id).filter((id) => id !== createdRole.id);
				const sourceIndex = orderedRoleIds.indexOf(sourceRoleId);
				const insertIndex = sourceIndex === -1 ? orderedRoleIds.length : sourceIndex + 1;
				orderedRoleIds.splice(insertIndex, 0, createdRole.id);
				setOptimisticRoleOrder(orderedRoleIds);
				await GuildCommands.updateRole(guild.id, createdRole.id, {
					hoist: sourceRole.hoist,
					mentionable: sourceRole.mentionable,
				});
				try {
					const submittableRoleOrder = createSubmittableRoleOrderIds({
						guildId: guild.id,
						orderedRoleIds,
						isRoleLocked: (roleId) => {
							if (roleId === createdRole.id) return false;
							const role = guild.roles[roleId];
							if (!role) return true;
							return isRoleLocked(role);
						},
					});
					if (submittableRoleOrder.length > 0) {
						await GuildCommands.setRoleOrder(guild.id, submittableRoleOrder);
					}
				} catch (error) {
					logger.error(`Failed to position duplicated role in guild ${guild.id}:`, error);
					setOptimisticRoleOrder(null);
				}
				ToastCommands.createToast({type: 'success', children: <Trans>Role created successfully</Trans>});
			} catch (_error) {
				pendingRoleCreationRef.current = false;
				setOptimisticRoleOrder(null);
				ModalCommands.push(
					modal(() => (
						<RoleCreateFailedModal data-flx="guild.guild-tabs.guild-roles-tab.handle-duplicate-role.role-create-failed-modal" />
					)),
				);
			}
		},
		[guild, canManageRoles, roles, isRoleLocked, currentUserPermissions, i18n],
	);
	const handleDeleteRole = useCallback(() => {
		if (!selectedRole || selectedRole.isEveryone || !guild) return;
		const currentIndex = roles.findIndex((r: GuildRole) => r.id === selectedRole.id);
		const nextRole = roles[currentIndex + 1] ?? roles[0];
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(DELETE_ROLE_DESCRIPTOR)}
					description={
						<div data-flx="guild.guild-tabs.guild-roles-tab.handle-delete-role.div">
							<Trans>
								Are you sure you want to delete the{' '}
								<strong data-flx="guild.guild-tabs.guild-roles-tab.handle-delete-role.strong">
									{selectedRole.name}
								</strong>{' '}
								role? Any members with this role will no longer have it.
							</Trans>
						</div>
					}
					primaryText={i18n._(DELETE_ROLE_DESCRIPTOR)}
					primaryVariant="danger"
					secondaryText={i18n._(CANCEL_DESCRIPTOR)}
					onPrimary={async () => {
						try {
							await GuildCommands.deleteRole(guild.id, selectedRole.id);
							ToastCommands.createToast({type: 'success', children: <Trans>Role deleted successfully</Trans>});
							setSelectedRoleId(nextRole?.id ?? null);
						} catch (_error) {
							window.setTimeout(() => {
								ModalCommands.push(
									modal(() => (
										<RoleDeleteFailedModal
											roleName={selectedRole.name}
											data-flx="guild.guild-tabs.guild-roles-tab.handle-delete-role.role-delete-failed-modal"
										/>
									)),
								);
							}, 0);
						}
					}}
					data-flx="guild.guild-tabs.guild-roles-tab.handle-delete-role.confirm-modal"
				/>
			)),
		);
	}, [selectedRole, guild, roles]);
	const handleContextMenuDeleteRole = useCallback(
		async (roleId: string) => {
			if (!guild) return;
			const role = guild.roles[roleId];
			if (!role || role.isEveryone || isRoleLocked(role)) return;
			const currentIndex = roles.findIndex((r: GuildRole) => r.id === roleId);
			const nextRole = roles[currentIndex + 1] ?? roles[0];
			try {
				await GuildCommands.deleteRole(guild.id, roleId);
				ToastCommands.createToast({type: 'success', children: <Trans>Role deleted successfully</Trans>});
				setSelectedRoleId((current) => (current === roleId ? (nextRole?.id ?? null) : current));
			} catch (_error) {
				ModalCommands.push(
					modal(() => (
						<RoleDeleteFailedModal
							roleName={role.name}
							data-flx="guild.guild-tabs.guild-roles-tab.handle-context-menu-delete-role.role-delete-failed-modal"
						/>
					)),
				);
			}
		},
		[guild, roles, isRoleLocked],
	);
	const evaluateRoleMove = useCallback(
		(draggedRoleId: string, targetRoleId: string | null, position: 'before' | 'after') => {
			if (!guild) return null;
			if (targetRoleId === null && !isGuildOwner) return null;
			return createRoleMovePreview({
				roles,
				draggedRoleId,
				targetRoleId,
				position,
				isRoleLocked,
			});
		},
		[guild, roles, isRoleLocked, isGuildOwner],
	);
	const handleRoleDrop = useCallback((preview: RoleMovePreview) => {
		setPendingRoleOrder(preview.order.map((role) => role.id));
	}, []);
	const evaluateHoistMove = useCallback(
		(draggedRoleId: string, targetRoleId: string | null, position: 'before' | 'after') => {
			if (!guild) return null;
			if (targetRoleId === null && !isGuildOwner) return null;
			return createRoleMovePreview({
				roles: hoistedRoles,
				draggedRoleId,
				targetRoleId,
				position,
				isRoleLocked,
			});
		},
		[guild, hoistedRoles, isRoleLocked, isGuildOwner],
	);
	const handleHoistDrop = useCallback((preview: RoleMovePreview) => {
		setPendingHoistOrder(preview.order.map((role) => role.id));
	}, []);
	const handleResetHoistOrder = useCallback(async () => {
		if (!guild) return;
		try {
			await GuildCommands.resetRoleHoistOrder(guild.id);
			ToastCommands.createToast({type: 'success', children: <Trans>Hoist order reset to default</Trans>});
		} catch (_error) {
			showGuildErrorModal({
				title: i18n._(COULDN_T_RESET_HOIST_ORDER_DESCRIPTOR),
				message: i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
				dataFlx: 'guild.guild-tabs.guild-roles-tab.reset-hoist-order-error-modal',
			});
		}
	}, [guild, i18n]);
	const handlePermissionToggle = useCallback(
		(permission: bigint) => {
			if (!selectedRole) return;
			const currentPermissions = selectedRoleWithUpdates?.permissions ?? selectedRole.permissions;
			const hasPermission = (currentPermissions & permission) === permission;
			const newPermissions = hasPermission ? currentPermissions & ~permission : currentPermissions | permission;
			handleRoleUpdate(selectedRole.id, {permissions: newPermissions});
		},
		[selectedRole, selectedRoleWithUpdates, handleRoleUpdate],
	);
	const getPermissionDisabledReason = useCallback(
		(permission: bigint): string | undefined => {
			if (!guild || !currentUser) return;
			if (!canManageRoles)
				return i18n._(YOU_NEED_THE_PERMISSION_TO_EDIT_THESE_PERMISSIONS_DESCRIPTOR, {manageRolesPermissionLabel});
			if (guild.isOwner(currentUser.id)) return;
			if (selectedRoleLocked) return i18n._(YOU_CANNOT_EDIT_A_ROLE_AT_OR_ABOVE_DESCRIPTOR);
			if ((currentUserPermissions & permission) !== permission)
				return i18n._(YOU_CANNOT_GRANT_A_PERMISSION_YOU_DON_T_DESCRIPTOR);
			if (selectedRole && wouldRemoveOwnPermission(permission, selectedRole.id)) {
				return i18n._(YOU_CANNOT_REMOVE_THIS_PERMISSION_BECAUSE_IT_WOULD_DESCRIPTOR);
			}
			return;
		},
		[
			guild,
			currentUser,
			currentUserPermissions,
			selectedRoleLocked,
			canManageRoles,
			manageRolesPermissionLabel,
			selectedRole,
			wouldRemoveOwnPermission,
		],
	);
	const getPermissionWarning = useCallback(
		(permission: bigint): string | undefined => {
			if (!guild || !currentUser || !selectedRole) return;
			if (guild.isOwner(currentUser.id)) return;
			if (wouldRemoveOwnPermission(permission, selectedRole.id)) {
				return i18n._(YOU_CANNOT_REMOVE_THIS_PERMISSION_BECAUSE_IT_WOULD_DESCRIPTOR);
			}
			return;
		},
		[guild, currentUser, selectedRole, wouldRemoveOwnPermission],
	);
	const handleClearPermissions = useCallback(() => {
		if (!selectedRole) return;
		handleRoleUpdate(selectedRole.id, {permissions: 0n});
	}, [selectedRole, handleRoleUpdate]);
	const handleEnterHoistOrderMode = useCallback(() => setHoistOrderMode(true), []);
	const handleExitHoistOrderMode = useCallback(() => setHoistOrderMode(false), []);
	const sidebarContent = useMemo(() => {
		if (!guild || !currentUser) return null;
		return (
			<RoleSidebar
				roles={roles}
				hoistedRoles={hoistedRoles}
				selectedRoleId={selectedRoleId}
				isGuildOwner={isGuildOwner}
				canManageRoles={canManageRoles}
				hoistOrderMode={hoistOrderMode}
				hasCustomHoistOrder={hasCustomHoistOrder}
				roleUpdates={roleUpdates}
				isRoleLocked={isRoleLocked}
				onSelectRole={setSelectedRoleId}
				onCreateRole={handleCreateRole}
				onDuplicateRole={handleDuplicateRole}
				onDeleteRole={handleContextMenuDeleteRole}
				onEnterHoistOrderMode={handleEnterHoistOrderMode}
				onExitHoistOrderMode={handleExitHoistOrderMode}
				onResetHoistOrder={handleResetHoistOrder}
				onEvaluateRoleMove={evaluateRoleMove}
				onRoleDrop={handleRoleDrop}
				onEvaluateHoistMove={evaluateHoistMove}
				onHoistDrop={handleHoistDrop}
				data-flx="guild.guild-tabs.guild-roles-tab.sidebar-content.role-sidebar"
			/>
		);
	}, [
		guild,
		currentUser,
		roles,
		hoistedRoles,
		selectedRoleId,
		isGuildOwner,
		canManageRoles,
		handleCreateRole,
		handleDuplicateRole,
		handleContextMenuDeleteRole,
		evaluateRoleMove,
		handleRoleDrop,
		evaluateHoistMove,
		handleHoistDrop,
		handleResetHoistOrder,
		roleUpdates,
		isRoleLocked,
		hoistOrderMode,
		hasCustomHoistOrder,
		handleEnterHoistOrderMode,
		handleExitHoistOrderMode,
	]);
	useEffect(() => {
		if (isMobile || !guild || !currentUser) return;
		if (!SettingsSidebar.hasOverride) {
			SettingsSidebar.setOverride(overrideOwnerId, sidebarContent, {defaultOn: true});
		}
		return () => {
			if (SettingsSidebar.ownerId === overrideOwnerId) {
				SettingsSidebar.clearOverride(overrideOwnerId);
			}
		};
	}, [overrideOwnerId, isMobile]);
	useEffect(() => {
		if (isMobile || !guild || !currentUser) return;
		if (SettingsSidebar.ownerId === overrideOwnerId && sidebarContent) {
			SettingsSidebar.updateOverride(overrideOwnerId, sidebarContent);
		}
	}, [sidebarContent, guild, currentUser, overrideOwnerId, isMobile]);
	const isComfyLayout = PermissionLayout.isComfy;
	const isGridLayout = PermissionLayout.isGrid;
	const rolesScrollerKey = useMemo(
		() => `guild-roles-right-scroller-${isComfyLayout ? 'comfy' : 'dense'}-${isGridLayout ? 'grid' : 'single'}`,
		[isComfyLayout, isGridLayout],
	);
	const handleMobileRoleSelect = useCallback((roleId: string) => {
		setSelectedRoleId(roleId);
		setMobileShowEditor(true);
	}, []);
	const handleMobileBack = useCallback(() => {
		setMobileShowEditor(false);
	}, []);
	useEffect(() => {
		const previousIds = previousRoleIdsRef.current;
		const currentIds = roles.map((role: GuildRole) => role.id);
		if (pendingRoleCreationRef.current && currentIds.length > previousIds.length) {
			const newRoleId = currentIds.find((id: string) => !previousIds.includes(id));
			if (newRoleId) {
				if (isMobile) {
					handleMobileRoleSelect(newRoleId);
				} else {
					setSelectedRoleId(newRoleId);
				}
				pendingRoleCreationRef.current = false;
			}
		}
		previousRoleIdsRef.current = currentIds;
	}, [roles, isMobile, handleMobileRoleSelect]);
	if (!guild || !currentUser) return null;
	if (isMobile && !mobileShowEditor) {
		return (
			<MobileRoleList
				roles={roles}
				roleUpdates={roleUpdates}
				canManageRoles={canManageRoles}
				isRoleLocked={isRoleLocked}
				onCreateRole={handleCreateRole}
				onSelectRole={handleMobileRoleSelect}
				data-flx="guild.guild-tabs.guild-roles-tab.mobile-role-list"
			/>
		);
	}
	return (
		<RoleEditor
			rolesScrollerKey={rolesScrollerKey}
			isMobile={isMobile}
			selectedRole={selectedRole}
			selectedRoleWithUpdates={selectedRoleWithUpdates}
			selectedRoleLocked={selectedRoleLocked}
			canManageRoles={canManageRoles}
			mentionEveryonePermissionLabel={mentionEveryonePermissionLabel}
			permissionSearchQuery={permissionSearchQuery}
			filteredPermissionSpecs={filteredPermissionSpecs}
			onMobileBack={handleMobileBack}
			onDeleteRole={handleDeleteRole}
			onRoleUpdate={handleRoleUpdate}
			onClearPermissions={handleClearPermissions}
			onPermissionToggle={handlePermissionToggle}
			onPermissionSearchQueryChange={setPermissionSearchQuery}
			getPermissionDisabledReason={getPermissionDisabledReason}
			getPermissionWarning={getPermissionWarning}
			data-flx="guild.guild-tabs.guild-roles-tab.role-editor"
		/>
	);
});

export default GuildRolesTab;
