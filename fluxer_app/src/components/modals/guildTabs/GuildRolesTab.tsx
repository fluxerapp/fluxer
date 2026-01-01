/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Trans, useLingui} from '@lingui/react/macro';
import {
	ArrowsDownUpIcon,
	CaretRightIcon,
	GridFourIcon,
	ListIcon,
	LockIcon,
	MagnifyingGlassIcon,
	PlusIcon,
	RowsIcon,
	TrashIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {matchSorter} from 'match-sorter';
import {observer} from 'mobx-react-lite';
import React from 'react';
import type {ConnectableElement} from 'react-dnd';
import {DndProvider, useDrag, useDrop} from 'react-dnd';
import {getEmptyImage, HTML5Backend} from 'react-dnd-html5-backend';
import * as GuildActionCreators from '~/actions/GuildActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as UnsavedChangesActionCreators from '~/actions/UnsavedChangesActionCreators';
import {Permissions} from '~/Constants';
import {RoleCreateFailedModal} from '~/components/alerts/RoleCreateFailedModal';
import {RoleDeleteFailedModal} from '~/components/alerts/RoleDeleteFailedModal';
import {RoleNameBlankModal} from '~/components/alerts/RoleNameBlankModal';
import {RoleUpdateFailedModal} from '~/components/alerts/RoleUpdateFailedModal';
import {ColorPickerField} from '~/components/form/ColorPickerField';
import {Input} from '~/components/form/Input';
import {Switch} from '~/components/form/Switch';
import {DropIndicator} from '~/components/layout/DropIndicator';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {Button} from '~/components/uikit/Button/Button';
import {Scroller} from '~/components/uikit/Scroller';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useMergeRefs} from '~/hooks/useMergeRefs';
import type {GuildRoleRecord} from '~/records/GuildRoleRecord';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import PermissionLayoutStore from '~/stores/PermissionLayoutStore';
import PermissionStore from '~/stores/PermissionStore';
import SettingsSidebarStore from '~/stores/SettingsSidebarStore';
import UserStore from '~/stores/UserStore';
import * as PermissionUtils from '~/utils/PermissionUtils';
import {
	DEFAULT_ROLE_COLOR_HEX,
	getRoleColor,
	PermissionRoleCategory,
	sortRolesByPosition,
} from '../shared/PermissionComponents';
import styles from './GuildRolesTab.module.css';
import {createRoleMovePreview} from './utils/roleMoveOperation';

const ROLE_DND_TYPE = 'guild-role';

interface RoleDragItem {
	type: typeof ROLE_DND_TYPE;
	id: string;
}

interface RoleUpdate {
	id: string;
	name?: string;
	color?: number;
	hoist?: boolean;
	mentionable?: boolean;
	permissions?: bigint;
}

const GUILD_ROLES_TAB_ID = 'roles';

const RoleItem: React.FC<{
	role: GuildRoleRecord;
	isSelected: boolean;
	isLocked: boolean;
	canManageRoles: boolean;
	onClick: () => void;
	onEvaluateMove: (
		draggedRoleId: string,
		targetRoleId: string | null,
		position: 'before' | 'after',
	) => ReturnType<typeof createRoleMovePreview>;
	onCommitMove: (preview: NonNullable<ReturnType<typeof createRoleMovePreview>>) => void;
}> = observer(({role, isSelected, isLocked, canManageRoles, onClick, onEvaluateMove, onCommitMove}) => {
	const {t} = useLingui();
	const elementRef = React.useRef<HTMLButtonElement | null>(null);
	const [dropIndicator, setDropIndicator] = React.useState<{position: 'top' | 'bottom'; isValid: boolean} | null>(null);

	const dragItem = React.useMemo<RoleDragItem>(() => ({type: ROLE_DND_TYPE, id: role.id}), [role.id]);
	const canDrag = canManageRoles && !role.isEveryone && !isLocked;

	const [{isDragging}, dragRef, preview] = useDrag(
		() => ({
			type: ROLE_DND_TYPE,
			item: () => dragItem,
			canDrag,
			collect: (monitor) => ({isDragging: monitor.isDragging()}),
			end: () => setDropIndicator(null),
		}),
		[dragItem, canDrag],
	);

	React.useEffect(() => {
		preview(getEmptyImage(), {captureDraggingState: true});
	}, [preview]);

	const getDropPosition = React.useCallback((monitor: any): 'before' | 'after' | null => {
		const node = elementRef.current;
		if (!node) return null;

		const rect = node.getBoundingClientRect();
		const clientOffset = monitor.getClientOffset?.();
		if (!clientOffset) return null;

		const middle = (rect.bottom - rect.top) / 2;
		const offsetY = clientOffset.y - rect.top;
		return offsetY < middle ? 'before' : 'after';
	}, []);

	const [{isOver}, dropRef] = useDrop(
		() => ({
			accept: ROLE_DND_TYPE,
			hover: (item: RoleDragItem, monitor) => {
				if (item.id === role.id || !canManageRoles) {
					setDropIndicator(null);
					return;
				}

				const position = getDropPosition(monitor);
				if (!position) {
					setDropIndicator(null);
					return;
				}

				const previewResult = onEvaluateMove(item.id, role.id, position);
				setDropIndicator({
					position: position === 'before' ? 'top' : 'bottom',
					isValid: previewResult !== null,
				});
			},
			drop: (item: RoleDragItem, monitor) => {
				if (item.id === role.id || !canManageRoles) {
					setDropIndicator(null);
					return;
				}

				const position = getDropPosition(monitor);
				if (!position) {
					setDropIndicator(null);
					return;
				}

				const previewResult = onEvaluateMove(item.id, role.id, position);
				if (!previewResult) {
					setDropIndicator(null);
					return;
				}

				onCommitMove(previewResult);
				setDropIndicator(null);
			},
			collect: (monitor) => ({
				isOver: monitor.isOver({shallow: true}),
			}),
		}),
		[canManageRoles, getDropPosition, onCommitMove, onEvaluateMove, role.id],
	);

	React.useEffect(() => {
		if (!isOver) setDropIndicator(null);
	}, [isOver]);

	const dragConnectorRef = React.useCallback(
		(node: ConnectableElement | null) => {
			dragRef(node);
		},
		[dragRef],
	);
	const dropConnectorRef = React.useCallback(
		(node: ConnectableElement | null) => {
			dropRef(node);
		},
		[dropRef],
	);
	const mergedRef = useMergeRefs([
		(element: HTMLButtonElement | null) => {
			elementRef.current = element;
		},
		dragConnectorRef,
		dropConnectorRef,
	]);

	return (
		<div className={styles.itemWrap}>
			{dropIndicator && <DropIndicator position={dropIndicator.position} isValid={dropIndicator.isValid} />}
			<button
				type="button"
				ref={mergedRef}
				className={clsx(
					styles.overwriteItem,
					styles.roleButton,
					{[styles.overwriteItemSelected]: isSelected},
					isDragging && styles.dragging,
					!canDrag && styles.noDrag,
				)}
				onClick={onClick}
			>
				<div
					className={styles.roleDot}
					style={{backgroundColor: role.color === 0 ? DEFAULT_ROLE_COLOR_HEX : getRoleColor(role.color)}}
				/>
				<span className={styles.overwriteName}>{role.name || '\u00A0'}</span>
				{isLocked && (
					<Tooltip text={t`You cannot edit this role because it is your highest role or above you`}>
						<LockIcon className={styles.lockIcon} />
					</Tooltip>
				)}
			</button>
		</div>
	);
});

const RoleTopDropZone = observer(
	({
		isVisible,
		onEvaluateMove,
		onCommitMove,
	}: {
		isVisible: boolean;
		onEvaluateMove: (
			draggedRoleId: string,
			targetRoleId: string | null,
			position: 'before' | 'after',
		) => ReturnType<typeof createRoleMovePreview>;
		onCommitMove: (preview: NonNullable<ReturnType<typeof createRoleMovePreview>>) => void;
	}) => {
		const [state, setState] = React.useState<{visible: boolean; isValid: boolean}>({visible: false, isValid: false});

		const [{isOver}, dropRef] = useDrop(
			() => ({
				accept: ROLE_DND_TYPE,
				canDrop: () => isVisible,
				hover: (item: RoleDragItem) => {
					if (!isVisible) return;
					const previewResult = onEvaluateMove(item.id, null, 'before');
					setState({visible: true, isValid: previewResult !== null});
				},
				drop: (item: RoleDragItem) => {
					if (!isVisible) return;
					const previewResult = onEvaluateMove(item.id, null, 'before');
					if (previewResult) onCommitMove(previewResult);
					setState({visible: false, isValid: false});
				},
				collect: (monitor) => ({
					isOver: monitor.isOver({shallow: true}),
				}),
			}),
			[isVisible, onEvaluateMove, onCommitMove],
		);

		const dropConnectorRef = React.useCallback(
			(node: ConnectableElement | null) => {
				dropRef(node);
			},
			[dropRef],
		);

		React.useEffect(() => {
			if (!isOver && state.visible) setState({visible: false, isValid: false});
		}, [isOver, state.visible]);

		if (!isVisible) return null;

		return (
			<div ref={dropConnectorRef} className={styles.dropZone} aria-hidden={!state.visible && !isOver}>
				<div className={styles.dropZoneTrack}>
					<div
						className={clsx(
							styles.dropZoneBar,
							(state.visible || isOver) && styles.visible,
							state.isValid ? styles.valid : styles.invalid,
						)}
					/>
				</div>
			</div>
		);
	},
);

const GuildRolesTab: React.FC<{guildId: string}> = observer(({guildId}) => {
	const {t, i18n} = useLingui();
	const guild = GuildStore.getGuild(guildId);
	const currentUser = UserStore.currentUser;
	const isMobile = MobileLayoutStore.enabled;

	const overrideOwnerId = React.useMemo(() => `guild-roles-${guildId}`, [guildId]);

	const [selectedRoleId, setSelectedRoleId] = React.useState<string | null>(null);
	const [mobileShowEditor, setMobileShowEditor] = React.useState(false);
	const [roleUpdates, setRoleUpdates] = React.useState<Map<string, RoleUpdate>>(new Map());
	const [pendingRoleOrder, setPendingRoleOrder] = React.useState<Array<string> | null>(null);
	const [hoistOrderMode, setHoistOrderMode] = React.useState(false);
	const [pendingHoistOrder, setPendingHoistOrder] = React.useState<Array<string> | null>(null);
	const [permissionSearchQuery, setPermissionSearchQuery] = React.useState('');
	const pendingRoleCreationRef = React.useRef(false);
	const previousRoleIdsRef = React.useRef<Array<string>>([]);

	const canManageRoles = PermissionStore.can(Permissions.MANAGE_ROLES, {guildId});
	const isGuildOwner = guild && currentUser ? guild.isOwner(currentUser.id) : false;

	const roles = React.useMemo(() => {
		if (!guild) return [];
		const rolesList = Object.values(guild.roles);
		const sorted = sortRolesByPosition(rolesList);

		if (pendingRoleOrder) {
			return pendingRoleOrder
				.map((id) => sorted.find((r) => r.id === id))
				.filter((r): r is GuildRoleRecord => r != null);
		}

		return sorted;
	}, [guild, pendingRoleOrder]);

	React.useEffect(() => {
		if (!pendingRoleOrder || !guild) return;
		const sortedIds = sortRolesByPosition(Object.values(guild.roles)).map((role) => role.id);
		if (pendingRoleOrder.length !== sortedIds.length) return;
		const matches = pendingRoleOrder.every((id, index) => id === sortedIds[index]);
		if (matches) setPendingRoleOrder(null);
	}, [pendingRoleOrder, guild]);

	const hoistedRoles = React.useMemo(() => {
		if (!guild) return [];
		const rolesList = Object.values(guild.roles).filter((role) => role.hoist && !role.isEveryone);
		const sorted = [...rolesList].sort((a, b) => {
			const aPos = a.effectiveHoistPosition;
			const bPos = b.effectiveHoistPosition;
			if (bPos !== aPos) return bPos - aPos;
			return BigInt(a.id) < BigInt(b.id) ? -1 : 1;
		});

		if (pendingHoistOrder) {
			return pendingHoistOrder
				.map((id) => sorted.find((r) => r.id === id))
				.filter((r): r is GuildRoleRecord => r != null);
		}

		return sorted;
	}, [guild, pendingHoistOrder]);

	const hasCustomHoistOrder = React.useMemo(() => {
		if (!guild) return false;
		return Object.values(guild.roles).some((role) => role.hoistPosition !== null);
	}, [guild]);

	const currentUserHighestRole = React.useMemo(() => {
		if (!guild || !currentUser) return null;
		return PermissionUtils.getHighestRole(guild.toJSON(), currentUser.id);
	}, [guild, currentUser]);

	const currentUserPermissions = React.useMemo(() => {
		if (!guild || !currentUser) return 0n;
		return PermissionUtils.computePermissions(currentUser.id, guild.toJSON());
	}, [guild, currentUser]);

	const currentUserMember = React.useMemo(() => {
		if (!guild || !currentUser) return null;
		return GuildMemberStore.getMember(guild.id, currentUser.id);
	}, [guild, currentUser]);

	const wouldRemoveOwnPermission = React.useCallback(
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

	const isRoleLocked = React.useCallback(
		(role: GuildRoleRecord): boolean => {
			if (!guild || !currentUser) return true;

			if (guild.isOwner(currentUser.id)) return false;
			if (role.isEveryone) return false;
			if (!canManageRoles) return true;
			if (!currentUserHighestRole) return true;

			const roleData = {id: role.id, permissions: role.permissions, position: role.position};
			return !PermissionUtils.isRoleHigher(guild.toJSON(), currentUser.id, currentUserHighestRole, roleData);
		},
		[guild, currentUser, currentUserHighestRole, canManageRoles],
	);

	const selectedRole = React.useMemo(() => {
		if (!selectedRoleId || !guild) return null;
		return guild.roles[selectedRoleId] ?? null;
	}, [selectedRoleId, guild]);

	const selectedRoleLocked = React.useMemo(
		() => (selectedRole ? isRoleLocked(selectedRole) : false),
		[selectedRole, isRoleLocked],
	);

	const selectedRoleWithUpdates = React.useMemo(() => {
		if (!selectedRole) return null;
		const updates = roleUpdates.get(selectedRole.id);
		if (!updates) return selectedRole;

		return selectedRole.withUpdates({
			name: updates.name ?? selectedRole.name,
			color: updates.color ?? selectedRole.color,
			hoist: updates.hoist ?? selectedRole.hoist,
			mentionable: updates.mentionable ?? selectedRole.mentionable,
			permissions: updates.permissions?.toString() ?? selectedRole.permissions.toString(),
			position: selectedRole.position,
		});
	}, [selectedRole, roleUpdates]);

	const permissionSpecs = React.useMemo(() => PermissionUtils.generatePermissionSpec(i18n), [i18n]);

	const filteredPermissionSpecs = React.useMemo(() => {
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

	React.useLayoutEffect(() => {
		if (!isMobile && roles.length > 0 && !selectedRoleId) {
			setSelectedRoleId(roles[0].id);
		}
	}, [roles, selectedRoleId, isMobile]);

	const hasUnsavedChanges = roleUpdates.size > 0 || pendingRoleOrder !== null || pendingHoistOrder !== null;

	React.useEffect(() => {
		UnsavedChangesActionCreators.setUnsavedChanges(GUILD_ROLES_TAB_ID, hasUnsavedChanges);
	}, [hasUnsavedChanges]);

	const handleSave = React.useCallback(async () => {
		if (!guild || !canManageRoles) return;

		for (const [_roleId, updates] of roleUpdates.entries()) {
			if (updates.name !== undefined && updates.name.trim() === '') {
				ModalActionCreators.push(modal(() => <RoleNameBlankModal />));
				return;
			}
		}

		try {
			if (pendingRoleOrder) {
				await GuildActionCreators.setRoleOrder(guild.id, pendingRoleOrder);
			}

			if (pendingHoistOrder) {
				await GuildActionCreators.setRoleHoistOrder(guild.id, pendingHoistOrder);
			}

			for (const [roleId, updates] of roleUpdates.entries()) {
				const updateData: Record<string, unknown> = {};
				if (updates.name !== undefined) updateData.name = updates.name;
				if (updates.color !== undefined) updateData.color = updates.color;
				if (updates.hoist !== undefined) updateData.hoist = updates.hoist;
				if (updates.mentionable !== undefined) updateData.mentionable = updates.mentionable;
				if (updates.permissions !== undefined) updateData.permissions = updates.permissions.toString();

				await GuildActionCreators.updateRole(guild.id, roleId, updateData);
			}

			setRoleUpdates(new Map());
			setPendingRoleOrder(null);
			setPendingHoistOrder(null);

			ToastActionCreators.createToast({type: 'success', children: <Trans>Roles updated successfully</Trans>});
		} catch (_error) {
			ModalActionCreators.push(modal(() => <RoleUpdateFailedModal />));
		}
	}, [guild, canManageRoles, roleUpdates, pendingRoleOrder, pendingHoistOrder]);

	const handleReset = React.useCallback(() => {
		setRoleUpdates(new Map());
		setPendingRoleOrder(null);
		setPendingHoistOrder(null);
		setHoistOrderMode(false);
	}, []);

	React.useEffect(() => {
		UnsavedChangesActionCreators.setTabData(GUILD_ROLES_TAB_ID, {
			onReset: handleReset,
			onSave: handleSave,
			isSubmitting: false,
		});
	}, [handleReset, handleSave]);

	React.useEffect(() => {
		return () => {
			UnsavedChangesActionCreators.clearUnsavedChanges(GUILD_ROLES_TAB_ID);
		};
	}, []);

	const handleRoleUpdate = React.useCallback(
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

	const handleCreateRole = React.useCallback(async () => {
		if (!guild) return;
		pendingRoleCreationRef.current = true;
		try {
			await GuildActionCreators.createRole(guild.id, t`New Role`);
			ToastActionCreators.createToast({type: 'success', children: <Trans>Role created successfully</Trans>});
		} catch (_error) {
			pendingRoleCreationRef.current = false;
			ModalActionCreators.push(modal(() => <RoleCreateFailedModal />));
		}
	}, [guild]);

	const handleDeleteRole = React.useCallback(() => {
		if (!selectedRole || selectedRole.isEveryone || !guild) return;

		const currentIndex = roles.findIndex((r) => r.id === selectedRole.id);
		const nextRole = roles[currentIndex + 1] ?? roles[0];

		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={t`Delete Role`}
					description={
						<div>
							<Trans>
								Are you sure you want to delete the <strong>{selectedRole.name}</strong> role? Any members with this
								role will no longer have it.
							</Trans>
						</div>
					}
					primaryText={t`Delete Role`}
					primaryVariant="danger-primary"
					secondaryText={t`Cancel`}
					onPrimary={async () => {
						try {
							await GuildActionCreators.deleteRole(guild.id, selectedRole.id);
							ToastActionCreators.createToast({type: 'success', children: <Trans>Role deleted successfully</Trans>});
							setSelectedRoleId(nextRole?.id ?? null);
						} catch (_error) {
							ModalActionCreators.push(modal(() => <RoleDeleteFailedModal roleName={selectedRole.name} />));
						}
					}}
				/>
			)),
		);
	}, [selectedRole, guild, roles]);

	const evaluateRoleMove = React.useCallback(
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

	const handleRoleDrop = React.useCallback((preview: NonNullable<ReturnType<typeof createRoleMovePreview>>) => {
		setPendingRoleOrder(preview.order.map((role) => role.id));
	}, []);

	const evaluateHoistMove = React.useCallback(
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

	const handleHoistDrop = React.useCallback((preview: NonNullable<ReturnType<typeof createRoleMovePreview>>) => {
		setPendingHoistOrder(preview.order.map((role) => role.id));
	}, []);

	const handleResetHoistOrder = React.useCallback(async () => {
		if (!guild) return;
		try {
			await GuildActionCreators.resetRoleHoistOrder(guild.id);
			ToastActionCreators.createToast({type: 'success', children: <Trans>Hoist order reset to default</Trans>});
		} catch (_error) {
			ToastActionCreators.createToast({type: 'error', children: <Trans>Failed to reset hoist order</Trans>});
		}
	}, [guild]);

	const handlePermissionToggle = React.useCallback(
		(permission: bigint) => {
			if (!selectedRole) return;
			const currentPermissions = selectedRoleWithUpdates?.permissions ?? selectedRole.permissions;
			const hasPermission = (currentPermissions & permission) === permission;
			const newPermissions = hasPermission ? currentPermissions & ~permission : currentPermissions | permission;
			handleRoleUpdate(selectedRole.id, {permissions: newPermissions});
		},
		[selectedRole, selectedRoleWithUpdates, handleRoleUpdate],
	);

	const getPermissionDisabledReason = React.useCallback(
		(permission: bigint): string | undefined => {
			if (!guild || !currentUser) return;
			if (!canManageRoles) return t`You need Manage Roles to edit these permissions`;
			if (guild.isOwner(currentUser.id)) return;
			if (selectedRoleLocked) return t`You cannot edit a role at or above your highest role`;
			if ((currentUserPermissions & permission) !== permission) return t`You cannot grant a permission you don't have`;
			if (selectedRole && wouldRemoveOwnPermission(permission, selectedRole.id)) {
				return t`You cannot remove this permission because it would remove it from yourself`;
			}
			return;
		},
		[
			guild,
			currentUser,
			currentUserPermissions,
			selectedRoleLocked,
			canManageRoles,
			selectedRole,
			wouldRemoveOwnPermission,
		],
	);

	const getPermissionWarning = React.useCallback(
		(permission: bigint): string | undefined => {
			if (!guild || !currentUser || !selectedRole) return;
			if (guild.isOwner(currentUser.id)) return;
			if (wouldRemoveOwnPermission(permission, selectedRole.id)) {
				return t`You cannot remove this permission because it would remove it from yourself`;
			}
			return;
		},
		[guild, currentUser, selectedRole, wouldRemoveOwnPermission],
	);

	const handleClearPermissions = React.useCallback(() => {
		if (!selectedRole) return;
		handleRoleUpdate(selectedRole.id, {permissions: 0n});
	}, [selectedRole, handleRoleUpdate]);

	const getRoleWithUpdates = React.useCallback(
		(role: GuildRoleRecord): GuildRoleRecord => {
			const updates = roleUpdates.get(role.id);
			if (!updates) return role;
			return role.withUpdates({
				name: updates.name ?? role.name,
				color: updates.color ?? role.color,
				hoist: updates.hoist ?? role.hoist,
				mentionable: updates.mentionable ?? role.mentionable,
				permissions: updates.permissions?.toString() ?? role.permissions.toString(),
				position: role.position,
			});
		},
		[roleUpdates],
	);

	const sidebarContent = React.useMemo(() => {
		if (!guild || !currentUser) return null;

		if (hoistOrderMode) {
			return (
				<div>
					<div className={styles.leftTitle} style={{padding: '6px 8px'}}>
						<Trans>Hoist Order</Trans>
					</div>
					<div style={{padding: '0 8px 8px 8px', display: 'flex', gap: '8px', flexDirection: 'column'}}>
						<Button variant="secondary" small={true} onClick={() => setHoistOrderMode(false)}>
							<Trans>Back to Roles</Trans>
						</Button>
						{hasCustomHoistOrder && (
							<Button variant="secondary" small={true} onClick={handleResetHoistOrder} disabled={!canManageRoles}>
								<Trans>Reset to Default</Trans>
							</Button>
						)}
					</div>
					<div style={{padding: '0 8px 8px 8px'}}>
						<p className={styles.subtleText} style={{marginBottom: '8px'}}>
							<Trans>Drag roles to customize the order they appear in the member list.</Trans>
						</p>
						<DndProvider backend={HTML5Backend}>
							{isGuildOwner && canManageRoles && (
								<RoleTopDropZone isVisible={true} onEvaluateMove={evaluateHoistMove} onCommitMove={handleHoistDrop} />
							)}
							{hoistedRoles.map((role) => {
								const roleWithUpdates = getRoleWithUpdates(role);
								return (
									<RoleItem
										key={role.id}
										role={roleWithUpdates}
										isSelected={selectedRoleId === role.id}
										isLocked={isRoleLocked(role)}
										canManageRoles={canManageRoles}
										onClick={() => setSelectedRoleId(role.id)}
										onEvaluateMove={evaluateHoistMove}
										onCommitMove={handleHoistDrop}
									/>
								);
							})}
							{hoistedRoles.length === 0 && (
								<p className={styles.subtleText}>
									<Trans>No hoisted roles. Enable "Show this role separately" on a role to see it here.</Trans>
								</p>
							)}
						</DndProvider>
					</div>
				</div>
			);
		}

		return (
			<div>
				<div className={styles.leftTitle} style={{padding: '6px 8px'}}>
					<Trans>Roles</Trans>
				</div>
				<div style={{padding: '0 8px 8px 8px', display: 'flex', gap: '8px', flexDirection: 'column'}}>
					<Button
						variant="secondary"
						small={true}
						leftIcon={<PlusIcon size={18} weight="bold" />}
						onClick={handleCreateRole}
						disabled={!canManageRoles}
					>
						<Trans>Create Role</Trans>
					</Button>
					{hoistedRoles.length > 0 && canManageRoles && (
						<Button
							variant="secondary"
							small={true}
							leftIcon={<ArrowsDownUpIcon size={18} weight="bold" />}
							onClick={() => setHoistOrderMode(true)}
						>
							<Trans>Custom Hoist Order</Trans>
						</Button>
					)}
				</div>
				<div style={{padding: '0 8px 8px 8px'}}>
					<DndProvider backend={HTML5Backend}>
						{isGuildOwner && canManageRoles && (
							<RoleTopDropZone isVisible={true} onEvaluateMove={evaluateRoleMove} onCommitMove={handleRoleDrop} />
						)}
						{roles.map((role) => {
							const roleWithUpdates = getRoleWithUpdates(role);
							return (
								<RoleItem
									key={role.id}
									role={roleWithUpdates}
									isSelected={selectedRoleId === role.id}
									isLocked={isRoleLocked(role)}
									canManageRoles={canManageRoles}
									onClick={() => setSelectedRoleId(role.id)}
									onEvaluateMove={evaluateRoleMove}
									onCommitMove={handleRoleDrop}
								/>
							);
						})}
					</DndProvider>
				</div>
			</div>
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
		evaluateRoleMove,
		handleRoleDrop,
		evaluateHoistMove,
		handleHoistDrop,
		handleResetHoistOrder,
		getRoleWithUpdates,
		isRoleLocked,
		hoistOrderMode,
		hasCustomHoistOrder,
	]);

	React.useEffect(() => {
		if (isMobile || !guild || !currentUser) return;
		if (!SettingsSidebarStore.hasOverride) {
			SettingsSidebarStore.setOverride(overrideOwnerId, sidebarContent, {defaultOn: true});
		}
		return () => {
			if (SettingsSidebarStore.ownerId === overrideOwnerId) {
				SettingsSidebarStore.clearOverride(overrideOwnerId);
			}
		};
	}, [overrideOwnerId, isMobile]);

	React.useEffect(() => {
		if (isMobile || !guild || !currentUser) return;
		if (SettingsSidebarStore.ownerId === overrideOwnerId && sidebarContent) {
			SettingsSidebarStore.updateOverride(overrideOwnerId, sidebarContent);
		}
	}, [sidebarContent, guild, currentUser, overrideOwnerId, isMobile]);

	const isComfyLayout = PermissionLayoutStore.isComfy;
	const isGridLayout = PermissionLayoutStore.isGrid;
	const rolesScrollerKey = React.useMemo(
		() => `guild-roles-right-scroller-${isComfyLayout ? 'comfy' : 'dense'}-${isGridLayout ? 'grid' : 'single'}`,
		[isComfyLayout, isGridLayout],
	);

	const handleMobileRoleSelect = React.useCallback((roleId: string) => {
		setSelectedRoleId(roleId);
		setMobileShowEditor(true);
	}, []);

	const handleMobileBack = React.useCallback(() => {
		setMobileShowEditor(false);
	}, []);

	React.useEffect(() => {
		const previousIds = previousRoleIdsRef.current;
		const currentIds = roles.map((role) => role.id);
		if (pendingRoleCreationRef.current && currentIds.length > previousIds.length) {
			const newRoleId = currentIds.find((id) => !previousIds.includes(id));
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
			<div className={styles.container}>
				<div className={styles.mobileRoleList}>
					<div className={styles.mobileListHeader}>
						<h2 className={styles.mobileListTitle}>
							<Trans>Roles</Trans>
						</h2>
						<Button
							variant="secondary"
							small={true}
							leftIcon={<PlusIcon size={18} weight="bold" />}
							onClick={handleCreateRole}
							disabled={!canManageRoles}
						>
							<Trans>Create Role</Trans>
						</Button>
					</div>
					<div className={styles.mobileRoles}>
						{roles.map((role) => {
							const roleWithUpdates = getRoleWithUpdates(role);
							const locked = isRoleLocked(role);
							return (
								<button
									key={role.id}
									type="button"
									className={styles.mobileRoleItem}
									onClick={() => handleMobileRoleSelect(role.id)}
								>
									<div
										className={styles.roleDot}
										style={{
											backgroundColor:
												roleWithUpdates.color === 0 ? DEFAULT_ROLE_COLOR_HEX : getRoleColor(roleWithUpdates.color),
										}}
									/>
									<span className={styles.mobileRoleName}>{roleWithUpdates.name || '\u00A0'}</span>
									{locked && <LockIcon className={styles.lockIcon} />}
									<CaretRightIcon className={styles.mobileRoleChevron} size={20} weight="bold" />
								</button>
							);
						})}
					</div>
				</div>
			</div>
		);
	}

	return (
		<div className={styles.container}>
			<div className={styles.right}>
				<Scroller className={styles.rightScroller} fade={true} key={rolesScrollerKey}>
					{selectedRoleWithUpdates && (
						<>
							{isMobile && (
								<div className={styles.mobileBackRow}>
									<Button variant="secondary" small={true} onClick={handleMobileBack}>
										<Trans>Back to Roles</Trans>
									</Button>
								</div>
							)}
							<div className={styles.sectionRow}>
								<div className={styles.sectionHeader}>
									<h2 className={styles.sectionTitle}>
										<Trans>Edit "{selectedRoleWithUpdates.name}"</Trans>
									</h2>
									<p className={styles.subtleText}>
										<Trans>Configure role settings and permissions</Trans>
									</p>
								</div>
								{!selectedRoleWithUpdates.isEveryone && (
									<Button
										variant="secondary"
										small={true}
										leftIcon={<TrashIcon size={18} />}
										onClick={handleDeleteRole}
										disabled={selectedRoleLocked || !canManageRoles}
									>
										<Trans>Delete Role</Trans>
									</Button>
								)}
							</div>

							<div className={styles.sectionSubtitle}>
								<Trans>Display</Trans>
							</div>

							<div className={styles.displayRow}>
								{!selectedRoleWithUpdates.isEveryone && (
									<div className={styles.nameField}>
										<Input
											type="text"
											label={t`Role Name`}
											value={selectedRoleWithUpdates.name}
											onChange={(e) => selectedRole && handleRoleUpdate(selectedRole.id, {name: e.target.value})}
											disabled={selectedRoleLocked || !canManageRoles}
											maxLength={100}
										/>
									</div>
								)}
								<div className={styles.colorField}>
									<ColorPickerField
										label={t`Role Color`}
										description={t`Type a color (hex, rgb(), hsl(), or name) or use the picker.`}
										value={selectedRoleWithUpdates.color === 0 ? 0x000000 : Number(selectedRoleWithUpdates.color)}
										onChange={(num) => {
											const clean = num >>> 0;
											if (selectedRole) {
												handleRoleUpdate(selectedRole.id, {color: clean === 0 ? 0 : clean});
											}
										}}
										disabled={selectedRoleLocked || !canManageRoles}
									/>
								</div>
							</div>

							{!selectedRoleWithUpdates.isEveryone && (
								<div className={styles.settingsGroup}>
									<Switch
										label={t`Show this role separately`}
										description={t`Lists members with this role in their own section in the member list.`}
										value={selectedRoleWithUpdates.hoist}
										onChange={(value) => selectedRole && handleRoleUpdate(selectedRole.id, {hoist: value})}
										disabled={selectedRoleLocked || !canManageRoles}
									/>

									<Switch
										label={t`Allow @mentions for this role`}
										description={t`Anyone can @mention this role. Members with "Use @everyone/@here and @roles" can always mention it.`}
										value={selectedRoleWithUpdates.mentionable}
										onChange={(value) => selectedRole && handleRoleUpdate(selectedRole.id, {mentionable: value})}
										disabled={selectedRoleLocked || !canManageRoles}
									/>
								</div>
							)}

							<div className={styles.sectionRow}>
								<div className={styles.permHeaderRow}>
									<p className={styles.permHelp}>
										<Trans>Use this button to quickly clear all permissions.</Trans>
									</p>
									<Button
										variant="secondary"
										small={true}
										onClick={handleClearPermissions}
										disabled={selectedRoleLocked || !canManageRoles}
									>
										<Trans>Clear Permissions</Trans>
									</Button>
								</div>
							</div>

							<div className={styles.sectionSubtitle}>
								<Trans>Permissions</Trans>
							</div>

							<div className={styles.permSearchRow}>
								<Input
									type="text"
									placeholder={t`Search Permissions...`}
									value={permissionSearchQuery}
									onChange={(e) => setPermissionSearchQuery(e.target.value)}
									leftIcon={<MagnifyingGlassIcon size={16} weight="bold" />}
									className={styles.permSearchInput}
								/>
								<div className={styles.layoutButtons}>
									<Tooltip text={PermissionLayoutStore.isComfy ? t`Dense layout` : t`Comfy layout`}>
										<button
											type="button"
											className={styles.layoutButton}
											onClick={() => PermissionLayoutStore.toggleLayoutMode()}
											aria-label={PermissionLayoutStore.isComfy ? t`Switch to dense layout` : t`Switch to comfy layout`}
										>
											{PermissionLayoutStore.isComfy ? (
												<RowsIcon size={20} weight="bold" />
											) : (
												<ListIcon size={20} weight="bold" />
											)}
										</button>
									</Tooltip>
									<Tooltip text={PermissionLayoutStore.isGrid ? t`Single column` : t`Two columns`}>
										<button
											type="button"
											className={styles.layoutButton}
											onClick={() => PermissionLayoutStore.toggleGridMode()}
											aria-label={PermissionLayoutStore.isGrid ? t`Switch to single column` : t`Switch to two columns`}
										>
											<GridFourIcon size={20} weight={PermissionLayoutStore.isGrid ? 'fill' : 'bold'} />
										</button>
									</Tooltip>
								</div>
							</div>

							<div className={styles.permCategories}>
								{filteredPermissionSpecs.map((spec, index) => (
									<PermissionRoleCategory
										key={spec.title}
										spec={spec}
										rolePermissions={selectedRoleWithUpdates.permissions}
										onPermissionToggle={handlePermissionToggle}
										disabled={selectedRoleLocked || !canManageRoles}
										getPermissionDisabledReason={getPermissionDisabledReason}
										getPermissionWarning={getPermissionWarning}
										isFirst={index === 0}
									/>
								))}
								{filteredPermissionSpecs.length === 0 && permissionSearchQuery && (
									<div className={styles.emptyState}>
										<Trans>No permissions found</Trans>
									</div>
								)}
							</div>
						</>
					)}
				</Scroller>
			</div>
		</div>
	);
});

export default GuildRolesTab;
