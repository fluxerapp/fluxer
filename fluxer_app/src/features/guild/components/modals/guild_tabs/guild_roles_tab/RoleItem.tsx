// SPDX-License-Identifier: AGPL-3.0-or-later

import {DEFAULT_ROLE_COLOR_HEX, getRoleColor} from '@app/features/app/components/dialogs/shared/PermissionComponents';
import {DropIndicator} from '@app/features/app/components/layout/DropIndicator';
import {useContextMenuHoverState} from '@app/features/app/hooks/useContextMenuHoverState';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import styles from '@app/features/guild/components/modals/guild_tabs/GuildRolesTab.module.css';
import {
	ROLE_DND_TYPE,
	type RoleDragItem,
} from '@app/features/guild/components/modals/guild_tabs/guild_roles_tab/shared';
import type {RoleMovePreview} from '@app/features/guild/components/modals/guild_tabs/RoleMoveOperation';
import {
	canRoleDropOnTarget,
	type RoleReorderAccess,
	type RoleReorderTarget,
	selectRoleReorderResolution,
} from '@app/features/guild/components/modals/guild_tabs/RoleReorderStateMachine';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import {openRoleContextMenu} from '@app/features/ui/action_menu/RoleContextMenu';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {LockIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import type {ConnectableElement} from 'react-dnd';
import {useDrag, useDrop} from 'react-dnd';
import {getEmptyImage} from 'react-dnd-html5-backend';

const YOU_CANNOT_EDIT_THIS_ROLE_BECAUSE_IT_IS_DESCRIPTOR = msg({
	message: 'You cannot edit this role because it is your highest role or above you',
	comment:
		'Tooltip on a disabled role row in the community roles settings tab. Explains the role hierarchy rule: you cannot edit roles at or above your own highest role.',
});

interface RoleItemProps {
	role: GuildRole;
	isSelected: boolean;
	isLocked: boolean;
	isGuildOwner: boolean;
	isTerminal: boolean;
	canManageRoles: boolean;
	onClick: () => void;
	onDuplicate: (roleId: string) => void;
	onDelete: (roleId: string) => void;
	onEvaluateMove: (
		draggedRoleId: string,
		targetRoleId: string | null,
		position: 'before' | 'after',
	) => RoleMovePreview | null;
	onCommitMove: (preview: RoleMovePreview) => void;
}

export const RoleItem: React.FC<RoleItemProps> = observer(
	({
		role,
		isSelected,
		isLocked,
		isGuildOwner,
		isTerminal,
		canManageRoles,
		onClick,
		onDuplicate,
		onDelete,
		onEvaluateMove,
		onCommitMove,
	}) => {
		const {i18n} = useLingui();
		const elementRef = useRef<HTMLButtonElement | null>(null);
		const [dropIndicator, setDropIndicator] = useState<{position: 'top' | 'bottom'; isValid: boolean} | null>(null);
		const contextMenuOpen = useContextMenuHoverState(elementRef, !MobileLayout.enabled);
		const canDelete = canManageRoles && !role.isEveryone && !isLocked;
		const handleContextMenu = useCallback(
			(event: React.MouseEvent<HTMLButtonElement>) => {
				openRoleContextMenu(event, role.id, {
					canDuplicate: canManageRoles,
					onDuplicate: () => onDuplicate(role.id),
					canDelete,
					onDelete: () => onDelete(role.id),
				});
			},
			[role.id, canManageRoles, canDelete, onDuplicate, onDelete],
		);
		const dragItem = useMemo<RoleDragItem>(
			() => ({type: ROLE_DND_TYPE, id: role.id, isEveryone: role.isEveryone, isLocked}),
			[role.id, role.isEveryone, isLocked],
		);
		const reorderTarget = useMemo<RoleReorderTarget>(
			() => ({
				role: {
					id: role.id,
					isEveryone: role.isEveryone,
					isLocked,
				},
				isTerminal,
			}),
			[role.id, role.isEveryone, isLocked, isTerminal],
		);
		const reorderAccess = useMemo<RoleReorderAccess>(
			() => ({
				canManageRoles,
				isGuildOwner,
			}),
			[canManageRoles, isGuildOwner],
		);
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
		useEffect(() => {
			preview(getEmptyImage(), {captureDraggingState: true});
		}, [preview]);
		const [{isOver}, dropRef] = useDrop(
			() => ({
				accept: ROLE_DND_TYPE,
				canDrop: (item: RoleDragItem) => canRoleDropOnTarget(item, reorderTarget, reorderAccess),
				hover: (item: RoleDragItem, monitor) => {
					const node = elementRef.current;
					if (!node) {
						setDropIndicator(null);
						return;
					}
					const clientOffset = monitor.getClientOffset?.();
					if (!clientOffset) {
						setDropIndicator(null);
						return;
					}
					const resolution = selectRoleReorderResolution(
						item,
						reorderTarget,
						reorderAccess,
						clientOffset,
						node.getBoundingClientRect(),
					);
					if (!resolution.indicator) {
						setDropIndicator(null);
						return;
					}
					const previewResult = resolution.intent
						? onEvaluateMove(item.id, resolution.intent.result.targetRoleId, resolution.intent.result.position)
						: null;
					setDropIndicator({
						position: resolution.indicator.position,
						isValid: resolution.indicator.isValid && previewResult !== null,
					});
				},
				drop: (item: RoleDragItem, monitor) => {
					const node = elementRef.current;
					if (!node) {
						setDropIndicator(null);
						return;
					}
					const clientOffset = monitor.getClientOffset?.();
					if (!clientOffset) {
						setDropIndicator(null);
						return;
					}
					const intent = selectRoleReorderResolution(
						item,
						reorderTarget,
						reorderAccess,
						clientOffset,
						node.getBoundingClientRect(),
					).intent;
					const previewResult = intent
						? onEvaluateMove(item.id, intent.result.targetRoleId, intent.result.position)
						: null;
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
			[onCommitMove, onEvaluateMove, reorderAccess, reorderTarget],
		);
		useEffect(() => {
			if (!isOver) setDropIndicator(null);
		}, [isOver]);
		const dragConnectorRef = useCallback(
			(node: ConnectableElement | null) => {
				dragRef(node);
			},
			[dragRef],
		);
		const dropConnectorRef = useCallback(
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
			<div className={styles.itemWrap} data-flx="guild.guild-tabs.guild-roles-tab.role-item.item-wrap">
				{dropIndicator && (
					<DropIndicator
						position={dropIndicator.position}
						isValid={dropIndicator.isValid}
						data-flx="guild.guild-tabs.guild-roles-tab.role-item.drop-indicator"
					/>
				)}
				<button
					type="button"
					ref={mergedRef}
					className={clsx(
						styles.overwriteItem,
						styles.roleButton,
						{[styles.overwriteItemSelected]: isSelected},
						contextMenuOpen && styles.overwriteItemHovered,
						isDragging && styles.dragging,
						!canDrag && styles.noDrag,
					)}
					onClick={onClick}
					onContextMenu={handleContextMenu}
					data-flx="guild.guild-tabs.guild-roles-tab.role-item.overwrite-item.click.button"
				>
					<div
						className={styles.roleDot}
						style={{backgroundColor: role.color === 0 ? DEFAULT_ROLE_COLOR_HEX : getRoleColor(role.color)}}
						data-flx="guild.guild-tabs.guild-roles-tab.role-item.role-dot"
					/>
					<span className={styles.overwriteName} data-flx="guild.guild-tabs.guild-roles-tab.role-item.overwrite-name">
						{role.name || '\u00A0'}
					</span>
					{isLocked && (
						<Tooltip
							text={i18n._(YOU_CANNOT_EDIT_THIS_ROLE_BECAUSE_IT_IS_DESCRIPTOR)}
							data-flx="guild.guild-tabs.guild-roles-tab.role-item.tooltip"
						>
							<LockIcon className={styles.lockIcon} data-flx="guild.guild-tabs.guild-roles-tab.role-item.lock-icon" />
						</Tooltip>
					)}
				</button>
			</div>
		);
	},
);
