// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/guild/components/modals/guild_tabs/GuildRolesTab.module.css';
import {RoleItem} from '@app/features/guild/components/modals/guild_tabs/guild_roles_tab/RoleItem';
import {
	applyRoleUpdate,
	ROLE_DND_TYPE,
	type RoleUpdate,
} from '@app/features/guild/components/modals/guild_tabs/guild_roles_tab/shared';
import type {RoleMovePreview} from '@app/features/guild/components/modals/guild_tabs/RoleMoveOperation';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import {ROLES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {useDragAutoScroll} from '@app/features/ui/hooks/useDragAutoScroll';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowsDownUpIcon, PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useRef} from 'react';
import {DndProvider, useDragLayer} from 'react-dnd';
import {HTML5Backend} from 'react-dnd-html5-backend';

const RoleDragAutoScroll: React.FC<{listRef: React.RefObject<HTMLDivElement | null>}> = ({listRef}) => {
	const isDraggingRole = useDragLayer((monitor) => monitor.isDragging() && monitor.getItemType() === ROLE_DND_TYPE);
	const getScrollElement = useCallback(() => {
		const list = listRef.current;
		if (list == null) return null;
		return list.closest<HTMLElement>('[data-fluxer-scroll-container="true"]');
	}, [listRef]);
	useDragAutoScroll({active: isDraggingRole, getScrollElement});
	return null;
};

interface RoleSidebarProps {
	roles: Array<GuildRole>;
	hoistedRoles: Array<GuildRole>;
	selectedRoleId: string | null;
	isGuildOwner: boolean;
	canManageRoles: boolean;
	hoistOrderMode: boolean;
	hasCustomHoistOrder: boolean;
	roleUpdates: Map<string, RoleUpdate>;
	isRoleLocked: (role: GuildRole) => boolean;
	onSelectRole: (roleId: string) => void;
	onCreateRole: () => void;
	onEnterHoistOrderMode: () => void;
	onExitHoistOrderMode: () => void;
	onResetHoistOrder: () => void;
	onEvaluateRoleMove: (
		draggedRoleId: string,
		targetRoleId: string | null,
		position: 'before' | 'after',
	) => RoleMovePreview | null;
	onRoleDrop: (preview: RoleMovePreview) => void;
	onEvaluateHoistMove: (
		draggedRoleId: string,
		targetRoleId: string | null,
		position: 'before' | 'after',
	) => RoleMovePreview | null;
	onHoistDrop: (preview: RoleMovePreview) => void;
}

export const RoleSidebar: React.FC<RoleSidebarProps> = observer(
	({
		roles,
		hoistedRoles,
		selectedRoleId,
		isGuildOwner,
		canManageRoles,
		hoistOrderMode,
		hasCustomHoistOrder,
		roleUpdates,
		isRoleLocked,
		onSelectRole,
		onCreateRole,
		onEnterHoistOrderMode,
		onExitHoistOrderMode,
		onResetHoistOrder,
		onEvaluateRoleMove,
		onRoleDrop,
		onEvaluateHoistMove,
		onHoistDrop,
	}) => {
		const {i18n} = useLingui();
		const listRef = useRef<HTMLDivElement>(null);
		if (hoistOrderMode) {
			return (
				<div data-flx="guild.guild-tabs.guild-roles-tab.sidebar-content.div">
					<div
						className={styles.leftTitle}
						style={{padding: `${remFromPx(6)} ${remFromPx(8)}`}}
						data-flx="guild.guild-tabs.guild-roles-tab.sidebar-content.left-title"
					>
						<Trans>Hoist order</Trans>
					</div>
					<div
						style={{
							padding: `0 ${remFromPx(8)} ${remFromPx(8)} ${remFromPx(8)}`,
							display: 'flex',
							gap: remFromPx(8),
							flexDirection: 'column',
						}}
						data-flx="guild.guild-tabs.guild-roles-tab.sidebar-content.div--2"
					>
						<Button
							variant="secondary"
							small={true}
							onClick={onExitHoistOrderMode}
							data-flx="guild.guild-tabs.guild-roles-tab.sidebar-content.button.set-hoist-order-mode"
						>
							<Trans>Back to roles</Trans>
						</Button>
						{hasCustomHoistOrder && (
							<Button
								variant="secondary"
								small={true}
								onClick={onResetHoistOrder}
								disabled={!canManageRoles}
								data-flx="guild.guild-tabs.guild-roles-tab.sidebar-content.button.reset-hoist-order"
							>
								<Trans>Reset to default</Trans>
							</Button>
						)}
					</div>
					<div
						ref={listRef}
						style={{padding: `0 ${remFromPx(8)} ${remFromPx(8)} ${remFromPx(8)}`}}
						data-flx="guild.guild-tabs.guild-roles-tab.sidebar-content.div--3"
					>
						<p
							className={styles.subtleText}
							style={{marginBottom: remFromPx(8)}}
							data-flx="guild.guild-tabs.guild-roles-tab.sidebar-content.subtle-text"
						>
							<Trans>Drag roles to customize the order they appear in the member list.</Trans>
						</p>
						<DndProvider
							backend={HTML5Backend}
							data-flx="guild.guild-tabs.guild-roles-tab.sidebar-content.dnd-provider"
						>
							<RoleDragAutoScroll listRef={listRef} />
							{hoistedRoles.map((role, index) => {
								const roleWithUpdates = applyRoleUpdate(role, roleUpdates.get(role.id));
								return (
									<RoleItem
										key={role.id}
										role={roleWithUpdates}
										isSelected={selectedRoleId === role.id}
										isLocked={isRoleLocked(role)}
										isGuildOwner={isGuildOwner}
										isTerminal={index === hoistedRoles.length - 1}
										canManageRoles={canManageRoles}
										onClick={() => onSelectRole(role.id)}
										onEvaluateMove={onEvaluateHoistMove}
										onCommitMove={onHoistDrop}
										data-flx="guild.guild-tabs.guild-roles-tab.sidebar-content.role-item.set-selected-role-id"
									/>
								);
							})}
							{hoistedRoles.length === 0 && (
								<p
									className={styles.subtleText}
									data-flx="guild.guild-tabs.guild-roles-tab.sidebar-content.subtle-text--2"
								>
									<Trans>No hoisted roles. Enable "Show this role separately" on a role to see it here.</Trans>
								</p>
							)}
						</DndProvider>
					</div>
				</div>
			);
		}
		return (
			<div data-flx="guild.guild-tabs.guild-roles-tab.sidebar-content.div--4">
				<div
					className={styles.leftTitle}
					style={{padding: `${remFromPx(6)} ${remFromPx(8)}`}}
					data-flx="guild.guild-tabs.guild-roles-tab.sidebar-content.left-title--2"
				>
					{i18n._(ROLES_DESCRIPTOR)}
				</div>
				<div
					style={{
						padding: `0 ${remFromPx(8)} ${remFromPx(8)} ${remFromPx(8)}`,
						display: 'flex',
						gap: remFromPx(8),
						flexDirection: 'column',
					}}
					data-flx="guild.guild-tabs.guild-roles-tab.sidebar-content.div--5"
				>
					<Button
						variant="secondary"
						small={true}
						leftIcon={
							<PlusIcon
								size={remFromPx(18)}
								weight="bold"
								data-flx="guild.guild-tabs.guild-roles-tab.sidebar-content.plus-icon"
							/>
						}
						onClick={onCreateRole}
						disabled={!canManageRoles}
						data-flx="guild.guild-tabs.guild-roles-tab.sidebar-content.button.create-role"
					>
						<Trans>Create role</Trans>
					</Button>
					{hoistedRoles.length > 0 && canManageRoles && (
						<Button
							variant="secondary"
							small={true}
							leftIcon={
								<ArrowsDownUpIcon
									size={remFromPx(18)}
									weight="bold"
									data-flx="guild.guild-tabs.guild-roles-tab.sidebar-content.arrows-down-up-icon"
								/>
							}
							onClick={onEnterHoistOrderMode}
							data-flx="guild.guild-tabs.guild-roles-tab.sidebar-content.button.set-hoist-order-mode--2"
						>
							<Trans>Custom hoist order</Trans>
						</Button>
					)}
				</div>
				<div
					ref={listRef}
					style={{padding: `0 ${remFromPx(8)} ${remFromPx(8)} ${remFromPx(8)}`}}
					data-flx="guild.guild-tabs.guild-roles-tab.sidebar-content.div--6"
				>
					<DndProvider
						backend={HTML5Backend}
						data-flx="guild.guild-tabs.guild-roles-tab.sidebar-content.dnd-provider--2"
					>
						<RoleDragAutoScroll listRef={listRef} />
						{roles.map((role: GuildRole, index) => {
							const roleWithUpdates = applyRoleUpdate(role, roleUpdates.get(role.id));
							return (
								<RoleItem
									key={role.id}
									role={roleWithUpdates}
									isSelected={selectedRoleId === role.id}
									isLocked={isRoleLocked(role)}
									isGuildOwner={isGuildOwner}
									isTerminal={index === roles.length - 1}
									canManageRoles={canManageRoles}
									onClick={() => onSelectRole(role.id)}
									onEvaluateMove={onEvaluateRoleMove}
									onCommitMove={onRoleDrop}
									data-flx="guild.guild-tabs.guild-roles-tab.sidebar-content.role-item.set-selected-role-id--2"
								/>
							);
						})}
					</DndProvider>
				</div>
			</div>
		);
	},
);
