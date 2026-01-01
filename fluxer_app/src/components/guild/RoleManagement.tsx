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
import {PlusIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as GuildMemberActionCreators from '~/actions/GuildMemberActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {GuildSettingsModal} from '~/components/modals/GuildSettingsModal';
import profileStyles from '~/components/popouts/UserProfilePopout.module.css';
import itemStyles from '~/components/uikit/ContextMenu/items/MenuItems.module.css';
import {MenuGroup} from '~/components/uikit/ContextMenu/MenuGroup';
import {MenuItem} from '~/components/uikit/ContextMenu/MenuItem';
import {MenuItemCheckbox} from '~/components/uikit/ContextMenu/MenuItemCheckbox';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {MenuBottomSheet, type MenuGroupType} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useRoleHierarchy} from '~/hooks/useRoleHierarchy';
import type {GuildRoleRecord} from '~/records/GuildRoleRecord';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildSettingsModalStore from '~/stores/GuildSettingsModalStore';
import GuildStore from '~/stores/GuildStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import * as ColorUtils from '~/utils/ColorUtils';
import styles from './RoleManagement.module.css';

const RoleBadge: React.FC<{
	role: GuildRoleRecord;
	canRemove: boolean;
	guildId: string;
	userId: string;
}> = observer(function RoleBadge({role, canRemove, guildId, userId}) {
	const {t} = useLingui();
	const roleColor = ColorUtils.int2rgb(role.color);
	const iconColor = ColorUtils.getBestContrastColor(role.color);

	const handleRemoveRole = () => {
		GuildMemberActionCreators.removeRole(guildId, userId, role.id);
	};

	const roleIndicator = <span className={styles.roleIndicator} style={{backgroundColor: roleColor}} />;

	return (
		<div key={role.id} className={clsx(styles.roleBadge, profileStyles.role)}>
			{canRemove ? (
				<Tooltip text={t`Remove role`} position="top">
					<FocusRing offset={-2}>
						<button type="button" className={styles.roleRemoveButton} onClick={handleRemoveRole}>
							<XIcon
								weight="regular"
								className={clsx(styles.roleRemoveIconContainer, profileStyles.roleRemoveIcon)}
								style={{color: iconColor}}
							/>
							{roleIndicator}
						</button>
					</FocusRing>
				</Tooltip>
			) : (
				<div className={styles.roleRemoveButtonContainer}>{roleIndicator}</div>
			)}

			<div aria-hidden={true} className={styles.roleName}>
				{role.name}
			</div>
		</div>
	);
});

export const RoleList: React.FC<{
	guildId: string;
	userId: string;
	roles: Array<GuildRoleRecord>;
	canManage: boolean;
}> = observer(function RoleList({guildId, userId, roles, canManage}) {
	const {canManageRole} = useRoleHierarchy(GuildStore.getGuild(guildId));

	if (roles.length === 0 && !canManage) {
		return null;
	}

	return (
		<div className={styles.roleListContainer}>
			{roles.map((role) => {
				const canRemoveRole =
					canManage &&
					canManageRole({
						id: role.id,
						position: role.position,
						permissions: role.permissions,
					});

				return <RoleBadge key={role.id} role={role} canRemove={canRemoveRole} guildId={guildId} userId={userId} />;
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
	const guild = GuildStore.getGuild(guildId);
	const currentMember = GuildMemberStore.getMember(guildId, userId);
	const {canManageRole} = useRoleHierarchy(guild);

	const handleOpenGuildSettings = React.useCallback(() => {
		onClose();
		if (GuildSettingsModalStore.navigateToTab(guildId, 'roles')) {
			return;
		}
		ModalActionCreators.push(modal(() => <GuildSettingsModal guildId={guildId} initialTab="roles" />));
	}, [guildId, onClose]);

	const allRoles = React.useMemo(() => {
		if (!guild) return [];
		return Object.values(guild.roles)
			.filter((role) => !role.isEveryone)
			.sort((a, b) => b.position - a.position)
			.map((role) => ({
				role,
				canManage: canManageRole({id: role.id, position: role.position, permissions: role.permissions}),
			}));
	}, [guild, canManageRole]);

	const handleToggleRole = React.useCallback(
		async (roleId: string, hasRole: boolean, canManage: boolean) => {
			if (!canManage) return;
			if (hasRole) {
				await GuildMemberActionCreators.removeRole(guildId, userId, roleId);
			} else {
				await GuildMemberActionCreators.addRole(guildId, userId, roleId);
			}
		},
		[guildId, userId],
	);

	if (allRoles.length === 0) {
		return (
			<MenuGroup>
				<MenuItem onClick={handleOpenGuildSettings}>
					<Trans>No roles yet, add roles in Community Settings</Trans>
				</MenuItem>
			</MenuGroup>
		);
	}

	return (
		<MenuGroup>
			{allRoles.map(({role, canManage}) => {
				const hasRole = currentMember?.roles.has(role.id) ?? false;
				return (
					<MenuItemCheckbox
						key={role.id}
						checked={hasRole}
						disabled={!canManage}
						onChange={() => handleToggleRole(role.id, hasRole, canManage)}
						closeOnChange={false}
					>
						<div className={itemStyles.roleContainer}>
							<div className={itemStyles.roleIcon} style={{backgroundColor: ColorUtils.int2rgb(role.color)}} />
							<span className={clsx(itemStyles.roleName, !canManage && itemStyles.roleDisabled)}>{role.name}</span>
						</div>
					</MenuItemCheckbox>
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

const ManageRolesBottomSheet: React.FC<ManageRolesBottomSheetProps> = observer(function ManageRolesBottomSheet({
	isOpen,
	onClose,
	guildId,
	userId,
}) {
	const {t} = useLingui();
	const guild = GuildStore.getGuild(guildId);
	const currentMember = GuildMemberStore.getMember(guildId, userId);
	const {canManageRole} = useRoleHierarchy(guild);

	const handleOpenGuildSettings = React.useCallback(() => {
		onClose();
		if (GuildSettingsModalStore.navigateToTab(guildId, 'roles')) {
			return;
		}
		ModalActionCreators.push(modal(() => <GuildSettingsModal guildId={guildId} initialTab="roles" />));
	}, [guildId, onClose]);

	const allRoles = React.useMemo(() => {
		if (!guild) return [];
		return Object.values(guild.roles)
			.filter((role) => !role.isEveryone)
			.sort((a, b) => b.position - a.position)
			.map((role) => ({
				role,
				canManage: canManageRole({id: role.id, position: role.position, permissions: role.permissions}),
			}));
	}, [guild, canManageRole]);

	const menuGroups: Array<MenuGroupType> = React.useMemo(() => {
		if (allRoles.length === 0) {
			return [
				{
					items: [
						{
							icon: <PlusIcon weight="bold" className={styles.iconSize} />,
							label: t`No roles yet, add roles in Community Settings`,
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
								await GuildMemberActionCreators.removeRole(guildId, userId, role.id);
							} else {
								await GuildMemberActionCreators.addRole(guildId, userId, role.id);
							}
						},
						icon: (
							<div className={styles.roleColorIndicator} style={{backgroundColor: ColorUtils.int2rgb(role.color)}} />
						),
					};
				}),
			},
		];
	}, [allRoles, currentMember, guildId, userId, handleOpenGuildSettings]);

	return <MenuBottomSheet isOpen={isOpen} onClose={onClose} title={t`Manage Roles`} groups={menuGroups} />;
});

export const AddRoleButton: React.FC<{
	guildId: string;
	userId: string;
	variant?: 'pill' | 'icon' | 'mobile';
}> = observer(function AddRoleButton({guildId, userId, variant = 'icon'}) {
	const {t} = useLingui();
	const guild = GuildStore.getGuild(guildId);
	const member = GuildMemberStore.getMember(guildId, userId);
	const isMobile = MobileLayoutStore.enabled;
	const [showBottomSheet, setShowBottomSheet] = React.useState(false);

	const handleClick = React.useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			if (isMobile || variant === 'mobile') {
				setShowBottomSheet(true);
			} else {
				ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
					<ManageRolesMenuContent guildId={guildId} userId={userId} onClose={onClose} />
				));
			}
		},
		[guildId, userId, isMobile, variant],
	);

	if (!guild || !member) return null;

	const hasRoles = Object.values(guild.roles).some((r) => !r.isEveryone);
	if (!hasRoles) return null;

	if (variant === 'mobile') {
		return (
			<>
				<button type="button" className={styles.addRoleButtonMobile} onClick={handleClick}>
					<PlusIcon weight="bold" className={styles.iconSizeMobile} />
					<span className={styles.addRoleLabelMobile}>
						<Trans>Add Role</Trans>
					</span>
				</button>
				<ManageRolesBottomSheet
					isOpen={showBottomSheet}
					onClose={() => setShowBottomSheet(false)}
					guildId={guildId}
					userId={userId}
				/>
			</>
		);
	}

	const button = (
		<FocusRing offset={-2}>
			<button type="button" className={clsx(styles.addRoleButton, styles.addRoleButtonIcon)} onClick={handleClick}>
				<PlusIcon weight="bold" className={styles.iconSize} />
				{variant === 'pill' && (
					<span className={styles.addRoleLabel}>
						<Trans>Add Role</Trans>
					</span>
				)}
			</button>
		</FocusRing>
	);

	if (variant === 'icon') {
		return <Tooltip text={t`Add Role`}>{button}</Tooltip>;
	}

	return button;
});
