// SPDX-License-Identifier: AGPL-3.0-or-later

import {DEFAULT_ROLE_COLOR_HEX, getRoleColor} from '@app/features/app/components/dialogs/shared/PermissionComponents';
import styles from '@app/features/guild/components/modals/guild_tabs/GuildRolesTab.module.css';
import {
	applyRoleUpdate,
	type RoleUpdate,
} from '@app/features/guild/components/modals/guild_tabs/guild_roles_tab/shared';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import {ROLES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {openRoleContextMenu} from '@app/features/ui/action_menu/RoleContextMenu';
import {Button} from '@app/features/ui/button/Button';
import {Trans, useLingui} from '@lingui/react/macro';
import {CaretRightIcon, LockIcon, PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface MobileRoleListProps {
	roles: Array<GuildRole>;
	roleUpdates: Map<string, RoleUpdate>;
	canManageRoles: boolean;
	isRoleLocked: (role: GuildRole) => boolean;
	onCreateRole: () => void;
	onSelectRole: (roleId: string) => void;
}

export const MobileRoleList: React.FC<MobileRoleListProps> = observer(
	({roles, roleUpdates, canManageRoles, isRoleLocked, onCreateRole, onSelectRole}) => {
		const {i18n} = useLingui();
		return (
			<div className={styles.container} data-flx="guild.guild-tabs.guild-roles-tab.container">
				<div className={styles.mobileRoleList} data-flx="guild.guild-tabs.guild-roles-tab.mobile-role-list">
					<div className={styles.mobileListHeader} data-flx="guild.guild-tabs.guild-roles-tab.mobile-list-header">
						<h2 className={styles.mobileListTitle} data-flx="guild.guild-tabs.guild-roles-tab.mobile-list-title">
							{i18n._(ROLES_DESCRIPTOR)}
						</h2>
						<Button
							variant="secondary"
							small={true}
							leftIcon={
								<PlusIcon size={remFromPx(18)} weight="bold" data-flx="guild.guild-tabs.guild-roles-tab.plus-icon" />
							}
							onClick={onCreateRole}
							disabled={!canManageRoles}
							data-flx="guild.guild-tabs.guild-roles-tab.button.create-role"
						>
							<Trans>Create role</Trans>
						</Button>
					</div>
					<div className={styles.mobileRoles} data-flx="guild.guild-tabs.guild-roles-tab.mobile-roles">
						{roles.map((role: GuildRole) => {
							const roleWithUpdates = applyRoleUpdate(role, roleUpdates.get(role.id));
							const locked = isRoleLocked(role);
							return (
								<button
									key={role.id}
									type="button"
									className={styles.mobileRoleItem}
									onClick={() => onSelectRole(role.id)}
									onContextMenu={(event) => openRoleContextMenu(event, role.id)}
									data-flx="guild.guild-tabs.guild-roles-tab.mobile-role-item.mobile-role-select.button"
								>
									<div
										className={styles.roleDot}
										style={{
											backgroundColor:
												roleWithUpdates.color === 0 ? DEFAULT_ROLE_COLOR_HEX : getRoleColor(roleWithUpdates.color),
										}}
										data-flx="guild.guild-tabs.guild-roles-tab.role-dot"
									/>
									<span className={styles.mobileRoleName} data-flx="guild.guild-tabs.guild-roles-tab.mobile-role-name">
										{roleWithUpdates.name || '\u00A0'}
									</span>
									{locked && (
										<LockIcon className={styles.lockIcon} data-flx="guild.guild-tabs.guild-roles-tab.lock-icon" />
									)}
									<CaretRightIcon
										className={styles.mobileRoleChevron}
										size={remFromPx(20)}
										weight="bold"
										data-flx="guild.guild-tabs.guild-roles-tab.mobile-role-chevron"
									/>
								</button>
							);
						})}
					</div>
				</div>
			</div>
		);
	},
);
