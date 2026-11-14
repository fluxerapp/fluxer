// SPDX-License-Identifier: AGPL-3.0-or-later

import {getRoleSwatchColor} from '@app/features/app/components/dialogs/shared/PermissionComponents';
import styles from '@app/features/guild/components/self_assignable_roles/SelfAssignableRoles.module.css';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import type React from 'react';

interface RoleChipLabelProps {
	readonly role: GuildRole | null;
	readonly name: string;
	readonly nameId?: string;
}

export const RoleChipLabel: React.FC<RoleChipLabelProps> = ({role, name, nameId}) => (
	<>
		{role && (
			<span
				className={styles.roleSwatch}
				style={{backgroundColor: getRoleSwatchColor(role)}}
				data-flx="guild.self-assignable-roles.self-assignable-roles-shared.role-chip-label.role-swatch"
			/>
		)}
		<span
			id={nameId}
			className={styles.roleChipName}
			data-flx="guild.self-assignable-roles.self-assignable-roles-shared.role-chip-label.role-chip-name"
		>
			{name}
		</span>
	</>
);

interface SelfAssignableRolesEmptyStateProps {
	readonly message: string;
	readonly children?: React.ReactNode;
}

export const SelfAssignableRolesEmptyState: React.FC<SelfAssignableRolesEmptyStateProps> = ({message, children}) => (
	<div
		className={styles.emptyState}
		data-flx="guild.self-assignable-roles.self-assignable-roles-shared.self-assignable-roles-empty-state.empty-state"
	>
		<p
			className={styles.emptyStateText}
			data-flx="guild.self-assignable-roles.self-assignable-roles-shared.self-assignable-roles-empty-state.empty-state-text"
		>
			{message}
		</p>
		{children}
	</div>
);
