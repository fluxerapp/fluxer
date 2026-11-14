// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RoleID} from '@app/api/BrandedTypes';
import type {GuildSelfAssignableRoleSectionUdt} from '@app/api/database/types/GuildTypes';

export interface GuildSelfAssignableRole {
	readonly roleId: RoleID;
	readonly description: string;
}

export class GuildSelfAssignableRoleSection {
	readonly id: bigint;
	readonly content: string;
	readonly roles: ReadonlyArray<GuildSelfAssignableRole>;
	readonly maxRoles: number | null;
	readonly requiredRoleIds: ReadonlyArray<RoleID>;

	constructor(section: GuildSelfAssignableRoleSectionUdt) {
		this.id = section.section_id;
		this.content = section.content ?? '';
		this.roles = (section.roles ?? []).map((role) => ({roleId: role.role_id, description: role.description ?? ''}));
		this.maxRoles = section.max_roles ?? null;
		this.requiredRoleIds = section.required_role_ids ?? [];
	}

	withoutRole(roleId: RoleID): GuildSelfAssignableRoleSection {
		const udt = this.toUdt();
		return new GuildSelfAssignableRoleSection({
			...udt,
			roles: (udt.roles ?? []).filter((role) => role.role_id !== roleId),
		});
	}

	toUdt(): GuildSelfAssignableRoleSectionUdt {
		return {
			section_id: this.id,
			content: this.content,
			roles:
				this.roles.length > 0
					? this.roles.map((role) => ({role_id: role.roleId, description: role.description}))
					: null,
			max_roles: this.maxRoles,
			required_role_ids: this.requiredRoleIds.length > 0 ? [...this.requiredRoleIds] : null,
		};
	}
}

export function selfAssignableRoleSectionsToRow(
	sections: ReadonlyArray<GuildSelfAssignableRoleSection>,
): Array<GuildSelfAssignableRoleSectionUdt> | null {
	return sections.length > 0 ? sections.map((section) => section.toUdt()) : null;
}
