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

import type {GuildRoleRecord} from '~/records/GuildRoleRecord';

interface RoleMoveOperation {
	roleId: string;
	precedingRoleId: string | null;
	position?: number;
}

interface RoleMovePreview {
	order: Array<GuildRoleRecord>;
	operation: RoleMoveOperation;
}

export const createRoleMovePreview = ({
	roles,
	draggedRoleId,
	targetRoleId,
	position,
	isRoleLocked,
}: {
	roles: ReadonlyArray<GuildRoleRecord>;
	draggedRoleId: string;
	targetRoleId: string | null;
	position: 'before' | 'after';
	isRoleLocked: (role: GuildRoleRecord) => boolean;
}): RoleMovePreview | null => {
	if (draggedRoleId === targetRoleId) return null;

	const draggedRole = roles.find((role) => role.id === draggedRoleId);
	if (!draggedRole) return null;

	if (draggedRole.isEveryone) return null;

	if (targetRoleId === null) {
		const draggedIndex = roles.findIndex((role) => role.id === draggedRoleId);
		if (draggedIndex === -1) return null;
		if (draggedIndex === 0) return null;

		for (let index = 0; index < draggedIndex; index++) {
			const role = roles[index];
			if (!role || role.id === draggedRoleId) continue;
			if (isRoleLocked(role)) {
				return null;
			}
		}

		const remaining = roles.filter((role) => role.id !== draggedRoleId);
		remaining.unshift(draggedRole);

		return {
			order: remaining,
			operation: {
				roleId: draggedRoleId,
				precedingRoleId: null,
			},
		};
	}

	const targetRole = roles.find((role) => role.id === targetRoleId);
	if (!targetRole) return null;

	if (targetRole.isEveryone && position === 'after') return null;

	const draggedIndex = roles.findIndex((role) => role.id === draggedRoleId);
	const targetIndex = roles.findIndex((role) => role.id === targetRoleId);
	if (draggedIndex === -1 || targetIndex === -1) return null;

	const destinationIndex = position === 'before' ? targetIndex : targetIndex + 1;
	const normalizedDestination = destinationIndex > draggedIndex ? destinationIndex - 1 : destinationIndex;

	if (normalizedDestination === draggedIndex) {
		return null;
	}

	if (normalizedDestination < draggedIndex) {
		for (let index = normalizedDestination; index < draggedIndex; index++) {
			const role = roles[index];
			if (!role || role.id === draggedRoleId) continue;
			if (isRoleLocked(role)) {
				return null;
			}
		}
	}

	const remaining = roles.filter((role) => role.id !== draggedRoleId);
	let insertIndex = destinationIndex;
	if (destinationIndex > draggedIndex) {
		insertIndex = destinationIndex - 1;
	}
	if (insertIndex < 0) insertIndex = 0;
	if (insertIndex > remaining.length) insertIndex = remaining.length;

	remaining.splice(insertIndex, 0, draggedRole);

	const precedingRole = insertIndex > 0 ? remaining[insertIndex - 1] : null;

	return {
		order: remaining,
		operation: {
			roleId: draggedRoleId,
			precedingRoleId: precedingRole ? precedingRole.id : null,
		},
	};
};
