// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RoleID} from '@app/api/BrandedTypes';
import type {GuildRole} from '@app/api/models/GuildRole';

export function computeMovedIds<T extends string | bigint>(before: ReadonlyArray<T>, after: ReadonlyArray<T>): Set<T> {
	const width = after.length + 1;
	const lengths = new Uint32Array((before.length + 1) * width);
	const lengthAt = (i: number, j: number): number => lengths[i * width + j] ?? 0;
	for (let i = before.length - 1; i >= 0; i--) {
		for (let j = after.length - 1; j >= 0; j--) {
			lengths[i * width + j] =
				before[i] === after[j] ? lengthAt(i + 1, j + 1) + 1 : Math.max(lengthAt(i + 1, j), lengthAt(i, j + 1));
		}
	}
	const keptIndices = new Set<number>();
	let i = 0;
	let j = 0;
	while (i < before.length && j < after.length) {
		if (before[i] === after[j]) {
			keptIndices.add(j);
			i++;
			j++;
		} else if (lengthAt(i + 1, j) >= lengthAt(i, j + 1)) {
			i++;
		} else {
			j++;
		}
	}
	const movedIds = new Set<T>();
	for (const [index, id] of after.entries()) {
		if (!keptIndices.has(index)) {
			movedIds.add(id);
		}
	}
	return movedIds;
}

export function getMemberListRoleOrderIds(roles: ReadonlyArray<GuildRole>, everyoneRoleId: RoleID): Array<RoleID> {
	return roles
		.filter((role) => role.isHoisted && role.id !== everyoneRoleId)
		.sort((a, b) => b.effectiveHoistPosition - a.effectiveHoistPosition || (a.id < b.id ? -1 : a.id > b.id ? 1 : 0))
		.map((role) => role.id);
}
