// SPDX-License-Identifier: AGPL-3.0-or-later

interface RolePositionPayloadItem {
	id: string;
	position: number;
}

interface RoleHoistPositionPayloadItem {
	id: string;
	hoist_position: number;
}

interface SubmittableRoleOrderParams {
	guildId: string;
	orderedRoleIds: ReadonlyArray<string>;
	isRoleLocked: (roleId: string) => boolean;
}

interface OrderedRoleIdsParams {
	guildId: string;
	orderedRoleIds: ReadonlyArray<string>;
}

function getNormalisedRoleOrderIds(params: OrderedRoleIdsParams): Array<string> {
	const {guildId, orderedRoleIds} = params;
	const seen = new Set<string>();
	const normalisedIds: Array<string> = [];
	for (const roleId of orderedRoleIds) {
		if (roleId === guildId) {
			continue;
		}
		if (seen.has(roleId)) {
			continue;
		}
		seen.add(roleId);
		normalisedIds.push(roleId);
	}
	return normalisedIds;
}

export function createSubmittableRoleOrderIds(params: SubmittableRoleOrderParams): Array<string> {
	const normalisedIds = getNormalisedRoleOrderIds(params);
	return normalisedIds.filter((roleId) => !params.isRoleLocked(roleId));
}

export function createRoleOrderPayload(params: OrderedRoleIdsParams): Array<RolePositionPayloadItem> {
	const orderedIds = getNormalisedRoleOrderIds(params);
	return orderedIds.map((id, index) => ({
		id,
		position: orderedIds.length - index,
	}));
}

export function createRoleHoistOrderPayload(params: OrderedRoleIdsParams): Array<RoleHoistPositionPayloadItem> {
	const orderedIds = getNormalisedRoleOrderIds(params);
	return orderedIds.map((id, index) => ({
		id,
		hoist_position: orderedIds.length - index,
	}));
}
