// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID} from '@app/api/BrandedTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {ThreadPermissionFlags} from '@fluxer/constants/src/ThreadPermissionUtils';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';

interface PermissionsDiff {
	added: Array<string>;
	removed: Array<string>;
}

export function computePermissionsDiff(
	oldPermissions: bigint,
	newPermissions: bigint,
	opts?: {threads?: boolean},
): PermissionsDiff {
	const added: Array<string> = [];
	const removed: Array<string> = [];
	const entries = opts?.threads
		? [...Object.entries(Permissions), ...Object.entries(ThreadPermissionFlags)]
		: Object.entries(Permissions);
	for (const [name, value] of entries) {
		const hadPermission = (oldPermissions & value) !== 0n;
		const hasPermission = (newPermissions & value) !== 0n;
		if (!hadPermission && hasPermission) {
			added.push(name);
		} else if (hadPermission && !hasPermission) {
			removed.push(name);
		}
	}
	return {added, removed};
}

export async function requirePermission(
	gatewayService: IGatewayService,
	params: {
		guildId: GuildID;
		userId: UserID;
		permission: bigint;
		channelId?: ChannelID;
	},
): Promise<void> {
	const result = await gatewayService.checkPermission(params);
	if (!result) {
		throw new MissingPermissionsError();
	}
}

interface OverwriteBits {
	type?: number;
	allow: bigint;
	deny: bigint;
}

export function overwriteAppliesToTarget(
	overwrite: {id: bigint; type: number},
	guildId: bigint,
	roleIds: ReadonlySet<bigint>,
): boolean {
	const isRole = overwrite.id === guildId || roleIds.has(overwrite.id);
	return overwrite.type === 0 ? isRole : !isRole;
}

export function overwriteGrantedBits(
	before: OverwriteBits | null | undefined,
	after: OverwriteBits | null | undefined,
	beforeApplies = true,
): bigint {
	if (before && after && before.type !== undefined && after.type !== undefined && before.type !== after.type) {
		return after.allow | (beforeApplies ? before.deny : 0n);
	}
	const beforeAllow = before?.allow ?? 0n;
	const beforeDeny = before?.deny ?? 0n;
	const afterAllow = after?.allow ?? 0n;
	const afterDeny = after?.deny ?? 0n;
	return (afterAllow & ~beforeAllow) | (beforeDeny & ~afterDeny);
}
