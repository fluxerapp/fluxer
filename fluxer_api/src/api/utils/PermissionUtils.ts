// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID} from '@app/api/BrandedTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';

interface PermissionsDiff {
	added: Array<string>;
	removed: Array<string>;
}

export function computePermissionsDiff(oldPermissions: bigint, newPermissions: bigint): PermissionsDiff {
	const added: Array<string> = [];
	const removed: Array<string> = [];
	for (const [name, value] of Object.entries(Permissions)) {
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

export async function hasPermission(
	gatewayService: IGatewayService,
	params: {
		guildId: GuildID;
		userId: UserID;
		permission: bigint;
		channelId?: ChannelID;
	},
): Promise<boolean> {
	return await gatewayService.checkPermission(params);
}
