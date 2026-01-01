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

import type {ChannelID, GuildID, UserID} from '~/BrandedTypes';
import {Permissions} from '~/Constants';
import {MissingPermissionsError} from '~/Errors';
import type {IGatewayService} from '~/infrastructure/IGatewayService';

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
	return gatewayService.checkPermission(params);
}
