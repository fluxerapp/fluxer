// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {UserRow} from '@app/api/database/types/UserTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {User} from '@app/api/models/User';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {dispatchUserUpdate} from '@app/api/user/UserMappers';

export async function bumpGiftInventory(
	repository: Pick<UserRepository, 'findUnique' | 'patchUpsert'>,
	gateway: IGatewayService,
	userId: UserID,
	extraPatch: Partial<UserRow> = {},
): Promise<User | null> {
	const user = await repository.findUnique(userId);
	if (!user) return null;
	const updated = await repository.patchUpsert(
		userId,
		{...extraPatch, gift_inventory_server_seq: (user.giftInventoryServerSeq ?? 0) + 1},
		user.toRow(),
	);
	await dispatchUserUpdate(gateway, updated);
	return updated;
}
