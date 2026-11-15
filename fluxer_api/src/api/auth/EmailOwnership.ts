// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {User} from '@app/api/models/User';
import {mapUserToPrivateResponse} from '@app/api/user/UserMappers';

type OwnershipContext = Pick<ApiContext, 'services'>;

export async function confirmEmailOwnership(ctx: OwnershipContext, user: User): Promise<User> {
	if (!user.email) return user;
	const updated = await ctx.services.users.patchUpsert(
		user.id,
		{email_verified: true, email_bounced: false},
		user.toRow(),
	);
	await ctx.services.gateway.dispatchPresence({
		userId: updated.id,
		event: 'USER_UPDATE',
		data: mapUserToPrivateResponse(updated),
	});
	return updated;
}
