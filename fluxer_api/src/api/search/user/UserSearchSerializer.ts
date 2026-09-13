// SPDX-License-Identifier: AGPL-3.0-or-later

import type {User} from '@app/api/models/User';
import type {SearchableUser} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';

export function convertToSearchableUser(user: User): SearchableUser {
	const createdAt = Math.floor(snowflakeToDate(BigInt(user.id)).getTime() / 1000);
	const lastActiveAt = user.lastActiveAt ? Math.floor(user.lastActiveAt.getTime() / 1000) : null;
	const tempBannedUntil = user.tempBannedUntil ? Math.floor(user.tempBannedUntil.getTime() / 1000) : null;
	const pendingDeletionAt = user.pendingDeletionAt ? Math.floor(user.pendingDeletionAt.getTime() / 1000) : null;
	return {
		id: user.id.toString(),
		username: user.username,
		discriminator: user.discriminator,
		email: user.email,
		isBot: user.isBot,
		isSystem: user.isSystem,
		flags: user.flags.toString(),
		premiumType: user.premiumType,
		emailVerified: user.emailVerified,
		emailBounced: user.emailBounced,
		suspiciousActivityFlags: user.suspiciousActivityFlags,
		acls: Array.from(user.acls),
		createdAt,
		lastActiveAt,
		tempBannedUntil,
		pendingDeletionAt,
		stripeSubscriptionId: user.stripeSubscriptionId,
		stripeCustomerId: user.stripeCustomerId,
	};
}
