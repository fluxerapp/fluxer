// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {deleteOneOrMany, executeQuery, fetchOne} from '@app/api/database/CassandraQueryExecution';
import type {UserSsoIdentityRow} from '@app/api/database/types/AuthTypes';
import {UserSsoIdentities} from '@app/api/Tables';

interface ConditionalInsertResult {
	'[applied]': boolean;
}

const FIND_SSO_IDENTITY_QUERY = UserSsoIdentities.select({
	where: [UserSsoIdentities.where.eq('provider_id'), UserSsoIdentities.where.eq('subject')],
	limit: 1,
});

export class SsoIdentityRepository {
	async findUserId(providerId: string, subject: string): Promise<UserID | null> {
		const row = await fetchOne<Pick<UserSsoIdentityRow, 'user_id'>>(
			FIND_SSO_IDENTITY_QUERY.bind({provider_id: providerId, subject}),
		);
		return row?.user_id ?? null;
	}

	async tryClaimIdentity(params: {
		providerId: string;
		subject: string;
		userId: UserID;
		claimedAt: Date;
	}): Promise<boolean> {
		const [result] = await executeQuery<ConditionalInsertResult>(
			UserSsoIdentities.insertIfNotExists({
				provider_id: params.providerId,
				subject: params.subject,
				user_id: params.userId,
				claimed_at: params.claimedAt,
			}),
		);
		if (!result || typeof result['[applied]'] !== 'boolean') {
			throw new Error('Unexpected database response for SSO identity claim');
		}
		return result['[applied]'];
	}

	async releaseIdentity(providerId: string, subject: string): Promise<void> {
		await deleteOneOrMany(UserSsoIdentities.deleteByPk({provider_id: providerId, subject}));
	}
}
