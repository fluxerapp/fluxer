// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {deleteOneOrMany, executeQuery, fetchMany, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {UserSsoIdentityRow} from '@app/api/database/types/AuthTypes';
import {UserSsoIdentities, UserSsoIdentitiesByUser} from '@app/api/Tables';

interface ConditionalInsertResult {
	'[applied]': boolean;
}

const FIND_SSO_IDENTITY_QUERY = UserSsoIdentities.select({
	where: [UserSsoIdentities.where.eq('provider_id'), UserSsoIdentities.where.eq('subject')],
	limit: 1,
});

const LIST_PROVIDER_IDENTITIES_QUERY = UserSsoIdentities.select({
	where: UserSsoIdentities.where.eq('provider_id'),
});

const LIST_USER_IDENTITIES_QUERY = UserSsoIdentitiesByUser.select({
	where: UserSsoIdentitiesByUser.where.eq('user_id'),
});

export class SsoIdentityRepository {
	async findIdentity(providerId: string, subject: string): Promise<UserSsoIdentityRow | null> {
		return fetchOne<UserSsoIdentityRow>(FIND_SSO_IDENTITY_QUERY.bind({provider_id: providerId, subject}));
	}

	async findUserId(providerId: string, subject: string): Promise<UserID | null> {
		return (await this.findIdentity(providerId, subject))?.user_id ?? null;
	}

	async tryClaimIdentity(params: {
		providerId: string;
		subject: string;
		userId: UserID;
		claimedAt: Date;
	}): Promise<boolean> {
		const row: UserSsoIdentityRow = {
			provider_id: params.providerId,
			subject: params.subject,
			user_id: params.userId,
			claimed_at: params.claimedAt,
		};
		const [result] = await executeQuery<ConditionalInsertResult>(UserSsoIdentities.insertIfNotExists(row));
		if (!result || typeof result['[applied]'] !== 'boolean') {
			throw new Error('Unexpected database response for SSO identity claim');
		}
		if (result['[applied]']) {
			await this.indexIdentity(row);
		}
		return result['[applied]'];
	}

	async indexIdentity(row: UserSsoIdentityRow): Promise<void> {
		await upsertOne(UserSsoIdentitiesByUser.insert(row));
	}

	async releaseIdentity(providerId: string, subject: string): Promise<void> {
		const identity = await this.findIdentity(providerId, subject);
		await deleteOneOrMany(UserSsoIdentities.deleteByPk({provider_id: providerId, subject}));
		if (identity) {
			await this.unindexIdentity(identity);
		}
	}

	async listForUser(userId: UserID): Promise<Array<UserSsoIdentityRow>> {
		const indexed = await fetchMany<UserSsoIdentityRow>(LIST_USER_IDENTITIES_QUERY.bind({user_id: userId}));
		const identities: Array<UserSsoIdentityRow> = [];
		for (const entry of indexed) {
			const identity = await this.findIdentity(entry.provider_id, entry.subject);
			if (identity?.user_id === userId) {
				identities.push(identity);
			}
		}
		return identities;
	}

	async listForProvider(providerId: string): Promise<Array<UserSsoIdentityRow>> {
		return fetchMany<UserSsoIdentityRow>(LIST_PROVIDER_IDENTITIES_QUERY.bind({provider_id: providerId}));
	}

	async releaseIdentitiesForUser(userId: UserID): Promise<number> {
		const indexed = await fetchMany<UserSsoIdentityRow>(LIST_USER_IDENTITIES_QUERY.bind({user_id: userId}));
		let released = 0;
		for (const entry of indexed) {
			const identity = await this.findIdentity(entry.provider_id, entry.subject);
			if (identity?.user_id === userId) {
				await deleteOneOrMany(
					UserSsoIdentities.deleteByPk({provider_id: identity.provider_id, subject: identity.subject}),
				);
				released += 1;
			}
			await this.unindexIdentity({...entry, user_id: userId});
		}
		return released;
	}

	private async unindexIdentity(identity: UserSsoIdentityRow): Promise<void> {
		await deleteOneOrMany(
			UserSsoIdentitiesByUser.deleteByPk({
				user_id: identity.user_id,
				provider_id: identity.provider_id,
				subject: identity.subject,
			}),
		);
	}
}
