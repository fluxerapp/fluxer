// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {
	deleteOneOrMany,
	executeConditional,
	executeGroupedBatches,
	fetchMany,
	fetchOne,
	upsertOne,
} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import type {WebAuthnCredentialRow} from '@app/api/database/types/AuthTypes';
import {WebAuthnCredential} from '@app/api/models/WebAuthnCredential';
import {WebAuthnCredentialLookup, WebAuthnCredentials} from '@app/api/Tables';
import {InvalidWebAuthnCredentialError} from '@fluxer/errors/src/domains/auth/InvalidWebAuthnCredentialError';

const FETCH_USER_ID_BY_CREDENTIAL_ID_CQL = WebAuthnCredentialLookup.selectCql({
	where: WebAuthnCredentialLookup.where.eq('credential_id'),
	limit: 1,
});
const FETCH_WEBAUTHN_CREDENTIALS_CQL = WebAuthnCredentials.selectCql({
	where: WebAuthnCredentials.where.eq('user_id'),
});
const FETCH_WEBAUTHN_CREDENTIAL_CQL = WebAuthnCredentials.selectCql({
	where: [WebAuthnCredentials.where.eq('user_id'), WebAuthnCredentials.where.eq('credential_id')],
	limit: 1,
});
const FETCH_WEBAUTHN_CREDENTIALS_FOR_USER_CQL = WebAuthnCredentials.selectCql({
	columns: ['credential_id'],
	where: WebAuthnCredentials.where.eq('user_id'),
});

export class WebAuthnRepository {
	async listWebAuthnCredentials(userId: UserID): Promise<Array<WebAuthnCredential>> {
		const credentials = await fetchMany<WebAuthnCredentialRow>(FETCH_WEBAUTHN_CREDENTIALS_CQL, {user_id: userId});
		return credentials.filter((cred) => cred.public_key).map((cred) => new WebAuthnCredential(cred));
	}

	async getWebAuthnCredential(userId: UserID, credentialId: string): Promise<WebAuthnCredential | null> {
		const cred = await fetchOne<WebAuthnCredentialRow>(FETCH_WEBAUTHN_CREDENTIAL_CQL, {
			user_id: userId,
			credential_id: credentialId,
		});
		if (!cred?.public_key) {
			return null;
		}
		return new WebAuthnCredential(cred);
	}

	async createWebAuthnCredential(
		userId: UserID,
		credentialId: string,
		publicKey: Buffer,
		counter: bigint,
		transports: Set<string> | null,
		name: string,
		rpId: string | null,
	): Promise<void> {
		const credentialData = {
			user_id: userId,
			credential_id: credentialId,
			public_key: publicKey,
			counter: counter,
			transports: transports,
			name: name,
			created_at: new Date(),
			last_used_at: null,
			version: 1 as const,
			rp_id: rpId,
			superseded_by: null,
		};
		await this.claimCredentialLookup(userId, credentialId);
		await upsertOne(WebAuthnCredentials.insert(credentialData));
	}

	private async claimCredentialLookup(userId: UserID, credentialId: string): Promise<void> {
		const lookup = {credential_id: credentialId, user_id: userId};
		if (await executeConditional(WebAuthnCredentialLookup.insertIfNotExists(lookup))) {
			return;
		}
		const ownerId = await this.getUserIdByCredentialId(credentialId);
		if (ownerId === userId) {
			return;
		}
		if (ownerId !== null && (await this.getWebAuthnCredential(ownerId, credentialId))) {
			throw new InvalidWebAuthnCredentialError();
		}
		const claimed =
			ownerId === null
				? await executeConditional(WebAuthnCredentialLookup.insertIfNotExists(lookup))
				: await executeConditional(
						WebAuthnCredentialLookup.conditionalPatchByPk(
							{credential_id: credentialId},
							{user_id: Db.set(userId)},
							{user_id: ownerId},
						),
					);
		if (!claimed) {
			throw new InvalidWebAuthnCredentialError();
		}
	}

	private async releaseCredentialLookup(userId: UserID, credentialId: string): Promise<void> {
		await executeConditional(
			WebAuthnCredentialLookup.conditionalDeleteByPk({credential_id: credentialId}, {user_id: userId}),
		);
	}

	async updateWebAuthnCredentialCounter(userId: UserID, credentialId: string, counter: bigint): Promise<void> {
		await upsertOne(
			WebAuthnCredentials.patchByPk(
				{user_id: userId, credential_id: credentialId},
				{
					counter: Db.set(counter),
				},
			),
		);
	}

	async updateWebAuthnCredentialLastUsed(userId: UserID, credentialId: string): Promise<void> {
		await upsertOne(
			WebAuthnCredentials.patchByPk(
				{user_id: userId, credential_id: credentialId},
				{
					last_used_at: Db.set(new Date()),
				},
			),
		);
	}

	async updateWebAuthnCredentialName(userId: UserID, credentialId: string, name: string): Promise<void> {
		await upsertOne(
			WebAuthnCredentials.patchByPk(
				{user_id: userId, credential_id: credentialId},
				{
					name: Db.set(name),
				},
			),
		);
	}

	async setWebAuthnCredentialSupersededBy(userId: UserID, credentialId: string, supersededBy: string): Promise<void> {
		await upsertOne(
			WebAuthnCredentials.patchByPk(
				{user_id: userId, credential_id: credentialId},
				{
					superseded_by: Db.set(supersededBy),
				},
			),
		);
	}

	async deleteWebAuthnCredential(userId: UserID, credentialId: string): Promise<void> {
		await deleteOneOrMany(
			WebAuthnCredentials.deleteByPk({
				user_id: userId,
				credential_id: credentialId,
			}),
		);
		await this.releaseCredentialLookup(userId, credentialId);
	}

	async getUserIdByCredentialId(credentialId: string): Promise<UserID | null> {
		const row = await fetchOne<{
			credential_id: string;
			user_id: UserID;
		}>(FETCH_USER_ID_BY_CREDENTIAL_ID_CQL, {
			credential_id: credentialId,
		});
		return row?.user_id ?? null;
	}

	async deleteAllWebAuthnCredentials(userId: UserID): Promise<void> {
		const credentials = await fetchMany<{
			credential_id: string;
		}>(FETCH_WEBAUTHN_CREDENTIALS_FOR_USER_CQL, {
			user_id: userId,
		});
		await executeGroupedBatches(
			credentials.map((cred) => [WebAuthnCredentials.deleteByPk({user_id: userId, credential_id: cred.credential_id})]),
		);
		for (const cred of credentials) {
			await this.releaseCredentialLookup(userId, cred.credential_id);
		}
	}
}
