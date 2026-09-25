// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {BatchBuilder, deleteOneOrMany, fetchMany, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import type {WebAuthnCredentialRow} from '@app/api/database/types/AuthTypes';
import {WebAuthnCredential} from '@app/api/models/WebAuthnCredential';
import {WebAuthnCredentialLookup, WebAuthnCredentials} from '@app/api/Tables';

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
		await upsertOne(WebAuthnCredentials.insert(credentialData));
		await upsertOne(
			WebAuthnCredentialLookup.insert({
				credential_id: credentialId,
				user_id: userId,
			}),
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
		await deleteOneOrMany(
			WebAuthnCredentialLookup.deleteByPk({
				credential_id: credentialId,
			}),
		);
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
		const batch = new BatchBuilder();
		for (const cred of credentials) {
			batch.addPrepared(
				WebAuthnCredentials.deleteByPk({
					user_id: userId,
					credential_id: cred.credential_id,
				}),
			);
			batch.addPrepared(
				WebAuthnCredentialLookup.deleteByPk({
					credential_id: cred.credential_id,
				}),
			);
		}
		await batch.execute();
	}
}
