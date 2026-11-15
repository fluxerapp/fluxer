// SPDX-License-Identifier: AGPL-3.0-or-later

import {createApplicationID, createUserID} from '@app/api/BrandedTypes';
import {
	type CassandraQueryExecutorForTesting,
	setCassandraQueryExecutorForTesting,
} from '@app/api/database/CassandraQueryExecution';
import {
	type CassandraParams,
	isConditionalQuery,
	type KvQueryMeta,
	type PreparedQuery,
} from '@app/api/database/CassandraTypes';
import {OAuth2TokenRepository} from '@app/api/oauth/repositories/OAuth2TokenRepository';
import {OAuth2AuthorizationCodes, OAuth2RefreshTokens} from '@app/api/Tables';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {randomBase64Url} from '@app/api/utils/SecretHash';
import {beforeEach, describe, expect, it} from 'vitest';

const APPLICATION_ID = createApplicationID(1_910_000_000_000_000_001n);
const OTHER_APPLICATION_ID = createApplicationID(1_910_000_000_000_000_002n);
const USER_ID = createUserID(1_910_000_000_000_000_100n);

const SINGLE_USE_TABLES = new Map<string, {pk: string; select: string; insert: (row: never) => PreparedQuery}>([
	[
		OAuth2AuthorizationCodes.name,
		{
			pk: 'code',
			select: OAuth2AuthorizationCodes.selectCql({where: OAuth2AuthorizationCodes.where.eq('code')}),
			insert: (row) => OAuth2AuthorizationCodes.insert(row),
		},
	],
	[
		OAuth2RefreshTokens.name,
		{
			pk: 'token_',
			select: OAuth2RefreshTokens.selectCql({where: OAuth2RefreshTokens.where.eq('token_')}),
			insert: (row) => OAuth2RefreshTokens.insert(row),
		},
	],
]);

class SkewedClockExecutor implements CassandraQueryExecutorForTesting {
	private tick = 0;
	private readonly writeTimestamps = new Map<string, number>();

	constructor(
		private readonly inner: InMemoryCassandraQueryExecutor,
		private readonly clientSkew: number,
	) {}

	reset(): void {
		this.inner.reset();
		this.writeTimestamps.clear();
	}

	async executeQuery<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
		query: PreparedQuery<P>,
	): Promise<Array<T>> {
		const meta = query.kvMeta;
		const tracked = meta ? SINGLE_USE_TABLES.get(meta.table.name) : undefined;
		if (!meta || !tracked || (meta.action !== 'upsert' && meta.action !== 'delete')) {
			return this.inner.executeQuery<T>(query);
		}
		this.tick += 1;
		const timestamp = isConditionalQuery(query) ? this.tick : this.tick + this.clientSkew;
		const key = `${meta.table.name}:${String((query.params as CassandraParams)[tracked.pk])}`;
		const stored = this.writeTimestamps.get(key);
		if (meta.action === 'upsert') {
			const result = await this.inner.executeQuery<T>(query);
			const applied = !meta.ifNotExists || (result[0] as {'[applied]'?: boolean} | undefined)?.['[applied]'] === true;
			if (applied && (stored === undefined || timestamp >= stored)) this.writeTimestamps.set(key, timestamp);
			return result;
		}
		if (stored === undefined || timestamp >= stored) {
			this.writeTimestamps.delete(key);
			return this.inner.executeQuery<T>(query);
		}
		const params = {[tracked.pk]: (query.params as CassandraParams)[tracked.pk]} as CassandraParams;
		const [survivor] = await this.inner.executeQuery<Record<string, unknown>>({cql: tracked.select, params});
		const result = await this.inner.executeQuery<T>(query);
		if (survivor) await this.inner.executeQuery(tracked.insert(survivor as never));
		return result;
	}

	async executeBatch(
		queries: Array<{query: string; params: object; meta?: KvQueryMeta}>,
		_atomic?: boolean,
	): Promise<void> {
		for (const entry of queries) {
			await this.executeQuery({cql: entry.query, params: entry.params as CassandraParams, kvMeta: entry.meta});
		}
	}
}

function codeRow() {
	return {
		code: randomBase64Url(32),
		application_id: APPLICATION_ID,
		user_id: USER_ID,
		redirect_uri: 'https://example.invalid/callback',
		scope: new Set(['identify']),
		nonce: null,
		code_challenge: null,
		code_challenge_method: null,
		created_at: new Date(),
	};
}

function refreshRow(applicationId = APPLICATION_ID) {
	return {
		token_: randomBase64Url(32),
		application_id: applicationId,
		user_id: USER_ID,
		scope: new Set(['identify']),
		created_at: new Date(),
	};
}

describe.each([
	['ahead of', 1_000],
	['behind', -1_000],
])('OAuth2TokenRepository when the API clock is %s Cassandra', (_label, clientSkew) => {
	let repository: OAuth2TokenRepository;

	beforeEach(() => {
		setCassandraQueryExecutorForTesting(new SkewedClockExecutor(new InMemoryCassandraQueryExecutor(), clientSkew));
		repository = new OAuth2TokenRepository();
	});

	it('redeems an authorization code once', async () => {
		const row = codeRow();
		await repository.createAuthorizationCode(row);
		expect(await repository.consumeAuthorizationCode(row.code, APPLICATION_ID)).toBe(true);
		expect(await repository.getAuthorizationCode(row.code)).toBeNull();
		expect(await repository.consumeAuthorizationCode(row.code, APPLICATION_ID)).toBe(false);
	});

	it('rotates a refresh token once', async () => {
		const row = refreshRow();
		await repository.createRefreshToken(row);
		expect(await repository.consumeRefreshToken(row.token_, APPLICATION_ID, USER_ID)).toBe(true);
		expect(await repository.getRefreshToken(row.token_)).toBeNull();
		expect(await repository.consumeRefreshToken(row.token_, APPLICATION_ID, USER_ID)).toBe(false);
	});

	it('revokes every refresh token of a user', async () => {
		const rows = [refreshRow(), refreshRow(OTHER_APPLICATION_ID)];
		for (const row of rows) await repository.createRefreshToken(row);
		await repository.deleteAllRefreshTokensForUser(USER_ID);
		for (const row of rows) expect(await repository.getRefreshToken(row.token_)).toBeNull();
		expect(await repository.listRefreshTokensForUser(USER_ID)).toEqual([]);
	});

	it('revokes only the refresh tokens of one application', async () => {
		const revoked = refreshRow();
		const kept = refreshRow(OTHER_APPLICATION_ID);
		await repository.createRefreshToken(revoked);
		await repository.createRefreshToken(kept);
		await repository.deleteAllTokensForUserAndApplication(USER_ID, APPLICATION_ID);
		expect(await repository.getRefreshToken(revoked.token_)).toBeNull();
		expect((await repository.listRefreshTokensForUser(USER_ID)).map((token) => token.token)).toEqual([kept.token_]);
	});
});
