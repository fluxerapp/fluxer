// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, registerUser} from '@app/api/auth/tests/AuthTestUtils';
import {getKvMeta} from '@app/api/database/CassandraMetaRegistry';
import {
	type CassandraQueryExecutorForTesting,
	setCassandraQueryExecutorForTesting,
} from '@app/api/database/CassandraQueryExecution';
import type {CassandraParams, PreparedQuery} from '@app/api/database/CassandraTypes';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {UserPrivateResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const DISCRIMINATOR_CAPACITY = 9999;
const AVAILABLE_DISCRIMINATOR = 42;
const ALL_CLAIMS = Array.from({length: DISCRIMINATOR_CAPACITY}, (_, index) => String(index + 1));
const CLAIMS_WITH_ONE_AVAILABLE = ALL_CLAIMS.filter((value) => value !== String(AVAILABLE_DISCRIMINATOR));

describe('registration username derivation through the public API', () => {
	let harness: ApiTestHarness;
	const storage = new InMemoryCassandraQueryExecutor();
	const seededKeys = new Set<string>();
	const requests = new Set<Promise<UserPrivateResponse>>();
	let exhaustedNames: RegExp | null = null;
	let armed = false;

	beforeAll(async () => {
		harness = await createAuthHarness();
	});

	beforeEach(async () => {
		seededKeys.clear();
		exhaustedNames = null;
		armed = false;
		const executor: CassandraQueryExecutorForTesting = {
			async executeQuery<T, P extends CassandraParams>(query: PreparedQuery<P>): Promise<Array<T>> {
				const metadata = query.kvMeta ?? getKvMeta(query.cql);
				const username = query.params.username;
				if (
					armed &&
					metadata?.table.name === 'users_by_username' &&
					metadata.action === 'select' &&
					typeof username === 'string' &&
					query.params.discriminator === undefined
				) {
					const key = `discrim-claimed:${username}`;
					if (!seededKeys.has(key)) {
						seededKeys.add(key);
						const claims = exhaustedNames?.test(username) ? ALL_CLAIMS : CLAIMS_WITH_ONE_AVAILABLE;
						await harness.kvProvider.sadd(key, ...claims);
					}
				}
				return storage.executeQuery<T>(query);
			},
			executeBatch: (queries, atomic) => storage.executeBatch(queries, atomic),
			reset: () => storage.reset(),
		};
		setCassandraQueryExecutorForTesting(executor);
		await harness.reset();
	});

	afterEach(async () => {
		armed = false;
		try {
			await Promise.allSettled(requests);
		} finally {
			setCassandraQueryExecutorForTesting(storage);
			try {
				if (seededKeys.size > 0) {
					await harness.kvProvider.del(...seededKeys);
				}
			} finally {
				requests.clear();
				seededKeys.clear();
				exhaustedNames = null;
			}
		}
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	function registerDerivedName(exhausted: RegExp | null): Promise<UserPrivateResponse> {
		exhaustedNames = exhausted;
		armed = true;
		const request = registerUser(harness, {
			email: 'username-derivation@example.com',
			password: 'a-strong-password',
			global_name: 'Åsa',
			date_of_birth: '2000-01-01',
			consent: true,
		}).then(({token}) => createBuilder<UserPrivateResponse>(harness, token).get('/users/@me').execute());
		requests.add(request);
		return request;
	}

	it('gives the derived name a tag', async () => {
		const user = await registerDerivedName(null);
		expect(user).toMatchObject({username: 'Asa', discriminator: '0042', global_name: 'Åsa'});
		expect(seededKeys.has('discrim-claimed:asa')).toBe(true);
	});

	it('moves to a close variant when every tag for the name is in use', async () => {
		const user = await registerDerivedName(/^asa$/u);
		expect(user.username).toMatch(/^Asa[1-9]\d{0,3}$/u);
		expect(user.discriminator).toBe('0042');
		expect(await harness.kvProvider.smembers('discrim-claimed:asa')).toHaveLength(9999);
	});

	it('falls back to a generated name when the variants are full too', async () => {
		const user = await registerDerivedName(/^asa(?:[1-9]\d{0,3})?$/u);
		expect(user.username.toLowerCase().startsWith('asa')).toBe(false);
		expect(user.discriminator).toBe('0042');
		expect(await harness.kvProvider.smembers('discrim-claimed:asa')).toHaveLength(9999);
	});
});
