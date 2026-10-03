// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	authorizeOAuth2,
	createOAuth2TestSetup,
	exchangeOAuth2AuthorizationCode,
} from '@app/api/oauth/tests/OAuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

const CONCURRENT_REQUESTS = 8;

interface TokenResult {
	status: number;
	accessToken: string | null;
}

function addQueryLatency(): void {
	const executeQuery = InMemoryCassandraQueryExecutor.prototype.executeQuery;
	vi.spyOn(InMemoryCassandraQueryExecutor.prototype, 'executeQuery').mockImplementation(async function (
		this: InMemoryCassandraQueryExecutor,
		...args: Parameters<typeof executeQuery>
	) {
		await new Promise((resolve) => setTimeout(resolve, 1));
		return executeQuery.apply(this, args);
	} as typeof executeQuery);
}

async function postToken(
	harness: ApiTestHarness,
	clientId: string,
	clientSecret: string,
	form: Record<string, string>,
): Promise<TokenResult> {
	const response = await harness.app.request('/oauth2/token', {
		method: 'POST',
		headers: {
			'Content-Type': 'application/x-www-form-urlencoded',
			Authorization: `Basic ${Buffer.from(`${clientId}:${clientSecret}`).toString('base64')}`,
			'x-forwarded-for': '127.0.0.1',
		},
		body: new URLSearchParams(form).toString(),
	});
	const body = (await response.json().catch(() => null)) as {access_token?: string} | null;
	return {status: response.status, accessToken: body?.access_token ?? null};
}

async function postConcurrently(
	harness: ApiTestHarness,
	clientId: string,
	clientSecret: string,
	form: Record<string, string>,
): Promise<Array<TokenResult>> {
	addQueryLatency();
	try {
		return await Promise.all(
			Array.from({length: CONCURRENT_REQUESTS}, () => postToken(harness, clientId, clientSecret, form)),
		);
	} finally {
		vi.restoreAllMocks();
	}
}

function expectSingleSuccess(results: Array<TokenResult>): void {
	const succeeded = results.filter((result) => result.status === HTTP_STATUS.OK);
	expect(succeeded).toHaveLength(1);
	expect(succeeded[0]!.accessToken).toBeTruthy();
	expect(results.filter((result) => result.status === HTTP_STATUS.BAD_REQUEST)).toHaveLength(CONCURRENT_REQUESTS - 1);
}

describe('OAuth2 concurrent grant redemption', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		vi.restoreAllMocks();
		await harness?.shutdown();
	});

	test('redeems an authorization code once when requests overlap', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const {code} = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const results = await postConcurrently(harness, application.id, application.client_secret, {
			grant_type: 'authorization_code',
			code,
			redirect_uri: redirectURI,
			client_id: application.id,
		});
		expectSingleSuccess(results);
	});

	test('rotates a refresh token once when requests overlap', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const {code} = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const initial = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code,
			redirect_uri: redirectURI,
		});
		const results = await postConcurrently(harness, application.id, application.client_secret, {
			grant_type: 'refresh_token',
			refresh_token: initial.refresh_token!,
			client_id: application.id,
		});
		expectSingleSuccess(results);
	});
});
