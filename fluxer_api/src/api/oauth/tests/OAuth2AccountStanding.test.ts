// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {
	authorizeOAuth2,
	createOAuth2TestSetup,
	exchangeOAuth2AuthorizationCode,
	introspectOAuth2Token,
	refreshOAuth2Token,
} from '@app/api/oauth/tests/OAuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const HOUR_MS = 3_600_000;

async function setStanding(userId: string, change: {addFlags?: bigint; tempBannedUntil?: Date | null}): Promise<void> {
	const users = getUserRepository();
	const user = (await users.findUnique(createUserID(BigInt(userId))))!;
	await users.patchUpsert(
		user.id,
		{
			flags: user.flags | (change.addFlags ?? 0n),
			...(change.tempBannedUntil !== undefined ? {temp_banned_until: change.tempBannedUntil} : {}),
		},
		user.toRow(),
	);
}

describe('OAuth2 grants follow the account standing', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});

	async function grant() {
		const setup = await createOAuth2TestSetup(harness);
		const code = await authorizeOAuth2(harness, setup.endUser.token, {
			client_id: setup.application.id,
			redirect_uri: setup.redirectURI,
			scope: 'identify email',
		});
		const tokens = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: setup.application.id,
			client_secret: setup.application.client_secret,
			code: code.code,
			redirect_uri: setup.redirectURI,
		});
		return {...setup, tokens};
	}

	async function refreshStatus(
		application: {id: string; client_secret: string},
		refreshToken: string,
	): Promise<number> {
		const response = await harness.app.request('/oauth2/token', {
			method: 'POST',
			headers: {
				'Content-Type': 'application/x-www-form-urlencoded',
				Authorization: `Basic ${Buffer.from(`${application.id}:${application.client_secret}`).toString('base64')}`,
				'x-forwarded-for': '127.0.0.1',
			},
			body: new URLSearchParams({grant_type: 'refresh_token', refresh_token: refreshToken}).toString(),
		});
		return response.status;
	}

	async function bearerStatus(path: string, accessToken: string): Promise<number> {
		const {response} = await createBuilder(harness, `Bearer ${accessToken}`).get(path).executeRaw();
		return response.status;
	}

	test('a closed account gets no refreshed tokens, no userinfo and inactive introspection', async () => {
		const {endUser, application, tokens} = await grant();
		expect(await bearerStatus('/oauth2/userinfo', tokens.access_token)).toBe(HTTP_STATUS.OK);
		await setStanding(endUser.userId, {addFlags: UserFlags.DELETED});
		expect(await bearerStatus('/oauth2/userinfo', tokens.access_token)).toBe(HTTP_STATUS.UNAUTHORIZED);
		expect(await bearerStatus('/oauth2/@me', tokens.access_token)).toBe(HTTP_STATUS.UNAUTHORIZED);
		expect(await refreshStatus(application, tokens.refresh_token)).toBe(HTTP_STATUS.BAD_REQUEST);
		const introspected = await introspectOAuth2Token(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			token: tokens.access_token,
		});
		expect(introspected.active).toBe(false);
		const introspectedRefresh = await introspectOAuth2Token(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			token: tokens.refresh_token,
		});
		expect(introspectedRefresh.active).toBe(false);
	});

	test('an authorization code issued before the account closed cannot be exchanged', async () => {
		const setup = await createOAuth2TestSetup(harness);
		const code = await authorizeOAuth2(harness, setup.endUser.token, {
			client_id: setup.application.id,
			redirect_uri: setup.redirectURI,
			scope: 'identify',
		});
		await setStanding(setup.endUser.userId, {addFlags: UserFlags.DELETED});
		await expect(
			exchangeOAuth2AuthorizationCode(harness, {
				client_id: setup.application.id,
				client_secret: setup.application.client_secret,
				code: code.code,
				redirect_uri: setup.redirectURI,
			}),
		).rejects.toThrow(/Expected 200, got 400/);
	});

	test('a temporary ban pauses the grant and the refresh token works again after it ends', async () => {
		const {endUser, application, tokens} = await grant();
		await setStanding(endUser.userId, {
			addFlags: UserFlags.DISABLED,
			tempBannedUntil: new Date(Date.now() + HOUR_MS),
		});
		expect(await bearerStatus('/oauth2/userinfo', tokens.access_token)).toBe(HTTP_STATUS.UNAUTHORIZED);
		expect(await refreshStatus(application, tokens.refresh_token)).toBe(HTTP_STATUS.BAD_REQUEST);
		await setStanding(endUser.userId, {tempBannedUntil: new Date(Date.now() - HOUR_MS)});
		const refreshed = await refreshOAuth2Token(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			refresh_token: tokens.refresh_token,
		});
		expect(await bearerStatus('/oauth2/userinfo', refreshed.access_token)).toBe(HTTP_STATUS.OK);
	});
});
