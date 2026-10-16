// SPDX-License-Identifier: AGPL-3.0-or-later

import {generateKeyPairSync} from 'node:crypto';
import {mkdtemp, rm, writeFile} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import path from 'node:path';
import {Config} from '@app/api/Config';
import {
	GOOGLE_PLAY_ANDROIDPUBLISHER_SCOPE,
	GooglePlayAccessTokenProvider,
	resolveGooglePlayCredentials,
} from '@app/api/store_billing/google_play/GooglePlayAccessTokenProvider';
import {GooglePlayApiError} from '@app/api/store_billing/google_play/GooglePlayApiError';
import {
	createGooglePlayDeveloperApiHandlers,
	type FakeGooglePlayDeveloperApi,
} from '@app/api/test/msw/handlers/GooglePlayDeveloperApiHandlers';
import {server} from '@app/api/test/msw/server';
import {HttpResponse, http} from 'msw';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

function generateRsaPem(): string {
	const {privateKey} = generateKeyPairSync('rsa', {modulusLength: 2048});
	return privateKey.export({format: 'pem', type: 'pkcs8'}).toString();
}

describe('GooglePlayAccessTokenProvider', () => {
	let fake: FakeGooglePlayDeveloperApi;
	let savedGooglePlay: typeof Config.googlePlay;
	let tempDir: string | null;

	beforeEach(() => {
		savedGooglePlay = {...Config.googlePlay};
		tempDir = null;
		fake = createGooglePlayDeveloperApiHandlers();
		server.use(...fake.handlers);
	});

	afterEach(async () => {
		Object.assign(Config.googlePlay, savedGooglePlay);
		if (tempDir) {
			await rm(tempDir, {recursive: true, force: true});
		}
	});

	async function writeServiceAccountFile(contents: unknown): Promise<string> {
		tempDir = await mkdtemp(path.join(tmpdir(), 'fluxer-play-sa-'));
		const filePath = path.join(tempDir, 'service-account.json');
		await writeFile(filePath, typeof contents === 'string' ? contents : JSON.stringify(contents));
		return filePath;
	}

	it('exchanges a signed RS256 assertion for an access token', async () => {
		const provider = new GooglePlayAccessTokenProvider();
		const token = await provider.getAccessToken();
		expect(token).toBe('ya29.fake-1');
		expect(fake.spies.tokenAssertions).toHaveLength(1);
		const [{header, claims}] = fake.spies.tokenAssertions;
		expect(header).toEqual({alg: 'RS256', typ: 'JWT'});
		expect(claims.iss).toBe(Config.googlePlay.clientEmail);
		expect(claims.aud).toBe('https://oauth2.googleapis.com/token');
		expect(claims['scope']).toBe(GOOGLE_PLAY_ANDROIDPUBLISHER_SCOPE);
		expect((claims.exp ?? 0) - (claims.iat ?? 0)).toBe(3600);
	});

	it('reuses the cached token until five minutes before expiry', async () => {
		let now = Date.now();
		const provider = new GooglePlayAccessTokenProvider({now: () => now});
		expect(await provider.getAccessToken()).toBe('ya29.fake-1');
		now += 54 * 60 * 1000;
		expect(await provider.getAccessToken()).toBe('ya29.fake-1');
		now += 60 * 1000 + 1;
		expect(await provider.getAccessToken()).toBe('ya29.fake-2');
		expect(fake.spies.tokenAssertions).toHaveLength(2);
	});

	it('accepts expires_in sent as a string', async () => {
		fake.setAccessTokenExpiresIn('600');
		let now = Date.now();
		const provider = new GooglePlayAccessTokenProvider({now: () => now});
		await provider.getAccessToken();
		now += 4 * 60 * 1000;
		expect(await provider.getAccessToken()).toBe('ya29.fake-1');
		now += 60 * 1000 + 1;
		expect(await provider.getAccessToken()).toBe('ya29.fake-2');
	});

	it('shares one token request between concurrent callers', async () => {
		const provider = new GooglePlayAccessTokenProvider();
		const tokens = await Promise.all([provider.getAccessToken(), provider.getAccessToken(), provider.getAccessToken()]);
		expect(new Set(tokens)).toEqual(new Set(['ya29.fake-1']));
		expect(fake.spies.tokenAssertions).toHaveLength(1);
	});

	it('fetches a new token after invalidation of the current one', async () => {
		const provider = new GooglePlayAccessTokenProvider();
		const first = await provider.getAccessToken();
		provider.invalidate('ya29.other');
		expect(await provider.getAccessToken()).toBe(first);
		provider.invalidate(first);
		expect(await provider.getAccessToken()).toBe('ya29.fake-2');
	});

	it('fetches a new token when the credentials change', async () => {
		const provider = new GooglePlayAccessTokenProvider();
		await provider.getAccessToken();
		const pem = generateRsaPem();
		Config.googlePlay.privateKey = pem;
		expect(await provider.getAccessToken()).toBe('ya29.fake-2');
	});

	it('reads credentials from a service account JSON file', async () => {
		const pem = generateRsaPem();
		Config.googlePlay.clientEmail = undefined;
		Config.googlePlay.privateKey = undefined;
		Config.googlePlay.serviceAccountJsonPath = await writeServiceAccountFile({
			type: 'service_account',
			client_email: 'json-account@fluxer-test.iam.gserviceaccount.com',
			private_key: pem,
			private_key_id: 'abc123',
			token_uri: 'https://oauth2.googleapis.com/token',
		});
		fake.setServiceAccountPrivateKey(pem);
		const provider = new GooglePlayAccessTokenProvider();
		expect(await provider.getAccessToken()).toBe('ya29.fake-1');
		const [{header, claims}] = fake.spies.tokenAssertions;
		expect(header['kid']).toBe('abc123');
		expect(claims.iss).toBe('json-account@fluxer-test.iam.gserviceaccount.com');
	});

	it('prefers explicit config values over the service account file', async () => {
		const filePem = generateRsaPem();
		Config.googlePlay.serviceAccountJsonPath = await writeServiceAccountFile({
			client_email: 'json-account@fluxer-test.iam.gserviceaccount.com',
			private_key: filePem,
		});
		const credentials = await resolveGooglePlayCredentials();
		expect(credentials.clientEmail).toBe(savedGooglePlay.clientEmail);
		expect(credentials.privateKey).toBe(savedGooglePlay.privateKey);
	});

	it('uses the token_uri from the service account file when the config keeps the default', async () => {
		Config.googlePlay.serviceAccountJsonPath = await writeServiceAccountFile({
			token_uri: 'https://oauth2.fluxer.test/token',
		});
		expect((await resolveGooglePlayCredentials()).tokenUri).toBe('https://oauth2.fluxer.test/token');
		Config.googlePlay.tokenUri = 'https://oauth2.override.test/token';
		expect((await resolveGooglePlayCredentials()).tokenUri).toBe('https://oauth2.override.test/token');
	});

	it('accepts escaped newlines in an inline private key', async () => {
		Config.googlePlay.privateKey = savedGooglePlay.privateKey?.replaceAll('\n', '\\n');
		const provider = new GooglePlayAccessTokenProvider();
		expect(await provider.getAccessToken()).toBe('ya29.fake-1');
	});

	it('rejects an unreadable service account file', async () => {
		Config.googlePlay.clientEmail = undefined;
		Config.googlePlay.privateKey = undefined;
		Config.googlePlay.serviceAccountJsonPath = await writeServiceAccountFile('not json');
		const provider = new GooglePlayAccessTokenProvider();
		await expect(provider.getAccessToken()).rejects.toMatchObject({kind: 'auth', reason: 'invalid_credentials'});
	});

	it('rejects when no credentials are configured', async () => {
		Config.googlePlay.clientEmail = undefined;
		Config.googlePlay.privateKey = undefined;
		Config.googlePlay.serviceAccountJsonPath = undefined;
		const provider = new GooglePlayAccessTokenProvider();
		await expect(provider.getAccessToken()).rejects.toMatchObject({kind: 'auth', reason: 'missing_credentials'});
		expect(fake.spies.tokenAssertions).toHaveLength(0);
	});

	it('reports a rejected grant as an auth error', async () => {
		fake.setServiceAccountPrivateKey(generateRsaPem());
		const provider = new GooglePlayAccessTokenProvider();
		const error = await provider.getAccessToken().catch((caught: unknown) => caught);
		expect(error).toBeInstanceOf(GooglePlayApiError);
		expect(error).toMatchObject({kind: 'auth', status: 400, reason: 'invalid_grant'});
	});

	it('reports token endpoint outages as retryable and does not cache the failure', async () => {
		fake.failNext('token', {status: 503, retryAfter: '7'});
		const provider = new GooglePlayAccessTokenProvider();
		await expect(provider.getAccessToken()).rejects.toMatchObject({
			kind: 'retryable',
			status: 503,
			retryAfterMs: 7000,
		});
		expect(await provider.getAccessToken()).toBe('ya29.fake-1');
	});

	it('reports a malformed token response as retryable', async () => {
		server.use(http.post('https://oauth2.googleapis.com/token', () => HttpResponse.json({token_type: 'Bearer'})));
		const provider = new GooglePlayAccessTokenProvider();
		await expect(provider.getAccessToken()).rejects.toMatchObject({kind: 'retryable', reason: 'malformed_response'});
	});

	it('reports network failures as retryable', async () => {
		server.use(http.post('https://oauth2.googleapis.com/token', () => HttpResponse.error()));
		const provider = new GooglePlayAccessTokenProvider();
		await expect(provider.getAccessToken()).rejects.toMatchObject({kind: 'retryable', reason: 'network_error'});
	});
});
