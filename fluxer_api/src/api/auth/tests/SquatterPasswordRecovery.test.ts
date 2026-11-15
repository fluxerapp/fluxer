// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createAuthHarness,
	createTotpSecret,
	createUniqueEmail,
	createUniqueUsername,
	findLastTestEmail,
	listTestEmails,
	registerUser,
	totpCodeNow,
} from '@app/api/auth/tests/AuthTestUtils';
import {createRegistrationResponse, createWebAuthnDevice} from '@app/api/auth/tests/WebAuthnTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getInstanceConfigRepository, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const SQUATTER_IP = '192.0.2.61';
const SQUATTER_PASSWORD = 'a-strong-squatter-password-61';
const OWNER_PASSWORD = 'the-real-owner-password-62';

describe('An address someone else registered with a password', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		await clearTestEmails(harness);
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function squat(): Promise<{email: string; token: string; userId: string}> {
		const email = createUniqueEmail('squatted');
		const registered = await createBuilderWithoutAuth<{user_id: string; token: string}>(harness)
			.post('/auth/register')
			.header('x-forwarded-for', SQUATTER_IP)
			.body({
				email,
				username: createUniqueUsername('squatter'),
				password: SQUATTER_PASSWORD,
				date_of_birth: '1990-01-01',
				consent: true,
			})
			.execute();
		const me = await createBuilder<{verified: boolean}>(harness, registered.token).get('/users/@me').execute();
		expect(me.verified).toBe(false);
		return {email, token: registered.token, userId: registered.user_id};
	}

	async function expectNoFactor(token: string, path: string, body: Record<string, unknown>, method = 'post') {
		const builder = createBuilder<{code: string}>(harness, token);
		const request = method === 'put' ? builder.put(path) : builder.post(path);
		const {json} = await request.header('x-forwarded-for', SQUATTER_IP).body(body).expect(403).executeWithResponse();
		expect(json.code, path).toBe(APIErrorCodes.MFA_EMAIL_VERIFICATION_REQUIRED);
	}

	it('gives the squatter no way to add a second factor or a passkey on any route', async () => {
		const squatter = await squat();
		const secret = createTotpSecret();
		await expectNoFactor(squatter.token, '/users/@me/mfa/totp/enable', {
			secret,
			code: totpCodeNow(secret),
			password: SQUATTER_PASSWORD,
		});
		await expectNoFactor(squatter.token, '/users/@me/mfa/webauthn/credentials/registration-options', {
			password: SQUATTER_PASSWORD,
		});
		const device = createWebAuthnDevice();
		const challenge = Buffer.alloc(32, 7).toString('base64url');
		const response = createRegistrationResponse(
			device,
			{
				challenge,
				rp: {id: 'localhost', name: 'Fluxer'},
				user: {id: Buffer.from(squatter.userId).toString('base64url'), name: 'squatter', displayName: 'squatter'},
			} as Parameters<typeof createRegistrationResponse>[1],
			'Squatter key',
		);
		await expectNoFactor(squatter.token, '/users/@me/mfa/webauthn/credentials', {
			response,
			challenge,
			name: 'Squatter key',
			password: SQUATTER_PASSWORD,
		});
		await expectNoFactor(
			squatter.token,
			'/users/@me/mfa/webauthn/two-factor',
			{enabled: true, password: SQUATTER_PASSWORD},
			'put',
		);
		await expectNoFactor(squatter.token, '/users/@me/mfa/backup-codes', {
			regenerate: true,
			password: SQUATTER_PASSWORD,
		});
		await expectNoFactor(squatter.token, '/users/@me/mfa/backup-codes/challenge', {});
		const migration = await createBuilder(harness, squatter.token)
			.post('/users/@me/mfa/webauthn/migration/registration-options')
			.body({})
			.executeRaw();
		expect(migration.response.status).toBeGreaterThanOrEqual(400);
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(squatter.userId)));
		expect(user.authenticatorTypes.size).toBe(0);
		expect(await getUserRepository().listWebAuthnCredentials(user.id)).toEqual([]);
	});

	it('lets the inbox owner take the account back with "Forgot your password?"', async () => {
		await getInstanceConfigRepository().updateAuthConfig({identifier_first: true});
		const squatter = await squat();
		const identify = await createBuilderWithoutAuth<{ticket: string; sign_in_method: string}>(harness)
			.post('/auth/identify')
			.body({email: squatter.email})
			.execute();
		expect(identify.sign_in_method).toBe('password');
		await createBuilderWithoutAuth(harness)
			.post('/auth/email-code/send')
			.body({ticket: identify.ticket})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.PASSWORD_SIGN_IN_REQUIRED)
			.execute();
		const grant = await createBuilder<{token: string}>(harness, '')
			.post('/test/oauth2/access-token')
			.body({user_id: squatter.userId, scopes: ['identify']})
			.execute();

		await clearTestEmails(harness);
		await createBuilderWithoutAuth(harness)
			.post('/auth/forgot')
			.body({email: squatter.email})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const resetMail = findLastTestEmail(await listTestEmails(harness, {recipient: squatter.email}), 'password_reset');
		expect(resetMail).not.toBeNull();
		const reset = await createBuilderWithoutAuth<{user_id: string; token: string}>(harness)
			.post('/auth/reset')
			.body({token: resetMail!.metadata['token'], password: OWNER_PASSWORD})
			.execute();
		expect(reset.user_id).toBe(squatter.userId);

		await createBuilder(harness, squatter.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.header('x-forwarded-for', SQUATTER_IP)
			.body({email: squatter.email, password: SQUATTER_PASSWORD})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();

		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(squatter.userId)));
		expect(user.emailVerified).toBe(false);
		expect((await getUserRepository().getAuthorizedIps(user.id)).map((entry) => entry.ip)).toContain(SQUATTER_IP);
		const oauth = await createBuilder(harness, `Bearer ${grant.token}`).get('/users/@me').executeRaw();
		expect(oauth.response.status).toBe(HTTP_STATUS.OK);

		await clearTestEmails(harness);
		await createBuilder(harness, reset.token).post('/users/@me/email-verification/send').body(null).execute();
		const code = findLastTestEmail(
			await listTestEmails(harness, {recipient: squatter.email}),
			'email_verification_code',
		)!.metadata['code'];
		const confirmed = await createBuilder<{verified: boolean}>(harness, reset.token)
			.post('/users/@me/email-verification/confirm')
			.body({code})
			.execute();
		expect(confirmed.verified).toBe(true);
	});

	it('also takes back an address a squatter registered from an older client', async () => {
		const email = createUniqueEmail('old-client');
		const registered = await registerUser(harness, {
			email,
			username: createUniqueUsername('oldsquat'),
			password: SQUATTER_PASSWORD,
			date_of_birth: '1990-01-01',
			consent: true,
		});
		await createBuilderWithoutAuth(harness).post('/auth/forgot').body({email}).expect(HTTP_STATUS.NO_CONTENT).execute();
		const resetMail = findLastTestEmail(await listTestEmails(harness, {recipient: email}), 'password_reset');
		expect(resetMail).not.toBeNull();
		const reset = await createBuilderWithoutAuth<{user_id: string}>(harness)
			.post('/auth/reset')
			.body({token: resetMail!.metadata['token'], password: OWNER_PASSWORD})
			.execute();
		expect(reset.user_id).toBe(registered.user_id);
		await createBuilder(harness, registered.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
	});
});
