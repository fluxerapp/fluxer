// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac, randomBytes, randomUUID} from 'node:crypto';
import {decode as base32Decode, encode as base32Encode} from 'hi-base32';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {TEST_CREDENTIALS, TEST_USER_DATA} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

interface RegisterResponse {
	user_id: string;
	token: string;
}

export interface LoginSuccessResponse {
	user_id: string;
	token: string;
}

export interface LoginMfaResponse {
	mfa: true;
	ticket: string;
	allowed_methods: Array<string>;
	totp: boolean;
	webauthn: boolean;
}

type LoginResponse =
	| {
			user_id: string;
			token: string;
	  }
	| {
			mfa: true;
			ticket: string;
			allowed_methods: Array<string>;
			totp: boolean;
			webauthn: boolean;
	  };

export interface UserMeResponse {
	id: string;
	email: string | null;
	username: string;
	global_name: string | null;
}

export interface TestEmailRecord {
	to: string;
	subject: string;
	type: string;
	timestamp: string;
	metadata: Record<string, string>;
}

export interface TestAccount {
	email: string;
	password: string;
	userId: string;
	token: string;
	ipAddress?: string;
	username?: string;
}

interface CreateTestAccountParams {
	email?: string;
	password?: string;
	username?: string;
	globalName?: string;
	dateOfBirth?: string;
	ipAddress?: string;
	skipSessionStart?: boolean;
	skipEmailVerification?: boolean;
}

export function createUniqueEmail(prefix = 'integration'): string {
	return `${prefix}-${randomUUID()}@example.com`;
}

function createRandomHexId(): string {
	return randomUUID().replace(/-/g, '');
}

export function createUniqueUsername(prefix = 'itest'): string {
	return `${prefix}_${createRandomHexId().slice(0, 12)}`;
}

export function createUniqueTestId(prefix: string): string {
	return `${prefix}-${randomUUID()}`;
}

export async function createAuthHarness(): Promise<ApiTestHarness> {
	return await createApiTestHarness();
}

export async function registerUser(harness: ApiTestHarness, body: Record<string, unknown>): Promise<RegisterResponse> {
	return createBuilder<RegisterResponse>(harness, '').post('/auth/register').body(body).execute();
}

export async function createTestAccount(
	harness: ApiTestHarness,
	params?: CreateTestAccountParams,
): Promise<TestAccount> {
	const email = params?.email ?? createUniqueEmail('account');
	const password = params?.password ?? TEST_CREDENTIALS.STRONG_PASSWORD;
	const username = params?.username ?? createUniqueUsername('account');
	const ipAddress = params?.ipAddress;
	const registrationBuilder = createBuilder<RegisterResponse>(harness, '')
		.post('/auth/register')
		.body({
			email,
			username,
			global_name: params?.globalName ?? TEST_USER_DATA.DEFAULT_GLOBAL_NAME,
			password,
			date_of_birth: params?.dateOfBirth ?? TEST_USER_DATA.DEFAULT_DATE_OF_BIRTH,
			consent: true,
		});
	if (ipAddress) {
		registrationBuilder.header('x-forwarded-for', ipAddress);
	}
	const reg = await registrationBuilder.execute();
	if (!params?.skipSessionStart) {
		const HAS_SESSION_STARTED = BigInt(1) << BigInt(39);
		const sessionStartBuilder = createBuilder<unknown>(harness, reg.token)
			.patch(`/test/users/${reg.user_id}/flags`)
			.body({
				flags: HAS_SESSION_STARTED.toString(),
			});
		if (ipAddress) {
			sessionStartBuilder.header('x-forwarded-for', ipAddress);
		}
		await sessionStartBuilder.execute();
	}
	if (!params?.skipEmailVerification) {
		const securityFlagsBuilder = createBuilder(harness, '').post(`/test/users/${reg.user_id}/security-flags`).body({
			email_verified: true,
			suspicious_activity_flags: 0,
		});
		if (ipAddress) {
			securityFlagsBuilder.header('x-forwarded-for', ipAddress);
		}
		await securityFlagsBuilder.execute();
	}
	return {email, password, userId: reg.user_id, token: reg.token, username, ipAddress};
}

export async function loginUser(
	harness: ApiTestHarness,
	body: {
		email: string;
		password: string;
		invite_code?: string | null;
	},
	options?: {
		ipAddress?: string;
	},
): Promise<LoginResponse> {
	const builder = createBuilder<LoginResponse>(harness, '').post('/auth/login').body(body);
	if (options?.ipAddress) {
		builder.header('x-forwarded-for', options.ipAddress);
	}
	return builder.execute();
}

export async function loginAccount(harness: ApiTestHarness, account: TestAccount): Promise<TestAccount> {
	const login = await loginUser(
		harness,
		{email: account.email, password: account.password},
		{ipAddress: account.ipAddress},
	);
	if ('mfa' in login) {
		throw new Error('Expected non-MFA login for test account');
	}
	const {token, user_id} = login as {
		user_id: string;
		token: string;
	};
	return {...account, token, userId: user_id};
}

export async function fetchMe(
	harness: ApiTestHarness,
	token: string,
	expectedStatus: 200 | 401 = 200,
): Promise<{
	response: Response;
	json: unknown;
}> {
	const {response, json} = await createBuilder(harness, token).get('/users/@me').expect(expectedStatus).executeRaw();
	return {response, json};
}

export async function fetchSettings(
	harness: ApiTestHarness,
	token: string,
	expectedStatus: 200 | 401 = 200,
): Promise<{
	response: Response;
	json: unknown;
}> {
	const {response, json} = await createBuilder(harness, token)
		.get('/users/@me/settings')
		.expect(expectedStatus)
		.executeRaw();
	return {response, json};
}

export async function listTestEmails(
	harness: ApiTestHarness,
	params?: {
		recipient?: string;
	},
): Promise<Array<TestEmailRecord>> {
	const query = params?.recipient ? `?recipient=${encodeURIComponent(params.recipient)}` : '';
	const response = await createBuilder<{
		emails: Array<TestEmailRecord>;
	}>(harness, '')
		.get(`/test/emails${query}`)
		.execute();
	return response.emails;
}

export async function clearTestEmails(harness: ApiTestHarness): Promise<void> {
	await createBuilder(harness, '').delete('/test/emails').expect(204).execute();
}

export function findLastTestEmail(emails: Array<TestEmailRecord>, type: string): TestEmailRecord | null {
	for (let i = emails.length - 1; i >= 0; i--) {
		const email = emails[i];
		if (email?.type === type) return email;
	}
	return null;
}

export function titleCaseEmail(email: string): string {
	return email
		.toLowerCase()
		.replace(/(^|[.@])([a-z])/g, (_match, prefix: string, char: string) => `${prefix}${char.toUpperCase()}`);
}

export interface BackupCodesResponse {
	backup_codes: Array<{
		code: string;
	}>;
}

export function createTotpSecret(): string {
	const buf = randomBytes(20);
	return base32Encode(buf).replace(/=/g, '');
}

export function generateTotpCode(secret: string, time = Date.now()): string {
	const key = Buffer.from(base32Decode.asBytes(secret.toUpperCase()));
	const epoch = Math.floor(time / 1000);
	const counter = Math.floor(epoch / 30);
	const counterBuf = Buffer.alloc(8);
	counterBuf.writeBigUInt64BE(BigInt(counter));
	const hmac = createHmac('sha1', key);
	hmac.update(counterBuf);
	const hash = hmac.digest();
	const offset = hash[hash.length - 1] & 0x0f;
	const binary =
		((hash[offset]! & 0x7f) << 24) |
		((hash[offset + 1]! & 0xff) << 16) |
		((hash[offset + 2]! & 0xff) << 8) |
		(hash[offset + 3]! & 0xff);
	const otp = binary % 1000000;
	return otp.toString().padStart(6, '0');
}

export function totpCodeNow(secret: string): string {
	return generateTotpCode(secret, Date.now());
}

export async function seedMfaTicket(
	harness: ApiTestHarness,
	ticket: string,
	userId: string,
	ttlSeconds: number,
): Promise<void> {
	await createBuilder(harness, '')
		.post('/test/auth/mfa-ticket')
		.body({
			ticket,
			user_id: userId,
			ttl_seconds: ttlSeconds,
		})
		.execute();
}

export async function setUserACLs(
	harness: ApiTestHarness,
	account: TestAccount,
	acls: Array<string>,
): Promise<TestAccount> {
	const builder = createBuilder(harness, `${account.token}`).post(`/test/users/${account.userId}/acls`).body({acls});
	if (account.ipAddress) {
		builder.header('x-forwarded-for', account.ipAddress);
	}
	await builder.execute();
	return await loginAccount(harness, account);
}

export async function unclaimAccount(harness: ApiTestHarness, userId: string): Promise<void> {
	await createBuilder(harness, '').post(`/test/users/${userId}/unclaim`).body(null).execute();
}

interface SsoConfig {
	enabled: boolean;
	enforced: boolean;
	authorization_url: string;
	token_url: string;
	client_id: string;
	client_secret: string;
	scope: string;
	allowed_domains: Array<string>;
	auto_provision: boolean;
	redirect_uri: string;
	display_name?: string;
}

export async function enableSso(
	harness: ApiTestHarness,
	token: string,
	overrides: Partial<SsoConfig> = {},
): Promise<void> {
	const ssoConfig: SsoConfig = {
		enabled: true,
		enforced: true,
		authorization_url: 'test',
		token_url: 'test',
		client_id: 'itest-client',
		client_secret: '',
		scope: 'openid email profile',
		allowed_domains: ['example.com'],
		auto_provision: true,
		redirect_uri: '',
		...overrides,
	};
	await createBuilder(harness, token).patch('/admin/instance/config').body({sso: ssoConfig}).execute();
}

export async function disableSso(harness: ApiTestHarness, token: string): Promise<void> {
	await createBuilder(harness, token)
		.patch('/admin/instance/config')
		.body({
			sso: {
				enabled: false,
			},
		})
		.execute();
}

export async function createSessionFromLogin(harness: ApiTestHarness, account: TestAccount): Promise<string> {
	const login = await loginUser(harness, {email: account.email, password: account.password});
	if ('mfa' in login && login.mfa) {
		throw new Error('Expected non-MFA login for test account');
	}
	const nonMfaLogin = login as {
		user_id: string;
		token: string;
	};
	return nonMfaLogin.token;
}

export function createFakeAuthToken(): string {
	return `flx_${createRandomHexId()}${createRandomHexId().slice(0, 4)}`;
}

export async function logoutSpecificSessions(
	harness: ApiTestHarness,
	token: string,
	sessionIdHashes: Array<string>,
	password: string,
): Promise<void> {
	await createBuilder(harness, token)
		.post('/auth/sessions/logout')
		.body({
			session_id_hashes: sessionIdHashes,
			password,
		})
		.expect(204)
		.execute();
}
