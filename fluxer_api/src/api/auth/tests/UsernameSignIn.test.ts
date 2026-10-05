// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createAuthHarness,
	createTestAccount,
	createUniqueUsername,
	findLastTestEmail,
	type LoginSuccessResponse,
	listTestEmails,
	setUserACLs,
} from '@app/api/auth/tests/AuthTestUtils';
import {createPasswordResetToken, createUserID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import {getCachedInstancePremiumMode, setCachedInstancePremiumMode} from '@app/api/limits/InstancePremiumModeCache';
import {
	getAdminRepository,
	getInstanceConfigRepository,
	getRateLimitService,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {type CaptchaErrorBody, solveCaptchaChallenge, useCheapCaptcha} from '@app/api/test/CaptchaTestUtils';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {grantPremium} from '@app/api/user/tests/UserTestUtils';
import {AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {PremiumFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {getSameIpDecisionKey} from '@fluxer/ip_utils/src/IpAddress';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const PASSWORD = 'username-mode-password-123';
const NEW_PASSWORD = 'username-mode-new-password-456';
const HAS_SESSION_STARTED = BigInt(1) << BigInt(39);

interface ValidationErrorBody {
	code: string;
	errors: Array<{path: string; code: string}>;
}

interface UsernameAccount {
	userId: string;
	token: string;
	username: string;
	password: string;
}

interface CurrentUser {
	id: string;
	username: string;
	discriminator: string;
	email: string | null;
	acls?: Array<string>;
}

const EMAIL_ONLY_PUBLIC_ROUTES: ReadonlyArray<[string, string]> = [
	['POST', '/auth/verify'],
	['POST', '/auth/forgot'],
	['POST', '/auth/email-revert'],
	['POST', '/auth/authorize-ip'],
	['POST', '/auth/ip-authorization/resend'],
	['POST', '/reports/dsa/email/send'],
	['POST', '/reports/dsa/email/verify'],
];

const EMAIL_ONLY_USER_ROUTES: ReadonlyArray<[string, string]> = [
	['POST', '/auth/verify/resend'],
	['POST', '/users/@me/mfa/backup-codes/challenge'],
	['POST', '/users/@me/mfa/backup-codes/challenge/resend'],
	['POST', '/users/@me/mfa/backup-codes/challenge/verify'],
	['POST', '/users/@me/mfa/backup-codes/challenge/regenerate'],
	['POST', '/users/@me/password-change/start'],
	['POST', '/users/@me/password-change/resend'],
	['POST', '/users/@me/password-change/verify'],
	['POST', '/users/@me/password-change/complete'],
	['POST', '/users/@me/email-change/start'],
	['POST', '/users/@me/email-change/resend-original'],
	['POST', '/users/@me/email-change/verify-original'],
	['POST', '/users/@me/email-change/request-new'],
	['POST', '/users/@me/email-change/resend-new'],
	['POST', '/users/@me/email-change/verify-new'],
	['POST', '/users/@me/email-change/apply'],
	['POST', '/users/@me/email-change/bounced/request-new'],
	['POST', '/users/@me/email-change/bounced/resend-new'],
	['POST', '/users/@me/email-change/bounced/verify-new'],
];

describe('Username sign-in', () => {
	let harness: ApiTestHarness;
	const config = getConfig();
	let originalSelfHosted: boolean;
	let originalHosts: {webApp: string; webAppOrigins: Array<string>; baseDomain: string};

	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		await clearTestEmails(harness);
		originalSelfHosted = config.instance.selfHosted;
		originalHosts = {
			webApp: config.endpoints.webApp,
			webAppOrigins: config.endpoints.webAppOrigins,
			baseDomain: config.instance.baseDomain,
		};
	});
	afterEach(() => {
		config.instance.selfHosted = originalSelfHosted;
		config.endpoints.webApp = originalHosts.webApp;
		config.endpoints.webAppOrigins = originalHosts.webAppOrigins;
		config.instance.baseDomain = originalHosts.baseDomain;
		getInstanceConfigRepository().clearCacheForTesting();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function useUsernameMode(): Promise<void> {
		config.instance.selfHosted = true;
		await getInstanceConfigRepository().setAccountIdentityMode(AccountIdentityModes.USERNAME, 'setup');
	}

	function useInstanceHosts(): void {
		config.endpoints.webApp = 'https://chat.example.org';
		config.endpoints.webAppOrigins = ['https://chat.example.org', 'https://alias.example.net:8443'];
		config.instance.baseDomain = 'example.org';
	}

	async function registerFromOlderApp(body: Record<string, unknown>): Promise<{user_id: string; token: string}> {
		return await createBuilderWithoutAuth<{user_id: string; token: string}>(harness)
			.post('/auth/register')
			.body({password: PASSWORD, date_of_birth: '2000-01-01', ...body})
			.execute();
	}

	async function useSelfHostedEmailMode(): Promise<void> {
		config.instance.selfHosted = true;
		await getInstanceConfigRepository().setAccountIdentityMode(
			AccountIdentityModes.EMAIL,
			'existing_instance',
			'random',
		);
	}

	async function useSelfHostedEmailUniqueMode(): Promise<void> {
		config.instance.selfHosted = true;
		await getInstanceConfigRepository().setAccountIdentityMode(AccountIdentityModes.EMAIL, 'setup', 'none');
	}

	async function registerEmailAccount(username: string): Promise<UsernameAccount & {email: string}> {
		const email = `${username.toLowerCase()}-${Date.now()}@example.com`;
		const registration = await createBuilderWithoutAuth<{user_id: string; token: string}>(harness)
			.post('/auth/register')
			.body({email, username, global_name: 'Email Person', password: PASSWORD, date_of_birth: '2000-01-01'})
			.execute();
		await markSessionStarted(registration.user_id);
		await createBuilder(harness, '')
			.post(`/test/users/${registration.user_id}/security-flags`)
			.body({email_verified: true})
			.execute();
		return {userId: registration.user_id, token: registration.token, username, password: PASSWORD, email};
	}

	async function markSessionStarted(userId: string): Promise<void> {
		await createBuilder(harness, '')
			.patch(`/test/users/${userId}/flags`)
			.body({flags: HAS_SESSION_STARTED.toString()})
			.execute();
	}

	async function registerUsernameAccount(username = createUniqueUsername('uname')): Promise<UsernameAccount> {
		const registration = await createBuilderWithoutAuth<{user_id: string; token: string}>(harness)
			.post('/auth/register')
			.body({username, global_name: 'Username Person', password: PASSWORD, date_of_birth: '2000-01-01'})
			.execute();
		await markSessionStarted(registration.user_id);
		return {userId: registration.user_id, token: registration.token, username, password: PASSWORD};
	}

	async function fetchCurrentUser(token: string): Promise<CurrentUser> {
		return await createBuilder<CurrentUser>(harness, token).get('/users/@me').execute();
	}

	async function expectFieldErrors(
		request: {path: string; method?: string; body: unknown; token?: string},
		expected: Array<{path: string; code: string}>,
	): Promise<void> {
		const builder = createBuilder<ValidationErrorBody>(harness, request.token ?? '');
		const method = (request.method ?? 'POST').toLowerCase() as 'post' | 'patch' | 'put';
		const {json} = await builder[method](request.path)
			.body(request.body)
			.expect(400, 'INVALID_FORM_BODY')
			.executeWithResponse();
		expect(json.errors.map(({path, code}) => ({path, code}))).toEqual(expected);
	}

	async function login(body: Record<string, unknown>): Promise<LoginSuccessResponse> {
		return await createBuilderWithoutAuth<LoginSuccessResponse>(harness).post('/auth/login').body(body).execute();
	}

	async function startSession(token: string): Promise<void> {
		await createBuilder(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'session', token, version: 1, ip: '127.0.0.1'})
			.execute();
	}

	async function markPremiumDiscriminator(userId: string): Promise<void> {
		await createBuilder(harness, '')
			.patch(`/test/users/${userId}/premium-flags`)
			.body({premium_flags: PremiumFlags.DISCRIMINATOR})
			.execute();
	}

	describe('registration', () => {
		it('accepts an email from older apps and never stores it', async () => {
			const existing = await createTestAccount(harness);
			await clearTestEmails(harness);
			await useUsernameMode();
			const registration = await createBuilderWithoutAuth<{user_id: string; token: string}>(harness)
				.post('/auth/register')
				.body({
					email: existing.email,
					username: 'withemail',
					password: PASSWORD,
					date_of_birth: '2000-01-01',
				})
				.execute();
			const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(registration.user_id)));
			expect({email: user.email, emailVerified: user.emailVerified, username: user.username}).toEqual({
				email: null,
				emailVerified: true,
				username: 'withemail',
			});
			expect((await fetchCurrentUser(registration.token)).discriminator).toBe('0000');
			expect(await listTestEmails(harness, {recipient: existing.email})).toEqual([]);
			await createBuilderWithoutAuth(harness)
				.post('/auth/register')
				.body({email: 'not-an-address', username: 'withoddemail', password: PASSWORD, date_of_birth: '2000-01-01'})
				.execute();
		});

		it('takes the username from an address at the instance host', async () => {
			await useUsernameMode();
			useInstanceHosts();
			const registration = await createBuilderWithoutAuth<{user_id: string; token: string}>(harness)
				.post('/auth/register')
				.body({
					email: 'Local.Person@CHAT.example.org',
					global_name: 'Shown Name',
					password: PASSWORD,
					date_of_birth: '2000-01-01',
				})
				.execute();
			const me = await fetchCurrentUser(registration.token);
			expect({username: me.username, discriminator: me.discriminator, email: me.email}).toEqual({
				username: 'Local_Person',
				discriminator: '0000',
				email: null,
			});
			expect((await login({email: 'Local.Person@chat.example.org', password: PASSWORD})).user_id).toBe(
				registration.user_id,
			);
			const fromDomain = await createBuilderWithoutAuth<{user_id: string; token: string}>(harness)
				.post('/auth/register')
				.body({email: 'domainperson@example.org', password: PASSWORD, date_of_birth: '2000-01-01'})
				.execute();
			expect((await fetchCurrentUser(fromDomain.token)).username).toBe('domainperson');
		});

		it('refuses a taken username from an address at the instance host', async () => {
			await useUsernameMode();
			useInstanceHosts();
			await registerUsernameAccount('TakenLocal');
			await expectFieldErrors(
				{
					path: '/auth/register',
					body: {email: 'takenlocal@chat.example.org', password: PASSWORD, date_of_birth: '2000-01-01'},
				},
				[{path: 'username', code: ValidationErrorCodes.USERNAME_ALREADY_TAKEN}],
			);
		});

		it('asks for an address at the instance host instead of turning a foreign address into a username', async () => {
			await useUsernameMode();
			useInstanceHosts();
			for (const email of [
				'secretlocal@chat.example.org.elsewhere.test',
				'secretlocal@elsewhere.test',
				'secretlocal@gmail.com@chat.example.org',
				'secretlocal@chat.example.org/elsewhere',
				'secretlocal',
			]) {
				await expectFieldErrors(
					{
						path: '/auth/register',
						body: {email, global_name: 'Visible Name', password: PASSWORD, date_of_birth: '2000-01-01'},
					},
					[{path: 'email', code: ValidationErrorCodes.INSTANCE_ADDRESS_REQUIRED}],
				);
			}
			const {json} = await createBuilderWithoutAuth<{errors: Array<{message: string}>}>(harness)
				.post('/auth/register')
				.body({email: 'secretlocal@elsewhere.test', password: PASSWORD, date_of_birth: '2000-01-01'})
				.expect(400, 'INVALID_FORM_BODY')
				.executeWithResponse();
			expect(json.errors[0]!.message).toContain('@chat.example.org');
			const named = await registerFromOlderApp({
				email: 'secretlocal@elsewhere.test',
				username: 'chosenname',
				global_name: 'Visible Name',
			});
			const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(named.user_id)));
			expect({username: user.username, email: user.email}).toEqual({username: 'chosenname', email: null});
		});

		it('refuses a local part that cannot be a username with the username field codes', async () => {
			await useUsernameMode();
			useInstanceHosts();
			for (const [email, code] of [
				['everyone@chat.example.org', ValidationErrorCodes.USERNAME_RESERVED_VALUE],
				['FluxerFan@chat.example.org', ValidationErrorCodes.USERNAME_CANNOT_CONTAIN_RESERVED_TERMS],
				['alex#1234@chat.example.org', ValidationErrorCodes.USERNAME_INVALID_CHARACTERS],
				['!!!@chat.example.org', ValidationErrorCodes.USERNAME_INVALID_CHARACTERS],
			] as const) {
				await expectFieldErrors(
					{path: '/auth/register', body: {email, password: PASSWORD, date_of_birth: '2000-01-01'}},
					[{path: 'username', code}],
				);
			}
		});

		it('maps a local part to the same username at sign-up and sign-in', async () => {
			await useUsernameMode();
			useInstanceHosts();
			for (const [email, username] of [
				['tagged#0@chat.example.org', 'tagged'],
				['zerotag#0000@chat.example.org', 'zerotag'],
				['dotted.name@chat.example.org', 'dotted_name'],
			] as const) {
				const registration = await registerFromOlderApp({email});
				expect((await fetchCurrentUser(registration.token)).username).toBe(username);
				expect((await login({email, password: PASSWORD})).user_id).toBe(registration.user_id);
				expect((await login({login: username, password: PASSWORD})).user_id).toBe(registration.user_id);
			}
		});

		it('matches the instance host with a port, in any case, as an IDN and through app origin aliases', async () => {
			await useUsernameMode();
			useInstanceHosts();
			const withPort = await registerFromOlderApp({email: 'portperson@CHAT.example.org:8443'});
			expect((await fetchCurrentUser(withPort.token)).username).toBe('portperson');
			const throughAlias = await registerFromOlderApp({email: 'aliasperson@alias.example.net'});
			expect((await fetchCurrentUser(throughAlias.token)).username).toBe('aliasperson');
			expect((await login({email: 'aliasperson@ALIAS.example.net:8443', password: PASSWORD})).user_id).toBe(
				throughAlias.user_id,
			);
			config.endpoints.webApp = 'https://bücher.example';
			config.endpoints.webAppOrigins = ['https://bücher.example'];
			const idn = await registerFromOlderApp({email: 'idnperson@Bücher.example'});
			expect((await fetchCurrentUser(idn.token)).username).toBe('idnperson');
			expect((await login({email: 'idnperson@xn--bcher-kva.example', password: PASSWORD})).user_id).toBe(idn.user_id);
		});

		it('requires a username when a password is given', async () => {
			await useUsernameMode();
			await expectFieldErrors({path: '/auth/register', body: {global_name: 'No Username', password: PASSWORD}}, [
				{path: 'username', code: ValidationErrorCodes.USERNAME_LENGTH_INVALID},
			]);
		});

		it('creates an email-less verified account and makes the first one the admin', async () => {
			await useUsernameMode();
			const first = await registerUsernameAccount('firstperson');
			const second = await registerUsernameAccount('secondperson');
			const firstUser = await getUserRepository().findUniqueAssert(createUserID(BigInt(first.userId)));
			const secondUser = await getUserRepository().findUniqueAssert(createUserID(BigInt(second.userId)));
			expect(firstUser.email).toBeNull();
			expect(firstUser.emailVerified).toBe(true);
			expect([...firstUser.acls]).toEqual([AdminACLs.WILDCARD]);
			expect([...secondUser.acls]).toEqual([]);
			expect(await listTestEmails(harness)).toEqual([]);
		});

		it('refuses a username that is already taken in any case', async () => {
			await useUsernameMode();
			await registerUsernameAccount('TakenName');
			await expectFieldErrors({path: '/auth/register', body: {username: 'takenname', password: PASSWORD}}, [
				{path: 'username', code: ValidationErrorCodes.USERNAME_ALREADY_TAKEN},
			]);
		});

		it('derives a free username for an unclaimed account', async () => {
			await useUsernameMode();
			await registerUsernameAccount('sharedname');
			const unclaimed = await createBuilderWithoutAuth<{user_id: string; token: string}>(harness)
				.post('/auth/register')
				.body({global_name: 'sharedname'})
				.execute();
			const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(unclaimed.user_id)));
			expect(user.username.toLowerCase()).not.toBe('sharedname');
			expect(user.username.toLowerCase().startsWith('sharedname')).toBe(true);
			expect(user.isUnclaimedAccount()).toBe(true);
		});
	});

	describe('login', () => {
		it('signs in with a username in any case and with the full tag', async () => {
			await useUsernameMode();
			const account = await registerUsernameAccount('LoginPerson');
			const me = await fetchCurrentUser(account.token);
			const byName = await login({login: 'loginperson', password: PASSWORD});
			expect(byName.user_id).toBe(account.userId);
			const byTag = await login({login: `LoginPerson#${me.discriminator.padStart(4, '0')}`, password: PASSWORD});
			expect(byTag.user_id).toBe(account.userId);
		});

		it('returns the same error for a wrong password, a wrong tag and an unknown username', async () => {
			await useUsernameMode();
			const account = await registerUsernameAccount('wrongcreds');
			const me = await fetchCurrentUser(account.token);
			const wrongTag = ((Number(me.discriminator) % 9999) + 1).toString().padStart(4, '0');
			const expected = [
				{path: 'login', code: ValidationErrorCodes.INVALID_LOGIN_OR_PASSWORD},
				{path: 'password', code: ValidationErrorCodes.INVALID_LOGIN_OR_PASSWORD},
			];
			await expectFieldErrors({path: '/auth/login', body: {login: 'wrongcreds', password: NEW_PASSWORD}}, expected);
			await expectFieldErrors(
				{path: '/auth/login', body: {login: `wrongcreds#${wrongTag}`, password: PASSWORD}},
				expected,
			);
			await expectFieldErrors({path: '/auth/login', body: {login: 'nobodyhere', password: PASSWORD}}, expected);
			await expectFieldErrors({path: '/auth/login', body: {login: 'not a handle!', password: PASSWORD}}, expected);
		});

		it('signs older apps in with a username or username@host in the email field', async () => {
			await useUsernameMode();
			useInstanceHosts();
			const account = await registerUsernameAccount('OldAppPerson');
			for (const email of [
				'oldappperson',
				'OldAppPerson@chat.example.org',
				'oldappperson@CHAT.EXAMPLE.ORG',
				'oldappperson@example.org',
				'oldappperson#0@chat.example.org',
				'oldappperson#0000@chat.example.org',
			]) {
				expect((await login({email, password: PASSWORD})).user_id).toBe(account.userId);
			}
		});

		it('gives older apps the wrong-credentials error on the email field', async () => {
			await useUsernameMode();
			useInstanceHosts();
			await registerUsernameAccount('oldappwrong');
			const expected = [
				{path: 'email', code: ValidationErrorCodes.INVALID_LOGIN_OR_PASSWORD},
				{path: 'password', code: ValidationErrorCodes.INVALID_LOGIN_OR_PASSWORD},
			];
			for (const body of [
				{email: 'oldappwrong@chat.example.org', password: NEW_PASSWORD},
				{email: 'oldappwrong', password: NEW_PASSWORD},
				{email: 'oldappwrong@elsewhere.test', password: PASSWORD},
				{email: 'oldappwrong@chat.example.org.elsewhere.test', password: PASSWORD},
				{email: 'oldappwrong#1234@chat.example.org', password: PASSWORD},
				{email: 'oldappwrong@gmail.com@chat.example.org', password: PASSWORD},
				{email: 'oldappwrong@chat.example.org/elsewhere', password: PASSWORD},
				{email: 'nobodyhere@chat.example.org', password: PASSWORD},
				{email: 'oldappwrong@example.org', login: 'oldappwrong', password: NEW_PASSWORD},
			]) {
				await expectFieldErrors({path: '/auth/login', body}, expected);
			}
		});

		it('reports a missing identifier on email', async () => {
			await useUsernameMode();
			await expectFieldErrors({path: '/auth/login', body: {password: PASSWORD}}, [
				{path: 'email', code: ValidationErrorCodes.INVALID_FORMAT},
			]);
		});

		it('limits failed sign-ins per username and source without locking out other sources', async () => {
			await useUsernameMode();
			const account = await registerUsernameAccount('lockouttarget');
			await getInstanceConfigRepository().updateCaptchaConfig({enabled: false});
			const previous = {testMode: config.dev.testModeEnabled, disabled: config.dev.disableRateLimits};
			config.dev.testModeEnabled = false;
			config.dev.disableRateLimits = false;
			try {
				for (let attempt = 0; attempt < 5; attempt++) {
					await createBuilderWithoutAuth(harness)
						.post('/auth/login')
						.header('x-forwarded-for', '198.51.100.40')
						.body({login: account.username, password: NEW_PASSWORD})
						.expect(400, 'INVALID_FORM_BODY')
						.execute();
				}
				await createBuilderWithoutAuth(harness)
					.post('/auth/login')
					.header('x-forwarded-for', '198.51.100.40')
					.body({login: account.username.toUpperCase(), password: PASSWORD})
					.expect(429)
					.execute();
				const owner = await createBuilderWithoutAuth<LoginSuccessResponse>(harness)
					.post('/auth/login')
					.header('x-forwarded-for', '198.51.100.41')
					.body({login: account.username, password: PASSWORD})
					.execute();
				expect(owner.user_id).toBe(account.userId);
			} finally {
				config.dev.testModeEnabled = previous.testMode;
				config.dev.disableRateLimits = previous.disabled;
			}
		});

		it('lets the owner through a spent shared limit from a known address or with a solved captcha', async () => {
			await useUsernameMode();
			const account = await registerUsernameAccount('floodtarget');
			await getInstanceConfigRepository().updateCaptchaConfig({enabled: false});
			const previous = {testMode: config.dev.testModeEnabled, disabled: config.dev.disableRateLimits};
			config.dev.testModeEnabled = false;
			config.dev.disableRateLimits = false;
			const attempt = (ip: string, password: string, captchaToken?: string) => {
				const builder = createBuilderWithoutAuth<LoginSuccessResponse>(harness)
					.post('/auth/login')
					.header('x-forwarded-for', ip)
					.body({login: account.username, password});
				if (captchaToken) builder.header('X-Captcha-Token', captchaToken);
				return builder;
			};
			try {
				expect((await attempt('198.51.100.90', PASSWORD).execute()).user_id).toBe(account.userId);
				for (let source = 0; source < 20; source++) {
					for (let failure = 0; failure < 5; failure++) {
						await attempt(`203.0.113.${source + 1}`, NEW_PASSWORD)
							.expect(400, 'INVALID_FORM_BODY')
							.execute();
					}
				}
				await attempt('198.51.100.91', NEW_PASSWORD).expect(429).execute();
				await attempt('198.51.100.92', PASSWORD).expect(429).execute();
				expect((await attempt('198.51.100.90', PASSWORD).execute()).user_id).toBe(account.userId);
				await useCheapCaptcha();
				const {json} = await createBuilderWithoutAuth<CaptchaErrorBody>(harness)
					.post('/auth/login')
					.header('x-forwarded-for', '198.51.100.93')
					.body({login: account.username, password: PASSWORD})
					.expect(400, APIErrorCodes.CAPTCHA_REQUIRED)
					.executeWithResponse();
				const token = await solveCaptchaChallenge(json);
				expect((await attempt('198.51.100.93', PASSWORD, token).execute()).user_id).toBe(account.userId);
			} finally {
				config.dev.testModeEnabled = previous.testMode;
				config.dev.disableRateLimits = previous.disabled;
			}
		});

		it('counts only failed sign-ins against the shared per-username limit', async () => {
			await useUsernameMode();
			const account = await registerUsernameAccount('sharedcount');
			await getInstanceConfigRepository().updateCaptchaConfig({enabled: false});
			const previous = {testMode: config.dev.testModeEnabled, disabled: config.dev.disableRateLimits};
			config.dev.testModeEnabled = false;
			config.dev.disableRateLimits = false;
			try {
				for (let source = 0; source < 20; source++) {
					for (let success = 0; success < 5; success++) {
						await createBuilderWithoutAuth(harness)
							.post('/auth/login')
							.header('x-forwarded-for', `203.0.113.${source + 101}`)
							.body({login: account.username, password: PASSWORD})
							.execute();
					}
				}
				await createBuilderWithoutAuth(harness)
					.post('/auth/login')
					.header('x-forwarded-for', '198.51.100.95')
					.body({login: account.username, password: NEW_PASSWORD})
					.expect(400, 'INVALID_FORM_BODY')
					.execute();
			} finally {
				config.dev.testModeEnabled = previous.testMode;
				config.dev.disableRateLimits = previous.disabled;
			}
		});

		it('never lets crafted identifiers fill another source bucket for a named user', async () => {
			await useUsernameMode();
			const account = await registerUsernameAccount('bucketvictim');
			await getInstanceConfigRepository().updateCaptchaConfig({enabled: false});
			const previous = {testMode: config.dev.testModeEnabled, disabled: config.dev.disableRateLimits};
			config.dev.testModeEnabled = false;
			config.dev.disableRateLimits = false;
			const victimSource = getSameIpDecisionKey('198.51.100.71') ?? '198.51.100.71';
			try {
				for (const [field, attackerIp] of [
					['email', '198.51.100.70'],
					['login', '198.51.100.72'],
				] as const) {
					for (let attempt = 0; attempt < 5; attempt++) {
						await createBuilderWithoutAuth(harness)
							.post('/auth/login')
							.header('x-forwarded-for', attackerIp)
							.body({[field]: `${account.username}:${victimSource}`, password: NEW_PASSWORD})
							.expect(400, 'INVALID_FORM_BODY')
							.execute();
					}
				}
				const owner = await createBuilderWithoutAuth<LoginSuccessResponse>(harness)
					.post('/auth/login')
					.header('x-forwarded-for', '198.51.100.71')
					.body({login: account.username, password: PASSWORD})
					.execute();
				expect(owner.user_id).toBe(account.userId);
			} finally {
				config.dev.testModeEnabled = previous.testMode;
				config.dev.disableRateLimits = previous.disabled;
			}
		});

		it('counts older-app sign-ins against the same per-username limit', async () => {
			await useUsernameMode();
			useInstanceHosts();
			const account = await registerUsernameAccount('oldapplimit');
			await getInstanceConfigRepository().updateCaptchaConfig({enabled: false});
			const previous = {testMode: config.dev.testModeEnabled, disabled: config.dev.disableRateLimits};
			config.dev.testModeEnabled = false;
			config.dev.disableRateLimits = false;
			try {
				for (const email of ['oldapplimit', 'OldAppLimit@chat.example.org', 'oldapplimit@example.org']) {
					await createBuilderWithoutAuth(harness)
						.post('/auth/login')
						.header('x-forwarded-for', '198.51.100.50')
						.body({email, password: NEW_PASSWORD})
						.expect(400, 'INVALID_FORM_BODY')
						.execute();
				}
				for (let attempt = 0; attempt < 2; attempt++) {
					await createBuilderWithoutAuth(harness)
						.post('/auth/login')
						.header('x-forwarded-for', '198.51.100.50')
						.body({login: account.username, password: NEW_PASSWORD})
						.expect(400, 'INVALID_FORM_BODY')
						.execute();
				}
				await createBuilderWithoutAuth(harness)
					.post('/auth/login')
					.header('x-forwarded-for', '198.51.100.50')
					.body({email: 'OLDAPPLIMIT@chat.example.org', password: PASSWORD})
					.expect(429)
					.execute();
			} finally {
				config.dev.testModeEnabled = previous.testMode;
				config.dev.disableRateLimits = previous.disabled;
			}
		});

		it('never asks for IP authorization', async () => {
			await useUsernameMode();
			const account = await registerUsernameAccount('newipperson');
			const result = await createBuilderWithoutAuth<LoginSuccessResponse>(harness)
				.post('/auth/login')
				.header('x-forwarded-for', '203.0.113.77')
				.body({login: account.username, password: PASSWORD})
				.execute();
			expect(result.user_id).toBe(account.userId);
			expect(await listTestEmails(harness)).toEqual([]);
		});
	});

	describe('email-only routes', () => {
		it('refuses every email route before minting anything', async () => {
			await useUsernameMode();
			const admin = await registerUsernameAccount('emailrouteadmin');
			const account = await registerUsernameAccount('emailrouteuser');
			for (const [method, path] of EMAIL_ONLY_PUBLIC_ROUTES) {
				const {response, json} = await createBuilderWithoutAuth<{code: string}>(harness)
					[method.toLowerCase() as 'post'](path)
					.body({})
					.executeRaw();
				expect({path, status: response.status, code: json?.code}).toEqual({
					path,
					status: 400,
					code: APIErrorCodes.EMAIL_UNAVAILABLE_ON_INSTANCE,
				});
			}
			for (const [method, path] of EMAIL_ONLY_USER_ROUTES) {
				const {response, json} = await createBuilder<{code: string}>(harness, account.token)
					[method.toLowerCase() as 'post'](path)
					.body({})
					.executeRaw();
				expect({path, status: response.status, code: json?.code}).toEqual({
					path,
					status: 400,
					code: APIErrorCodes.EMAIL_UNAVAILABLE_ON_INSTANCE,
				});
			}
			for (const [method, path] of [
				['post', `/admin/users/${account.userId}/password-reset`],
				['post', `/admin/users/${account.userId}/verification-email`],
				['patch', `/admin/users/${account.userId}/email`],
				['put', `/admin/users/${account.userId}/email-verification`],
			] as const) {
				const {response, json} = await createBuilder<{code: string}>(harness, admin.token)
					[method](path)
					.body({})
					.executeRaw();
				expect({path, status: response.status, code: json?.code}).toEqual({
					path,
					status: 400,
					code: APIErrorCodes.EMAIL_UNAVAILABLE_ON_INSTANCE,
				});
			}
			expect(await listTestEmails(harness)).toEqual([]);
		});

		it('treats a missing email verification as satisfied', async () => {
			await useUsernameMode();
			const account = await registerUsernameAccount('unverifiedguild');
			await createBuilder(harness, '')
				.post(`/test/users/${account.userId}/security-flags`)
				.body({email_verified: false})
				.execute();
			await createBuilder(harness, account.token).post('/guilds').body({name: 'Username Guild'}).execute();
		});
	});

	describe('usernames stay unique', () => {
		it('refuses a rename to a name someone else holds', async () => {
			await useUsernameMode();
			await registerUsernameAccount('holder');
			const other = await registerUsernameAccount('renamer');
			await expectFieldErrors(
				{
					path: '/users/@me',
					method: 'PATCH',
					token: other.token,
					body: {username: 'HOLDER', password: PASSWORD},
				},
				[{path: 'username', code: ValidationErrorCodes.USERNAME_ALREADY_TAKEN}],
			);
			const renamed = await createBuilder<CurrentUser>(harness, other.token)
				.patch('/users/@me')
				.body({username: 'freshname', password: PASSWORD})
				.execute();
			expect(renamed.username).toBe('freshname');
		});

		it('refuses an admin rename to a name someone else holds', async () => {
			await useUsernameMode();
			const admin = await registerUsernameAccount('renameadmin');
			await registerUsernameAccount('adminholder');
			const target = await registerUsernameAccount('admintarget');
			const {response, json} = await createBuilder<ValidationErrorBody>(harness, admin.token)
				.patch(`/admin/users/${target.userId}/username`)
				.body({username: 'AdminHolder'})
				.executeRaw();
			expect(response.status).toBe(400);
			expect(json.errors.map(({path, code}) => ({path, code}))).toEqual([
				{path: 'username', code: ValidationErrorCodes.USERNAME_ALREADY_TAKEN},
			]);
		});

		it('lets an unclaimed account claim with a username and password', async () => {
			await useUsernameMode();
			const unclaimed = await createBuilderWithoutAuth<{user_id: string; token: string}>(harness)
				.post('/auth/register')
				.body({global_name: 'Guest'})
				.execute();
			await markSessionStarted(unclaimed.user_id);
			const claimed = await createBuilder<CurrentUser>(harness, unclaimed.token)
				.patch('/users/@me')
				.body({username: 'claimedguest', new_password: PASSWORD})
				.execute();
			expect({username: claimed.username, discriminator: claimed.discriminator}).toEqual({
				username: 'claimedguest',
				discriminator: '0000',
			});
			const result = await login({login: 'claimedguest', password: PASSWORD});
			expect(result.user_id).toBe(unclaimed.user_id);
		});
	});

	describe('username availability', () => {
		async function checkAvailability(username: string): Promise<boolean> {
			const result = await createBuilderWithoutAuth<{available: boolean}>(harness)
				.get(`/auth/username-availability?username=${encodeURIComponent(username)}`)
				.execute();
			return result.available;
		}

		async function expectAvailabilityFieldError(username: string, code: string): Promise<void> {
			const {json} = await createBuilderWithoutAuth<ValidationErrorBody>(harness)
				.get(`/auth/username-availability?username=${encodeURIComponent(username)}`)
				.expect(400, 'INVALID_FORM_BODY')
				.executeWithResponse();
			expect(json.errors.map(({path, code}) => ({path, code}))).toEqual([{path: 'username', code}]);
		}

		it('reports a held name as taken in any case and a free one as available', async () => {
			await useUsernameMode();
			await registerUsernameAccount('HeldName');
			expect(await checkAvailability('heldname')).toBe(false);
			expect(await checkAvailability('HELDNAME')).toBe(false);
			expect(await checkAvailability(' heldname ')).toBe(false);
			expect(await checkAvailability('freename')).toBe(true);
		});

		it('rejects an invalid or reserved name with the registration field codes', async () => {
			await useUsernameMode();
			await expectAvailabilityFieldError('not valid', ValidationErrorCodes.USERNAME_INVALID_CHARACTERS);
			await expectAvailabilityFieldError('everyone', ValidationErrorCodes.USERNAME_RESERVED_VALUE);
			await expectAvailabilityFieldError('myfluxername', ValidationErrorCodes.USERNAME_CANNOT_CONTAIN_RESERVED_TERMS);
			await expectAvailabilityFieldError('a'.repeat(33), ValidationErrorCodes.USERNAME_LENGTH_INVALID);
		});

		it('refuses an email instance with tags and the hosted service', async () => {
			await useSelfHostedEmailMode();
			await createBuilderWithoutAuth(harness)
				.get('/auth/username-availability?username=anyname')
				.expect(400, APIErrorCodes.USERNAME_SIGN_IN_ONLY)
				.execute();
			config.instance.selfHosted = false;
			await createBuilderWithoutAuth(harness)
				.get('/auth/username-availability?username=anyname')
				.expect(400, APIErrorCodes.USERNAME_SIGN_IN_ONLY)
				.execute();
		});
	});

	describe('password change', () => {
		it('is username mode only', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post('/users/@me/password')
				.body({password: account.password, new_password: NEW_PASSWORD})
				.expect(400, APIErrorCodes.USERNAME_SIGN_IN_ONLY)
				.execute();
		});

		it('needs sudo, replaces the session and ends every other session', async () => {
			await useUsernameMode();
			const account = await registerUsernameAccount('changepassword');
			const otherSession = await login({login: account.username, password: PASSWORD});
			await createBuilder(harness, account.token)
				.post('/users/@me/password')
				.body({new_password: NEW_PASSWORD})
				.expect(403, APIErrorCodes.SUDO_MODE_REQUIRED)
				.execute();
			const result = await createBuilder<{token: string; auth_session_id_hash: string}>(harness, account.token)
				.post('/users/@me/password')
				.body({password: PASSWORD, new_password: NEW_PASSWORD})
				.execute();
			expect(result.token.length).toBeGreaterThan(0);
			expect(result.auth_session_id_hash.length).toBeGreaterThan(0);
			await createBuilder(harness, account.token).get('/users/@me').expect(401).execute();
			await createBuilder(harness, otherSession.token).get('/users/@me').expect(401).execute();
			expect((await fetchCurrentUser(result.token)).id).toBe(account.userId);
			await expectFieldErrors({path: '/auth/login', body: {login: account.username, password: PASSWORD}}, [
				{path: 'login', code: ValidationErrorCodes.INVALID_LOGIN_OR_PASSWORD},
				{path: 'password', code: ValidationErrorCodes.INVALID_LOGIN_OR_PASSWORD},
			]);
			expect((await login({login: account.username, password: NEW_PASSWORD})).user_id).toBe(account.userId);
		});
	});

	describe('single sign-on accounts without a password', () => {
		it('cannot set a password or create a recovery kit with an empty sudo body', async () => {
			await useUsernameMode();
			const account = await registerUsernameAccount('ssoperson');
			const users = getUserRepository();
			const userId = createUserID(BigInt(account.userId));
			const user = await users.findUniqueAssert(userId);
			await users.patchUpsert(userId, {password_hash: null, traits: new Set(['sso'])}, user.toRow());
			await expectFieldErrors({path: '/users/@me/password', token: account.token, body: {new_password: NEW_PASSWORD}}, [
				{path: 'new_password', code: ValidationErrorCodes.PASSWORD_NOT_SET},
			]);
			await expectFieldErrors({path: '/users/@me/recovery-kit', token: account.token, body: {}}, [
				{path: 'password', code: ValidationErrorCodes.PASSWORD_NOT_SET},
			]);
			expect((await users.findUniqueAssert(userId)).passwordHash).toBeNull();
		});
	});

	describe('admin password reset link', () => {
		it('is username mode only', async () => {
			const admin = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
			const target = await createTestAccount(harness);
			await createBuilder(harness, admin.token)
				.post(`/admin/users/${target.userId}/password-reset-link`)
				.expect(400, APIErrorCodes.USERNAME_SIGN_IN_ONLY)
				.execute();
		});

		it('needs its own ACL', async () => {
			await useUsernameMode();
			await registerUsernameAccount('bootstrapadmin');
			const limitedAdmin = await registerUsernameAccount('limitedadmin');
			await createBuilder(harness, '')
				.post(`/test/users/${limitedAdmin.userId}/acls`)
				.body({acls: [AdminACLs.AUTHENTICATE, AdminACLs.USER_UPDATE_EMAIL]})
				.execute();
			const target = await registerUsernameAccount('linktargetacl');
			await createBuilder(harness, limitedAdmin.token)
				.post(`/admin/users/${target.userId}/password-reset-link`)
				.expect(403)
				.execute();
		});

		it('issues a link that resets the password through /auth/reset and is audited', async () => {
			await useUsernameMode();
			const admin = await registerUsernameAccount('resetadmin');
			const target = await registerUsernameAccount('resettarget');
			const before = Date.now();
			const link = await createBuilder<{url: string; expires_at: string}>(harness, admin.token)
				.post(`/admin/users/${target.userId}/password-reset-link`)
				.execute();
			const [base, token] = link.url.split('/reset#token=');
			expect(base).toBe(config.email.appBaseUrl);
			expect(token).toMatch(/^[A-Za-z0-9_-]+$/);
			const expiresAt = Date.parse(link.expires_at);
			expect(expiresAt).toBeGreaterThan(before + 59 * 60 * 1000);
			expect(expiresAt).toBeLessThanOrEqual(Date.now() + 60 * 60 * 1000);
			const validation = await createBuilderWithoutAuth<{valid: boolean}>(harness)
				.get(`/auth/reset/${token}`)
				.execute();
			expect(validation.valid).toBe(true);
			const reset = await createBuilderWithoutAuth<LoginSuccessResponse>(harness)
				.post('/auth/reset')
				.body({token, password: NEW_PASSWORD})
				.execute();
			expect(reset.user_id).toBe(target.userId);
			await createBuilder(harness, target.token).get('/users/@me').expect(401).execute();
			expect((await login({login: target.username, password: NEW_PASSWORD})).user_id).toBe(target.userId);
			await createBuilderWithoutAuth(harness)
				.post('/auth/reset')
				.body({token, password: PASSWORD})
				.expect(400)
				.execute();
			const audit = (await getAdminRepository().listAllAuditLogsPaginated(1000)).find(
				(log) => log.action === 'create_password_reset_link',
			);
			expect(audit?.targetId.toString()).toBe(target.userId);
			expect(Object.fromEntries(audit!.metadata)).toEqual({});
			expect(await listTestEmails(harness)).toEqual([]);
		});

		it('revokes earlier links when a new one is created', async () => {
			await useUsernameMode();
			const admin = await registerUsernameAccount('revokeadmin');
			const target = await registerUsernameAccount('revoketarget');
			const createLink = async () =>
				(
					await createBuilder<{url: string}>(harness, admin.token)
						.post(`/admin/users/${target.userId}/password-reset-link`)
						.execute()
				).url.split('/reset#token=')[1]!;
			const first = await createLink();
			const second = await createLink();
			const validate = async (token: string) =>
				(await createBuilderWithoutAuth<{valid: boolean}>(harness).get(`/auth/reset/${token}`).execute()).valid;
			expect(await validate(first)).toBe(false);
			expect(await validate(second)).toBe(true);
		});
	});

	describe('tags', () => {
		const DISCRIMINATOR_REFUSED = [
			{path: 'discriminator', code: ValidationErrorCodes.DISCRIMINATOR_NOT_SUPPORTED_ON_INSTANCE},
		];

		it('keeps unique names when username mode is given a random tag style', async () => {
			config.instance.selfHosted = true;
			await getInstanceConfigRepository().setAccountIdentityMode(AccountIdentityModes.USERNAME, 'setup', 'random');
			expect(await getInstanceConfigRepository().getTagStyle()).toBe('none');
			const account = await registerUsernameAccount('ForcedUnique');
			expect((await fetchCurrentUser(account.token)).discriminator).toBe('0000');
			await expectFieldErrors(
				{
					path: '/auth/register',
					body: {username: 'forcedunique', password: PASSWORD, date_of_birth: '2000-01-01'},
				},
				[{path: 'username', code: ValidationErrorCodes.USERNAME_ALREADY_TAKEN}],
			);
		});

		it('gives every new person the 0000 tag', async () => {
			await useUsernameMode();
			const account = await registerUsernameAccount('tagperson');
			expect((await fetchCurrentUser(account.token)).discriminator).toBe('0000');
			const unclaimed = await createBuilderWithoutAuth<{user_id: string}>(harness)
				.post('/auth/register')
				.body({global_name: 'tagperson'})
				.execute();
			const unclaimedUser = await getUserRepository().findUniqueAssert(createUserID(BigInt(unclaimed.user_id)));
			expect(unclaimedUser.discriminator).toBe(0);
			expect(unclaimedUser.username.toLowerCase()).not.toBe('tagperson');
		});

		it('never lets two people hold the same name', async () => {
			await useUsernameMode();
			await registerUsernameAccount('OnlyOne');
			for (const username of ['onlyone', 'ONLYONE', 'DeletedUser']) {
				await expectFieldErrors({path: '/auth/register', body: {username, password: PASSWORD}}, [
					{path: 'username', code: ValidationErrorCodes.USERNAME_ALREADY_TAKEN},
				]);
			}
		});

		it('keeps a name held while an admin deletion is pending', async () => {
			await useUsernameMode();
			const admin = await registerUsernameAccount('heldadmin');
			const target = await registerUsernameAccount('PendingGone');
			await createBuilder(harness, admin.token)
				.put(`/admin/users/${target.userId}/deletion`)
				.body({reason_code: 2, days_until_deletion: 30})
				.execute();
			await expectFieldErrors({path: '/auth/register', body: {username: 'pendinggone', password: PASSWORD}}, [
				{path: 'username', code: ValidationErrorCodes.USERNAME_ALREADY_TAKEN},
			]);
		});

		it('signs in with name#0 and name#0000 and refuses any other tag', async () => {
			await useUsernameMode();
			const account = await registerUsernameAccount('ZeroTag');
			expect((await login({login: 'zerotag#0', password: PASSWORD})).user_id).toBe(account.userId);
			expect((await login({login: 'ZeroTag#0000', password: PASSWORD})).user_id).toBe(account.userId);
			await expectFieldErrors({path: '/auth/login', body: {login: 'zerotag#0001', password: PASSWORD}}, [
				{path: 'login', code: ValidationErrorCodes.INVALID_LOGIN_OR_PASSWORD},
				{path: 'password', code: ValidationErrorCodes.INVALID_LOGIN_OR_PASSWORD},
			]);
		});

		it('refuses a tag change, even with the custom tag perk, and keeps 0000 through renames', async () => {
			await useUsernameMode();
			const account = await registerUsernameAccount('fixedtag');
			await grantPremium(harness, account.userId, UserPremiumTypes.SUBSCRIPTION);
			await expectFieldErrors(
				{path: '/users/@me', method: 'PATCH', token: account.token, body: {discriminator: '1234', password: PASSWORD}},
				DISCRIMINATOR_REFUSED,
			);
			await expectFieldErrors(
				{
					path: '/users/@me',
					method: 'PATCH',
					token: account.token,
					body: {username: 'fixedtagtwo', discriminator: '0042', password: PASSWORD},
				},
				DISCRIMINATOR_REFUSED,
			);
			const unchanged = await createBuilder<CurrentUser>(harness, account.token)
				.patch('/users/@me')
				.body({discriminator: '0000', password: PASSWORD})
				.execute();
			expect(unchanged.discriminator).toBe('0000');
			const renamed = await createBuilder<CurrentUser>(harness, account.token)
				.patch('/users/@me')
				.body({username: 'FixedTagTwo', password: PASSWORD})
				.execute();
			expect({username: renamed.username, discriminator: renamed.discriminator}).toEqual({
				username: 'FixedTagTwo',
				discriminator: '0000',
			});
			const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
			expect(user.premiumFlags & PremiumFlags.DISCRIMINATOR).toBe(0);
		});

		it('refuses an admin tag change and keeps 0000 on admin renames', async () => {
			await useUsernameMode();
			const admin = await registerUsernameAccount('tagadmin');
			const target = await registerUsernameAccount('tagtarget');
			const {response, json} = await createBuilder<ValidationErrorBody>(harness, admin.token)
				.patch(`/admin/users/${target.userId}/username`)
				.body({username: 'tagtargetnew', discriminator: '0007'})
				.executeRaw();
			expect(response.status).toBe(400);
			expect(json.errors.map(({path, code}) => ({path, code}))).toEqual(DISCRIMINATOR_REFUSED);
			await createBuilder(harness, admin.token)
				.patch(`/admin/users/${target.userId}/username`)
				.body({username: 'TagTargetNew'})
				.execute();
			const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(target.userId)));
			expect({username: user.username, discriminator: user.discriminator}).toEqual({
				username: 'TagTargetNew',
				discriminator: 0,
			});
		});

		it('skips the premium tag reroll at session start', async () => {
			await useUsernameMode();
			const account = await registerUsernameAccount('noreroll');
			await markPremiumDiscriminator(account.userId);
			const previousPremiumMode = getCachedInstancePremiumMode();
			setCachedInstancePremiumMode('mirror');
			try {
				await startSession(account.token);
			} finally {
				setCachedInstancePremiumMode(previousPremiumMode);
			}
			const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
			expect(user.discriminator).toBe(0);
			expect(user.premiumFlags & PremiumFlags.DISCRIMINATOR).toBe(PremiumFlags.DISCRIMINATOR);
		});

		it('keeps random tags for bots', async () => {
			await useUsernameMode();
			const owner = await registerUsernameAccount('botowner');
			const app = await createBuilder<{id: string; bot?: {discriminator: string}}>(harness, owner.token)
				.post('/oauth2/applications')
				.body({name: 'botowner', redirect_uris: []})
				.execute();
			expect(app.bot?.discriminator).toMatch(/^\d{4}$/);
			expect(app.bot?.discriminator).not.toBe('0000');
		});

		it('checks tag availability by name alone', async () => {
			await useUsernameMode();
			await registerUsernameAccount('heldtag');
			const account = await registerUsernameAccount('checktag');
			const checkTag = async (username: string, discriminator: string) =>
				(
					await createBuilder<{taken: boolean}>(harness, account.token)
						.get(`/users/check-tag?username=${username}&discriminator=${discriminator}`)
						.execute()
				).taken;
			expect(await checkTag('HeldTag', '0000')).toBe(true);
			expect(await checkTag('heldtag', '1234')).toBe(true);
			expect(await checkTag('CheckTag', '0000')).toBe(false);
			expect(await checkTag('freetag', '0000')).toBe(false);
		});

		it('shows bare names in the change log and finds people by bare name', async () => {
			await useUsernameMode();
			const admin = await registerUsernameAccount('logadmin');
			const target = await registerUsernameAccount('logbefore');
			await createBuilder(harness, target.token)
				.patch('/users/@me')
				.body({username: 'logafter', password: PASSWORD})
				.execute();
			const log = await createBuilder<{entries: Array<{field: string; old_value: string; new_value: string}>}>(
				harness,
				admin.token,
			)
				.get(`/admin/users/${target.userId}/change-log`)
				.execute();
			const tagChanges = log.entries
				.filter((entry) => entry.field === 'fluxer_tag')
				.map(({old_value, new_value}) => ({old_value, new_value}));
			expect(tagChanges).toEqual([{old_value: 'logbefore', new_value: 'logafter'}]);
			const found = await createBuilder<{users: Array<{id: string}>}>(harness, admin.token)
				.get('/admin/users?resolve=LogAfter')
				.execute();
			expect(found.users.map((user) => user.id)).toEqual([target.userId]);
		});
	});

	describe('email mode with unique usernames', () => {
		const TAG_REFUSED = [{path: 'discriminator', code: ValidationErrorCodes.DISCRIMINATOR_NOT_SUPPORTED_ON_INSTANCE}];

		it('gives email registrations 0000 and refuses a taken name in any case', async () => {
			await useSelfHostedEmailUniqueMode();
			const account = await registerEmailAccount('EmailUnique');
			expect((await fetchCurrentUser(account.token)).discriminator).toBe('0000');
			await expectFieldErrors(
				{
					path: '/auth/register',
					body: {email: 'other-unique@example.com', username: 'emailunique', password: PASSWORD},
				},
				[{path: 'username', code: ValidationErrorCodes.USERNAME_ALREADY_TAKEN}],
			);
		});

		it('keeps email sign-in and refuses a username login', async () => {
			await useSelfHostedEmailUniqueMode();
			const account = await registerEmailAccount('emailsignin');
			expect((await login({email: account.email, password: PASSWORD})).user_id).toBe(account.userId);
			await expectFieldErrors({path: '/auth/login', body: {login: 'emailsignin', password: PASSWORD}}, [
				{path: 'login', code: ValidationErrorCodes.INVALID_EMAIL_FORMAT},
			]);
		});

		it('refuses a rename to a held name and any tag change', async () => {
			await useSelfHostedEmailUniqueMode();
			await registerEmailAccount('heldbyother');
			const account = await registerEmailAccount('renamer');
			await grantPremium(harness, account.userId, UserPremiumTypes.SUBSCRIPTION);
			await expectFieldErrors(
				{
					path: '/users/@me',
					method: 'PATCH',
					token: account.token,
					body: {username: 'HeldByOther', password: PASSWORD},
				},
				[{path: 'username', code: ValidationErrorCodes.USERNAME_ALREADY_TAKEN}],
			);
			await expectFieldErrors(
				{path: '/users/@me', method: 'PATCH', token: account.token, body: {discriminator: '1234', password: PASSWORD}},
				TAG_REFUSED,
			);
			const renamed = await createBuilder<CurrentUser>(harness, account.token)
				.patch('/users/@me')
				.body({username: 'renamedfree', password: PASSWORD})
				.execute();
			expect({username: renamed.username, discriminator: renamed.discriminator}).toEqual({
				username: 'renamedfree',
				discriminator: '0000',
			});
			const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
			expect(user.premiumFlags & PremiumFlags.DISCRIMINATOR).toBe(0);
		});

		it('skips the premium tag reroll at session start', async () => {
			await useSelfHostedEmailUniqueMode();
			const account = await registerEmailAccount('emailnoreroll');
			await markPremiumDiscriminator(account.userId);
			const previousPremiumMode = getCachedInstancePremiumMode();
			setCachedInstancePremiumMode('mirror');
			try {
				await startSession(account.token);
			} finally {
				setCachedInstancePremiumMode(previousPremiumMode);
			}
			const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
			expect(user.discriminator).toBe(0);
			expect(user.premiumFlags & PremiumFlags.DISCRIMINATOR).toBe(PremiumFlags.DISCRIMINATOR);
		});

		it('answers the availability check', async () => {
			await useSelfHostedEmailUniqueMode();
			await registerEmailAccount('availheld');
			const taken = await createBuilderWithoutAuth<{available: boolean}>(harness)
				.get('/auth/username-availability?username=AvailHeld')
				.execute();
			expect(taken.available).toBe(false);
			const free = await createBuilderWithoutAuth<{available: boolean}>(harness)
				.get('/auth/username-availability?username=availfree')
				.execute();
			expect(free.available).toBe(true);
		});

		it('still serves the email routes', async () => {
			await useSelfHostedEmailUniqueMode();
			const account = await registerEmailAccount('emailroutes');
			await createBuilderWithoutAuth(harness).post('/auth/forgot').body({email: account.email}).expect(204).execute();
			await createBuilder(harness, account.token)
				.get('/users/@me/recovery-kit')
				.expect(400, APIErrorCodes.USERNAME_SIGN_IN_ONLY)
				.execute();
		});
	});

	describe('email mode stays as it was', () => {
		it('signs in by email and by login on the hosted service', async () => {
			const account = await createTestAccount(harness);
			expect((await login({email: account.email, password: account.password})).user_id).toBe(account.userId);
			expect((await login({login: account.email, password: account.password})).user_id).toBe(account.userId);
			await expectFieldErrors({path: '/auth/login', body: {email: account.email, password: NEW_PASSWORD}}, [
				{path: 'email', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
				{path: 'password', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
			]);
			await expectFieldErrors({path: '/auth/login', body: {login: account.email, password: NEW_PASSWORD}}, [
				{path: 'login', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
				{path: 'password', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
			]);
			await expectFieldErrors({path: '/auth/login', body: {login: account.username, password: account.password}}, [
				{path: 'login', code: ValidationErrorCodes.INVALID_EMAIL_FORMAT},
			]);
		});

		for (const variant of ['hosted', 'self-hosted email'] as const) {
			it(`answers older-app bodies as before on the ${variant} instance`, async () => {
				if (variant === 'self-hosted email') {
					await useSelfHostedEmailMode();
				}
				useInstanceHosts();
				const account = await createTestAccount(harness);
				const notAnAddress = [
					{path: 'email', code: ValidationErrorCodes.INVALID_EMAIL_FORMAT},
					{path: 'email', code: ValidationErrorCodes.INVALID_EMAIL_LOCAL_PART},
				];
				await expectFieldErrors(
					{path: '/auth/login', body: {email: account.username, password: PASSWORD}},
					notAnAddress,
				);
				await expectFieldErrors(
					{path: '/auth/login', body: {email: `${account.username}@chat.example.org`, password: PASSWORD}},
					[
						{path: 'email', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
						{path: 'password', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
					],
				);
				await expectFieldErrors({path: '/auth/login', body: {password: PASSWORD}}, [
					{path: 'email', code: ValidationErrorCodes.INVALID_FORMAT},
				]);
				for (const login of ['', null]) {
					await expectFieldErrors({path: '/auth/login', body: {login, password: PASSWORD}}, [
						{path: 'email', code: ValidationErrorCodes.INVALID_FORMAT},
					]);
				}
				for (const strayLogin of ['', null, 5, 'x'.repeat(400)]) {
					const signedIn = await createBuilderWithoutAuth<LoginSuccessResponse>(harness)
						.post('/auth/login')
						.body({email: account.email, login: strayLogin, password: account.password})
						.execute();
					expect(signedIn.user_id).toBe(account.userId);
				}
				await expectFieldErrors(
					{path: '/auth/register', body: {email: 'not-an-address', password: PASSWORD, consent: true}},
					notAnAddress,
				);
				await expectFieldErrors(
					{
						path: '/auth/register',
						body: {email: account.email, password: PASSWORD, date_of_birth: '2000-01-01', consent: true},
					},
					[{path: 'email', code: ValidationErrorCodes.EMAIL_ALREADY_IN_USE}],
				);
				const email = `instancelocal-${Date.now()}@chat.example.org`;
				const registration = await createBuilderWithoutAuth<{user_id: string}>(harness)
					.post('/auth/register')
					.body({email, global_name: 'Hosted Person', password: PASSWORD, date_of_birth: '2000-01-01', consent: true})
					.execute();
				const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(registration.user_id)));
				expect({email: user.email, username: user.username}).toEqual({email, username: 'Hosted_Person'});
			});
		}

		it('counts the per-email sign-in limit before it needs a client IP', async () => {
			const account = await createTestAccount(harness);
			const emailLimit = {
				identifier: `login:email:${account.email.toLowerCase()}`,
				maxAttempts: 5,
				windowMs: 15 * 60 * 1000,
			};
			await getRateLimitService().resetLimit(emailLimit.identifier);
			await createBuilderWithoutAuth(harness)
				.post('/auth/login')
				.header('x-forwarded-for', '')
				.body({email: account.email, password: account.password})
				.expect(403)
				.execute();
			expect((await getRateLimitService().peekLimit(emailLimit)).remaining).toBe(4);
		});

		it('reports email in discovery on the hosted service', async () => {
			const discovery = await createBuilderWithoutAuth<{features: {account_identity: string}}>(harness)
				.get('/.well-known/fluxer')
				.execute();
			expect(discovery.features.account_identity).toBe(AccountIdentityModes.EMAIL);
		});

		for (const variant of ['hosted', 'self-hosted email'] as const) {
			it(`allocates random tags and allows tag changes on the ${variant} instance`, async () => {
				if (variant === 'self-hosted email') {
					await useSelfHostedEmailMode();
				}
				const account = await createTestAccount(harness);
				const me = await fetchCurrentUser(account.token);
				expect(me.discriminator).toMatch(/^\d{4}$/);
				expect(me.discriminator).not.toBe('0000');
				await grantPremium(harness, account.userId, UserPremiumTypes.SUBSCRIPTION);
				const changed = await createBuilder<CurrentUser>(harness, account.token)
					.patch('/users/@me')
					.body({discriminator: '0042', password: account.password})
					.execute();
				expect(changed.discriminator).toBe('0042');
			});
		}

		it('rerolls an expired premium tag on the hosted service', async () => {
			const account = await createTestAccount(harness);
			await markPremiumDiscriminator(account.userId);
			await startSession(account.token);
			const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
			expect(user.premiumFlags & PremiumFlags.DISCRIMINATOR).toBe(0);
			expect(user.discriminator).not.toBe(0);
		});

		for (const variant of ['hosted', 'self-hosted email'] as const) {
			it(`runs forgot and reset by email on the ${variant} instance`, async () => {
				if (variant === 'self-hosted email') {
					await useSelfHostedEmailMode();
				}
				const account = await createTestAccount(harness);
				await createBuilderWithoutAuth(harness).post('/auth/forgot').body({email: account.email}).expect(204).execute();
				const resetEmail = findLastTestEmail(
					await listTestEmails(harness, {recipient: account.email}),
					'password_reset',
				);
				const token = resetEmail!.metadata!.token!;
				const reset = await createBuilderWithoutAuth<LoginSuccessResponse>(harness)
					.post('/auth/reset')
					.body({token, password: NEW_PASSWORD})
					.execute();
				expect(reset.user_id).toBe(account.userId);
				expect((await login({email: account.email, password: NEW_PASSWORD})).user_id).toBe(account.userId);
			});
		}

		it('uses up the emailed reset link even when the session cannot be created', async () => {
			const account = await createTestAccount(harness);
			await createBuilderWithoutAuth(harness).post('/auth/forgot').body({email: account.email}).expect(204).execute();
			const token = findLastTestEmail(await listTestEmails(harness, {recipient: account.email}), 'password_reset')!
				.metadata!.token!;
			const users = getUserRepository();
			const userId = createUserID(BigInt(account.userId));
			const user = await users.findUniqueAssert(userId);
			await users.patchUpsert(userId, {traits: new Set(['registration_pending_approval'])}, user.toRow());
			await createBuilderWithoutAuth(harness)
				.post('/auth/reset')
				.body({token, password: NEW_PASSWORD})
				.expect(403)
				.execute();
			const validation = await createBuilderWithoutAuth<{valid: boolean}>(harness)
				.get(`/auth/reset/${token}`)
				.execute();
			expect(validation.valid).toBe(false);
		});

		it('keeps allowing an email on self-hosted email instances', async () => {
			await useSelfHostedEmailMode();
			const account = await createTestAccount(harness);
			const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
			expect(user.email).toBe(account.email);
			expect((await login({email: account.email, password: account.password})).user_id).toBe(account.userId);
		});

		it('rejects a reset token without an email', async () => {
			const account = await createTestAccount(harness);
			const token = createPasswordResetToken('a'.repeat(64));
			await getUserRepository().createPasswordResetToken({
				token_: token,
				user_id: createUserID(BigInt(account.userId)),
				email: null,
			});
			const validation = await createBuilderWithoutAuth<{valid: boolean}>(harness)
				.get(`/auth/reset/${token}`)
				.execute();
			expect(validation.valid).toBe(false);
			await createBuilderWithoutAuth(harness)
				.post('/auth/reset')
				.body({token, password: NEW_PASSWORD})
				.expect(400)
				.execute();
		});
	});
});
