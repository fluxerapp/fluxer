// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {assertSignInAllowed} from '@app/api/auth/AuthUtility';
import {
	clearTestEmails,
	createTestAccount,
	createTotpSecret,
	createUniqueEmail,
	disableSso,
	enableSso,
	listTestEmails,
	type TestAccount,
	totpCodeNow,
} from '@app/api/auth/tests/AuthTestUtils';
import {
	createAuthenticationResponse,
	createRegistrationResponse,
	createWebAuthnDevice,
	type WebAuthnAuthenticationOptions,
	type WebAuthnDevice,
	type WebAuthnRegistrationOptions,
} from '@app/api/auth/tests/WebAuthnTestUtils';
import {createApiContext} from '@app/api/CreateApiContext';
import {EnforcementDecisionRepository} from '@app/api/enforcement/EnforcementDecisionRepository';
import {formatNoticeDate} from '@app/api/enforcement/NoticeDates';
import {
	createStaff,
	decisionBody,
	emailsTo,
	getDecision,
	loadUser,
	suspend,
	suspendRequest,
} from '@app/api/enforcement/tests/EnforcementTestUtils';
import {applySharedListUpdate, resetSharedListsForTests} from '@app/api/infrastructure/activity/SharedLists';
import {ipBanCache} from '@app/api/middleware/IpBanMiddleware';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {AccountTemporarilySuspendedError} from '@fluxer/errors/src/domains/user/AccountTemporarilySuspendedError';
import type {EnforcementNotice} from '@fluxer/schema/src/domains/auth/EnforcementNoticeSchemas';
import {ms} from 'itty-time';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

interface SignInBody {
	code?: string;
	notice?: EnforcementNotice | null;
	token?: string;
	mfa?: boolean;
	ticket?: string;
	ip_authorization_required?: boolean;
}

const BLOCKED_IP = '45.33.10.1';
const DAY = ms('1 day');

describe('Enforcement sign-in', () => {
	let harness: ApiTestHarness;
	let staff: TestAccount;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		ipBanCache.resetCaches();
		resetSharedListsForTests();
		await clearTestEmails(harness);
		staff = await createStaff(harness);
	});

	afterEach(() => {
		vi.restoreAllMocks();
		resetSharedListsForTests();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	function login(account: TestAccount, ip?: string, password = account.password) {
		const builder = createBuilderWithoutAuth<SignInBody>(harness)
			.post('/auth/login')
			.body({email: account.email, password});
		if (ip) builder.header('x-forwarded-for', ip);
		return builder.executeRaw();
	}

	function completeTotp(ticket: string, secret: string, ip?: string) {
		const builder = createBuilderWithoutAuth<SignInBody>(harness)
			.post('/auth/login/mfa/totp')
			.body({ticket, code: totpCodeNow(secret)});
		if (ip) builder.header('x-forwarded-for', ip);
		return builder.executeRaw();
	}

	async function enableTotp(account: TestAccount): Promise<string> {
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: totpCodeNow(secret), password: account.password})
			.expect(HTTP_STATUS.OK)
			.execute();
		return secret;
	}

	async function middlewareBlock(ip: string): Promise<string> {
		const {response, text} = await createBuilderWithoutAuth(harness)
			.get('/users/@me')
			.header('x-forwarded-for', ip)
			.executeRaw();
		expect(response.status).toBe(HTTP_STATUS.FORBIDDEN);
		return text;
	}

	async function suspendPermanently(target: TestAccount, ips: Array<string> = []): Promise<string> {
		const {user} = await suspendRequest(harness, staff, target.userId, {
			duration_hours: 0,
			decision: decisionBody(),
			...(ips.length > 0 ? {block_networks: {ips}} : {}),
		})
			.expect(HTTP_STATUS.OK)
			.execute();
		return user.enforcement_decision_id!;
	}

	async function setLegacySuspension(account: TestAccount, until: Date): Promise<void> {
		const users = getUserRepository();
		const user = await loadUser(account);
		await users.patchUpsert(user.id, {temp_banned_until: until}, user.toRow());
		await users.updateFlags(user.id, (flags) => flags | UserFlags.DISABLED);
	}

	async function ipAuthorizationEmails(account: TestAccount) {
		return (await listTestEmails(harness, {recipient: account.email})).filter(
			(email) => email.type === 'ip_authorization',
		);
	}

	describe('notices', () => {
		test('an account without MFA sees its notice after the password', async () => {
			const target = await createTestAccount(harness);
			const decisionId = await suspend(harness, staff, target.userId);
			const {response, json} = await login(target);
			expect(response.status).toBe(HTTP_STATUS.FORBIDDEN);
			expect(json.code).toBe(APIErrorCodes.ACCOUNT_SUSPENDED_TEMPORARILY);
			expect(json.notice).toMatchObject({
				decision_id: decisionId,
				kind: 'suspension',
				permanent: false,
				rule: {title: 'Spam'},
				appeal: {email: 'appeals@fluxer.com', account_has_email: true},
			});
			expect(json.notice?.summary).toMatch(/^We suspended your account until /);
		});

		test('a wrong password on a suspended account gets no notice', async () => {
			const target = await createTestAccount(harness);
			await suspend(harness, staff, target.userId);
			const {response, json} = await login(target, undefined, 'not-the-password-1');
			expect(response.status).toBe(HTTP_STATUS.BAD_REQUEST);
			expect(json.notice).toBeUndefined();
		});

		test('the notice uses the request locale', async () => {
			const target = await createTestAccount(harness);
			await suspend(harness, staff, target.userId);
			const {json} = await createBuilderWithoutAuth<SignInBody>(harness)
				.post('/auth/login')
				.header('accept-language', 'de')
				.body({email: target.email, password: target.password})
				.executeRaw();
			expect(json.notice?.summary).toMatch(/\d{2}\.\d{2}\.\d{4}/);
		});

		test('an MFA account gets its ticket and sees the notice only after the code', async () => {
			const target = await createTestAccount(harness);
			const secret = await enableTotp(target);
			const decisionId = await suspend(harness, staff, target.userId);
			const first = await login(target);
			expect(first.response.status).toBe(HTTP_STATUS.OK);
			expect(first.json.mfa).toBe(true);
			expect(first.json.notice).toBeUndefined();
			const second = await completeTotp(first.json.ticket!, secret);
			expect(second.response.status).toBe(HTTP_STATUS.FORBIDDEN);
			expect(second.json.code).toBe(APIErrorCodes.ACCOUNT_SUSPENDED_TEMPORARILY);
			expect(second.json.notice?.decision_id).toBe(decisionId);
		});

		test('a passkey sign-in sees the notice', async () => {
			const target = await createTestAccount(harness);
			const device = await registerPasskey(target);
			const decisionId = await suspend(harness, staff, target.userId);
			const {response, json} = await passkeySignIn(device);
			expect(response.status).toBe(HTTP_STATUS.FORBIDDEN);
			expect(json.notice?.decision_id).toBe(decisionId);
		});

		test('an SSO sign-in sees the notice', async () => {
			const admin = await createStaff(harness);
			await enableSso(harness, admin.token);
			try {
				const email = createUniqueEmail('sso-signin');
				const first = await ssoSignIn(email);
				expect(first.response.status).toBe(HTTP_STATUS.OK);
				const decisionId = await suspend(harness, staff, first.json.user_id!);
				const second = await ssoSignIn(email);
				expect(second.response.status).toBe(HTTP_STATUS.FORBIDDEN);
				expect(second.json.code).toBe(APIErrorCodes.ACCOUNT_SUSPENDED_TEMPORARILY);
				expect(second.json.notice?.decision_id).toBe(decisionId);
			} finally {
				await disableSso(harness, admin.token);
			}
		});

		test('a permanent suspension returns PERMANENTLY', async () => {
			const target = await createTestAccount(harness);
			await suspendPermanently(target);
			const {json} = await login(target);
			expect(json.code).toBe(APIErrorCodes.ACCOUNT_SUSPENDED_PERMANENTLY);
			expect(json.notice).toMatchObject({kind: 'suspension', permanent: true, ends_at: null});
		});

		test('a pre-v2 100-year ban returns PERMANENTLY with the legacy notice', async () => {
			const target = await createTestAccount(harness);
			const until = new Date();
			until.setUTCFullYear(until.getUTCFullYear() + 100);
			await setLegacySuspension(target, until);
			const {json} = await login(target);
			expect(json.code).toBe(APIErrorCodes.ACCOUNT_SUSPENDED_PERMANENTLY);
			expect(json.notice).toMatchObject({
				decision_id: null,
				kind: 'suspension',
				permanent: true,
				facts: ["We don't have more detail on record for this decision."],
			});
		});

		test('a pre-v2 temporary suspension returns TEMPORARILY with the legacy notice', async () => {
			const target = await createTestAccount(harness);
			await setLegacySuspension(target, new Date(Date.now() + DAY));
			const {json} = await login(target);
			expect(json.code).toBe(APIErrorCodes.ACCOUNT_SUSPENDED_TEMPORARILY);
			expect(json.notice).toMatchObject({decision_id: null, kind: 'suspension', permanent: false});
		});

		test('a pre-v2 closure returns PERMANENTLY with its stored public reason', async () => {
			const target = await createTestAccount(harness);
			const users = getUserRepository();
			const user = await loadUser(target);
			await users.updateDeletionSchedule(user, {
				flags: user.flags | UserFlags.DELETED,
				pending_deletion_at: new Date(Date.now() + 30 * DAY),
				deletion_reason_code: 3,
				deletion_public_reason: 'Sent spam to many people.',
			});
			const {json} = await login(target);
			expect(json.code).toBe(APIErrorCodes.ACCOUNT_SUSPENDED_PERMANENTLY);
			expect(json.notice).toMatchObject({
				decision_id: null,
				kind: 'closure',
				statement: 'Sent spam to many people.',
			});
		});

		test.each([DeletionReasons.USER_REQUESTED, DeletionReasons.INACTIVITY])(
			'a pre-v2 staff deletion with code %s gets no enforcement notice',
			async (code) => {
				const target = await createTestAccount(harness);
				const user = await loadUser(target);
				await getUserRepository().updateDeletionSchedule(user, {
					flags: user.flags | UserFlags.DELETED,
					pending_deletion_at: new Date(Date.now() + 30 * DAY),
					deletion_reason_code: code,
					deletion_public_reason: 'Requested by the holder',
				});
				const {json} = await login(target);
				expect(json.code).toBe(APIErrorCodes.ACCOUNT_SUSPENDED_PERMANENTLY);
				expect(json.notice).toBeNull();
			},
		);

		test('an expired suspension is cleared at sign-in, recorded as expired and can still be reversed', async () => {
			const target = await createTestAccount(harness);
			const decisionId = await suspend(harness, staff, target.userId);
			const user = await loadUser(target);
			await getUserRepository().patchUpsert(user.id, {temp_banned_until: new Date(Date.now() - 1000)}, user.toRow());
			const {response} = await login(target);
			expect(response.status).toBe(HTTP_STATUS.OK);
			const cleared = await loadUser(target);
			expect(cleared.enforcementDecisionId).toBeNull();
			expect(cleared.tempBannedUntil).toBeNull();
			expect(cleared.flags & UserFlags.DISABLED).toBe(0n);
			const expired = await createBuilder<{status: string; end_outcome: string | null; ended_by: string | null}>(
				harness,
				staff.token,
			)
				.get(`/admin/enforcement-decisions/${decisionId}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(expired).toMatchObject({status: 'ended', end_outcome: 'expired', ended_by: '0'});
			await createBuilder(harness, staff.token)
				.post(`/admin/enforcement-decisions/${decisionId}/end`)
				.header('X-Audit-Log-Reason', 'Appeal upheld')
				.body({outcome: 'lifted'})
				.expect(HTTP_STATUS.CONFLICT)
				.execute();
			await createBuilder(harness, staff.token)
				.post(`/admin/enforcement-decisions/${decisionId}/end`)
				.header('X-Audit-Log-Reason', 'Appeal upheld')
				.body({outcome: 'reversed'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const [ended] = await emailsTo(harness, target, 'enforcement_ended');
			expect(ended?.metadata).toMatchObject({outcome: 'reversed', decision_id: decisionId, restores_access: 'no'});
		});
	});

	describe('accounts with no email', () => {
		test('the first sign-in delivers the notice and starts the appeal deadline', async () => {
			const target = await createTestAccount(harness);
			const user = await loadUser(target);
			await getUserRepository().patchUpsert(user.id, {email: null}, user.toRow());
			const decisionId = await suspend(harness, staff, target.userId);
			expect((await getDecision(harness, staff, decisionId)).notice_status).toBe('undelivered');

			const before = Date.now();
			const first = await signInDirectly(target);
			const decision = await new EnforcementDecisionRepository().get(BigInt(decisionId));
			expect(decision?.notice_status).toBe('sent');
			expect(decision?.notice_channels).toEqual(new Set(['sign_in']));
			expect(decision?.notice_locale).toBe('en-US');
			expect(decision?.notice_text).toContain(first.summary);
			const sentAt = decision!.notice_sent_at!;
			expect(sentAt.getTime()).toBeGreaterThanOrEqual(before);
			expect(first.appeal).toEqual({
				email: 'appeals@fluxer.com',
				deadline: formatNoticeDate(new Date(sentAt.getTime() + 60 * DAY), 'en-US'),
				account_has_email: false,
			});

			const second = await signInDirectly(target);
			expect(second.appeal).toEqual(first.appeal);
			const unchanged = await new EnforcementDecisionRepository().get(BigInt(decisionId));
			expect(unchanged?.notice_sent_at).toEqual(sentAt);
		});

		async function signInDirectly(account: TestAccount): Promise<EnforcementNotice> {
			const ctx = createApiContext({
				requestId: 'sign-in-test',
				clientIp: '127.0.0.1',
				userAgent: null,
				locale: 'en-US',
				blockedNetworkSignIn: null,
			});
			const error = await assertSignInAllowed(ctx, await loadUser(account)).catch((caught: unknown) => caught);
			expect(error).toBeInstanceOf(AccountTemporarilySuspendedError);
			return (error as AccountTemporarilySuspendedError).data!['notice'] as EnforcementNotice;
		}
	});

	describe.each([
		{
			source: 'a linked block',
			block: async () => {
				const other = await createTestAccount(harness);
				const decisionId = await suspend(harness, staff, other.userId);
				await new AdminRepository().banIpForDecision(BLOCKED_IP, BigInt(decisionId), new Date(Date.now() + 90 * DAY));
				await ipBanCache.refresh();
			},
		},
		{
			source: 'the shared list',
			block: async () => {
				applySharedListUpdate('ip_blocked', `${BLOCKED_IP}\n`);
			},
		},
	])('signing in from $source', ({block}) => {
		test('every outcome but a restricted account gets the same block, with no ticket and no email', async () => {
			const plain = await createTestAccount(harness);
			const withMfa = await createTestAccount(harness);
			await enableTotp(withMfa);
			await block();
			await clearTestEmails(harness);
			const expected = await middlewareBlock(BLOCKED_IP);
			const unknown = {...plain, email: createUniqueEmail('nobody')};
			const attempts = [
				await login(unknown, BLOCKED_IP),
				await login(plain, BLOCKED_IP, 'not-the-password-1'),
				await login(plain, BLOCKED_IP),
				await login(withMfa, BLOCKED_IP),
			];
			for (const attempt of attempts) {
				expect(attempt.response.status).toBe(HTTP_STATUS.FORBIDDEN);
				expect(attempt.text).toBe(expected);
			}
			expect(await ipAuthorizationEmails(plain)).toEqual([]);
			expect((await loadUser(plain)).flags & UserFlags.DISABLED).toBe(0n);
		});

		test('a restricted account sees its notice', async () => {
			const target = await createTestAccount(harness);
			const decisionId = await suspend(harness, staff, target.userId);
			await block();
			const {response, json} = await login(target, BLOCKED_IP);
			expect(response.status).toBe(HTTP_STATUS.FORBIDDEN);
			expect(json.code).toBe(APIErrorCodes.ACCOUNT_SUSPENDED_TEMPORARILY);
			expect(json.notice?.decision_id).toBe(decisionId);
		});

		test('a restricted MFA account gets its ticket and then the notice', async () => {
			const target = await createTestAccount(harness);
			const secret = await enableTotp(target);
			const decisionId = await suspend(harness, staff, target.userId);
			await block();
			const first = await login(target, BLOCKED_IP);
			expect(first.response.status).toBe(HTTP_STATUS.OK);
			expect(first.json.ticket).toBeTruthy();
			const second = await completeTotp(first.json.ticket!, secret, BLOCKED_IP);
			expect(second.response.status).toBe(HTTP_STATUS.FORBIDDEN);
			expect(second.json.notice?.decision_id).toBe(decisionId);
		});

		test('an unrestricted MFA ticket from another network cannot be completed', async () => {
			const target = await createTestAccount(harness);
			const secret = await enableTotp(target);
			const first = await login(target);
			await block();
			const second = await completeTotp(first.json.ticket!, secret, BLOCKED_IP);
			expect(second.text).toBe(await middlewareBlock(BLOCKED_IP));
		});

		test('a self-deleted account is not reactivated', async () => {
			const target = await createTestAccount(harness);
			await createBuilderWithoutAuth(harness)
				.post(`/test/users/${target.userId}/set-pending-deletion`)
				.body({pending_deletion_at: new Date(Date.now() + 14 * DAY).toISOString(), set_self_deleted_flag: true})
				.expect(HTTP_STATUS.OK)
				.execute();
			await block();
			const {text} = await login(target, BLOCKED_IP);
			expect(text).toBe(await middlewareBlock(BLOCKED_IP));
			const user = await loadUser(target);
			expect(user.flags & UserFlags.SELF_DELETED).toBe(UserFlags.SELF_DELETED);
			expect(user.pendingDeletionAt).not.toBeNull();
		});

		test('other routes stay blocked', async () => {
			await block();
			const {response} = await createBuilderWithoutAuth(harness)
				.post('/auth/forgot')
				.header('x-forwarded-for', BLOCKED_IP)
				.body({email: createUniqueEmail('nobody')})
				.executeRaw();
			expect(response.status).toBe(HTTP_STATUS.FORBIDDEN);
		});
	});

	test('a holder behind a block from their own decision sees the notice, not the block', async () => {
		const target = await createTestAccount(harness, {ipAddress: BLOCKED_IP});
		const decisionId = await suspendPermanently(target, [BLOCKED_IP]);
		const {json} = await login(target, BLOCKED_IP);
		expect(json.code).toBe(APIErrorCodes.ACCOUNT_SUSPENDED_PERMANENTLY);
		expect(json.notice?.decision_id).toBe(decisionId);
	});

	test('SSO from a blocked network never provisions an account', async () => {
		const admin = await createStaff(harness);
		await enableSso(harness, admin.token);
		try {
			applySharedListUpdate('ip_blocked', `${BLOCKED_IP}\n`);
			const email = createUniqueEmail('sso-blocked');
			const {text} = await ssoSignIn(email, BLOCKED_IP);
			expect(text).toBe(await middlewareBlock(BLOCKED_IP));
			expect(await getUserRepository().findByEmail(email)).toBeNull();
		} finally {
			await disableSso(harness, admin.token);
		}
	});

	async function registerPasskey(account: TestAccount): Promise<WebAuthnDevice> {
		const device = createWebAuthnDevice();
		const secret = await enableTotp(account);
		const options = await createBuilder<WebAuthnRegistrationOptions>(harness, account.token)
			.post('/users/@me/mfa/webauthn/credentials/registration-options')
			.body({mfa_method: 'totp', mfa_code: totpCodeNow(secret)})
			.execute();
		if (options.rp.id) device.rpId = options.rp.id;
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/webauthn/credentials')
			.body({
				response: createRegistrationResponse(device, options, 'Passkey'),
				challenge: options.challenge,
				name: 'Passkey',
				mfa_method: 'totp',
				mfa_code: totpCodeNow(secret),
			})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		return device;
	}

	async function passkeySignIn(device: WebAuthnDevice) {
		const options = await createBuilderWithoutAuth<WebAuthnAuthenticationOptions>(harness)
			.post('/auth/webauthn/authentication-options')
			.body(null)
			.execute();
		if (options.rpId) device.rpId = options.rpId;
		return createBuilderWithoutAuth<SignInBody>(harness)
			.post('/auth/webauthn/authenticate')
			.body({response: createAuthenticationResponse(device, options), challenge: options.challenge})
			.executeRaw();
	}

	async function ssoSignIn(email: string, ip?: string) {
		const start = createBuilderWithoutAuth<{state: string}>(harness).post('/auth/sso/start').body({});
		if (ip) start.header('x-forwarded-for', ip);
		const {state} = await start.execute();
		const complete = createBuilderWithoutAuth<SignInBody & {user_id?: string}>(harness)
			.post('/auth/sso/complete')
			.body({code: email, state});
		if (ip) complete.header('x-forwarded-for', ip);
		return complete.executeRaw();
	}
});
