// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DEFERRED_PHONE_ON_COMMUNITY_JOIN,
	PHONE_GATE_PROMOTED_FROM_DEFERRAL,
	SuspiciousActivityFlags,
} from '@fluxer/constants/src/UserConstants';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';
import {setInjectedRegistrationRiskEvaluator} from '../../middleware/ServiceMiddleware';
import {getInstanceConfigRepository} from '../../middleware/ServiceSingletons';
import {
	authorizeOAuth2,
	createOAuth2Application,
	exchangeOAuth2AuthorizationCode,
} from '../../oauth/tests/OAuthTestUtils';
import {PHONE_GATE_ESCAPE_MAX_GUILDS} from '../../risk/DeferredPhoneGate';
import {
	RecommendedAction,
	RiskConfidence,
	RiskDecisionMethod,
	RiskLevel,
	type RiskLevel as RiskLevelType,
} from '../../risk/RiskTypes';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {NoopGatewayService} from '../../test/NoopGatewayService';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';
import type {IRegistrationRiskEvaluator} from '../services/IRegistrationRiskEvaluator';
import {
	createAuthHarness,
	createTestAccount,
	createUniqueEmail,
	createUniqueUsername,
	loginAccount,
	registerUser,
} from './AuthTestUtils';

function phoneRiskEvaluator(level: RiskLevelType, riskScore: number): IRegistrationRiskEvaluator {
	return {
		async evaluate() {
			return {
				level,
				recommendedAction: RecommendedAction.RequireOutboundPhone,
				assessment: {
					suspicious: true,
					level,
					confidence: RiskConfidence.High,
					riskScore,
					reasoning: 'deferred phone gate test',
					recommendedAction: RecommendedAction.RequireOutboundPhone,
					method: RiskDecisionMethod.Noop,
					modelUsed: 'test',
					rounds: 0,
					elapsedMs: 0,
					signals: {},
				},
			};
		},
	};
}

async function createGuildWithInvite(harness: ApiTestHarness): Promise<{guildId: string; inviteCode: string}> {
	let owner = await createTestAccount(harness);
	await createBuilderWithoutAuth(harness)
		.post(`/test/users/${owner.userId}/acls`)
		.body({acls: ['*']})
		.expect(200)
		.execute();
	owner = await loginAccount(harness, owner);
	const guild = await createBuilder<GuildResponse>(harness, owner.token)
		.post('/guilds')
		.body({name: `PhoneGate-${Date.now()}`})
		.execute();
	const invite = await createBuilder<{code: string}>(harness, owner.token)
		.post(`/channels/${guild.system_channel_id}`.concat('/invites'))
		.body({max_uses: 0, max_age: 0, unique: false, temporary: false})
		.execute();
	return {guildId: guild.id, inviteCode: invite.code};
}

async function readFlags(userId: string): Promise<number> {
	const {UserRepository} = await import('../../user/repositories/UserRepository');
	const {createUserID} = await import('../../BrandedTypes');
	const user = await new UserRepository().findUnique(createUserID(BigInt(userId)));
	return user?.suspiciousActivityFlags ?? 0;
}

async function readGuildIds(userId: string): Promise<Array<string>> {
	const {GuildRepository} = await import('../../guild/repositories/GuildRepository');
	const {createUserID} = await import('../../BrandedTypes');
	const guilds = await new GuildRepository().listUserGuilds(createUserID(BigInt(userId)));
	return guilds.map((guild) => guild.id.toString());
}

const ESCAPE_PATH = '/users/@me/required-actions/phone-gate-escape';
const RATE_LIMIT_HEADER = 'x-fluxer-test-enable-rate-limits';

interface EscapeSubject {
	userId: string;
	token: string;
}

interface EscapePreviewResponse {
	available: boolean;
	guilds: Array<{id: string; name: string}>;
	owned_guilds: Array<{id: string; name: string}>;
}

describe('Deferred phone verification gate', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		setInjectedRegistrationRiskEvaluator(undefined);
		await harness.reset();
	});
	afterAll(async () => {
		setInjectedRegistrationRiskEvaluator(undefined);
		await harness?.shutdown();
	});

	it('applies the phone requirement immediately while the gate is off', async () => {
		await getInstanceConfigRepository().setInstancePolicyConfig({deferred_phone_gate_enabled: false});
		setInjectedRegistrationRiskEvaluator(phoneRiskEvaluator(RiskLevel.High, 70));
		const registration = await registerUser(harness, {
			email: createUniqueEmail('gate-off'),
			username: createUniqueUsername('gate_off'),
			global_name: 'Gate Off',
			password: 'StrongPassword!123',
			date_of_birth: '2000-01-01',
			consent: true,
		});
		const flags = await readFlags(registration.user_id);
		expect(flags & SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE).not.toBe(0);
		expect(flags & DEFERRED_PHONE_ON_COMMUNITY_JOIN).toBe(0);
	});

	it('defers the phone requirement at registration while the gate is on', async () => {
		await getInstanceConfigRepository().setInstancePolicyConfig({deferred_phone_gate_enabled: true});
		setInjectedRegistrationRiskEvaluator(phoneRiskEvaluator(RiskLevel.High, 70));
		const registration = await registerUser(harness, {
			email: createUniqueEmail('gate-on'),
			username: createUniqueUsername('gate_on'),
			global_name: 'Gate On',
			password: 'StrongPassword!123',
			date_of_birth: '2000-01-01',
			consent: true,
		});
		const flags = await readFlags(registration.user_id);
		expect(flags & DEFERRED_PHONE_ON_COMMUNITY_JOIN).not.toBe(0);
		expect(flags & SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE).not.toBe(0);
		const me = await createBuilder<{required_actions: Array<string>}>(harness, registration.token)
			.get('/users/@me')
			.expect(200)
			.execute();
		expect(me.required_actions ?? []).toEqual([]);
	});

	it('lets a deferred account join a small guild without being challenged', async () => {
		await getInstanceConfigRepository().setInstancePolicyConfig({deferred_phone_gate_enabled: true});
		const {guildId, inviteCode} = await createGuildWithInvite(harness);
		setInjectedRegistrationRiskEvaluator(phoneRiskEvaluator(RiskLevel.High, 70));
		const registration = await registerUser(harness, {
			email: createUniqueEmail('gate-small'),
			username: createUniqueUsername('gate_small'),
			global_name: 'Gate Small',
			password: 'StrongPassword!123',
			date_of_birth: '2000-01-01',
			consent: true,
		});
		setInjectedRegistrationRiskEvaluator(undefined);
		await createBuilder(harness, registration.token).post(`/invites/${inviteCode}`).expect(200).execute();
		const flags = await readFlags(registration.user_id);
		expect(flags & DEFERRED_PHONE_ON_COMMUNITY_JOIN).not.toBe(0);
		expect(guildId).toBeTruthy();
	});

	it('does not defer the inbound-SMS tier, which stays enforced from registration', async () => {
		await getInstanceConfigRepository().setInstancePolicyConfig({deferred_phone_gate_enabled: true});
		setInjectedRegistrationRiskEvaluator({
			async evaluate() {
				return {
					level: RiskLevel.VeryHigh,
					recommendedAction: RecommendedAction.RequireInboundPhone,
					assessment: {
						suspicious: true,
						level: RiskLevel.VeryHigh,
						confidence: RiskConfidence.High,
						riskScore: 90,
						reasoning: 'inbound tier',
						recommendedAction: RecommendedAction.RequireInboundPhone,
						method: RiskDecisionMethod.Noop,
						modelUsed: 'test',
						rounds: 0,
						elapsedMs: 0,
						signals: {},
					},
				};
			},
		});
		const registration = await registerUser(harness, {
			email: createUniqueEmail('gate-inbound'),
			username: createUniqueUsername('gate_inbound'),
			global_name: 'Gate Inbound',
			password: 'StrongPassword!123',
			date_of_birth: '2000-01-01',
			consent: true,
		});
		const flags = await readFlags(registration.user_id);
		expect(flags & DEFERRED_PHONE_ON_COMMUNITY_JOIN).toBe(0);
		expect(flags & SuspiciousActivityFlags.REQUIRE_INBOUND_PHONE_VERIFICATION).not.toBe(0);
	});

	it('promotes the requirement and refuses the join on a qualifying guild inside the window', async () => {
		await getInstanceConfigRepository().setInstancePolicyConfig({
			deferred_phone_gate_enabled: true,
			deferred_phone_gate_member_threshold: 1,
			deferred_phone_gate_window_hours: 24,
		});
		const {inviteCode} = await createGuildWithInvite(harness);
		const filler = await createTestAccount(harness);
		await createBuilder(harness, filler.token).post(`/invites/${inviteCode}`).expect(200).execute();

		setInjectedRegistrationRiskEvaluator(phoneRiskEvaluator(RiskLevel.High, 70));
		const registration = await registerUser(harness, {
			email: createUniqueEmail('gate-qualifying'),
			username: createUniqueUsername('gate_qualifying'),
			global_name: 'Gate Qualifying',
			password: 'StrongPassword!123',
			date_of_birth: '2000-01-01',
			consent: true,
		});
		setInjectedRegistrationRiskEvaluator(undefined);
		expect((await readFlags(registration.user_id)) & DEFERRED_PHONE_ON_COMMUNITY_JOIN).not.toBe(0);

		await createBuilder(harness, registration.token).post(`/invites/${inviteCode}`).expect(403).execute();

		const flags = await readFlags(registration.user_id);
		expect(flags & DEFERRED_PHONE_ON_COMMUNITY_JOIN).toBe(0);
		expect(flags & SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE).not.toBe(0);
	});

	describe('phone gate escape', () => {
		async function configurePhoneGate(
			overrides: {
				deferred_phone_gate_enabled?: boolean;
				deferred_phone_gate_member_threshold?: number;
				deferred_phone_gate_window_hours?: number;
				single_community_enabled?: boolean;
				single_community_guild_id?: string | null;
			} = {},
		): Promise<void> {
			await getInstanceConfigRepository().setInstancePolicyConfig({
				deferred_phone_gate_enabled: true,
				deferred_phone_gate_member_threshold: 1,
				deferred_phone_gate_window_hours: 24,
				...overrides,
			});
		}

		async function registerDeferredUser(prefix: string): Promise<EscapeSubject> {
			setInjectedRegistrationRiskEvaluator(phoneRiskEvaluator(RiskLevel.High, 70));
			const registration = await registerUser(harness, {
				email: createUniqueEmail(prefix),
				username: createUniqueUsername(prefix),
				global_name: 'Gate Escape',
				password: 'StrongPassword!123',
				date_of_birth: '2000-01-01',
				consent: true,
			});
			setInjectedRegistrationRiskEvaluator(undefined);
			return {userId: registration.user_id, token: registration.token};
		}

		async function addFillerMember(inviteCode: string): Promise<void> {
			const filler = await createTestAccount(harness);
			await createBuilder(harness, filler.token).post(`/invites/${inviteCode}`).expect(200).execute();
		}

		async function makeDiscoverable(guildId: string): Promise<void> {
			await createBuilderWithoutAuth(harness)
				.post(`/test/guilds/${guildId}/features`)
				.body({add_features: ['DISCOVERABLE']})
				.execute();
		}

		async function setSuspiciousFlags(userId: string, flags: number): Promise<void> {
			await createBuilderWithoutAuth(harness)
				.post(`/test/users/${userId}/security-flags`)
				.body({suspicious_activity_flags: flags})
				.execute();
		}

		async function promoteThroughRefusedJoin(subject: EscapeSubject, inviteCode: string): Promise<void> {
			await createBuilder(harness, subject.token).post(`/invites/${inviteCode}`).expect(403).execute();
		}

		async function createPromotedSubject(prefix: string): Promise<EscapeSubject> {
			const gate = await createGuildWithInvite(harness);
			await addFillerMember(gate.inviteCode);
			const subject = await registerDeferredUser(prefix);
			await promoteThroughRefusedJoin(subject, gate.inviteCode);
			return subject;
		}

		function previewEscape(subject: EscapeSubject) {
			return createBuilder<EscapePreviewResponse>(harness, subject.token).get(ESCAPE_PATH).expect(200).execute();
		}

		function executeEscape(subject: EscapeSubject) {
			return createBuilder<{required_actions: Array<string> | null}>(harness, subject.token)
				.post(ESCAPE_PATH)
				.body({})
				.expect(200)
				.execute();
		}

		function expectEscapeRefused(subject: EscapeSubject) {
			return createBuilder(harness, subject.token)
				.post(ESCAPE_PATH)
				.body({})
				.expect(400, 'PHONE_GATE_ESCAPE_UNAVAILABLE')
				.execute();
		}

		it('answers both escape routes for an account every ordinary route refuses', async () => {
			await configurePhoneGate();
			const subject = await createPromotedSubject('escape_reach');

			await createBuilder(harness, subject.token)
				.get('/users/@me/guilds')
				.expect(403, 'ACCOUNT_SUSPICIOUS_ACTIVITY')
				.execute();

			const preview = await previewEscape(subject);
			expect(preview.available).toBe(true);
			await executeEscape(subject);
		});

		it('records the promotion without leaking the bit into the client projection', async () => {
			await configurePhoneGate();
			const subject = await createPromotedSubject('escape_bit');

			const flags = await readFlags(subject.userId);
			expect(flags & DEFERRED_PHONE_ON_COMMUNITY_JOIN).toBe(0);
			expect(flags & SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE).not.toBe(0);
			expect(flags & PHONE_GATE_PROMOTED_FROM_DEFERRAL).not.toBe(0);

			const me = await createBuilder<{required_actions: Array<string> | null; suspicious_activity_flags?: number}>(
				harness,
				subject.token,
			)
				.get('/users/@me')
				.expect(200)
				.execute();
			expect(me.required_actions).toEqual(['REQUIRE_VERIFIED_PHONE']);
			expect(me.suspicious_activity_flags).toBeUndefined();
		});

		it('unlocks an account that is in no community at all', async () => {
			await configurePhoneGate();
			const subject = await createPromotedSubject('escape_empty');
			expect(await readGuildIds(subject.userId)).toEqual([]);

			const preview = await previewEscape(subject);
			expect(preview).toEqual({available: true, guilds: [], owned_guilds: []});

			const updated = await executeEscape(subject);
			expect(updated.required_actions ?? []).toEqual([]);

			const me = await createBuilder<{required_actions: Array<string> | null}>(harness, subject.token)
				.get('/users/@me')
				.expect(200)
				.execute();
			expect(me.required_actions ?? []).toEqual([]);
			await createBuilder(harness, subject.token).get('/users/@me/guilds').expect(200).execute();

			const flags = await readFlags(subject.userId);
			expect(flags & DEFERRED_PHONE_ON_COMMUNITY_JOIN).not.toBe(0);
			expect(flags & PHONE_GATE_PROMOTED_FROM_DEFERRAL).toBe(0);
			expect(flags & SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE).not.toBe(0);
		});

		it('leaves the qualifying community, unlocks the account and dispatches a single user update', async () => {
			await configurePhoneGate();
			const member = await createGuildWithInvite(harness);
			const subject = await registerDeferredUser('escape_leave');
			await createBuilder(harness, subject.token).post(`/invites/${member.inviteCode}`).expect(200).execute();
			const gate = await createGuildWithInvite(harness);
			await addFillerMember(gate.inviteCode);
			await promoteThroughRefusedJoin(subject, gate.inviteCode);

			const preview = await previewEscape(subject);
			expect(preview.guilds.map((guild) => guild.id)).toEqual([member.guildId]);
			expect(preview.owned_guilds).toEqual([]);

			const dispatchSpy = vi.spyOn(NoopGatewayService.prototype, 'dispatchPresence');
			try {
				const updated = await executeEscape(subject);
				expect(updated.required_actions ?? []).toEqual([]);
				const userUpdates = dispatchSpy.mock.calls.filter(
					([params]) => params.event === 'USER_UPDATE' && params.userId.toString() === subject.userId,
				);
				expect(userUpdates).toHaveLength(1);
			} finally {
				dispatchSpy.mockRestore();
			}

			expect(await readGuildIds(subject.userId)).toEqual([]);
			const flags = await readFlags(subject.userId);
			expect(flags & DEFERRED_PHONE_ON_COMMUNITY_JOIN).not.toBe(0);
			expect(flags & PHONE_GATE_PROMOTED_FROM_DEFERRAL).toBe(0);
		});

		it('applies the member threshold strictly, so a community sitting on it is kept', async () => {
			await configurePhoneGate({deferred_phone_gate_member_threshold: 2});
			const onThreshold = await createGuildWithInvite(harness);
			const aboveThreshold = await createGuildWithInvite(harness);
			await addFillerMember(aboveThreshold.inviteCode);
			const subject = await registerDeferredUser('escape_bound');
			await createBuilder(harness, subject.token).post(`/invites/${onThreshold.inviteCode}`).expect(200).execute();
			await createBuilder(harness, subject.token).post(`/invites/${aboveThreshold.inviteCode}`).expect(200).execute();
			const gate = await createGuildWithInvite(harness);
			await addFillerMember(gate.inviteCode);
			await addFillerMember(gate.inviteCode);
			await promoteThroughRefusedJoin(subject, gate.inviteCode);

			const preview = await previewEscape(subject);
			expect(preview.guilds.map((guild) => guild.id)).toEqual([aboveThreshold.guildId]);

			await executeEscape(subject);
			expect(await readGuildIds(subject.userId)).toEqual([onThreshold.guildId]);
		});

		it('leaves a discoverable community that sits far below the member threshold', async () => {
			await configurePhoneGate({deferred_phone_gate_member_threshold: 50});
			const discoverable = await createGuildWithInvite(harness);
			const subject = await registerDeferredUser('escape_disc');
			await createBuilder(harness, subject.token).post(`/invites/${discoverable.inviteCode}`).expect(200).execute();
			await makeDiscoverable(discoverable.guildId);
			const gate = await createGuildWithInvite(harness);
			await makeDiscoverable(gate.guildId);
			await promoteThroughRefusedJoin(subject, gate.inviteCode);

			const preview = await previewEscape(subject);
			expect(preview.guilds.map((guild) => guild.id)).toEqual([discoverable.guildId]);

			await executeEscape(subject);
			expect(await readGuildIds(subject.userId)).toEqual([]);
		});

		it('keeps a qualifying community the user owns and reports it separately', async () => {
			await configurePhoneGate();
			const member = await createGuildWithInvite(harness);
			const subject = await registerDeferredUser('escape_owned');
			await createBuilder(harness, subject.token).post(`/invites/${member.inviteCode}`).expect(200).execute();
			await createBuilderWithoutAuth(harness)
				.post(`/test/users/${subject.userId}/security-flags`)
				.body({email_verified: true})
				.execute();
			const owned = await createBuilder<GuildResponse>(harness, subject.token)
				.post('/guilds')
				.body({name: `PhoneGateOwned-${Date.now()}`})
				.execute();
			const ownedInvite = await createBuilder<{code: string}>(harness, subject.token)
				.post(`/channels/${owned.system_channel_id}`.concat('/invites'))
				.body({max_uses: 0, max_age: 0, unique: false, temporary: false})
				.execute();
			await addFillerMember(ownedInvite.code);
			const gate = await createGuildWithInvite(harness);
			await addFillerMember(gate.inviteCode);
			await promoteThroughRefusedJoin(subject, gate.inviteCode);

			const preview = await previewEscape(subject);
			expect(preview.guilds.map((guild) => guild.id)).toEqual([member.guildId]);
			expect(preview.owned_guilds.map((guild) => guild.id)).toEqual([owned.id]);

			const updated = await executeEscape(subject);
			expect(updated.required_actions ?? []).toEqual([]);
			expect(await readGuildIds(subject.userId)).toEqual([owned.id]);
			expect((await readFlags(subject.userId)) & DEFERRED_PHONE_ON_COMMUNITY_JOIN).not.toBe(0);
		});

		it('leaves what it can when the qualifying set exceeds the per-call limit, and finishes on a second call', async () => {
			await configurePhoneGate();
			const subject = await registerDeferredUser('escape_batch');
			for (let index = 0; index < PHONE_GATE_ESCAPE_MAX_GUILDS + 1; index++) {
				const guild = await createGuildWithInvite(harness);
				await createBuilder(harness, subject.token).post(`/invites/${guild.inviteCode}`).expect(200).execute();
			}
			const gate = await createGuildWithInvite(harness);
			await addFillerMember(gate.inviteCode);
			await promoteThroughRefusedJoin(subject, gate.inviteCode);

			const preview = await previewEscape(subject);
			expect(preview.guilds).toHaveLength(PHONE_GATE_ESCAPE_MAX_GUILDS + 1);

			const afterFirst = await executeEscape(subject);
			expect(afterFirst.required_actions ?? []).toEqual(['REQUIRE_VERIFIED_PHONE']);
			expect(await readGuildIds(subject.userId)).toHaveLength(1);
			const flagsAfterFirst = await readFlags(subject.userId);
			expect(flagsAfterFirst & DEFERRED_PHONE_ON_COMMUNITY_JOIN).toBe(0);
			expect(flagsAfterFirst & PHONE_GATE_PROMOTED_FROM_DEFERRAL).not.toBe(0);

			const afterSecond = await executeEscape(subject);
			expect(afterSecond.required_actions ?? []).toEqual([]);
			expect(await readGuildIds(subject.userId)).toEqual([]);
			const flags = await readFlags(subject.userId);
			expect(flags & DEFERRED_PHONE_ON_COMMUNITY_JOIN).not.toBe(0);
			expect(flags & PHONE_GATE_PROMOTED_FROM_DEFERRAL).toBe(0);
		});

		it('refuses an account whose phone requirement was never deferred', async () => {
			await configurePhoneGate();
			const member = await createGuildWithInvite(harness);
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token).post(`/invites/${member.inviteCode}`).expect(200).execute();
			await setSuspiciousFlags(account.userId, SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE);
			const subject: EscapeSubject = {userId: account.userId, token: account.token};

			const preview = await previewEscape(subject);
			expect(preview).toEqual({available: false, guilds: [], owned_guilds: []});
			await expectEscapeRefused(subject);
			expect(await readGuildIds(subject.userId)).toEqual([member.guildId]);
			expect(await readFlags(subject.userId)).toBe(SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE);
		});

		it('refuses while the gate is switched off, leaving flags and memberships untouched', async () => {
			await configurePhoneGate();
			const member = await createGuildWithInvite(harness);
			const subject = await registerDeferredUser('escape_off');
			await createBuilder(harness, subject.token).post(`/invites/${member.inviteCode}`).expect(200).execute();
			const gate = await createGuildWithInvite(harness);
			await addFillerMember(gate.inviteCode);
			await promoteThroughRefusedJoin(subject, gate.inviteCode);
			const flagsBefore = await readFlags(subject.userId);

			await configurePhoneGate({deferred_phone_gate_enabled: false});
			const preview = await previewEscape(subject);
			expect(preview.available).toBe(false);
			await expectEscapeRefused(subject);

			expect(await readFlags(subject.userId)).toBe(flagsBefore);
			expect(await readGuildIds(subject.userId)).toEqual([member.guildId]);
		});

		it('refuses in single-community mode and leaves the designated community alone', async () => {
			await configurePhoneGate();
			const member = await createGuildWithInvite(harness);
			const subject = await registerDeferredUser('escape_single');
			await createBuilder(harness, subject.token).post(`/invites/${member.inviteCode}`).expect(200).execute();
			const gate = await createGuildWithInvite(harness);
			await addFillerMember(gate.inviteCode);
			await promoteThroughRefusedJoin(subject, gate.inviteCode);

			await configurePhoneGate({single_community_enabled: true, single_community_guild_id: member.guildId});
			const preview = await previewEscape(subject);
			expect(preview.available).toBe(false);
			await expectEscapeRefused(subject);
			expect(await readGuildIds(subject.userId)).toEqual([member.guildId]);
		});

		it('refuses an account that is still deferred and has never been locked out', async () => {
			await configurePhoneGate();
			const member = await createGuildWithInvite(harness);
			const subject = await registerDeferredUser('escape_defer');
			await createBuilder(harness, subject.token).post(`/invites/${member.inviteCode}`).expect(200).execute();
			const flagsBefore = await readFlags(subject.userId);

			const preview = await previewEscape(subject);
			expect(preview).toEqual({available: false, guilds: [], owned_guilds: []});
			await expectEscapeRefused(subject);
			expect(await readFlags(subject.userId)).toBe(flagsBefore);
			expect(await readGuildIds(subject.userId)).toEqual([member.guildId]);
		});

		it('refuses a promoted account that also carries the inbound-SMS tier', async () => {
			await configurePhoneGate();
			const subject = await createPromotedSubject('escape_inbound');
			const promotedFlags = await readFlags(subject.userId);
			await setSuspiciousFlags(
				subject.userId,
				promotedFlags | SuspiciousActivityFlags.REQUIRE_INBOUND_PHONE_VERIFICATION,
			);

			const preview = await previewEscape(subject);
			expect(preview.available).toBe(false);
			await expectEscapeRefused(subject);
		});

		it('closes itself after a successful escape', async () => {
			await configurePhoneGate();
			const subject = await createPromotedSubject('escape_twice');
			await executeEscape(subject);
			const flagsAfterFirst = await readFlags(subject.userId);

			await expectEscapeRefused(subject);
			expect(await readFlags(subject.userId)).toBe(flagsAfterFirst);
			const preview = await previewEscape(subject);
			expect(preview.available).toBe(false);
		});

		it('leaves a remaining email requirement in place', async () => {
			await configurePhoneGate();
			const subject = await createPromotedSubject('escape_email');
			const promotedFlags = await readFlags(subject.userId);
			await setSuspiciousFlags(subject.userId, promotedFlags | SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL);

			const updated = await executeEscape(subject);
			expect(updated.required_actions ?? []).toEqual(['REQUIRE_VERIFIED_EMAIL']);
			const me = await createBuilder<{required_actions: Array<string> | null}>(harness, subject.token)
				.get('/users/@me')
				.expect(200)
				.execute();
			expect(me.required_actions ?? []).toEqual(['REQUIRE_VERIFIED_EMAIL']);
		});

		it('restores the deferral, so a qualifying join re-promotes inside the window and not outside it', async () => {
			await configurePhoneGate();
			const subject = await createPromotedSubject('escape_rearm');
			await executeEscape(subject);

			const insideWindow = await createGuildWithInvite(harness);
			await addFillerMember(insideWindow.inviteCode);
			await promoteThroughRefusedJoin(subject, insideWindow.inviteCode);
			const repromoted = await readFlags(subject.userId);
			expect(repromoted & DEFERRED_PHONE_ON_COMMUNITY_JOIN).toBe(0);
			expect(repromoted & PHONE_GATE_PROMOTED_FROM_DEFERRAL).not.toBe(0);

			await executeEscape(subject);
			await configurePhoneGate({deferred_phone_gate_window_hours: 0.0001});
			const outsideWindow = await createGuildWithInvite(harness);
			await addFillerMember(outsideWindow.inviteCode);
			const beforeJoin = await readFlags(subject.userId);
			expect(beforeJoin & DEFERRED_PHONE_ON_COMMUNITY_JOIN).not.toBe(0);
			await createBuilder(harness, subject.token).post(`/invites/${outsideWindow.inviteCode}`).expect(200).execute();

			const flags = await readFlags(subject.userId);
			expect(flags & DEFERRED_PHONE_ON_COMMUNITY_JOIN).not.toBe(0);
			expect(flags & PHONE_GATE_PROMOTED_FROM_DEFERRAL).toBe(0);
		});

		it('refuses bot tokens and OAuth2 bearer tokens on both routes', async () => {
			await configurePhoneGate();
			const owner = await createTestAccount(harness);
			const redirectURI = 'https://example.com/callback';
			const application = await createOAuth2Application(harness, owner, {redirect_uris: [redirectURI]});
			const {code} = await authorizeOAuth2(harness, owner.token, {
				client_id: application.id,
				redirect_uri: redirectURI,
				scope: 'identify',
			});
			const {access_token: accessToken} = await exchangeOAuth2AuthorizationCode(harness, {
				client_id: application.id,
				client_secret: application.client_secret,
				code,
				redirect_uri: redirectURI,
			});

			for (const token of [`Bot ${application.bot.token}`, `Bearer ${accessToken}`]) {
				await createBuilder(harness, token).get(ESCAPE_PATH).expect(403).execute();
				await createBuilder(harness, token).post(ESCAPE_PATH).body({}).expect(403).execute();
			}
		});

		it('spends the execute budget only on execute calls', async () => {
			await configurePhoneGate();
			const subject = await createPromotedSubject('escape_limit');
			for (let index = 0; index < 3; index++) {
				await createBuilder(harness, subject.token)
					.get(ESCAPE_PATH)
					.header(RATE_LIMIT_HEADER, 'true')
					.expect(200)
					.execute();
			}
			for (let index = 0; index < 5; index++) {
				const {response} = await createBuilder(harness, subject.token)
					.post(ESCAPE_PATH)
					.body({})
					.header(RATE_LIMIT_HEADER, 'true')
					.executeRaw();
				expect(response.status).toBe(index === 0 ? 200 : 400);
			}
			const {response} = await createBuilder(harness, subject.token)
				.post(ESCAPE_PATH)
				.body({})
				.header(RATE_LIMIT_HEADER, 'true')
				.executeRaw();
			expect(response.status).toBe(429);
		});
	});
});
