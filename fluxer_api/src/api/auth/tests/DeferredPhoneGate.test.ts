// SPDX-License-Identifier: AGPL-3.0-or-later

import {DEFERRED_PHONE_ON_COMMUNITY_JOIN, SuspiciousActivityFlags} from '@fluxer/constants/src/UserConstants';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {setInjectedRegistrationRiskEvaluator} from '../../middleware/ServiceMiddleware';
import {getInstanceConfigRepository} from '../../middleware/ServiceSingletons';
import {
	RecommendedAction,
	RiskConfidence,
	RiskDecisionMethod,
	RiskLevel,
	type RiskLevel as RiskLevelType,
} from '../../risk/RiskTypes';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
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
});
