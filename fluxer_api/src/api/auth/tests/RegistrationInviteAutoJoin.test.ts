// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IRegistrationRiskEvaluator} from '@app/api/auth/services/IRegistrationRiskEvaluator';
import {
	createAuthHarness,
	createTestAccount,
	createUniqueEmail,
	createUniqueUsername,
	loginAccount,
	registerUser,
} from '@app/api/auth/tests/AuthTestUtils';
import {setInjectedRegistrationRiskEvaluator} from '@app/api/middleware/ServiceMiddleware';
import {
	RecommendedAction,
	RiskConfidence,
	RiskDecisionMethod,
	RiskLevel,
	type RiskLevel as RiskLevelType,
} from '@app/api/risk/RiskTypes';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

function createStaticRiskEvaluator(params: {
	level: RiskLevelType;
	riskScore: number;
	recommendedAction: RecommendedAction;
}): IRegistrationRiskEvaluator {
	return {
		async evaluate() {
			return {
				level: params.level,
				recommendedAction: params.recommendedAction,
				assessment: {
					suspicious: params.level !== RiskLevel.Low,
					level: params.level,
					confidence: RiskConfidence.High,
					riskScore: params.riskScore,
					reasoning: 'test risk verdict',
					recommendedAction: params.recommendedAction,
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

async function createGuildInvite(harness: ApiTestHarness): Promise<{
	guildId: string;
	inviteCode: string;
	ownerToken: string;
}> {
	let owner = await createTestAccount(harness);
	await createBuilderWithoutAuth(harness)
		.post(`/test/users/${owner.userId}/acls`)
		.body({
			acls: ['*'],
		})
		.expect(200)
		.execute();
	owner = await loginAccount(harness, owner);
	const guild = await createBuilder<GuildResponse>(harness, owner.token)
		.post('/guilds')
		.body({
			name: `InviteGuild-${Date.now()}`,
		})
		.execute();
	if (!guild.system_channel_id) {
		throw new Error('Guild creation did not return a system_channel_id');
	}
	const invite = await createBuilder<{
		code: string;
	}>(harness, owner.token)
		.post(`/channels/${guild.system_channel_id}/invites`)
		.body({
			max_uses: 0,
			max_age: 0,
			unique: false,
			temporary: false,
		})
		.execute();
	return {guildId: guild.id, inviteCode: invite.code, ownerToken: owner.token};
}

describe('Auth registration invite auto-join risk gating', () => {
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
	it('auto-joins the invite on medium-risk registrations', async () => {
		const {guildId, inviteCode, ownerToken} = await createGuildInvite(harness);
		setInjectedRegistrationRiskEvaluator(
			createStaticRiskEvaluator({
				level: RiskLevel.Medium,
				riskScore: 40,
				recommendedAction: RecommendedAction.RequireVerifiedEmail,
			}),
		);
		const registration = await registerUser(harness, {
			email: createUniqueEmail('invite-medium'),
			username: createUniqueUsername('invite_medium'),
			global_name: 'Invite Medium',
			password: 'StrongPassword!123',
			date_of_birth: '2000-01-01',
			consent: true,
			invite_code: inviteCode,
		});
		const memberLookup = await createBuilder(harness, ownerToken)
			.get(`/guilds/${guildId}/members/${registration.user_id}`)
			.expect(200)
			.executeWithResponse();
		expect(memberLookup.response.status).toBe(200);
	});
	it('auto-joins the invite on high-risk registrations', async () => {
		const {guildId, inviteCode, ownerToken} = await createGuildInvite(harness);
		setInjectedRegistrationRiskEvaluator(
			createStaticRiskEvaluator({
				level: RiskLevel.High,
				riskScore: 70,
				recommendedAction: RecommendedAction.RequireOutboundPhone,
			}),
		);
		const registration = await registerUser(harness, {
			email: createUniqueEmail('invite-high'),
			username: createUniqueUsername('invite_high'),
			global_name: 'Invite High',
			password: 'StrongPassword!123',
			date_of_birth: '2000-01-01',
			consent: true,
			invite_code: inviteCode,
		});
		const memberLookup = await createBuilder(harness, ownerToken)
			.get(`/guilds/${guildId}/members/${registration.user_id}`)
			.expect(200)
			.executeWithResponse();
		expect(memberLookup.response.status).toBe(200);
	});
	it('does not auto-join the invite on very-high-risk registrations', async () => {
		const {guildId, inviteCode, ownerToken} = await createGuildInvite(harness);
		setInjectedRegistrationRiskEvaluator(
			createStaticRiskEvaluator({
				level: RiskLevel.VeryHigh,
				riskScore: 90,
				recommendedAction: RecommendedAction.RequireInboundPhone,
			}),
		);
		const registration = await registerUser(harness, {
			email: createUniqueEmail('invite-veryhigh'),
			username: createUniqueUsername('invite_veryhigh'),
			global_name: 'Invite VeryHigh',
			password: 'StrongPassword!123',
			date_of_birth: '2000-01-01',
			consent: true,
			invite_code: inviteCode,
		});
		const memberLookup = await createBuilder(harness, ownerToken)
			.get(`/guilds/${guildId}/members/${registration.user_id}`)
			.expect(404)
			.executeWithResponse();
		expect(memberLookup.response.status).toBe(404);
	});
});
