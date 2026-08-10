// SPDX-License-Identifier: AGPL-3.0-or-later

import {SuspiciousActivityFlags} from '@fluxer/constants/src/UserConstants';
import {describe, expect, it} from 'vitest';
import {
	createCurrentBehaviorTestAccountPolicyEvaluator,
	TEST_POLICY_CONTACT_DOMAIN,
	TEST_POLICY_CONTACT_SUBDOMAIN,
	TEST_POLICY_REPUTATION_EXEMPT_DOMAIN,
} from '../../test/AccountPolicyTestEvaluator';
import {
	type AccountPolicyContext,
	createAccountPolicyContactContext,
	createAccountPolicyContactDomainContext,
	createAccountPolicyEvaluatorFromConfig,
	normalizePolicyContactDomain,
} from '../AccountPolicyEvaluator';
import {RecommendedAction, type RiskAssessment, RiskConfidence, RiskDecisionMethod, RiskLevel} from '../RiskTypes';

function createAssessment(params: {level?: RiskLevel; action?: RecommendedAction; score?: number}): RiskAssessment {
	const level = params.level ?? RiskLevel.Low;
	const action = params.action ?? RecommendedAction.Allow;
	return {
		suspicious: level !== RiskLevel.Low,
		level,
		confidence: RiskConfidence.High,
		riskScore: params.score ?? 0,
		reasoning: 'test assessment',
		recommendedAction: action,
		method: RiskDecisionMethod.Noop,
		modelUsed: 'test',
		rounds: 0,
		elapsedMs: 0,
		signals: {},
	};
}

function createContext(overrides: {
	domain?: string | null;
	domainStepUpRequired?: boolean;
	regionStepUpRequired?: boolean;
	level?: RiskLevel;
	action?: RecommendedAction;
	score?: number;
}): AccountPolicyContext {
	const action = overrides.action ?? RecommendedAction.Allow;
	const level = overrides.level ?? RiskLevel.Low;
	return {
		contact: {
			value: 'test@example.com',
			domain: overrides.domain ?? 'example.com',
			domainAdminListed: false,
			domainDisposable: false,
			domainBlocked: false,
			domainStepUpRequired: overrides.domainStepUpRequired ?? false,
		},
		region: {
			code: 'US',
			stepUpRequired: overrides.regionStepUpRequired ?? false,
		},
		assessment: {
			raw: createAssessment({level, action, score: overrides.score}),
			level,
			action,
		},
	};
}

describe('AccountPolicyEvaluator', () => {
	it('normalizes contact domains before policy evaluation', () => {
		expect(normalizePolicyContactDomain(' Example.COM ')).toBe('example.com');
		expect(normalizePolicyContactDomain(null)).toBeNull();
	});

	it.each([
		[RecommendedAction.Allow, 0],
		[RecommendedAction.RequireVerifiedEmail, SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL],
		[RecommendedAction.RequireOutboundPhone, SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE],
		[
			RecommendedAction.RequireInboundPhone,
			SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE | SuspiciousActivityFlags.REQUIRE_INBOUND_PHONE_VERIFICATION,
		],
		[
			RecommendedAction.Block,
			SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE | SuspiciousActivityFlags.REQUIRE_INBOUND_PHONE_VERIFICATION,
		],
	])('maps assessment action %s to bitfield %i', (action, expected) => {
		const evaluator = createCurrentBehaviorTestAccountPolicyEvaluator();
		const decision = evaluator.evaluate(createContext({action}));
		expect(decision.flagBits).toBe(expected);
	});

	it('adds contact-domain and region step-up bits exactly like the current fixture', () => {
		const evaluator = createCurrentBehaviorTestAccountPolicyEvaluator();
		const domainDecision = evaluator.evaluate(createContext({domainStepUpRequired: true}));
		expect(domainDecision.flagBits).toBe(SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE);
		expect(domainDecision.riskHistoryOutcomeCodes).toEqual(['challenged']);

		const regionDecision = evaluator.evaluate(createContext({regionStepUpRequired: true}));
		expect(regionDecision.flagBits).toBe(
			SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE | SuspiciousActivityFlags.REQUIRE_INBOUND_PHONE_VERIFICATION,
		);
		expect(regionDecision.riskHistoryOutcomeCodes).toEqual(['challenged']);
	});

	it('uses configured contact-domain exclusions to suppress only bitfield rules', () => {
		const evaluator = createCurrentBehaviorTestAccountPolicyEvaluator();
		const decision = evaluator.evaluate(
			createContext({
				domain: TEST_POLICY_CONTACT_DOMAIN,
				domainStepUpRequired: true,
				regionStepUpRequired: true,
				action: RecommendedAction.RequireInboundPhone,
				level: RiskLevel.VeryHigh,
				score: 90,
			}),
		);
		expect(decision.flagBits).toBe(0);
		expect(decision.riskHistoryOutcomeCodes).toEqual([]);
		expect(decision.auditEvents).toEqual([
			{event: 'assessment_threshold_notice', level: 'warn', ruleId: 'emit_assessment_threshold_notice'},
		]);
		expect(decision.inviteAutoJoinEnabled).toBe(false);
	});

	it('gates invite auto-join and emits threshold audit events from data rules', () => {
		const evaluator = createCurrentBehaviorTestAccountPolicyEvaluator();
		const decision = evaluator.evaluate(createContext({level: RiskLevel.VeryHigh, score: 80}));
		expect(decision.inviteAutoJoinEnabled).toBe(false);
		expect(decision.inviteAutoJoinSkipReason).toBe('assessment_score_gte_80');
		expect(decision.auditEvents).toEqual([
			{event: 'assessment_threshold_notice', level: 'warn', ruleId: 'emit_assessment_threshold_notice'},
		]);
	});

	it('resolves generic contact capabilities from configured contact rules', () => {
		const evaluator = createCurrentBehaviorTestAccountPolicyEvaluator();
		const decision = evaluator.evaluateContact(createAccountPolicyContactContext(`test@${TEST_POLICY_CONTACT_DOMAIN}`));
		expect(decision.hasCapability('captcha_exempt')).toBe(true);
		expect(decision.hasCapability('followup_risk_exempt')).toBe(true);
		expect(decision.hasCapability('required_actions_exempt')).toBe(true);

		const subdomainDecision = evaluator.evaluateContact(
			createAccountPolicyContactContext(`test@${TEST_POLICY_CONTACT_SUBDOMAIN}`),
		);
		expect(subdomainDecision.capabilities).toEqual([]);
	});

	it('resolves contact-domain reputation capabilities from configured contact rules', () => {
		const evaluator = createCurrentBehaviorTestAccountPolicyEvaluator();
		const decision = evaluator.evaluateContact(
			createAccountPolicyContactDomainContext(TEST_POLICY_REPUTATION_EXEMPT_DOMAIN),
		);
		expect(decision.hasCapability('reputation_checks_exempt')).toBe(true);
		expect(evaluator.evaluateContact(createAccountPolicyContactDomainContext('other.example')).capabilities).toEqual(
			[],
		);
	});

	it('classifies configured contact and network policy data', () => {
		const evaluator = createCurrentBehaviorTestAccountPolicyEvaluator();
		expect(evaluator.isLowRiskEmailTld('stable')).toBe(true);
		expect(evaluator.classifyEmailTld('stable')).toBeNull();
		expect(evaluator.classifyEmailTld('unknown')).toBe('high');
		expect(evaluator.isBlockedRegistrationEmailDomain('name.example.blocked')).toBe(true);
		expect(evaluator.isBlockedRegistrationEmailDomain('name.example.stable')).toBe(false);
		expect(evaluator.isTrustedCommercialPrivacyProvider('Example Privacy Relay LLC')).toBe(true);
		expect(evaluator.isTrustedCommercialPrivacyProvider('Unlisted Provider LLC')).toBe(false);
		expect(evaluator.isEducationOrganizationName('North Example Academy')).toBe(true);
		expect(evaluator.isEducationOrganizationName('Example Hosting LLC')).toBe(false);
		expect(evaluator.classifyReverseDnsHostname('host.cell.example')).toBe('cellular');
		expect(evaluator.classifyReverseDnsHostname('host.biz.example')).toBe('business');
		expect(evaluator.classifyReverseDnsHostname('host.static.example')).toBe('static');
		expect(evaluator.classifyReverseDnsHostname('host.dyn.example')).toBe('dynamic');
		expect(evaluator.classifyReverseDnsHostname('host.example')).toBe('unknown');
		expect(evaluator.getMinimumAgeForRegion('ZZ', 13)).toBe(16);
		expect(evaluator.getMinimumAgeForRegion('US', 13)).toBe(13);
	});

	it('loads override documents from JSON strings', () => {
		const evaluator = createAccountPolicyEvaluatorFromConfig(
			JSON.stringify({
				version: 1,
				id: 'test_override',
				defaults: {
					flag_bits: 0,
					features: {
						invite_auto_join: true,
					},
				},
				rules: [
					{
						id: 'override-rule',
						when: {fact: 'assessment.score', gte: 10},
						effects: [{type: 'or_bits', bits: SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL}],
					},
				],
			}),
		);
		expect(evaluator.evaluate(createContext({score: 9})).flagBits).toBe(0);
		expect(evaluator.evaluate(createContext({score: 10})).flagBits).toBe(
			SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL,
		);
	});

	it('uses a disabled no-op policy when config is absent', () => {
		const evaluator = createAccountPolicyEvaluatorFromConfig(undefined);
		expect(evaluator.evaluate(createContext({action: RecommendedAction.RequireInboundPhone})).flagBits).toBe(0);
		expect(evaluator.classifyEmailTld('unknown')).toBeNull();
		expect(createAccountPolicyEvaluatorFromConfig('').isBlockedRegistrationEmailDomain('name.example.blocked')).toBe(
			false,
		);
	});

	it('rejects unsupported facts before runtime evaluation', () => {
		expect(() =>
			createAccountPolicyEvaluatorFromConfig({
				version: 1,
				id: 'bad',
				rules: [
					{
						id: 'bad-rule',
						when: {fact: 'contact.unsupported', equals: true},
						effects: [{type: 'or_bits', bits: 1}],
					},
				],
			}),
		).toThrow('unknown fact path');
	});
});
