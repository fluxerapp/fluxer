// SPDX-License-Identifier: AGPL-3.0-or-later

import {DeletionReasons} from '@fluxer/constants/src/Core';
import enforcementCases from '@fluxer/constants/src/EnforcementReasonCases.json';
import {
	ACCOUNT_DELETION_MIN_DAYS,
	ENFORCEMENT_APPEAL_WINDOW_DAYS,
	ENFORCEMENT_CLOSURE_MIN_DAYS,
	ENFORCEMENT_REASONS,
	ENFORCEMENT_RULES,
	type EnforcementKind,
	type EnforcementLimit,
	isEnforcementKindAllowed,
	isEnforcementLimitAllowed,
	isEnforcementReasonCode,
} from '@fluxer/constants/src/EnforcementReasons';
import {describe, expect, it} from 'vitest';

const ALL_KINDS: ReadonlyArray<EnforcementKind> = [
	'warning',
	'suspension',
	'closure',
	'feature_limit',
	'content_removal',
	'profile_reset',
	'community_removal',
];

const ALL_LIMITS: ReadonlyArray<EnforcementLimit> = ['direct_messages', 'reports', 'purchases'];

const RULE_TABLE = [
	['spam', 'guidelines', '6-spam-and-service-abuse', true, true],
	['service_abuse', 'guidelines', '6-spam-and-service-abuse', true, true],
	['raids', 'guidelines', '1-harassment-threats-and-privacy', true, true],
	['automation', 'guidelines', '6-spam-and-service-abuse', true, true],
	['intimate_images', 'guidelines', '1-harassment-threats-and-privacy', true, false],
	['fraud', 'guidelines', '5-illegal-and-dangerous-activity', true, false],
	['child_safety', 'guidelines', '4-sexual-content-and-protection-of-minors', true, false],
	['privacy', 'guidelines', '1-harassment-threats-and-privacy', true, false],
	['harassment', 'guidelines', '1-harassment-threats-and-privacy', true, false],
	['payments', 'terms', '4-4-failed-payments-and-disputes', true, false],
	['operator_pass_payments', 'operator_pass_terms', '9-payment-disputes', true, false],
	['unwanted_sexual_content', 'guidelines', '1-harassment-threats-and-privacy', true, false],
	['violent_content', 'guidelines', '3-violence-extremism-and-self-harm', true, true],
	['ban_evasion', 'guidelines', '6-spam-and-service-abuse', true, false],
	['hate', 'guidelines', '2-hate-and-discrimination', true, false],
	['malware', 'guidelines', '5-illegal-and-dangerous-activity', true, false],
	['impersonation', 'guidelines', '6-spam-and-service-abuse', true, true],
	['threats', 'guidelines', '1-harassment-threats-and-privacy', true, false],
	['violent_extremism', 'guidelines', '3-violence-extremism-and-self-harm', true, false],
	['self_harm', 'guidelines', '3-violence-extremism-and-self-harm', true, false],
	['adult_content', 'guidelines', '4-sexual-content-and-protection-of-minors', true, true],
	['illegal_activity', 'guidelines', '5-illegal-and-dangerous-activity', true, false],
	['harmful_deception', 'guidelines', '7-harmful-deception-and-manipulated-media', true, false],
	['report_abuse', 'guidelines', '8-reporting', true, true],
	['age_requirement', 'terms', '1-agreement-and-eligibility', false, false],
	['copyright', 'terms', '3-4-copyright', true, true],
] as const;

const enforcementCodes = Object.keys(ENFORCEMENT_REASONS).map(Number).filter(isEnforcementReasonCode);

describe('ENFORCEMENT_REASONS', () => {
	it('classifies every deletion reason except the non-enforcement codes', () => {
		const expected = Object.values(DeletionReasons).filter(
			(code) =>
				code !== DeletionReasons.USER_REQUESTED &&
				code !== DeletionReasons.OTHER &&
				code !== DeletionReasons.INACTIVITY &&
				code !== DeletionReasons.BELOW_MINIMUM_AGE &&
				code !== DeletionReasons.UNCLAIMED_ACCOUNT_ABANDONED,
		);
		expect(enforcementCodes.sort((a, b) => a - b)).toEqual(expected.sort((a, b) => a - b));
	});

	it('matches the code table', () => {
		expect(
			Object.entries(ENFORCEMENT_REASONS).map(([code, reason]) => [
				Number(code),
				reason.rule,
				reason.handling,
				reason.kinds,
			]),
		).toEqual(enforcementCases.reasons);
	});

	it('only names rules that exist', () => {
		for (const reason of Object.values(ENFORCEMENT_REASONS)) {
			expect(ENFORCEMENT_RULES).toHaveProperty(reason.rule);
		}
	});

	it('limits restricted codes to permanent suspension and content removal', () => {
		for (const code of enforcementCodes.filter(
			(candidate) => ENFORCEMENT_REASONS[candidate].handling === 'restricted',
		)) {
			expect(ALL_KINDS.filter((kind) => isEnforcementKindAllowed(code, kind))).toEqual([
				'suspension',
				'content_removal',
			]);
		}
	});
});

describe('ENFORCEMENT_RULES', () => {
	it('pins policy anchors, the new account bar and network screen visibility', () => {
		expect(
			Object.entries(ENFORCEMENT_RULES).map(([key, rule]) => [
				key,
				rule.policy,
				rule.anchor,
				rule.barsNewAccounts,
				rule.showOnNetworkScreen,
			]),
		).toEqual(RULE_TABLE);
	});
});

describe('enforcement limits', () => {
	it('allows each limit only for its listed codes', () => {
		expect(
			ALL_LIMITS.map((limit) => [limit, enforcementCodes.filter((code) => isEnforcementLimitAllowed(code, limit))]),
		).toEqual(enforcementCases.limits);
	});

	it('allows a feature limit only where some limit applies', () => {
		const withFeatureLimit = enforcementCodes.filter((code) => isEnforcementKindAllowed(code, 'feature_limit'));
		const withAnyLimit = enforcementCodes.filter((code) =>
			ALL_LIMITS.some((limit) => isEnforcementLimitAllowed(code, limit)),
		);
		expect(withFeatureLimit).toEqual(withAnyLimit);
	});
});

describe('enforcement windows', () => {
	it('keeps closures open past the appeal window', () => {
		expect(ACCOUNT_DELETION_MIN_DAYS).toBe(14);
		expect(ENFORCEMENT_APPEAL_WINDOW_DAYS).toBe(60);
		expect(ENFORCEMENT_CLOSURE_MIN_DAYS).toBe(ENFORCEMENT_APPEAL_WINDOW_DAYS + ACCOUNT_DELETION_MIN_DAYS);
	});
});
