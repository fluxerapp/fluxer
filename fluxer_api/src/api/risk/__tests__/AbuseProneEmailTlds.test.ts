// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	AbuseProneEmailTldRisk,
	classifyAbuseProneEmailTld,
	isBlockedRegistrationEmailDomain,
	isLowRiskEmailTld,
} from '@app/api/risk/AbuseProneEmailTlds';
import {setInjectedAccountPolicyEvaluator} from '@app/api/risk/AccountPolicyService';
import {createCurrentBehaviorTestAccountPolicyEvaluator} from '@app/api/test/AccountPolicyTestEvaluator';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

beforeEach(() => {
	setInjectedAccountPolicyEvaluator(createCurrentBehaviorTestAccountPolicyEvaluator());
});

afterEach(() => {
	setInjectedAccountPolicyEvaluator(undefined);
});

describe('isBlockedRegistrationEmailDomain', () => {
	it('blocks TLDs from the configured deny list', () => {
		expect(isBlockedRegistrationEmailDomain('name.example.blocked')).toBe(true);
	});
	it('does not hard-block merely unfamiliar TLDs because those are soft-scored only', () => {
		expect(isBlockedRegistrationEmailDomain('example.unknown')).toBe(false);
		expect(isBlockedRegistrationEmailDomain('example.stable')).toBe(false);
	});
	it('does not block malformed domains', () => {
		expect(isBlockedRegistrationEmailDomain('not-a-domain')).toBe(false);
	});
});

describe('classifyAbuseProneEmailTld', () => {
	it('returns null for configured low-risk TLDs', () => {
		expect(classifyAbuseProneEmailTld('stable')).toBeNull();
		expect(classifyAbuseProneEmailTld('trusted')).toBeNull();
	});
	it('returns High for everything not on the configured low-risk list', () => {
		expect(classifyAbuseProneEmailTld('unknown')).toBe(AbuseProneEmailTldRisk.High);
		expect(classifyAbuseProneEmailTld('blocked')).toBe(AbuseProneEmailTldRisk.High);
	});
	it('normalizes leading dots and casing', () => {
		expect(classifyAbuseProneEmailTld('.Stable')).toBeNull();
		expect(classifyAbuseProneEmailTld('.Unknown')).toBe(AbuseProneEmailTldRisk.High);
	});
});

describe('isLowRiskEmailTld', () => {
	it('reports configured allowlist membership', () => {
		expect(isLowRiskEmailTld('stable')).toBe(true);
		expect(isLowRiskEmailTld('unknown')).toBe(false);
	});
});
