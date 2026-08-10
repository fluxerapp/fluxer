// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAccountPolicyEvaluator, type IAccountPolicyEvaluator} from '../risk/AccountPolicyEvaluator';

export const TEST_POLICY_CONTACT_DOMAIN = 'policy-exempt.example';
export const TEST_POLICY_CONTACT_SUBDOMAIN = `sub.${TEST_POLICY_CONTACT_DOMAIN}`;
export const TEST_POLICY_REPUTATION_EXEMPT_DOMAIN = 'reputation-exempt.example';

const CURRENT_BEHAVIOR_TEST_POLICY_DSL = {
	version: 1,
	id: 'current_behavior_test_policy',
	sets: {
		contact_domain_exclusions: [TEST_POLICY_CONTACT_DOMAIN],
		contact_domain_reputation_exemptions: [TEST_POLICY_REPUTATION_EXEMPT_DOMAIN],
	},
	definitions: {
		bits: {
			email_contact_proof: 1,
			phone_contact_proof: 4,
			inbound_contact_proof: 256,
		},
	},
	defaults: {
		flag_bits: 0,
		features: {
			invite_auto_join: true,
		},
	},
	rules: [
		{
			id: 'assessment_action_bits',
			when: {
				not: {
					fact: 'contact.domain',
					in_set: 'contact_domain_exclusions',
				},
			},
			effects: [
				{
					type: 'or_bits_from_map',
					fact: 'assessment.action',
					map: {
						allow: [],
						require_verified_email: ['email_contact_proof'],
						require_outbound_phone: ['phone_contact_proof'],
						require_inbound_phone: ['phone_contact_proof', 'inbound_contact_proof'],
						block: ['phone_contact_proof', 'inbound_contact_proof'],
					},
				},
			],
		},
		{
			id: 'contact_domain_step_up_bits',
			when: {
				all: [
					{
						not: {
							fact: 'contact.domain',
							in_set: 'contact_domain_exclusions',
						},
					},
					{
						fact: 'contact.domain_step_up_required',
						equals: true,
					},
				],
			},
			effects: [
				{
					type: 'or_bits',
					bits: ['phone_contact_proof'],
				},
			],
		},
		{
			id: 'region_step_up_bits',
			when: {
				all: [
					{
						not: {
							fact: 'contact.domain',
							in_set: 'contact_domain_exclusions',
						},
					},
					{
						fact: 'region.step_up_required',
						equals: true,
					},
				],
			},
			effects: [
				{
					type: 'or_bits',
					bits: ['phone_contact_proof', 'inbound_contact_proof'],
				},
			],
		},
		{
			id: 'mark_flagged_decision',
			when: {
				fact: 'decision.flag_bits',
				gt: 0,
			},
			effects: [
				{
					type: 'append_markers',
					markers: ['challenged'],
				},
			],
		},
		{
			id: 'emit_assessment_threshold_notice',
			when: {
				fact: 'assessment.level',
				equals: 'very_high',
			},
			effects: [
				{
					type: 'emit',
					event: 'assessment_threshold_notice',
					level: 'warn',
				},
			],
		},
		{
			id: 'disable_invite_auto_join_for_assessment_threshold',
			when: {
				fact: 'assessment.score',
				gte: 80,
			},
			effects: [
				{
					type: 'set_feature',
					feature: 'invite_auto_join',
					enabled: false,
					reason: 'assessment_score_gte_80',
				},
			],
		},
	],
	contact_capability_rules: [
		{
			id: 'contact_capability_profile',
			when: {
				fact: 'contact.domain',
				in_set: 'contact_domain_exclusions',
			},
			capabilities: ['captcha_exempt', 'followup_risk_exempt', 'required_actions_exempt'],
		},
		{
			id: 'contact_domain_reputation_profile',
			when: {
				fact: 'contact.domain',
				in_set: 'contact_domain_reputation_exemptions',
			},
			capabilities: ['reputation_checks_exempt'],
		},
	],
	classifiers: {
		email_tlds: {
			low_risk: ['stable', 'trusted'],
			blocked: ['blocked'],
		},
		network: {
			trusted_privacy_provider_markers: ['privacy relay', 'sample shield'],
			education_org_markers: ['academy', 'learning network'],
			reverse_dns_markers: {
				cellular: ['\\bcell[-._]', '\\bmobile[-._]'],
				business: ['\\bbiz[-._]'],
				static: ['\\bstatic[-._]'],
				dynamic: ['\\bdyn[-._]', '\\bpool[-._]'],
			},
		},
		regional_minimum_age: {
			by_country: {
				ZZ: 16,
				YY: 15,
			},
		},
	},
} as const;

export function createCurrentBehaviorTestAccountPolicyEvaluator(): IAccountPolicyEvaluator {
	return createAccountPolicyEvaluator(CURRENT_BEHAVIOR_TEST_POLICY_DSL);
}
