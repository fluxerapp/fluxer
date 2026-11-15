// SPDX-License-Identifier: AGPL-3.0-or-later

import {DeletionReasons} from '@fluxer/constants/src/Core';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export type EnforcementRuleKey =
	| 'spam'
	| 'service_abuse'
	| 'raids'
	| 'automation'
	| 'intimate_images'
	| 'fraud'
	| 'child_safety'
	| 'privacy'
	| 'harassment'
	| 'payments'
	| 'operator_pass_payments'
	| 'unwanted_sexual_content'
	| 'violent_content'
	| 'ban_evasion'
	| 'hate'
	| 'malware'
	| 'impersonation'
	| 'threats'
	| 'violent_extremism'
	| 'self_harm'
	| 'adult_content'
	| 'illegal_activity'
	| 'harmful_deception'
	| 'report_abuse'
	| 'age_requirement'
	| 'copyright';

type EnforcementHandling = 'standard' | 'protected' | 'restricted';

export type EnforcementKind =
	| 'warning'
	| 'suspension'
	| 'closure'
	| 'feature_limit'
	| 'content_removal'
	| 'profile_reset'
	| 'community_removal';

export type EnforcementLimit = 'direct_messages' | 'reports' | 'purchases';

export type EnforcementReasonCode = Exclude<
	ValueOf<typeof DeletionReasons>,
	| typeof DeletionReasons.USER_REQUESTED
	| typeof DeletionReasons.OTHER
	| typeof DeletionReasons.INACTIVITY
	| typeof DeletionReasons.BELOW_MINIMUM_AGE
	| typeof DeletionReasons.UNCLAIMED_ACCOUNT_ABANDONED
>;

interface EnforcementRule {
	policy: 'guidelines' | 'terms' | 'operator_pass_terms';
	anchor: string;
	barsNewAccounts: boolean;
	showOnNetworkScreen: boolean;
}

interface EnforcementReason {
	rule: EnforcementRuleKey;
	handling: EnforcementHandling;
	kinds: ReadonlyArray<EnforcementKind> | 'any';
}

export const ACCOUNT_DELETION_MIN_DAYS = 14;
export const ENFORCEMENT_APPEAL_WINDOW_DAYS = 60;
export const ENFORCEMENT_CLOSURE_MIN_DAYS = ENFORCEMENT_APPEAL_WINDOW_DAYS + ACCOUNT_DELETION_MIN_DAYS;

const HARASSMENT_ANCHOR = '1-harassment-threats-and-privacy';
const VIOLENCE_ANCHOR = '3-violence-extremism-and-self-harm';
const SEXUAL_CONTENT_ANCHOR = '4-sexual-content-and-protection-of-minors';
const ILLEGAL_ACTIVITY_ANCHOR = '5-illegal-and-dangerous-activity';
const SERVICE_ABUSE_ANCHOR = '6-spam-and-service-abuse';

function guidelinesRule(anchor: string, showOnNetworkScreen: boolean): EnforcementRule {
	return {policy: 'guidelines', anchor, barsNewAccounts: true, showOnNetworkScreen};
}

export const ENFORCEMENT_RULES: Readonly<Record<EnforcementRuleKey, EnforcementRule>> = {
	spam: guidelinesRule(SERVICE_ABUSE_ANCHOR, true),
	service_abuse: guidelinesRule(SERVICE_ABUSE_ANCHOR, true),
	raids: guidelinesRule(HARASSMENT_ANCHOR, true),
	automation: guidelinesRule(SERVICE_ABUSE_ANCHOR, true),
	intimate_images: guidelinesRule(HARASSMENT_ANCHOR, false),
	fraud: guidelinesRule(ILLEGAL_ACTIVITY_ANCHOR, false),
	child_safety: guidelinesRule(SEXUAL_CONTENT_ANCHOR, false),
	privacy: guidelinesRule(HARASSMENT_ANCHOR, false),
	harassment: guidelinesRule(HARASSMENT_ANCHOR, false),
	payments: {
		policy: 'terms',
		anchor: '4-4-failed-payments-and-disputes',
		barsNewAccounts: true,
		showOnNetworkScreen: false,
	},
	operator_pass_payments: {
		policy: 'operator_pass_terms',
		anchor: '9-payment-disputes',
		barsNewAccounts: true,
		showOnNetworkScreen: false,
	},
	unwanted_sexual_content: guidelinesRule(HARASSMENT_ANCHOR, false),
	violent_content: guidelinesRule(VIOLENCE_ANCHOR, true),
	ban_evasion: guidelinesRule(SERVICE_ABUSE_ANCHOR, false),
	hate: guidelinesRule('2-hate-and-discrimination', false),
	malware: guidelinesRule(ILLEGAL_ACTIVITY_ANCHOR, false),
	impersonation: guidelinesRule(SERVICE_ABUSE_ANCHOR, true),
	threats: guidelinesRule(HARASSMENT_ANCHOR, false),
	violent_extremism: guidelinesRule(VIOLENCE_ANCHOR, false),
	self_harm: guidelinesRule(VIOLENCE_ANCHOR, false),
	adult_content: guidelinesRule(SEXUAL_CONTENT_ANCHOR, true),
	illegal_activity: guidelinesRule(ILLEGAL_ACTIVITY_ANCHOR, false),
	harmful_deception: guidelinesRule('7-harmful-deception-and-manipulated-media', false),
	report_abuse: guidelinesRule('8-reporting', true),
	age_requirement: {
		policy: 'terms',
		anchor: '1-agreement-and-eligibility',
		barsNewAccounts: false,
		showOnNetworkScreen: false,
	},
	copyright: {policy: 'terms', anchor: '3-4-copyright', barsNewAccounts: true, showOnNetworkScreen: true},
};

const RESTRICTED_KINDS: ReadonlyArray<EnforcementKind> = ['suspension', 'content_removal'];
const PAYMENT_KINDS: ReadonlyArray<EnforcementKind> = ['warning', 'suspension', 'closure', 'feature_limit'];

export const ENFORCEMENT_REASONS: Readonly<Record<EnforcementReasonCode, EnforcementReason>> = {
	[DeletionReasons.SPAM]: {rule: 'spam', handling: 'standard', kinds: 'any'},
	[DeletionReasons.CHEATING_OR_EXPLOITATION]: {rule: 'service_abuse', handling: 'standard', kinds: 'any'},
	[DeletionReasons.COORDINATED_RAIDING]: {rule: 'raids', handling: 'standard', kinds: 'any'},
	[DeletionReasons.AUTOMATION_OR_SELFBOT]: {rule: 'automation', handling: 'standard', kinds: 'any'},
	[DeletionReasons.NONCONSENSUAL_SEXUAL_CONTENT]: {rule: 'intimate_images', handling: 'protected', kinds: 'any'},
	[DeletionReasons.SCAM_OR_SOCIAL_ENGINEERING]: {rule: 'fraud', handling: 'standard', kinds: 'any'},
	[DeletionReasons.CHILD_SEXUAL_CONTENT]: {rule: 'child_safety', handling: 'restricted', kinds: RESTRICTED_KINDS},
	[DeletionReasons.PRIVACY_VIOLATION_OR_DOXXING]: {rule: 'privacy', handling: 'protected', kinds: 'any'},
	[DeletionReasons.HARASSMENT_OR_BULLYING]: {rule: 'harassment', handling: 'protected', kinds: 'any'},
	[DeletionReasons.PAYMENT_FRAUD]: {rule: 'payments', handling: 'standard', kinds: PAYMENT_KINDS},
	[DeletionReasons.CHILD_SAFETY_VIOLATION]: {rule: 'child_safety', handling: 'restricted', kinds: RESTRICTED_KINDS},
	[DeletionReasons.BILLING_DISPUTE_OR_ABUSE]: {rule: 'payments', handling: 'standard', kinds: PAYMENT_KINDS},
	[DeletionReasons.UNSOLICITED_EXPLICIT_CONTENT]: {
		rule: 'unwanted_sexual_content',
		handling: 'protected',
		kinds: 'any',
	},
	[DeletionReasons.GRAPHIC_VIOLENCE]: {rule: 'violent_content', handling: 'standard', kinds: 'any'},
	[DeletionReasons.BAN_EVASION]: {
		rule: 'ban_evasion',
		handling: 'standard',
		kinds: ['warning', 'suspension', 'closure'],
	},
	[DeletionReasons.TOKEN_OR_CREDENTIAL_SCAM]: {rule: 'fraud', handling: 'standard', kinds: 'any'},
	[DeletionReasons.HATE_SPEECH_OR_EXTREMIST_CONTENT]: {rule: 'hate', handling: 'standard', kinds: 'any'},
	[DeletionReasons.MALICIOUS_LINKS_OR_MALWARE]: {rule: 'malware', handling: 'standard', kinds: 'any'},
	[DeletionReasons.IMPERSONATION_OR_FAKE_IDENTITY]: {rule: 'impersonation', handling: 'standard', kinds: 'any'},
	[DeletionReasons.THREATS_OR_VIOLENT_SPEECH]: {rule: 'threats', handling: 'protected', kinds: 'any'},
	[DeletionReasons.VIOLENT_EXTREMISM]: {rule: 'violent_extremism', handling: 'restricted', kinds: RESTRICTED_KINDS},
	[DeletionReasons.SELF_HARM_PROMOTION]: {
		rule: 'self_harm',
		handling: 'standard',
		kinds: ['warning', 'suspension', 'content_removal', 'profile_reset', 'community_removal'],
	},
	[DeletionReasons.SEXUAL_CONTENT_PLACEMENT]: {rule: 'adult_content', handling: 'standard', kinds: 'any'},
	[DeletionReasons.ILLEGAL_GOODS_OR_ACTIVITY]: {rule: 'illegal_activity', handling: 'standard', kinds: 'any'},
	[DeletionReasons.HARMFUL_DECEPTION]: {rule: 'harmful_deception', handling: 'standard', kinds: 'any'},
	[DeletionReasons.REPORTING_ABUSE]: {
		rule: 'report_abuse',
		handling: 'standard',
		kinds: ['warning', 'feature_limit', 'suspension'],
	},
	[DeletionReasons.UNDERAGE_ACCOUNT]: {rule: 'age_requirement', handling: 'standard', kinds: ['closure']},
	[DeletionReasons.COPYRIGHT]: {
		rule: 'copyright',
		handling: 'standard',
		kinds: ['warning', 'content_removal', 'profile_reset', 'community_removal'],
	},
	[DeletionReasons.OPERATOR_PASS_PAYMENT_DISPUTE]: {
		rule: 'operator_pass_payments',
		handling: 'standard',
		kinds: PAYMENT_KINDS,
	},
};

const ENFORCEMENT_SPAMMER_LIMIT_RULES: ReadonlySet<EnforcementRuleKey> = new Set([
	'spam',
	'automation',
	'fraud',
	'malware',
]);

const ENFORCEMENT_LIMIT_RULES: Readonly<Record<EnforcementLimit, ReadonlySet<EnforcementRuleKey>>> = {
	direct_messages: ENFORCEMENT_SPAMMER_LIMIT_RULES,
	reports: new Set(['report_abuse']),
	purchases: new Set(['payments', 'operator_pass_payments']),
};

const ACCOUNT_KINDS: ReadonlySet<EnforcementKind> = new Set(['suspension', 'closure']);

export function isAccountKind(kind: EnforcementKind): boolean {
	return ACCOUNT_KINDS.has(kind);
}

export function isEnforcementReasonCode(code: number | null | undefined): code is EnforcementReasonCode {
	return code !== null && code !== undefined && Object.hasOwn(ENFORCEMENT_REASONS, code);
}

export function enforcementReasonOf(code: number | null | undefined): EnforcementReason | null {
	return isEnforcementReasonCode(code) ? ENFORCEMENT_REASONS[code] : null;
}

const ENFORCEMENT_LIMITS = Object.keys(ENFORCEMENT_LIMIT_RULES) as ReadonlyArray<EnforcementLimit>;

export function isEnforcementLimitAllowed(code: EnforcementReasonCode, limit: EnforcementLimit): boolean {
	return ENFORCEMENT_LIMIT_RULES[limit].has(ENFORCEMENT_REASONS[code].rule);
}

export function isEnforcementKindAllowed(code: EnforcementReasonCode, kind: EnforcementKind): boolean {
	const {kinds} = ENFORCEMENT_REASONS[code];
	if (kinds !== 'any' && !kinds.includes(kind)) return false;
	return kind !== 'feature_limit' || ENFORCEMENT_LIMITS.some((limit) => isEnforcementLimitAllowed(code, limit));
}
