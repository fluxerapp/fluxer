// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';

export interface RegistrationEventByIpRow {
	ip: string;
	created_at: Date;
	user_id: UserID;
	email: string | null;
	email_domain: string | null;
	locale: string | null;
}

export const REGISTRATION_EVENT_BY_IP_COLUMNS = [
	'ip',
	'created_at',
	'user_id',
	'email',
	'email_domain',
	'locale',
] as const satisfies ReadonlyArray<keyof RegistrationEventByIpRow>;

export interface RegistrationEventBySubnetRow {
	subnet: string;
	created_at: Date;
	user_id: UserID;
	ip: string;
	email: string | null;
	email_domain: string | null;
	locale: string | null;
}

export const REGISTRATION_EVENT_BY_SUBNET_COLUMNS = [
	'subnet',
	'created_at',
	'user_id',
	'ip',
	'email',
	'email_domain',
	'locale',
] as const satisfies ReadonlyArray<keyof RegistrationEventBySubnetRow>;

export interface RegistrationEventByEmailDomainRow {
	email_domain: string;
	created_at: Date;
	user_id: UserID;
	ip: string;
	email: string | null;
	locale: string | null;
}

export const REGISTRATION_EVENT_BY_EMAIL_DOMAIN_COLUMNS = [
	'email_domain',
	'created_at',
	'user_id',
	'ip',
	'email',
	'locale',
] as const satisfies ReadonlyArray<keyof RegistrationEventByEmailDomainRow>;

export interface RegistrationEventByPlusAddressBaseRow {
	plus_address_base: string;
	created_at: Date;
	user_id: UserID;
	ip: string;
	email: string | null;
	locale: string | null;
}

export const REGISTRATION_EVENT_BY_PLUS_ADDRESS_BASE_COLUMNS = [
	'plus_address_base',
	'created_at',
	'user_id',
	'ip',
	'email',
	'locale',
] as const satisfies ReadonlyArray<keyof RegistrationEventByPlusAddressBaseRow>;

export interface LatestRiskContextByUserRow {
	user_id: UserID;
	updated_at: Date;
	ip: string;
	subnet: string | null;
	email_domain: string | null;
	asn: number | null;
}

export const LATEST_RISK_CONTEXT_BY_USER_COLUMNS = [
	'user_id',
	'updated_at',
	'ip',
	'subnet',
	'email_domain',
	'asn',
] as const satisfies ReadonlyArray<keyof LatestRiskContextByUserRow>;

export interface SuspiciousIpRow {
	ip: string;
	created_at: Date;
	updated_at: Date;
	expires_at: Date | null;
	source: string;
	reason: string;
	source_user_id: UserID | null;
	deletion_reason_code: number | null;
	provider_name: string | null;
	asn: number | null;
	asn_name: string | null;
	asn_type: string | null;
	risk_note: string | null;
}

export const SUSPICIOUS_IP_COLUMNS = [
	'ip',
	'created_at',
	'updated_at',
	'expires_at',
	'source',
	'reason',
	'source_user_id',
	'deletion_reason_code',
	'provider_name',
	'asn',
	'asn_name',
	'asn_type',
	'risk_note',
] as const satisfies ReadonlyArray<keyof SuspiciousIpRow>;

export interface RiskOutcomeByIpRow {
	ip: string;
	created_at: Date;
	user_id: UserID;
	outcome_code: string;
	source: string;
}

export const RISK_OUTCOME_BY_IP_COLUMNS = [
	'ip',
	'created_at',
	'user_id',
	'outcome_code',
	'source',
] as const satisfies ReadonlyArray<keyof RiskOutcomeByIpRow>;

export interface RiskOutcomeBySubnetRow {
	subnet: string;
	created_at: Date;
	user_id: UserID;
	outcome_code: string;
	source: string;
}

export const RISK_OUTCOME_BY_SUBNET_COLUMNS = [
	'subnet',
	'created_at',
	'user_id',
	'outcome_code',
	'source',
] as const satisfies ReadonlyArray<keyof RiskOutcomeBySubnetRow>;

export interface RiskOutcomeByEmailDomainRow {
	email_domain: string;
	created_at: Date;
	user_id: UserID;
	outcome_code: string;
	source: string;
}

export const RISK_OUTCOME_BY_EMAIL_DOMAIN_COLUMNS = [
	'email_domain',
	'created_at',
	'user_id',
	'outcome_code',
	'source',
] as const satisfies ReadonlyArray<keyof RiskOutcomeByEmailDomainRow>;

export interface RiskOutcomeByAsnRow {
	asn: number;
	created_at: Date;
	user_id: UserID;
	outcome_code: string;
	source: string;
}

export const RISK_OUTCOME_BY_ASN_COLUMNS = [
	'asn',
	'created_at',
	'user_id',
	'outcome_code',
	'source',
] as const satisfies ReadonlyArray<keyof RiskOutcomeByAsnRow>;

export interface RiskAssessmentRow {
	assessment_id: string;
	created_at: Date;
	user_id: UserID | null;
	ip: string;
	email: string | null;
	locale: string | null;
	risk_level: string;
	risk_score: number;
	suspicious: boolean;
	method: string;
	model_used: string;
	recommended_action: string;
	reasoning: string;
	signals_json: string;
}

export const RISK_ASSESSMENT_COLUMNS = [
	'assessment_id',
	'created_at',
	'user_id',
	'ip',
	'email',
	'locale',
	'risk_level',
	'risk_score',
	'suspicious',
	'method',
	'model_used',
	'recommended_action',
	'reasoning',
	'signals_json',
] as const satisfies ReadonlyArray<keyof RiskAssessmentRow>;

export interface RiskAssessmentByUserRow {
	user_id: UserID;
	created_at: Date;
	assessment_id: string;
	risk_level: string;
	risk_score: number;
}

export const RISK_ASSESSMENT_BY_USER_COLUMNS = [
	'user_id',
	'created_at',
	'assessment_id',
	'risk_level',
	'risk_score',
] as const satisfies ReadonlyArray<keyof RiskAssessmentByUserRow>;

export interface InboundSmsChallengeRow {
	challenge_code: string;
	user_id: UserID;
	our_number: string;
	created_at: Date;
	expires_at: Date;
	consumed_at: Date | null;
	consumed_from_phone: string | null;
}

export const INBOUND_SMS_CHALLENGE_COLUMNS = [
	'challenge_code',
	'user_id',
	'our_number',
	'created_at',
	'expires_at',
	'consumed_at',
	'consumed_from_phone',
] as const satisfies ReadonlyArray<keyof InboundSmsChallengeRow>;

export interface InboundSmsChallengeByUserRow {
	user_id: UserID;
	created_at: Date;
	challenge_code: string;
	expires_at: Date;
}

export const INBOUND_SMS_CHALLENGE_BY_USER_COLUMNS = [
	'user_id',
	'created_at',
	'challenge_code',
	'expires_at',
] as const satisfies ReadonlyArray<keyof InboundSmsChallengeByUserRow>;

export interface PhoneLookupCacheRow {
	phone: string;
	looked_up_at: Date;
	valid: boolean;
	line_type: string | null;
	country_code: string | null;
	carrier_name: string | null;
	sms_pumping_risk_score: number | null;
}

export const PHONE_LOOKUP_CACHE_COLUMNS = [
	'phone',
	'looked_up_at',
	'valid',
	'line_type',
	'country_code',
	'carrier_name',
	'sms_pumping_risk_score',
] as const satisfies ReadonlyArray<keyof PhoneLookupCacheRow>;

export interface PhoneVerificationAttemptRow {
	attempt_id: string;
	created_at: Date;
	phone: string;
	country_code: string | null;
	line_type: string | null;
	carrier_name: string | null;
	sms_pumping_risk_score: number | null;
	verdict: string;
	reject_reason: string | null;
	inbound_reason: string | null;
	lookup_cache_hit: boolean;
}

export const PHONE_VERIFICATION_ATTEMPT_COLUMNS = [
	'attempt_id',
	'created_at',
	'phone',
	'country_code',
	'line_type',
	'carrier_name',
	'sms_pumping_risk_score',
	'verdict',
	'reject_reason',
	'inbound_reason',
	'lookup_cache_hit',
] as const satisfies ReadonlyArray<keyof PhoneVerificationAttemptRow>;
