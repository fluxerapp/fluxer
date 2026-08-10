// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	RecommendedAction,
	type RegistrationEvent,
	type ReverseDnsClassification,
	type RiskAssessment,
	RiskConfidence,
	RiskDecisionMethod,
	RiskLevel,
} from './RiskTypes';

type JsonScalar = string | number | boolean | null;
type JsonRecord = Record<string, unknown>;
type PolicyBitfield = number;
type PolicyBitDefinitions = Readonly<Record<string, PolicyBitfield>>;
type AccountPolicyAuditLevel = 'debug' | 'info' | 'warn' | 'error';
type AccountPolicyRiskHistoryOutcome = 'challenged';
export type AccountPolicyContactCapability =
	| 'captcha_exempt'
	| 'followup_risk_exempt'
	| 'reputation_checks_exempt'
	| 'required_actions_exempt';
export type AccountPolicyEmailTldRisk = 'high';

const FACT_PATHS = new Set([
	'contact.value',
	'contact.domain',
	'contact.domain_admin_listed',
	'contact.domain_disposable',
	'contact.domain_blocked',
	'contact.domain_step_up_required',
	'region.code',
	'region.step_up_required',
	'assessment.action',
	'assessment.level',
	'assessment.score',
	'decision.flag_bits',
]);
const CONTACT_CAPABILITY_FACT_PATHS = new Set([
	'contact.value',
	'contact.domain',
	'contact.domain_admin_listed',
	'contact.domain_disposable',
	'contact.domain_blocked',
	'contact.domain_step_up_required',
]);
const CONTACT_CAPABILITIES = new Set<AccountPolicyContactCapability>([
	'captcha_exempt',
	'followup_risk_exempt',
	'reputation_checks_exempt',
	'required_actions_exempt',
]);
const REVERSE_DNS_CLASSIFICATION_ORDER: ReadonlyArray<Exclude<ReverseDnsClassification, 'unknown'>> = [
	'cellular',
	'business',
	'static',
	'dynamic',
];

const COMPARISON_KEYS = ['equals', 'not_equals', 'gt', 'gte', 'lt', 'lte', 'in', 'in_set'] as const;

interface AccountPolicyDefaults {
	readonly flagBits: PolicyBitfield;
	readonly inviteAutoJoinEnabled: boolean;
}

export interface AccountPolicyContactContext {
	readonly value: RegistrationEvent['email'] | null;
	readonly domain: string | null;
	readonly domainAdminListed: boolean;
	readonly domainDisposable: boolean;
	readonly domainBlocked: boolean;
	readonly domainStepUpRequired: boolean;
}

export interface AccountPolicyRegionContext {
	readonly code: string | null;
	readonly stepUpRequired: boolean;
}

export interface AccountPolicyContext {
	readonly contact: AccountPolicyContactContext;
	readonly region: AccountPolicyRegionContext;
	readonly assessment: {
		readonly raw: RiskAssessment;
		readonly level: RiskLevel;
		readonly action: RecommendedAction;
	};
}

interface AccountPolicyAuditEvent {
	readonly event: string;
	readonly level: AccountPolicyAuditLevel;
	readonly ruleId: string;
}

export interface AccountPolicyDecision {
	readonly flagBits: number;
	readonly inviteAutoJoinEnabled: boolean;
	readonly inviteAutoJoinSkipReason: string | null;
	readonly riskHistoryOutcomeCodes: ReadonlyArray<AccountPolicyRiskHistoryOutcome>;
	readonly auditEvents: ReadonlyArray<AccountPolicyAuditEvent>;
	readonly matchedRuleIds: ReadonlyArray<string>;
}

export interface IAccountPolicyEvaluator {
	evaluate(context: AccountPolicyContext): AccountPolicyDecision;
	evaluateContact(contact: AccountPolicyContactContext): AccountPolicyContactDecision;
	isLowRiskEmailTld(tld: string | null | undefined): boolean;
	classifyEmailTld(tld: string | null | undefined): AccountPolicyEmailTldRisk | null;
	isBlockedRegistrationEmailDomain(domain: string | null | undefined): boolean;
	isTrustedCommercialPrivacyProvider(providerName: string | null | undefined): boolean;
	isEducationOrganizationName(organizationName: string | null | undefined): boolean;
	classifyReverseDnsHostname(hostname: string | null | undefined): ReverseDnsClassification;
	getMinimumAgeForRegion(countryCode: string | null | undefined, defaultAge: number): number;
}

interface MutableAccountPolicyDecision {
	flagBits: PolicyBitfield;
	inviteAutoJoinEnabled: boolean;
	inviteAutoJoinSkipReason: string | null;
	riskHistoryOutcomeCodes: Array<AccountPolicyRiskHistoryOutcome>;
	auditEvents: Array<AccountPolicyAuditEvent>;
	matchedRuleIds: Array<string>;
}

interface FactCondition {
	readonly fact: string;
	readonly equals?: JsonScalar;
	readonly not_equals?: JsonScalar;
	readonly gt?: number;
	readonly gte?: number;
	readonly lt?: number;
	readonly lte?: number;
	readonly in?: ReadonlyArray<JsonScalar>;
	readonly in_set?: string;
}

type AccountPolicyCondition =
	| FactCondition
	| {
			readonly all: ReadonlyArray<AccountPolicyCondition>;
	  }
	| {
			readonly any: ReadonlyArray<AccountPolicyCondition>;
	  }
	| {
			readonly not: AccountPolicyCondition;
	  };

type AccountPolicyEffect =
	| {
			readonly type: 'or_bits';
			readonly bits: PolicyBitfield;
	  }
	| {
			readonly type: 'or_bits_from_map';
			readonly fact: string;
			readonly map: Readonly<Record<string, PolicyBitfield>>;
	  }
	| {
			readonly type: 'set_feature';
			readonly feature: 'invite_auto_join';
			readonly enabled: boolean;
			readonly reason?: string;
	  }
	| {
			readonly type: 'append_markers';
			readonly markers: ReadonlyArray<AccountPolicyRiskHistoryOutcome>;
	  }
	| {
			readonly type: 'emit';
			readonly event: string;
			readonly level: AccountPolicyAuditLevel;
	  };

interface AccountPolicyRule {
	readonly id: string;
	readonly when: AccountPolicyCondition | null;
	readonly effects: ReadonlyArray<AccountPolicyEffect>;
}

interface AccountPolicyContactCapabilityRule {
	readonly id: string;
	readonly when: AccountPolicyCondition | null;
	readonly capabilities: ReadonlyArray<AccountPolicyContactCapability>;
}

interface AccountPolicyClassifiers {
	readonly emailTlds: {
		readonly lowRisk: ReadonlySet<string>;
		readonly blocked: ReadonlySet<string>;
	};
	readonly network: {
		readonly trustedPrivacyProviderMarkers: ReadonlyArray<ReadonlyArray<string>>;
		readonly educationOrgMarkers: ReadonlyArray<string>;
		readonly reverseDnsMarkers: Readonly<Record<ReverseDnsClassification, ReadonlyArray<RegExp>>>;
	};
	readonly regionalMinimumAge: {
		readonly byCountry: Readonly<Record<string, number>>;
	};
}

interface AccountPolicyDocument {
	readonly version: 1;
	readonly id: string;
	readonly sets: Readonly<Record<string, ReadonlyArray<JsonScalar>>>;
	readonly defaults: AccountPolicyDefaults;
	readonly rules: ReadonlyArray<AccountPolicyRule>;
	readonly contactCapabilityRules: ReadonlyArray<AccountPolicyContactCapabilityRule>;
	readonly classifiers: AccountPolicyClassifiers;
}

function isRecord(value: unknown): value is JsonRecord {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function asRecord(value: unknown, path: string): JsonRecord {
	if (!isRecord(value)) {
		throw new Error(`${path} must be an object`);
	}
	return value;
}

function asString(value: unknown, path: string): string {
	if (typeof value !== 'string' || value.trim().length === 0) {
		throw new Error(`${path} must be a non-empty string`);
	}
	return value;
}

function asBoolean(value: unknown, path: string): boolean {
	if (typeof value !== 'boolean') {
		throw new Error(`${path} must be a boolean`);
	}
	return value;
}

function asNumber(value: unknown, path: string): number {
	if (typeof value !== 'number' || !Number.isFinite(value)) {
		throw new Error(`${path} must be a finite number`);
	}
	return value;
}

function asBitfield(value: unknown, path: string): PolicyBitfield {
	const bitfield = asNumber(value, path);
	if (!Number.isInteger(bitfield) || bitfield < 0) {
		throw new Error(`${path} must be a non-negative integer bitfield`);
	}
	return bitfield;
}

function parseBitfieldValue(value: unknown, path: string, bitDefinitions: PolicyBitDefinitions): PolicyBitfield {
	if (typeof value === 'number') {
		return asBitfield(value, path);
	}
	if (typeof value === 'string') {
		const bit = bitDefinitions[value];
		if (bit === undefined) {
			throw new Error(`${path} references unknown bit alias: ${value}`);
		}
		return bit;
	}
	if (Array.isArray(value)) {
		let bitfield = 0;
		value.forEach((item, index) => {
			bitfield |= parseBitfieldValue(item, `${path}[${index}]`, bitDefinitions);
		});
		return bitfield;
	}
	throw new Error(`${path} must be a bitfield number, bit alias, or bit alias array`);
}

function asJsonScalar(value: unknown, path: string): JsonScalar {
	if (typeof value === 'string' || typeof value === 'number' || typeof value === 'boolean' || value === null) {
		return value;
	}
	throw new Error(`${path} must be a JSON scalar`);
}

function asArray(value: unknown, path: string): ReadonlyArray<unknown> {
	if (!Array.isArray(value)) {
		throw new Error(`${path} must be an array`);
	}
	return value;
}

function asStringArray(value: unknown, path: string): ReadonlyArray<string> {
	return asArray(value, path).map((item, index) => asString(item, `${path}[${index}]`));
}

function parseRiskHistoryOutcome(value: unknown, path: string): AccountPolicyRiskHistoryOutcome {
	const outcome = asString(value, path);
	if (outcome !== 'challenged') {
		throw new Error(`${path} references unknown marker: ${outcome}`);
	}
	return outcome;
}

function parseRiskHistoryOutcomes(value: unknown, path: string): ReadonlyArray<AccountPolicyRiskHistoryOutcome> {
	return asArray(value, path).map((item, index) => parseRiskHistoryOutcome(item, `${path}[${index}]`));
}

function parseAuditLevel(value: unknown, path: string): AccountPolicyAuditLevel {
	const level = asString(value, path);
	if (level !== 'debug' && level !== 'info' && level !== 'warn' && level !== 'error') {
		throw new Error(`${path} references unknown event level: ${level}`);
	}
	return level;
}

function addUnique<T>(target: Array<T>, values: ReadonlyArray<T>): void {
	for (const value of values) {
		if (!target.includes(value)) {
			target.push(value);
		}
	}
}

function hasKey(record: JsonRecord, key: string): boolean {
	return Object.hasOwn(record, key);
}

function normalizePolicyToken(value: string): string {
	return value.trim().toLowerCase();
}

function normalizePolicyCountryCode(value: string): string {
	return value.trim().toUpperCase();
}

function normalizePolicyTld(value: string): string | null {
	const normalized = normalizePolicyToken(value);
	if (!normalized) return null;
	return normalized.startsWith('.') ? normalized.slice(1) : normalized;
}

function extractPolicyTldFromDomain(domain: string | null | undefined): string | null {
	const normalized = normalizePolicyContactDomain(domain);
	if (!normalized) return null;
	const dotIndex = normalized.lastIndexOf('.');
	if (dotIndex < 0 || dotIndex === normalized.length - 1) return null;
	return normalizePolicyTld(normalized.slice(dotIndex + 1));
}

function normalizeProviderTokens(providerName: string | null | undefined): Array<string> {
	if (!providerName) return [];
	return providerName
		.toLowerCase()
		.replace(/[^a-z0-9]+/g, ' ')
		.trim()
		.split(/\s+/)
		.filter((token) => token.length > 0);
}

function containsTokenRun(tokens: ReadonlyArray<string>, run: ReadonlyArray<string>): boolean {
	if (run.length === 0 || tokens.length < run.length) return false;
	outer: for (let i = 0; i <= tokens.length - run.length; i++) {
		for (let j = 0; j < run.length; j++) {
			if (tokens[i + j] !== run[j]) continue outer;
		}
		return true;
	}
	return false;
}

export function normalizePolicyContactDomain(domain: string | null | undefined): string | null {
	const normalized = domain?.trim().toLowerCase();
	return normalized ? normalized : null;
}

function extractPolicyContactDomain(value: string | null | undefined): string | null {
	const normalized = value?.trim().toLowerCase();
	if (!normalized) return null;
	const atIndex = normalized.lastIndexOf('@');
	if (atIndex <= 0 || atIndex === normalized.length - 1) return null;
	return normalizePolicyContactDomain(normalized.slice(atIndex + 1));
}

export function createAccountPolicyContactContext(
	value: string | null | undefined,
	overrides: Partial<Omit<AccountPolicyContactContext, 'value'>> = {},
): AccountPolicyContactContext {
	return {
		value: value ?? null,
		domain: normalizePolicyContactDomain(overrides.domain ?? extractPolicyContactDomain(value)),
		domainAdminListed: overrides.domainAdminListed ?? false,
		domainDisposable: overrides.domainDisposable ?? false,
		domainBlocked: overrides.domainBlocked ?? false,
		domainStepUpRequired: overrides.domainStepUpRequired ?? false,
	};
}

export function createAccountPolicyContactDomainContext(
	domain: string | null | undefined,
	overrides: Partial<Omit<AccountPolicyContactContext, 'value' | 'domain'>> = {},
): AccountPolicyContactContext {
	return createAccountPolicyContactContext(null, {
		...overrides,
		domain,
	});
}

export class AccountPolicyContactDecision {
	private readonly capabilitySet: ReadonlySet<AccountPolicyContactCapability>;

	constructor(capabilities: ReadonlyArray<AccountPolicyContactCapability>) {
		this.capabilitySet = new Set(capabilities);
	}

	hasCapability(capability: AccountPolicyContactCapability): boolean {
		return this.capabilitySet.has(capability);
	}

	get capabilities(): ReadonlyArray<AccountPolicyContactCapability> {
		return [...this.capabilitySet];
	}
}

class AccountPolicyDocumentParser {
	parse(rawDocument: unknown): AccountPolicyDocument {
		const document = asRecord(rawDocument, 'policy DSL');
		const version = document['version'];
		if (version !== 1) {
			throw new Error('policy DSL version must be 1');
		}
		const bitDefinitions = this.parseBitDefinitions(document['definitions']);
		const sets = this.parseSets(document['sets']);
		const defaults = this.parseDefaults(document['defaults'], bitDefinitions);
		const seenRuleIds = new Set<string>();
		const rules = asArray(document['rules'], 'rules').map((item, index) => {
			const rule = this.parseRule(item, `rules[${index}]`, sets, bitDefinitions);
			if (seenRuleIds.has(rule.id)) {
				throw new Error(`rules[${index}].id duplicates rule id: ${rule.id}`);
			}
			seenRuleIds.add(rule.id);
			return rule;
		});
		const contactCapabilityRules = this.parseContactCapabilityRules(
			document['contact_capability_rules'],
			sets,
			seenRuleIds,
		);
		const classifiers = this.parseClassifiers(document['classifiers']);
		return {
			version,
			id: typeof document['id'] === 'string' && document['id'].trim() ? document['id'] : 'policy',
			sets,
			defaults,
			rules,
			contactCapabilityRules,
			classifiers,
		};
	}

	private parseBitDefinitions(rawDefinitions: unknown): PolicyBitDefinitions {
		const definitions = rawDefinitions === undefined ? {} : asRecord(rawDefinitions, 'definitions');
		const rawBits = definitions['bits'] === undefined ? {} : asRecord(definitions['bits'], 'definitions.bits');
		const bits: Record<string, PolicyBitfield> = {};
		for (const [name, value] of Object.entries(rawBits)) {
			if (!name.trim()) throw new Error('definitions.bits contains an empty bit alias');
			bits[name] = asBitfield(value, `definitions.bits.${name.replace(/[^a-z0-9_]/gi, '_')}`);
		}
		return bits;
	}

	private parseSets(rawSets: unknown): Readonly<Record<string, ReadonlyArray<JsonScalar>>> {
		const sets = rawSets === undefined ? {} : asRecord(rawSets, 'sets');
		const parsed: Record<string, ReadonlyArray<JsonScalar>> = {};
		for (const [name, value] of Object.entries(sets)) {
			if (!name.trim()) throw new Error('sets contains an empty set name');
			parsed[name] = asArray(value, `sets.${name}`).map((item, index) => asJsonScalar(item, `sets.${name}[${index}]`));
		}
		return parsed;
	}

	private parseDefaults(rawDefaults: unknown, bitDefinitions: PolicyBitDefinitions): AccountPolicyDefaults {
		const defaults = rawDefaults === undefined ? {} : asRecord(rawDefaults, 'defaults');
		const features = defaults['features'] === undefined ? {} : asRecord(defaults['features'], 'defaults.features');
		return {
			flagBits:
				defaults['flag_bits'] === undefined
					? 0
					: parseBitfieldValue(defaults['flag_bits'], 'defaults.flag_bits', bitDefinitions),
			inviteAutoJoinEnabled:
				features['invite_auto_join'] === undefined
					? true
					: asBoolean(features['invite_auto_join'], 'defaults.features.invite_auto_join'),
		};
	}

	private parseRule(
		rawRule: unknown,
		path: string,
		sets: Readonly<Record<string, ReadonlyArray<JsonScalar>>>,
		bitDefinitions: PolicyBitDefinitions,
	): AccountPolicyRule {
		const rule = asRecord(rawRule, path);
		const id = asString(rule['id'], `${path}.id`);
		const when =
			rule['when'] === undefined ? null : this.parseCondition(rule['when'], `${path}.when`, sets, FACT_PATHS);
		const effects = asArray(rule['effects'], `${path}.effects`).map((effect, index) =>
			this.parseEffect(effect, `${path}.effects[${index}]`, bitDefinitions),
		);
		if (effects.length === 0) {
			throw new Error(`${path}.effects must not be empty`);
		}
		return {id, when, effects};
	}

	private parseContactCapabilityRules(
		rawRules: unknown,
		sets: Readonly<Record<string, ReadonlyArray<JsonScalar>>>,
		seenRuleIds: Set<string>,
	): ReadonlyArray<AccountPolicyContactCapabilityRule> {
		if (rawRules === undefined) return [];
		return asArray(rawRules, 'contact_capability_rules').map((item, index) => {
			const path = `contact_capability_rules[${index}]`;
			const rule = asRecord(item, path);
			const id = asString(rule['id'], `${path}.id`);
			if (seenRuleIds.has(id)) {
				throw new Error(`${path}.id duplicates rule id: ${id}`);
			}
			seenRuleIds.add(id);
			const when =
				rule['when'] === undefined
					? null
					: this.parseCondition(rule['when'], `${path}.when`, sets, CONTACT_CAPABILITY_FACT_PATHS);
			const capabilities = asArray(rule['capabilities'], `${path}.capabilities`).map((capability, capabilityIndex) =>
				this.parseContactCapability(capability, `${path}.capabilities[${capabilityIndex}]`),
			);
			if (capabilities.length === 0) {
				throw new Error(`${path}.capabilities must not be empty`);
			}
			return {id, when, capabilities};
		});
	}

	private parseCondition(
		rawCondition: unknown,
		path: string,
		sets: Readonly<Record<string, ReadonlyArray<JsonScalar>>>,
		factPaths: ReadonlySet<string>,
	): AccountPolicyCondition {
		const condition = asRecord(rawCondition, path);
		if (hasKey(condition, 'all')) {
			const children = asArray(condition['all'], `${path}.all`).map((item, index) =>
				this.parseCondition(item, `${path}.all[${index}]`, sets, factPaths),
			);
			if (children.length === 0) throw new Error(`${path}.all must not be empty`);
			return {all: children};
		}
		if (hasKey(condition, 'any')) {
			const children = asArray(condition['any'], `${path}.any`).map((item, index) =>
				this.parseCondition(item, `${path}.any[${index}]`, sets, factPaths),
			);
			if (children.length === 0) throw new Error(`${path}.any must not be empty`);
			return {any: children};
		}
		if (hasKey(condition, 'not')) {
			return {not: this.parseCondition(condition['not'], `${path}.not`, sets, factPaths)};
		}
		const fact = asString(condition['fact'], `${path}.fact`);
		this.assertFactPath(fact, `${path}.fact`, factPaths);
		const comparisonCount = COMPARISON_KEYS.filter((key) => hasKey(condition, key)).length;
		if (comparisonCount !== 1) {
			throw new Error(`${path} must contain exactly one comparison`);
		}
		const parsed: FactCondition = {fact};
		if (hasKey(condition, 'equals')) {
			return {...parsed, equals: asJsonScalar(condition['equals'], `${path}.equals`)};
		}
		if (hasKey(condition, 'not_equals')) {
			return {...parsed, not_equals: asJsonScalar(condition['not_equals'], `${path}.not_equals`)};
		}
		if (hasKey(condition, 'gt')) return {...parsed, gt: asNumber(condition['gt'], `${path}.gt`)};
		if (hasKey(condition, 'gte')) return {...parsed, gte: asNumber(condition['gte'], `${path}.gte`)};
		if (hasKey(condition, 'lt')) return {...parsed, lt: asNumber(condition['lt'], `${path}.lt`)};
		if (hasKey(condition, 'lte')) return {...parsed, lte: asNumber(condition['lte'], `${path}.lte`)};
		if (hasKey(condition, 'in_set')) {
			const setName = asString(condition['in_set'], `${path}.in_set`);
			if (!(setName in sets)) {
				throw new Error(`${path}.in_set references unknown set: ${setName}`);
			}
			return {...parsed, in_set: setName};
		}
		return {
			...parsed,
			in: asArray(condition['in'], `${path}.in`).map((item, index) => asJsonScalar(item, `${path}.in[${index}]`)),
		};
	}

	private parseEffect(rawEffect: unknown, path: string, bitDefinitions: PolicyBitDefinitions): AccountPolicyEffect {
		const effect = asRecord(rawEffect, path);
		const type = asString(effect['type'], `${path}.type`);
		switch (type) {
			case 'or_bits':
				return {
					type,
					bits: parseBitfieldValue(effect['bits'], `${path}.bits`, bitDefinitions),
				};
			case 'or_bits_from_map': {
				const fact = asString(effect['fact'], `${path}.fact`);
				this.assertFactPath(fact, `${path}.fact`, FACT_PATHS);
				return {
					type,
					fact,
					map: this.parseBitfieldMap(effect['map'], `${path}.map`, bitDefinitions),
				};
			}
			case 'set_feature': {
				const feature = asString(effect['feature'], `${path}.feature`);
				if (feature !== 'invite_auto_join') {
					throw new Error(`${path}.feature references unknown feature: ${feature}`);
				}
				return {
					type,
					feature,
					enabled: asBoolean(effect['enabled'], `${path}.enabled`),
					reason: effect['reason'] === undefined ? undefined : asString(effect['reason'], `${path}.reason`),
				};
			}
			case 'append_markers':
				return {
					type,
					markers: parseRiskHistoryOutcomes(effect['markers'], `${path}.markers`),
				};
			case 'emit':
				return {
					type,
					event: asString(effect['event'], `${path}.event`),
					level: parseAuditLevel(effect['level'], `${path}.level`),
				};
			default:
				throw new Error(`${path}.type references unknown effect type: ${type}`);
		}
	}

	private parseBitfieldMap(
		rawMap: unknown,
		path: string,
		bitDefinitions: PolicyBitDefinitions,
	): Readonly<Record<string, PolicyBitfield>> {
		const map = asRecord(rawMap, path);
		const parsed: Record<string, PolicyBitfield> = {};
		for (const [key, value] of Object.entries(map)) {
			if (!key.trim()) throw new Error(`${path} contains an empty map key`);
			parsed[key] = parseBitfieldValue(value, `${path}.${key.replace(/[^a-z0-9_]/gi, '_')}`, bitDefinitions);
		}
		return parsed;
	}

	private parseContactCapability(value: unknown, path: string): AccountPolicyContactCapability {
		const capability = asString(value, path);
		if (!CONTACT_CAPABILITIES.has(capability as AccountPolicyContactCapability)) {
			throw new Error(`${path} references unknown contact capability: ${capability}`);
		}
		return capability as AccountPolicyContactCapability;
	}

	private parseClassifiers(rawClassifiers: unknown): AccountPolicyClassifiers {
		const classifiers = rawClassifiers === undefined ? {} : asRecord(rawClassifiers, 'classifiers');
		const emailTlds =
			classifiers['email_tlds'] === undefined ? {} : asRecord(classifiers['email_tlds'], 'classifiers.email_tlds');
		const network = classifiers['network'] === undefined ? {} : asRecord(classifiers['network'], 'classifiers.network');
		return {
			emailTlds: {
				lowRisk: this.parseNormalizedSet(emailTlds['low_risk'], 'classifiers.email_tlds.low_risk', normalizePolicyTld),
				blocked: this.parseNormalizedSet(emailTlds['blocked'], 'classifiers.email_tlds.blocked', normalizePolicyTld),
			},
			network: {
				trustedPrivacyProviderMarkers: this.parseTokenRuns(
					network['trusted_privacy_provider_markers'],
					'classifiers.network.trusted_privacy_provider_markers',
				),
				educationOrgMarkers: this.parseNormalizedStringList(
					network['education_org_markers'],
					'classifiers.network.education_org_markers',
				),
				reverseDnsMarkers: this.parseReverseDnsMarkers(
					network['reverse_dns_markers'],
					'classifiers.network.reverse_dns_markers',
				),
			},
			regionalMinimumAge: {
				byCountry: this.parseRegionalMinimumAge(
					classifiers['regional_minimum_age'],
					'classifiers.regional_minimum_age',
				),
			},
		};
	}

	private parseNormalizedSet(
		rawValues: unknown,
		path: string,
		normalize: (value: string) => string | null,
	): ReadonlySet<string> {
		if (rawValues === undefined) return new Set();
		const values = asStringArray(rawValues, path);
		const parsed = new Set<string>();
		values.forEach((value, index) => {
			const normalized = normalize(value);
			if (!normalized) {
				throw new Error(`${path}[${index}] must not be empty`);
			}
			parsed.add(normalized);
		});
		return parsed;
	}

	private parseNormalizedStringList(rawValues: unknown, path: string): ReadonlyArray<string> {
		if (rawValues === undefined) return [];
		return asStringArray(rawValues, path).map((value, index) => {
			const normalized = normalizePolicyToken(value);
			if (!normalized) {
				throw new Error(`${path}[${index}] must not be empty`);
			}
			return normalized;
		});
	}

	private parseTokenRuns(rawValues: unknown, path: string): ReadonlyArray<ReadonlyArray<string>> {
		return this.parseNormalizedStringList(rawValues, path).map((value, index) => {
			const tokens = normalizeProviderTokens(value);
			if (tokens.length === 0) {
				throw new Error(`${path}[${index}] must contain at least one token`);
			}
			return tokens;
		});
	}

	private parseReverseDnsMarkers(
		rawMarkers: unknown,
		path: string,
	): Readonly<Record<ReverseDnsClassification, ReadonlyArray<RegExp>>> {
		const markers = rawMarkers === undefined ? {} : asRecord(rawMarkers, path);
		return {
			unknown: [],
			cellular: this.parseRegexList(markers['cellular'], `${path}.cellular`),
			business: this.parseRegexList(markers['business'], `${path}.business`),
			static: this.parseRegexList(markers['static'], `${path}.static`),
			dynamic: this.parseRegexList(markers['dynamic'], `${path}.dynamic`),
		};
	}

	private parseRegexList(rawPatterns: unknown, path: string): ReadonlyArray<RegExp> {
		if (rawPatterns === undefined) return [];
		return asStringArray(rawPatterns, path).map((pattern, index) => {
			try {
				return new RegExp(pattern);
			} catch (error) {
				throw new Error(
					`${path}[${index}] must be a valid regular expression: ${
						error instanceof Error ? error.message : String(error)
					}`,
				);
			}
		});
	}

	private parseRegionalMinimumAge(rawMinimumAge: unknown, path: string): Readonly<Record<string, number>> {
		if (rawMinimumAge === undefined) return {};
		const minimumAge = asRecord(rawMinimumAge, path);
		const byCountry =
			minimumAge['by_country'] === undefined ? {} : asRecord(minimumAge['by_country'], `${path}.by_country`);
		const parsed: Record<string, number> = {};
		for (const [countryCode, rawAge] of Object.entries(byCountry)) {
			const normalized = normalizePolicyCountryCode(countryCode);
			if (!/^[A-Z]{2}$/.test(normalized)) {
				throw new Error(`${path}.by_country contains invalid country code: ${countryCode}`);
			}
			const age = asNumber(rawAge, `${path}.by_country.${normalized}`);
			if (!Number.isInteger(age) || age < 0) {
				throw new Error(`${path}.by_country.${normalized} must be a non-negative integer`);
			}
			parsed[normalized] = age;
		}
		return parsed;
	}

	private assertFactPath(fact: string, path: string, factPaths: ReadonlySet<string>): void {
		if (!factPaths.has(fact)) {
			throw new Error(`${path} references unknown fact path: ${fact}`);
		}
	}
}

class AccountPolicyFactReader {
	read(path: string, context: AccountPolicyContext, decision: MutableAccountPolicyDecision): JsonScalar {
		switch (path) {
			case 'contact.value':
				return context.contact.value;
			case 'contact.domain':
				return context.contact.domain;
			case 'contact.domain_admin_listed':
				return context.contact.domainAdminListed;
			case 'contact.domain_disposable':
				return context.contact.domainDisposable;
			case 'contact.domain_blocked':
				return context.contact.domainBlocked;
			case 'contact.domain_step_up_required':
				return context.contact.domainStepUpRequired;
			case 'region.code':
				return context.region.code;
			case 'region.step_up_required':
				return context.region.stepUpRequired;
			case 'assessment.action':
				return context.assessment.action;
			case 'assessment.level':
				return context.assessment.level;
			case 'assessment.score':
				return context.assessment.raw.riskScore;
			case 'decision.flag_bits':
				return decision.flagBits;
			default:
				throw new Error(`unknown policy fact path: ${path}`);
		}
	}
}

class AccountPolicyConditionEvaluator {
	constructor(
		private readonly sets: Readonly<Record<string, ReadonlyArray<JsonScalar>>>,
		private readonly facts = new AccountPolicyFactReader(),
	) {}

	matches(
		condition: AccountPolicyCondition | null,
		context: AccountPolicyContext,
		decision: MutableAccountPolicyDecision,
	): boolean {
		if (condition === null) return true;
		if ('all' in condition) return condition.all.every((child) => this.matches(child, context, decision));
		if ('any' in condition) return condition.any.some((child) => this.matches(child, context, decision));
		if ('not' in condition) return !this.matches(condition.not, context, decision);
		const actual = this.facts.read(condition.fact, context, decision);
		return this.compare(actual, condition);
	}

	private compare(actual: JsonScalar, condition: FactCondition): boolean {
		if (condition.equals !== undefined) return actual === condition.equals;
		if (condition.not_equals !== undefined) return actual !== condition.not_equals;
		if (condition.in !== undefined) return condition.in.includes(actual);
		if (condition.in_set !== undefined) return (this.sets[condition.in_set] ?? []).includes(actual);
		if (typeof actual !== 'number') return false;
		if (condition.gt !== undefined) return actual > condition.gt;
		if (condition.gte !== undefined) return actual >= condition.gte;
		if (condition.lt !== undefined) return actual < condition.lt;
		if (condition.lte !== undefined) return actual <= condition.lte;
		return false;
	}
}

class AccountPolicyRuleEvaluator {
	constructor(
		private readonly facts: AccountPolicyFactReader,
		private readonly conditionEvaluator: AccountPolicyConditionEvaluator,
	) {}

	apply(rule: AccountPolicyRule, context: AccountPolicyContext, decision: MutableAccountPolicyDecision): void {
		if (!this.conditionEvaluator.matches(rule.when, context, decision)) return;
		decision.matchedRuleIds.push(rule.id);
		for (const effect of rule.effects) {
			this.applyEffect(rule.id, effect, context, decision);
		}
	}

	private applyEffect(
		ruleId: string,
		effect: AccountPolicyEffect,
		context: AccountPolicyContext,
		decision: MutableAccountPolicyDecision,
	): void {
		switch (effect.type) {
			case 'or_bits':
				decision.flagBits |= effect.bits;
				return;
			case 'or_bits_from_map': {
				const value = this.facts.read(effect.fact, context, decision);
				if (typeof value !== 'string') return;
				decision.flagBits |= effect.map[value] ?? 0;
				return;
			}
			case 'set_feature':
				decision.inviteAutoJoinEnabled = effect.enabled;
				decision.inviteAutoJoinSkipReason = effect.enabled ? null : (effect.reason ?? ruleId);
				return;
			case 'append_markers':
				addUnique(decision.riskHistoryOutcomeCodes, effect.markers);
				return;
			case 'emit':
				decision.auditEvents.push({event: effect.event, level: effect.level, ruleId});
				return;
		}
	}
}

class AccountPolicyContactCapabilityEvaluator {
	constructor(private readonly conditionEvaluator: AccountPolicyConditionEvaluator) {}

	evaluate(
		rules: ReadonlyArray<AccountPolicyContactCapabilityRule>,
		contact: AccountPolicyContactContext,
	): AccountPolicyContactDecision {
		const context: AccountPolicyContext = {
			contact,
			region: {code: null, stepUpRequired: false},
			assessment: {
				raw: {
					suspicious: false,
					level: RiskLevel.Low,
					confidence: RiskConfidence.Low,
					riskScore: 0,
					reasoning: 'contact policy capability evaluation',
					recommendedAction: RecommendedAction.Allow,
					method: RiskDecisionMethod.Noop,
					modelUsed: 'account-policy',
					rounds: 0,
					elapsedMs: 0,
					signals: {},
				},
				level: RiskLevel.Low,
				action: RecommendedAction.Allow,
			},
		};
		const decision: MutableAccountPolicyDecision = {
			flagBits: 0,
			inviteAutoJoinEnabled: true,
			inviteAutoJoinSkipReason: null,
			riskHistoryOutcomeCodes: [],
			auditEvents: [],
			matchedRuleIds: [],
		};
		const capabilities: Array<AccountPolicyContactCapability> = [];
		for (const rule of rules) {
			if (!this.conditionEvaluator.matches(rule.when, context, decision)) continue;
			addUnique(capabilities, rule.capabilities);
		}
		return new AccountPolicyContactDecision(capabilities);
	}
}

class AccountPolicyEvaluator implements IAccountPolicyEvaluator {
	private readonly ruleEvaluator: AccountPolicyRuleEvaluator;
	private readonly contactCapabilityEvaluator: AccountPolicyContactCapabilityEvaluator;

	constructor(private readonly document: AccountPolicyDocument) {
		const facts = new AccountPolicyFactReader();
		const conditionEvaluator = new AccountPolicyConditionEvaluator(document.sets, facts);
		this.ruleEvaluator = new AccountPolicyRuleEvaluator(facts, conditionEvaluator);
		this.contactCapabilityEvaluator = new AccountPolicyContactCapabilityEvaluator(conditionEvaluator);
	}

	evaluate(context: AccountPolicyContext): AccountPolicyDecision {
		const decision: MutableAccountPolicyDecision = {
			flagBits: this.document.defaults.flagBits,
			inviteAutoJoinEnabled: this.document.defaults.inviteAutoJoinEnabled,
			inviteAutoJoinSkipReason: this.document.defaults.inviteAutoJoinEnabled ? null : 'default_disabled',
			riskHistoryOutcomeCodes: [],
			auditEvents: [],
			matchedRuleIds: [],
		};
		for (const rule of this.document.rules) {
			this.ruleEvaluator.apply(rule, context, decision);
		}
		return {
			flagBits: decision.flagBits,
			inviteAutoJoinEnabled: decision.inviteAutoJoinEnabled,
			inviteAutoJoinSkipReason: decision.inviteAutoJoinSkipReason,
			riskHistoryOutcomeCodes: [...decision.riskHistoryOutcomeCodes],
			auditEvents: [...decision.auditEvents],
			matchedRuleIds: [...decision.matchedRuleIds],
		};
	}

	evaluateContact(contact: AccountPolicyContactContext): AccountPolicyContactDecision {
		return this.contactCapabilityEvaluator.evaluate(this.document.contactCapabilityRules, contact);
	}

	isLowRiskEmailTld(tld: string | null | undefined): boolean {
		const normalized = tld ? normalizePolicyTld(tld) : null;
		return normalized ? this.document.classifiers.emailTlds.lowRisk.has(normalized) : false;
	}

	classifyEmailTld(tld: string | null | undefined): AccountPolicyEmailTldRisk | null {
		const normalized = tld ? normalizePolicyTld(tld) : null;
		if (!normalized) return null;
		if (this.document.classifiers.emailTlds.lowRisk.size === 0) return null;
		return this.document.classifiers.emailTlds.lowRisk.has(normalized) ? null : 'high';
	}

	isBlockedRegistrationEmailDomain(domain: string | null | undefined): boolean {
		const tld = extractPolicyTldFromDomain(domain);
		return tld ? this.document.classifiers.emailTlds.blocked.has(tld) : false;
	}

	isTrustedCommercialPrivacyProvider(providerName: string | null | undefined): boolean {
		const tokens = normalizeProviderTokens(providerName);
		return this.document.classifiers.network.trustedPrivacyProviderMarkers.some((marker) =>
			containsTokenRun(tokens, marker),
		);
	}

	isEducationOrganizationName(organizationName: string | null | undefined): boolean {
		const normalized = organizationName ? normalizePolicyToken(organizationName) : '';
		return normalized
			? this.document.classifiers.network.educationOrgMarkers.some((marker) => normalized.includes(marker))
			: false;
	}

	classifyReverseDnsHostname(hostname: string | null | undefined): ReverseDnsClassification {
		const normalized = hostname ? normalizePolicyToken(hostname) : '';
		if (!normalized) return 'unknown';
		for (const classification of REVERSE_DNS_CLASSIFICATION_ORDER) {
			const markers = this.document.classifiers.network.reverseDnsMarkers[classification];
			if (markers.some((marker) => marker.test(normalized))) {
				return classification;
			}
		}
		return 'unknown';
	}

	getMinimumAgeForRegion(countryCode: string | null | undefined, defaultAge: number): number {
		const normalized = countryCode ? normalizePolicyCountryCode(countryCode) : '';
		return normalized ? (this.document.classifiers.regionalMinimumAge.byCountry[normalized] ?? defaultAge) : defaultAge;
	}
}

function parseAccountPolicyDsl(rawDocument: unknown): AccountPolicyDocument {
	return new AccountPolicyDocumentParser().parse(rawDocument);
}

export function createAccountPolicyEvaluator(rawDocument: unknown): AccountPolicyEvaluator {
	return new AccountPolicyEvaluator(parseAccountPolicyDsl(rawDocument));
}

function createDisabledAccountPolicyEvaluator(): AccountPolicyEvaluator {
	return createAccountPolicyEvaluator({version: 1, id: 'disabled', rules: []});
}

export function isAccountPolicyConfigMissing(rawDocument: unknown): boolean {
	return rawDocument === undefined || rawDocument === null || rawDocument === '';
}

export function createAccountPolicyEvaluatorFromConfig(rawDocument: unknown): AccountPolicyEvaluator {
	if (isAccountPolicyConfigMissing(rawDocument)) {
		return createDisabledAccountPolicyEvaluator();
	}
	if (typeof rawDocument === 'string') {
		try {
			return createAccountPolicyEvaluator(JSON.parse(rawDocument));
		} catch (error) {
			throw new Error(
				`FLUXER_ACCOUNT_POLICY_DSL must contain valid JSON: ${error instanceof Error ? error.message : String(error)}`,
			);
		}
	}
	return createAccountPolicyEvaluator(rawDocument);
}

export function isAssessmentThresholdAuditEvent(event: AccountPolicyAuditEvent): boolean {
	return event.event === 'assessment_threshold_notice' && event.level === 'warn';
}
