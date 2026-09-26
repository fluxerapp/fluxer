// SPDX-License-Identifier: AGPL-3.0-or-later

import {AbuseProneEmailTldRisk, classifyAbuseProneEmailTld} from '@app/api/risk/AbuseProneEmailTlds';
import {isMajorEmailProvider} from '@app/api/risk/MajorEmailProviders';
import {derivePlusAddressBase} from '@app/api/risk/PlusAddressUtils';
import type {RiskToolbox} from '@app/api/risk/RiskToolbox';
import {
	type HistoricalOutcomeResult,
	type IpInfoAnonymousResult,
	RecommendedAction,
	type RegistrationEvent,
	type RiskAssessment,
	RiskConfidence,
	RiskDecisionMethod,
	RiskLevel,
	type RiskSignals,
} from '@app/api/risk/RiskTypes';
import {isTrustedCommercialPrivacyProvider} from '@app/api/risk/TrustedPrivacyProviders';

interface DeterministicRiskEngineLogger {
	info(payload: object, msg: string): void;
	warn(payload: object, msg: string): void;
	error(payload: object, msg: string): void;
}

interface DeterministicRiskEngineConfig {
	logger: DeterministicRiskEngineLogger;
}

const NULL_LOGGER: DeterministicRiskEngineLogger = {
	info: () => undefined,
	warn: () => undefined,
	error: () => undefined,
};
const DEFAULT_CONFIG: DeterministicRiskEngineConfig = {
	logger: NULL_LOGGER,
};
const ENGINE_NAME = 'deterministic';

interface ScoreContribution {
	readonly rule: string;
	readonly points: number;
	readonly reason: string;
	readonly flags?: {
		readonly floorsToHigh?: boolean;
	};
}

interface VelocityTier {
	readonly minCount: number;
	readonly points: number;
	readonly severity: string;
}

const THRESHOLD_MEDIUM = 30;
const THRESHOLD_HIGH = 60;
const THRESHOLD_VERY_HIGH = 80;
const RESI_PROXY_PERSISTENCE_HIGH = 70;
const RESI_PROXY_PERSISTENCE_MEDIUM = 30;
const POINTS_RESI_PROXY_HIGH_PERSISTENCE = 45;
const POINTS_RESI_PROXY_MEDIUM_PERSISTENCE = 20;
const POINTS_RESI_PROXY_LOW_PERSISTENCE = 10;
const POINTS_UNTRUSTED_VPN = 25;
const POINTS_RESI_PROXY_HOSTING_CROSSTAG = POINTS_UNTRUSTED_VPN;
const MOBILE_PROXY_DAMPEN_MULTIPLIER = 0.6;
const FREQUENT_ABUSER_WINDOW_HOURS = 180 * 24;
const FREQUENT_ABUSER_MIN_ENFORCED = 5;
const POINTS_FREQUENT_ABUSER_IP = 40;
const POINTS_FREQUENT_ABUSER_SUBNET = 25;
const POINTS_SUSPICIOUS_IP = 35;
const SHARED_CONN_MIN_REGISTRATIONS = 3;
const SHARED_CONN_CAP_POINTS = 8;
const IP_VELOCITY_TIERS: ReadonlyArray<VelocityTier> = [
	{minCount: 5, points: 40, severity: 'high'},
	{minCount: 3, points: 20, severity: 'moderate'},
	{minCount: 1, points: 8, severity: 'low'},
];
const SUBNET_VELOCITY_TIERS: ReadonlyArray<VelocityTier> = [
	{minCount: 15, points: 25, severity: 'high'},
	{minCount: 8, points: 12, severity: 'moderate'},
];
const OUTCOME_WEIGHT_IP = 1.0;
const OUTCOME_WEIGHT_SUBNET = 0.7;
const OUTCOME_MIN_RATIO = 0.05;
const OUTCOME_MIN_ABSOLUTE = 2;
const OUTCOME_STRONG_RATIO = 0.3;
const OUTCOME_STRONG_ABSOLUTE = 3;
const OUTCOME_MODERATE_RATIO = 0.15;
const OUTCOME_MODERATE_ABSOLUTE = 2;
const OUTCOME_POINTS_STRONG = 35;
const OUTCOME_POINTS_MODERATE = 20;
const OUTCOME_POINTS_WEAK = 8;
const ASN_OUTCOME_MIN_ENFORCED = 3;
const ASN_OUTCOME_MIN_RATIO = 0.15;
const ASN_OUTCOME_RATIO_MULTIPLIER = 30;
const ASN_OUTCOME_POINTS_MAX = 20;
const EMAIL_GIBBERISH_THRESHOLD = 70;
const EMAIL_SUSPICIOUS_THRESHOLD = 50;
const POINTS_EMAIL_GIBBERISH = 20;
const POINTS_EMAIL_SUSPICIOUS = 10;
const POINTS_DISPOSABLE_DOMAIN = 25;
const PLUS_ADDRESS_TIERS: ReadonlyArray<VelocityTier> = [
	{minCount: 4, points: 65, severity: 'heavy'},
	{minCount: 3, points: 30, severity: 'elevated'},
];
const POINTS_FRESH_DOMAIN = 15;
const POINTS_FRESH_DOMAIN_WITH_ABUSE_TLD = 20;
const POINTS_ABUSE_PRONE_TLD = 10;
const POINTS_NO_MX = 15;
const DOMAIN_VELOCITY_TIERS: ReadonlyArray<VelocityTier> = [
	{minCount: 5, points: 20, severity: 'high'},
	{minCount: 3, points: 10, severity: 'moderate'},
];
const POINTS_UA_BOT = 40;
const POINTS_UA_HEADLESS = 30;
const POINTS_UA_SUSPICIOUS = 10;
const POINTS_GEO_MISMATCH = 5;
const POINTS_SUSPICIOUS_HOUR = 3;
const CONFIDENCE_MIN_CONTRIBUTIONS_HIGH = 3;
const CONFIDENCE_MIN_CONTRIBUTIONS_MEDIUM = 2;
const CONFIDENCE_LOW_SCORE_MARGIN = 10;
const RULE = {
	ipReputation: 'ip_reputation',
	ipVelocity: 'ip_velocity',
	subnetVelocity: 'subnet_velocity',
	historicalOutcomeIp: 'historical_outcome_ip',
	historicalOutcomeSubnet: 'historical_outcome_subnet',
	historicalOutcomeAsn: 'historical_outcome_asn',
	emailGibberish: 'email_gibberish',
	emailDisposable: 'email_disposable',
	plusAddressReuse: 'plus_address_reuse',
	domainAge: 'domain_age',
	domainTld: 'domain_tld',
	domainMx: 'domain_mx',
	domainVelocity: 'domain_velocity',
	userAgent: 'user_agent',
	geoMismatch: 'geo_locale_mismatch',
	suspiciousHour: 'suspicious_hour',
	policyResidentialProxyFloor: 'policy_residential_proxy_floor',
	frequentAbuserIp: 'frequent_abuser_ip',
	frequentAbuserSubnet: 'frequent_abuser_subnet',
	suspiciousIp: 'suspicious_ip',
	sharedConnectionDampener: 'shared_connection_dampener',
} as const;

function extractEmailDomain(email: string | null | undefined): string | null {
	if (!email) return null;
	const at = email.lastIndexOf('@');
	if (at < 0 || at === email.length - 1) return null;
	return email.slice(at + 1).toLowerCase();
}

function isNicheEmailDomain(domain: string | null): domain is string {
	return domain !== null && !isMajorEmailProvider(domain);
}

function clamp(value: number, min: number, max: number): number {
	if (Number.isNaN(value)) return min;
	return Math.min(Math.max(value, min), max);
}

async function settled<T>(
	promise: Promise<T | null> | null,
	logger: DeterministicRiskEngineLogger,
	source: string,
): Promise<T | undefined> {
	if (promise === null) return undefined;
	try {
		const value = await promise;
		return value ?? undefined;
	} catch (err) {
		logger.warn(
			{source, error: err instanceof Error ? err.message : String(err)},
			'DeterministicRiskEngine: signal lookup failed',
		);
		return undefined;
	}
}

function scoreByTiers(
	count: number,
	tiers: ReadonlyArray<VelocityTier>,
	rule: string,
	formatReason: (count: number, severity: string) => string,
): ScoreContribution | null {
	for (const tier of tiers) {
		if (count >= tier.minCount) {
			return {
				rule,
				points: tier.points,
				reason: formatReason(count, tier.severity),
			};
		}
	}
	return null;
}

function scoreIpReputation(signals: RiskSignals): Array<ScoreContribution> {
	const ipInfo = signals.ipInfoAnonymous;
	if (!ipInfo?.available) return [];
	const providerName = ipInfo.providerName ?? null;
	const asnType = ipInfo.asnType?.toLowerCase() ?? null;
	const isHostingAsn = ipInfo.isHosting || asnType === 'hosting';
	const isTrustedVpn = isTrustedCommercialPrivacyProvider(providerName);
	const isMobileService = /_mobile$/i.test(providerName ?? '') || asnType === 'mobile';
	if (isTrustedVpn) {
		return [
			{
				rule: RULE.ipReputation,
				points: 0,
				reason: `trusted commercial VPN (provider: ${providerName})`,
			},
		];
	}
	if (ipInfo.isResidentialProxy) {
		if (isHostingAsn) {
			return [
				{
					rule: RULE.ipReputation,
					points: POINTS_RESI_PROXY_HOSTING_CROSSTAG,
					reason: `residential-proxy cross-tag on hosting ASN — treating as untrusted commercial VPN (provider: ${providerName ?? 'unknown'})`,
					flags: {floorsToHigh: true},
				},
			];
		}
		const persistence = ipInfo.percentDaysSeen;
		const provLabel = providerName ?? 'unknown';
		if (persistence == null) {
			let points = POINTS_RESI_PROXY_MEDIUM_PERSISTENCE;
			if (isMobileService) points = Math.round(points * MOBILE_PROXY_DAMPEN_MULTIPLIER);
			return [
				{
					rule: RULE.ipReputation,
					points,
					reason: `residential proxy (provider: ${provLabel}, persistence: unknown${isMobileService ? ', mobile gateway' : ''})`,
					flags: {floorsToHigh: true},
				},
			];
		}
		if (persistence >= RESI_PROXY_PERSISTENCE_HIGH) {
			let points = POINTS_RESI_PROXY_HIGH_PERSISTENCE;
			if (isMobileService) points = Math.round(points * MOBILE_PROXY_DAMPEN_MULTIPLIER);
			return [
				{
					rule: RULE.ipReputation,
					points,
					reason: `persistent residential proxy (provider: ${provLabel}, persistence: ${persistence}%${isMobileService ? ', mobile gateway' : ''})`,
					flags: {floorsToHigh: true},
				},
			];
		}
		if (persistence >= RESI_PROXY_PERSISTENCE_MEDIUM) {
			let points = POINTS_RESI_PROXY_MEDIUM_PERSISTENCE;
			if (isMobileService) points = Math.round(points * MOBILE_PROXY_DAMPEN_MULTIPLIER);
			return [
				{
					rule: RULE.ipReputation,
					points,
					reason: `intermittent residential proxy (provider: ${provLabel}, persistence: ${persistence}%${isMobileService ? ', mobile gateway' : ''})`,
					flags: {floorsToHigh: true},
				},
			];
		}
		let points = POINTS_RESI_PROXY_LOW_PERSISTENCE;
		if (isMobileService) points = Math.round(points * MOBILE_PROXY_DAMPEN_MULTIPLIER);
		return [
			{
				rule: RULE.ipReputation,
				points,
				reason: `transient residential proxy sighting (provider: ${provLabel}, persistence: ${persistence}%${isMobileService ? ', mobile gateway' : ''})`,
				flags: {floorsToHigh: true},
			},
		];
	}
	const isAnonymizingNetwork = ipInfo.isVpn || ipInfo.isProxy || isHostingAsn || ipInfo.isHosting;
	if (isAnonymizingNetwork) {
		const label = ipInfo.isVpn || ipInfo.isProxy ? 'untrusted VPN/proxy' : 'hosting/datacenter network';
		return [
			{
				rule: RULE.ipReputation,
				points: POINTS_UNTRUSTED_VPN,
				reason: `${label} detected by IPinfo (provider: ${providerName ?? 'unknown'}, connection_type: ${ipInfo.connectionType})`,
			},
		];
	}
	return [];
}

function isTrustedCommercialPrivacySignal(ipInfo: IpInfoAnonymousResult | undefined): boolean {
	if (!ipInfo?.available) return false;
	if (!ipInfo.isAnonymous && !ipInfo.isVpn && !ipInfo.isProxy && !ipInfo.isRelay) return false;
	return isTrustedCommercialPrivacyProvider(ipInfo.providerName);
}

function isHighSharedAccessSignal(ipInfo: IpInfoAnonymousResult | undefined): boolean {
	if (!ipInfo?.available || ipInfo.isAnonymous || ipInfo.isHosting) return false;
	const asnType = ipInfo.asnType?.trim().toLowerCase() ?? null;
	return ipInfo.isMobile || ipInfo.connectionType === 'mobile' || asnType === 'mobile';
}

function scoreSuspiciousIp(signals: RiskSignals): Array<ScoreContribution> {
	const suspiciousIp = signals.suspiciousIp;
	if (!suspiciousIp) return [];
	const ipInfo = signals.ipInfoAnonymous;
	if (isHighSharedAccessSignal(ipInfo)) {
		return [
			{
				rule: RULE.suspiciousIp,
				points: 0,
				reason: `suspicious IP marker ignored for shared mobile/CGNAT network: ${suspiciousIp.ip}`,
			},
		];
	}
	return [
		{
			rule: RULE.suspiciousIp,
			points: POINTS_SUSPICIOUS_IP,
			reason: `suspicious IP marker from ${suspiciousIp.source}: ${suspiciousIp.reason}`,
		},
	];
}

function scoreIpVelocity(signals: RiskSignals): Array<ScoreContribution> {
	const contributions: Array<ScoreContribution> = [];
	const byIp = signals.registrationsByIp;
	if (byIp && byIp.totalRegistrations > 0) {
		const c = scoreByTiers(
			byIp.totalRegistrations,
			IP_VELOCITY_TIERS,
			RULE.ipVelocity,
			(n, sev) => `${sev} IP velocity: ${n} registrations in ${byIp.windowHours}h`,
		);
		if (c) contributions.push(c);
	}
	const bySub = signals.registrationsBySubnet;
	if (bySub) {
		const c = scoreByTiers(
			bySub.totalRegistrations,
			SUBNET_VELOCITY_TIERS,
			RULE.subnetVelocity,
			(n, sev) => `${sev} subnet velocity: ${n} registrations in ${bySub.windowHours}h`,
		);
		if (c) contributions.push(c);
	}
	return contributions;
}

function scoreHistoricalOutcomes(signals: RiskSignals): Array<ScoreContribution> {
	const contributions: Array<ScoreContribution> = [];
	const perSource: Array<ScoreContribution | null> = [
		scoreOutcomeRatio(signals.historicalOutcomesByIp, RULE.historicalOutcomeIp, 'IP', OUTCOME_WEIGHT_IP),
		scoreOutcomeRatio(
			signals.historicalOutcomesBySubnet,
			RULE.historicalOutcomeSubnet,
			'subnet',
			OUTCOME_WEIGHT_SUBNET,
		),
	];
	for (const c of perSource) {
		if (c) contributions.push(c);
	}
	const asn = signals.historicalOutcomesByAsn;
	if (asn && asn.enforcedUsers >= ASN_OUTCOME_MIN_ENFORCED && asn.sampledUsers > 0) {
		const ratio = asn.enforcedUsers / asn.sampledUsers;
		if (ratio > ASN_OUTCOME_MIN_RATIO) {
			const points = Math.min(Math.round(ratio * ASN_OUTCOME_RATIO_MULTIPLIER), ASN_OUTCOME_POINTS_MAX);
			contributions.push({
				rule: RULE.historicalOutcomeAsn,
				points,
				reason: `ASN enforcement ratio: ${asn.enforcedUsers}/${asn.sampledUsers} enforced (${(ratio * 100).toFixed(0)}%)`,
			});
		}
	}
	return contributions;
}

function scoreOutcomeRatio(
	outcome: HistoricalOutcomeResult | undefined,
	rule: string,
	label: string,
	weight: number,
): ScoreContribution | null {
	if (!outcome || outcome.sampledUsers === 0) return null;
	const enforced = outcome.enforcedUsers + outcome.disabledUsers + outcome.spammerUsers;
	if (enforced === 0) return null;
	const ratio = enforced / outcome.sampledUsers;
	if (ratio < OUTCOME_MIN_RATIO && enforced < OUTCOME_MIN_ABSOLUTE) return null;
	const pct = (ratio * 100).toFixed(0);
	let rawPoints: number;
	let reason: string;
	if (enforced >= OUTCOME_STRONG_ABSOLUTE && ratio > OUTCOME_STRONG_RATIO) {
		rawPoints = OUTCOME_POINTS_STRONG;
		reason = `high ${label} enforcement: ${enforced}/${outcome.sampledUsers} (${pct}%)`;
	} else if (enforced >= OUTCOME_MODERATE_ABSOLUTE || ratio > OUTCOME_MODERATE_RATIO) {
		rawPoints = OUTCOME_POINTS_MODERATE;
		reason = `moderate ${label} enforcement: ${enforced}/${outcome.sampledUsers} (${pct}%)`;
	} else {
		rawPoints = OUTCOME_POINTS_WEAK;
		reason = `some ${label} enforcement: ${enforced}/${outcome.sampledUsers}`;
	}
	return {rule, points: Math.round(rawPoints * weight), reason};
}

function scoreEmail(event: RegistrationEvent, signals: RiskSignals): Array<ScoreContribution> {
	if (!event.email) return [];
	const contributions: Array<ScoreContribution> = [];
	const syntax = signals.emailSyntax;
	if (syntax) {
		if (syntax.gibberishScore >= EMAIL_GIBBERISH_THRESHOLD) {
			contributions.push({
				rule: RULE.emailGibberish,
				points: POINTS_EMAIL_GIBBERISH,
				reason: `gibberish email local-part (score: ${syntax.gibberishScore})`,
			});
		} else if (syntax.gibberishScore >= EMAIL_SUSPICIOUS_THRESHOLD) {
			contributions.push({
				rule: RULE.emailGibberish,
				points: POINTS_EMAIL_SUSPICIOUS,
				reason: `suspicious email local-part (score: ${syntax.gibberishScore})`,
			});
		}
	}
	if (signals.domainDisposable?.isDisposable) {
		contributions.push({
			rule: RULE.emailDisposable,
			points: POINTS_DISPOSABLE_DOMAIN,
			reason: `disposable email domain: ${signals.domainDisposable.domain}`,
		});
	}
	const plus = signals.registrationsByPlusAddressBase;
	if (plus) {
		const c = scoreByTiers(
			plus.uniqueEmails,
			PLUS_ADDRESS_TIERS,
			RULE.plusAddressReuse,
			(n, sev) => `${sev} plus-address reuse: ${n} distinct aliases for ${plus.identifier} in ${plus.windowHours}h`,
		);
		if (c) contributions.push(c);
	}
	return contributions;
}

function scoreEmailDomain(signals: RiskSignals, domain: string): Array<ScoreContribution> {
	const contributions: Array<ScoreContribution> = [];
	const tld = domain.split('.').at(-1) ?? '';
	const tldRisk = classifyAbuseProneEmailTld(tld);
	if (signals.domainAge?.isNewlyRegistered) {
		contributions.push({
			rule: RULE.domainAge,
			points: POINTS_FRESH_DOMAIN,
			reason: `newly registered email domain (${signals.domainAge.ageDays} days old)`,
		});
		if (tldRisk === AbuseProneEmailTldRisk.High) {
			contributions.push({
				rule: RULE.domainTld,
				points: POINTS_FRESH_DOMAIN_WITH_ABUSE_TLD,
				reason: `abuse-prone TLD (.${tld}) with fresh domain`,
			});
		}
	} else if (tldRisk === AbuseProneEmailTldRisk.High) {
		contributions.push({
			rule: RULE.domainTld,
			points: POINTS_ABUSE_PRONE_TLD,
			reason: `non-allowlisted TLD (.${tld})`,
		});
	}
	if (signals.domainMx && !signals.domainMx.hasMx) {
		contributions.push({
			rule: RULE.domainMx,
			points: POINTS_NO_MX,
			reason: `no MX records for domain ${domain}`,
		});
	}
	const vel = signals.registrationsByEmailDomain;
	if (vel) {
		const c = scoreByTiers(
			vel.totalRegistrations,
			DOMAIN_VELOCITY_TIERS,
			RULE.domainVelocity,
			(n) => `email domain velocity: ${n} registrations in ${vel.windowHours}h`,
		);
		if (c) contributions.push(c);
	}
	return contributions;
}

function scoreUserAgent(signals: RiskSignals): Array<ScoreContribution> {
	const ua = signals.userAgent;
	if (!ua) return [];
	if (ua.isBot || ua.isAutomation) {
		return [
			{
				rule: RULE.userAgent,
				points: POINTS_UA_BOT,
				reason: `bot/automation UA detected: ${ua.riskNote}`,
			},
		];
	}
	if (ua.isHeadless) {
		return [
			{
				rule: RULE.userAgent,
				points: POINTS_UA_HEADLESS,
				reason: `headless browser UA: ${ua.riskNote}`,
			},
		];
	}
	if (ua.suspiciousPatterns.length > 0) {
		return [
			{
				rule: RULE.userAgent,
				points: POINTS_UA_SUSPICIOUS,
				reason: `suspicious UA patterns: ${ua.suspiciousPatterns.join(', ')}`,
			},
		];
	}
	return [];
}

function scoreGeoLocale(signals: RiskSignals): Array<ScoreContribution> {
	const geo = signals.localeGeoMatch;
	if (!geo?.mismatchDetected) return [];
	return [
		{
			rule: RULE.geoMismatch,
			points: POINTS_GEO_MISMATCH,
			reason: `geo-locale mismatch: ${geo.notes.join(', ')}`,
		},
	];
}

function scoreFrequentAbuser(signals: RiskSignals): Array<ScoreContribution> {
	const contributions: Array<ScoreContribution> = [];
	const byIp = signals.frequentAbuserByIp;
	if (byIp) {
		const enforced = byIp.enforcedUsers + byIp.disabledUsers + byIp.spammerUsers;
		if (enforced >= FREQUENT_ABUSER_MIN_ENFORCED) {
			contributions.push({
				rule: RULE.frequentAbuserIp,
				points: POINTS_FREQUENT_ABUSER_IP,
				reason: `frequent abuser IP: ${enforced} enforcements in the last ${byIp.windowHours / 24}d`,
			});
		}
	}
	const bySubnet = signals.frequentAbuserBySubnet;
	if (bySubnet) {
		const enforced = bySubnet.enforcedUsers + bySubnet.disabledUsers + bySubnet.spammerUsers;
		if (enforced >= FREQUENT_ABUSER_MIN_ENFORCED) {
			contributions.push({
				rule: RULE.frequentAbuserSubnet,
				points: POINTS_FREQUENT_ABUSER_SUBNET,
				reason: `frequent abuser subnet: ${enforced} enforcements in the last ${bySubnet.windowHours / 24}d`,
			});
		}
	}
	return contributions;
}

function applySharedConnectionDampener(
	contributions: Array<ScoreContribution>,
	signals: RiskSignals,
): Array<ScoreContribution> {
	const byIp = signals.registrationsByIp;
	if (!byIp || byIp.totalRegistrations < SHARED_CONN_MIN_REGISTRATIONS) {
		return contributions;
	}
	const shortOutcome = signals.historicalOutcomesByIp;
	const longOutcome = signals.frequentAbuserByIp;
	const hasShortEnforcement =
		shortOutcome != null && shortOutcome.enforcedUsers + shortOutcome.disabledUsers + shortOutcome.spammerUsers > 0;
	const hasLongEnforcement =
		longOutcome != null && longOutcome.enforcedUsers + longOutcome.disabledUsers + longOutcome.spammerUsers > 0;
	if (hasShortEnforcement || hasLongEnforcement) return contributions;
	const ipInfo = signals.ipInfoAnonymous;
	const isCommercialAnonymizer = ipInfo?.isVpn || ipInfo?.isProxy || ipInfo?.isHosting;
	if (isCommercialAnonymizer) return contributions;
	const connectionType = ipInfo?.connectionType ?? null;
	const isSharedCandidate =
		connectionType === 'residential' ||
		connectionType === 'mobile' ||
		connectionType === 'education' ||
		(connectionType == null && signals.reverseDns?.classification === 'dynamic');
	if (!isSharedCandidate) return contributions;
	const rdnsClass = signals.reverseDns?.classification;
	if (rdnsClass === 'static' || rdnsClass === 'business') {
		return contributions;
	}
	const ipVelocityContrib = contributions.find((c) => c.rule === RULE.ipVelocity);
	if (!ipVelocityContrib || ipVelocityContrib.points <= SHARED_CONN_CAP_POINTS) {
		return contributions;
	}
	const delta = ipVelocityContrib.points - SHARED_CONN_CAP_POINTS;
	const next = contributions.filter((c) => c !== ipVelocityContrib);
	next.push({
		rule: RULE.ipVelocity,
		points: SHARED_CONN_CAP_POINTS,
		reason: `${ipVelocityContrib.reason} (capped — shared ${connectionType ?? 'residential'} connection)`,
	});
	next.push({
		rule: RULE.sharedConnectionDampener,
		points: -delta,
		reason: `shared ${connectionType ?? 'residential'} connection detected — IP velocity dampened by ${delta} points`,
	});
	return next;
}

function scoreTiming(signals: RiskSignals): Array<ScoreContribution> {
	const timing = signals.registrationTiming;
	if (!timing?.isSuspiciousHour) return [];
	return [
		{
			rule: RULE.suspiciousHour,
			points: POINTS_SUSPICIOUS_HOUR,
			reason: `registration at suspicious local hour (${timing.localHour})`,
		},
	];
}

function collectContributions(event: RegistrationEvent, signals: RiskSignals): Array<ScoreContribution> {
	let contributions: Array<ScoreContribution> = [];
	const trustedCommercialPrivacy = isTrustedCommercialPrivacySignal(signals.ipInfoAnonymous);
	contributions.push(...scoreIpReputation(signals));
	if (!trustedCommercialPrivacy) {
		contributions.push(...scoreSuspiciousIp(signals));
		contributions.push(...scoreIpVelocity(signals));
		contributions.push(...scoreHistoricalOutcomes(signals));
		contributions.push(...scoreFrequentAbuser(signals));
	}
	if (!event.isUnclaimed) {
		contributions.push(...scoreEmail(event, signals));
		const emailDomain = extractEmailDomain(event.email);
		if (isNicheEmailDomain(emailDomain)) {
			contributions.push(...scoreEmailDomain(signals, emailDomain));
		}
	}
	contributions.push(...scoreUserAgent(signals));
	if (!trustedCommercialPrivacy) {
		contributions.push(...scoreGeoLocale(signals));
	}
	contributions.push(...scoreTiming(signals));
	if (!trustedCommercialPrivacy) {
		contributions = applySharedConnectionDampener(contributions, signals);
	}
	return contributions;
}

function scoreToLevel(score: number): RiskLevel {
	if (score >= THRESHOLD_VERY_HIGH) return RiskLevel.VeryHigh;
	if (score >= THRESHOLD_HIGH) return RiskLevel.High;
	if (score >= THRESHOLD_MEDIUM) return RiskLevel.Medium;
	return RiskLevel.Low;
}

function levelToAction(level: RiskLevel): RecommendedAction {
	switch (level) {
		case RiskLevel.VeryHigh:
			return RecommendedAction.RequireInboundPhone;
		case RiskLevel.High:
			return RecommendedAction.RequireOutboundPhone;
		case RiskLevel.Medium:
			return RecommendedAction.RequireVerifiedEmail;
		case RiskLevel.Low:
			return RecommendedAction.Allow;
	}
}

function deriveConfidence(score: number, contributionCount: number): RiskConfidence {
	if (contributionCount === 0) {
		return RiskConfidence.High;
	}
	if (
		contributionCount >= CONFIDENCE_MIN_CONTRIBUTIONS_HIGH &&
		(score >= THRESHOLD_HIGH || score < THRESHOLD_MEDIUM - CONFIDENCE_LOW_SCORE_MARGIN)
	) {
		return RiskConfidence.High;
	}
	if (contributionCount >= CONFIDENCE_MIN_CONTRIBUTIONS_MEDIUM) return RiskConfidence.Medium;
	return RiskConfidence.Low;
}

export class DeterministicRiskEngine {
	private readonly toolbox: RiskToolbox;
	private readonly config: DeterministicRiskEngineConfig;

	constructor(toolbox: RiskToolbox, configOverrides: Partial<DeterministicRiskEngineConfig> = {}) {
		this.toolbox = toolbox;
		this.config = {...DEFAULT_CONFIG, ...configOverrides};
	}

	async classify(event: RegistrationEvent): Promise<RiskAssessment> {
		const startedAt = Date.now();
		const signals = await this.collectSignals(event);
		const contributions = collectContributions(event, signals);
		let rawScore = 0;
		for (const c of contributions) rawScore += c.points;
		const score = clamp(Math.round(rawScore), 0, 100);
		let level = scoreToLevel(score);
		const floorTrigger = contributions.find((c) => c.flags?.floorsToHigh === true);
		if (floorTrigger && (level === RiskLevel.Low || level === RiskLevel.Medium)) {
			level = RiskLevel.High;
			contributions.push({
				rule: RULE.policyResidentialProxyFloor,
				points: 0,
				reason: `policy: ${floorTrigger.reason} — floor raised to High`,
			});
		}
		const action = levelToAction(level);
		const elapsedMs = Date.now() - startedAt;
		const reasoning = contributions.map((c) => c.reason).join('; ');
		this.config.logger.info(
			{
				ip: event.ip,
				email: event.email,
				score,
				level,
				elapsedMs,
				contributionCount: contributions.length,
				contributions: contributions.map((c) => ({rule: c.rule, points: c.points})),
			},
			'DeterministicRiskEngine: classification complete',
		);
		return {
			suspicious: level !== RiskLevel.Low,
			level,
			confidence: deriveConfidence(score, contributions.length),
			riskScore: score,
			reasoning,
			recommendedAction: action,
			method: RiskDecisionMethod.Deterministic,
			modelUsed: ENGINE_NAME,
			rounds: 0,
			elapsedMs,
			signals,
		};
	}

	private async collectSignals(event: RegistrationEvent): Promise<RiskSignals> {
		const {logger} = this.config;
		const toolbox = this.toolbox;
		const ip = event.ip;
		const emailDomain = extractEmailDomain(event.email);
		const nicheDomain: string | null = isNicheEmailDomain(emailDomain) ? emailDomain : null;
		const plusAddressBase = derivePlusAddressBase(event.email);
		const geoIpAsnP = settled(toolbox.lookupGeoIpAsn({ip}), logger, 'geoIpAsn');
		const geoIpCityP = settled(toolbox.lookupGeoIpCity({ip}), logger, 'geoIpCity');
		const ipInfoP = settled(toolbox.lookupIpInfo({ip}), logger, 'ipInfoAnonymous');
		const userAgentP = settled(
			event.userAgent ? toolbox.analyzeUserAgent({userAgent: event.userAgent}) : null,
			logger,
			'userAgent',
		);
		const emailSyntaxP = settled(
			event.email ? toolbox.analyzeEmailSyntax({email: event.email}) : null,
			logger,
			'emailSyntax',
		);
		const domainDisposableP = settled(
			nicheDomain ? toolbox.checkDomainDisposable({domain: nicheDomain}) : null,
			logger,
			'domainDisposable',
		);
		const registrationsByIpP = settled(
			toolbox.getRegistrationsByIp({ip, windowHours: 24}),
			logger,
			'registrationsByIp',
		);
		const suspiciousIpP = settled(toolbox.getSuspiciousIp({ip}), logger, 'suspiciousIp');
		const registrationsBySubnetP = settled(
			toolbox.getRegistrationsBySubnet({ip, windowHours: 24}),
			logger,
			'registrationsBySubnet',
		);
		const historicalOutcomesByIpP = settled(
			toolbox.getHistoricalOutcomesByIp({ip, windowHours: 24}),
			logger,
			'historicalOutcomesByIp',
		);
		const historicalOutcomesBySubnetP = settled(
			toolbox.getHistoricalOutcomesBySubnet({ip, windowHours: 24}),
			logger,
			'historicalOutcomesBySubnet',
		);
		const frequentAbuserByIpP = settled(
			toolbox.getHistoricalOutcomesByIp({ip, windowHours: FREQUENT_ABUSER_WINDOW_HOURS}),
			logger,
			'frequentAbuserByIp',
		);
		const frequentAbuserBySubnetP = settled(
			toolbox.getHistoricalOutcomesBySubnet({ip, windowHours: FREQUENT_ABUSER_WINDOW_HOURS}),
			logger,
			'frequentAbuserBySubnet',
		);
		const reverseDnsP = settled(toolbox.lookupReverseDns({ip}), logger, 'reverseDns');
		const registrationsByEmailDomainP = settled(
			nicheDomain ? toolbox.getRegistrationsByEmailDomain({domain: nicheDomain, windowHours: 168}) : null,
			logger,
			'registrationsByEmailDomain',
		);
		const registrationsByPlusAddressBaseP = settled(
			plusAddressBase ? toolbox.getRegistrationsByPlusAddressBase({plusAddressBase, windowHours: 720}) : null,
			logger,
			'registrationsByPlusAddressBase',
		);
		const historicalOutcomesByEmailDomainP = settled(
			nicheDomain ? toolbox.getHistoricalOutcomesByEmailDomain({domain: nicheDomain, windowHours: 168}) : null,
			logger,
			'historicalOutcomesByEmailDomain',
		);
		const domainMxP = settled(nicheDomain ? toolbox.checkMx({domain: nicheDomain}) : null, logger, 'domainMx');
		const domainAgeP = settled(nicheDomain ? toolbox.checkDomainAge({domain: nicheDomain}) : null, logger, 'domainAge');
		const registrationTimingP = settled(
			event.timezone ? toolbox.analyzeRegistrationTiming({timezone: event.timezone}) : null,
			logger,
			'registrationTiming',
		);
		const historicalOutcomesByAsnP = geoIpAsnP.then((asn) =>
			settled(
				asn?.asn != null ? toolbox.getHistoricalOutcomesByAsn({asn: asn.asn, windowHours: 24}) : null,
				logger,
				'historicalOutcomesByAsn',
			),
		);
		const localeGeoMatchP = geoIpCityP.then((city) =>
			settled(
				city?.countryIso
					? toolbox.checkGeoVsLocale({
							geoipCountryIso: city.countryIso,
							registrationLocale: event.locale,
							registrationTimezone: event.timezone,
						})
					: null,
				logger,
				'localeGeoMatch',
			),
		);
		return {
			geoIpAsn: await geoIpAsnP,
			geoIpCity: await geoIpCityP,
			ipInfoAnonymous: await ipInfoP,
			userAgent: await userAgentP,
			emailSyntax: await emailSyntaxP,
			domainDisposable: await domainDisposableP,
			suspiciousIp: await suspiciousIpP,
			registrationsByIp: await registrationsByIpP,
			registrationsBySubnet: await registrationsBySubnetP,
			historicalOutcomesByIp: await historicalOutcomesByIpP,
			historicalOutcomesBySubnet: await historicalOutcomesBySubnetP,
			registrationsByEmailDomain: await registrationsByEmailDomainP,
			registrationsByPlusAddressBase: await registrationsByPlusAddressBaseP,
			historicalOutcomesByEmailDomain: await historicalOutcomesByEmailDomainP,
			domainMx: await domainMxP,
			domainAge: await domainAgeP,
			registrationTiming: await registrationTimingP,
			historicalOutcomesByAsn: await historicalOutcomesByAsnP,
			localeGeoMatch: await localeGeoMatchP,
			frequentAbuserByIp: await frequentAbuserByIpP,
			frequentAbuserBySubnet: await frequentAbuserBySubnetP,
			reverseDns: await reverseDnsP,
		};
	}
}
