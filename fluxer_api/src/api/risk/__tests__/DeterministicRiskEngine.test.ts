// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, it} from 'vitest';
import {createCurrentBehaviorTestAccountPolicyEvaluator} from '../../test/AccountPolicyTestEvaluator';
import {setInjectedAccountPolicyEvaluator} from '../AccountPolicyService';
import {DeterministicRiskEngine} from '../DeterministicRiskEngine';
import type {RiskToolbox} from '../RiskToolbox';
import {type IpInfoAnonymousResult, RecommendedAction, type RegistrationEvent, RiskLevel} from '../RiskTypes';

const BASE_EVENT: RegistrationEvent = {
	email: null,
	ip: '203.0.113.5',
	locale: null,
	timezone: null,
	userAgent: null,
	isUnclaimed: true,
};

beforeEach(() => {
	setInjectedAccountPolicyEvaluator(createCurrentBehaviorTestAccountPolicyEvaluator());
});

afterEach(() => {
	setInjectedAccountPolicyEvaluator(undefined);
});

function createToolbox(
	overrides: {
		analyzeEmailSyntax?: RiskToolbox['analyzeEmailSyntax'];
		lookupIpInfo?: RiskToolbox['lookupIpInfo'];
		getSuspiciousIp?: RiskToolbox['getSuspiciousIp'];
		getRegistrationsByPlusAddressBase?: RiskToolbox['getRegistrationsByPlusAddressBase'];
	} = {},
): RiskToolbox {
	return {
		analyzeEmailSyntax:
			overrides.analyzeEmailSyntax ??
			(async ({email}) => ({
				email,
				localPart: '',
				domain: '',
				localPartLength: 0,
				entropy: 0,
				keyboardMashDetected: false,
				looksLikeName: true,
				pronounceability: 1,
				unusualRepeats: false,
				digitRatio: 0,
				hasDots: false,
				hasPlusTag: false,
				gibberishScore: 0,
				valid: true,
			})),
		checkDomainDisposable: async ({domain}) => ({
			domain,
			isDisposable: false,
		}),
		checkMx: async ({domain}) => ({
			domain,
			hasMx: true,
			recordCount: 1,
			records: [],
			error: null,
		}),
		checkDomainAge: async ({domain}) => ({
			domain,
			available: true,
			creationDate: null,
			ageDays: 365,
			isNewlyRegistered: false,
			riskNote: 'ok',
		}),
		lookupGeoIpCity: async ({ip}) => ({
			ip,
			available: true,
			found: true,
			countryIso: 'US',
			country: 'United States',
			region: null,
			city: null,
			latitude: null,
			longitude: null,
			accuracyRadiusKm: null,
			timeZone: null,
		}),
		lookupGeoIpAsn: async ({ip}) => ({
			ip,
			available: true,
			found: true,
			asn: 64512,
			asnOrg: 'Example Hosting',
		}),
		lookupIpInfo: overrides.lookupIpInfo ?? (async ({ip}) => baseIpInfo(ip)),
		lookupReverseDns: async ({ip}) => ({ip, hostname: null, classification: 'unknown'}),
		getSuspiciousIp: overrides.getSuspiciousIp ?? (async () => null),
		getRegistrationsByIp: async ({ip, windowHours}) => baseVelocity(ip, windowHours),
		getRegistrationsBySubnet: async ({ip, windowHours}) => baseVelocity(ip, windowHours),
		getRegistrationsByEmailDomain: async ({domain, windowHours}) => baseVelocity(domain, windowHours),
		getRegistrationsByPlusAddressBase:
			overrides.getRegistrationsByPlusAddressBase ??
			(async ({plusAddressBase, windowHours}) => baseVelocity(plusAddressBase, windowHours)),
		getHistoricalOutcomesByIp: async ({ip, windowHours}) => baseHistoricalOutcome(ip, windowHours),
		getHistoricalOutcomesBySubnet: async ({ip, windowHours}) => baseHistoricalOutcome(ip, windowHours),
		getHistoricalOutcomesByEmailDomain: async ({domain, windowHours}) => baseHistoricalOutcome(domain, windowHours),
		getHistoricalOutcomesByAsn: async ({asn, windowHours}) => baseHistoricalOutcome(String(asn), windowHours),
		checkGeoVsLocale: async ({geoipCountryIso, registrationLocale, registrationTimezone}) => ({
			geoipCountryIso,
			registrationLocale,
			registrationTimezone,
			localeGeoMatch: null,
			timezoneGeoMatch: null,
			mismatchDetected: false,
			notes: [],
		}),
		analyzeUserAgent: async ({userAgent}) => ({
			raw: userAgent,
			browser: 'Unknown',
			browserVersion: null,
			os: 'Unknown',
			isBot: false,
			isHeadless: false,
			isAutomation: false,
			isOutdated: false,
			suspiciousPatterns: [],
			riskNote: 'ok',
		}),
		analyzeRegistrationTiming: async ({timezone}) => ({
			timezone,
			localHour: 12,
			isSuspiciousHour: false,
			riskNote: 'ok',
		}),
	};
}

function baseIpInfo(ip: string, overrides: Partial<IpInfoAnonymousResult> = {}): IpInfoAnonymousResult {
	return {
		ip,
		available: true,
		isAnonymous: false,
		providerName: null,
		isVpn: false,
		isProxy: false,
		isResidentialProxy: false,
		isTor: false,
		isRelay: false,
		isHosting: false,
		isMobile: false,
		asnType: null,
		asnOrg: null,
		connectionType: 'residential',
		percentDaysSeen: null,
		riskNote: 'ok',
		...overrides,
	};
}

function baseVelocity(identifier: string, windowHours: number) {
	return {
		identifier,
		windowHours,
		totalRegistrations: 0,
		truncated: false,
		uniqueEmails: 0,
		uniqueLocales: [],
		uniqueIps: 0,
		riskNote: 'none',
	};
}

function baseHistoricalOutcome(identifier: string, windowHours: number) {
	return {
		identifier,
		windowHours,
		sampledRegistrations: 0,
		truncated: false,
		sampledUsers: 0,
		resolvedUsers: 0,
		adverseUsers: 0,
		challengedUsers: 0,
		enforcedUsers: 0,
		spammerUsers: 0,
		disabledUsers: 0,
		disabledSuspiciousUsers: 0,
		riskNote: 'none',
	};
}

describe('DeterministicRiskEngine', () => {
	it('allows configured privacy providers without adding hosting risk', async () => {
		const engine = new DeterministicRiskEngine(
			createToolbox({
				lookupIpInfo: async ({ip}) =>
					baseIpInfo(ip, {
						isAnonymous: true,
						isVpn: true,
						providerName: 'Example Privacy Relay LLC',
					}),
			}),
		);
		const result = await engine.classify(BASE_EVENT);
		expect(result.level).toBe(RiskLevel.Low);
		expect(result.riskScore).toBe(0);
		expect(result.recommendedAction).toBe(RecommendedAction.Allow);
	});
	it('treats unknown VPN providers as risky', async () => {
		const engine = new DeterministicRiskEngine(
			createToolbox({
				lookupIpInfo: async ({ip}) =>
					baseIpInfo(ip, {
						isAnonymous: true,
						isVpn: true,
						providerName: 'Unknown VPN LLC',
					}),
			}),
		);
		const result = await engine.classify(BASE_EVENT);
		expect(result.riskScore).toBeGreaterThan(0);
		expect(result.reasoning).toContain('untrusted VPN/proxy detected by IPinfo');
	});
	it('scores suspicious IP markers', async () => {
		const engine = new DeterministicRiskEngine(
			createToolbox({
				getSuspiciousIp: async ({ip}) => ({
					ip,
					source: 'scheduled_deletion',
					reason: 'account_scheduled_for_deletion',
					sourceUserId: '123',
					deletionReasonCode: 3,
					createdAt: new Date().toISOString(),
					expiresAt: null,
					riskNote: 'test',
				}),
			}),
		);
		const result = await engine.classify(BASE_EVENT);
		expect(result.level).toBe(RiskLevel.Medium);
		expect(result.recommendedAction).toBe(RecommendedAction.RequireVerifiedEmail);
		expect(result.reasoning).toContain('suspicious IP marker from scheduled_deletion');
	});
	it('ignores suspicious IP markers for trusted paid VPN providers', async () => {
		const engine = new DeterministicRiskEngine(
			createToolbox({
				lookupIpInfo: async ({ip}) =>
					baseIpInfo(ip, {
						isAnonymous: true,
						isVpn: true,
						providerName: 'Example Privacy Relay LLC',
					}),
				getSuspiciousIp: async ({ip}) => ({
					ip,
					source: 'scheduled_deletion',
					reason: 'account_scheduled_for_deletion',
					sourceUserId: '123',
					deletionReasonCode: 3,
					createdAt: new Date().toISOString(),
					expiresAt: null,
					riskNote: 'test',
				}),
			}),
		);
		const result = await engine.classify(BASE_EVENT);
		expect(result.level).toBe(RiskLevel.Low);
		expect(result.riskScore).toBe(0);
		expect(result.reasoning).toContain('trusted commercial VPN');
	});
	it('allows privacy relays without adding hosting risk', async () => {
		const engine = new DeterministicRiskEngine(
			createToolbox({
				lookupIpInfo: async ({ip}) =>
					baseIpInfo(ip, {
						isAnonymous: true,
						isRelay: true,
						providerName: 'Example Privacy Relay LLC',
					}),
			}),
		);
		const result = await engine.classify(BASE_EVENT);
		expect(result.level).toBe(RiskLevel.Low);
		expect(result.riskScore).toBe(0);
	});
	it('treats hosting/datacenter networks as untrusted anonymizing networks', async () => {
		const engine = new DeterministicRiskEngine(
			createToolbox({
				lookupIpInfo: async ({ip}) =>
					baseIpInfo(ip, {
						isHosting: true,
						asnType: 'hosting',
					}),
			}),
		);
		const result = await engine.classify(BASE_EVENT);
		expect(result.riskScore).toBeGreaterThan(0);
		expect(result.reasoning).toContain('hosting/datacenter network detected by IPinfo');
	});
	it('treats heavy plus-address reuse as high risk', async () => {
		const engine = new DeterministicRiskEngine(
			createToolbox({
				analyzeEmailSyntax: async ({email}) => ({
					...(await createToolbox().analyzeEmailSyntax({email})),
					hasPlusTag: true,
				}),
				getRegistrationsByPlusAddressBase: async ({plusAddressBase, windowHours}) => ({
					...baseVelocity(plusAddressBase, windowHours),
					totalRegistrations: 4,
					uniqueEmails: 4,
					uniqueIps: 1,
				}),
			}),
		);
		const result = await engine.classify({
			...BASE_EVENT,
			email: 'alice+promo@example.com',
			isUnclaimed: false,
		});
		expect(result.level).toBe(RiskLevel.High);
		expect(result.recommendedAction).toBe(RecommendedAction.RequireOutboundPhone);
		expect(result.reasoning).toContain('heavy plus-address reuse');
	});
});
