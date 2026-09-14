// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	DisposableCheckResult,
	DomainAgeResult,
	EmailSyntaxResult,
	GeoIpAsnResult,
	GeoIpCityResult,
	HistoricalOutcomeResult,
	IpInfoAnonymousResult,
	LocaleGeoMatchResult,
	MxCheckResult,
	RegistrationTimingResult,
	ReverseDnsResult,
	SuspiciousIpResult,
	UserAgentResult,
	VelocityResult,
} from '@app/api/risk/RiskTypes';

export interface RiskToolbox {
	analyzeEmailSyntax(args: {email: string}): Promise<EmailSyntaxResult>;
	checkDomainDisposable(args: {domain: string}): Promise<DisposableCheckResult>;
	checkMx(args: {domain: string}): Promise<MxCheckResult>;
	checkDomainAge(args: {domain: string}): Promise<DomainAgeResult>;
	lookupGeoIpCity(args: {ip: string}): Promise<GeoIpCityResult>;
	lookupGeoIpAsn(args: {ip: string}): Promise<GeoIpAsnResult>;
	lookupIpInfo(args: {ip: string}): Promise<IpInfoAnonymousResult>;
	lookupReverseDns(args: {ip: string}): Promise<ReverseDnsResult>;
	getSuspiciousIp(args: {ip: string}): Promise<SuspiciousIpResult | null>;
	getRegistrationsByIp(args: {ip: string; windowHours: number}): Promise<VelocityResult>;
	getRegistrationsBySubnet(args: {ip: string; windowHours: number}): Promise<VelocityResult>;
	getRegistrationsByEmailDomain(args: {domain: string; windowHours: number}): Promise<VelocityResult>;
	getRegistrationsByPlusAddressBase(args: {plusAddressBase: string; windowHours: number}): Promise<VelocityResult>;
	getHistoricalOutcomesByIp(args: {ip: string; windowHours: number}): Promise<HistoricalOutcomeResult>;
	getHistoricalOutcomesBySubnet(args: {ip: string; windowHours: number}): Promise<HistoricalOutcomeResult>;
	getHistoricalOutcomesByEmailDomain(args: {domain: string; windowHours: number}): Promise<HistoricalOutcomeResult>;
	getHistoricalOutcomesByAsn(args: {asn: number; windowHours: number}): Promise<HistoricalOutcomeResult>;
	checkGeoVsLocale(args: {
		geoipCountryIso: string | null;
		registrationLocale: string | null;
		registrationTimezone: string | null;
	}): Promise<LocaleGeoMatchResult>;
	analyzeUserAgent(args: {userAgent: string}): Promise<UserAgentResult>;
	analyzeRegistrationTiming(args: {timezone: string | null}): Promise<RegistrationTimingResult>;
}
