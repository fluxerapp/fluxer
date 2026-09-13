// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IAdminRepository} from '@app/api/admin/IAdminRepository';
import {createDisposableDomainChecker} from '@app/api/risk/adapters/DisposableDomainChecker';
import {createDnsMxChecker, type MxResolver, NodeDnsMxResolver} from '@app/api/risk/adapters/DnsMxChecker';
import {createDomainAgeChecker} from '@app/api/risk/adapters/DomainAgeChecker';
import {analyzeEmailSyntax} from '@app/api/risk/adapters/EmailSyntaxAnalyzer';
import {createGeoIpAsnAdapter, createGeoIpCityAdapter} from '@app/api/risk/adapters/GeoIpAdapters';
import {createHistoricalOutcomeAdapter} from '@app/api/risk/adapters/HistoricalOutcomeAdapter';
import {unavailableIpInfoAnonymousResult} from '@app/api/risk/adapters/IpInfoAdapter';
import {checkGeoVsLocale} from '@app/api/risk/adapters/LocaleGeoMatcher';
import {analyzeRegistrationTiming} from '@app/api/risk/adapters/RegistrationTimingAnalyzer';
import {analyzeUserAgent} from '@app/api/risk/adapters/UserAgentAnalyzer';
import {createVelocityAdapter, type IRegistrationEventsRepository} from '@app/api/risk/adapters/VelocityAdapter';
import type {IRiskHistoryRepository} from '@app/api/risk/HistoricalOutcomeRepository';
import type {RiskToolbox} from '@app/api/risk/RiskToolbox';
import type {IpInfoAnonymousResult, ReverseDnsResult} from '@app/api/risk/RiskTypes';
import type {ISuspiciousIpRepository} from '@app/api/risk/SuspiciousIpRepository';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {GeoipAsnResult, GeoipResult} from '@pkgs/geoip/src/GeoipLookup';
import type {IpInfoService} from '@pkgs/geoip/src/IpInfoService';

interface RiskToolboxFactoryOptions {
	adminRepository: Pick<IAdminRepository, 'isEmailDomainSuspicious' | 'isEmailDomainDisposable'>;
	ipInfoChecker?: (ip: string) => Promise<IpInfoAnonymousResult>;
	reverseDnsLookup?: (ip: string) => Promise<ReverseDnsResult>;
	ipInfoService: IpInfoService;
	registrationEventsRepository: IRegistrationEventsRepository;
	historicalOutcomeRepository: IRiskHistoryRepository;
	suspiciousIpRepository: ISuspiciousIpRepository;
	mxResolver?: MxResolver;
	mxCacheTtlMs?: number;
	cacheService?: ICacheService;
	lookupLocalCity?: (ip: string) => Promise<GeoipResult>;
	lookupLocalAsn?: (ip: string) => Promise<GeoipAsnResult>;
}

export function createRiskToolbox(opts: RiskToolboxFactoryOptions): RiskToolbox {
	const checkDomainDisposable = createDisposableDomainChecker({adminRepository: opts.adminRepository});
	const lookupGeoIpCity = createGeoIpCityAdapter({
		ipInfoService: opts.ipInfoService,
		lookupLocalCity: opts.lookupLocalCity,
	});
	const lookupGeoIpAsn = createGeoIpAsnAdapter({
		ipInfoService: opts.ipInfoService,
		lookupLocalAsn: opts.lookupLocalAsn,
	});
	const checkMx = createDnsMxChecker({
		resolver: opts.mxResolver ?? new NodeDnsMxResolver(),
		cacheTtlMs: opts.mxCacheTtlMs,
	});
	const checkDomainAge = createDomainAgeChecker({cacheService: opts.cacheService});
	const velocity = createVelocityAdapter({repository: opts.registrationEventsRepository});
	const historicalOutcomes = createHistoricalOutcomeAdapter({
		repository: opts.historicalOutcomeRepository,
	});
	const lookupIpInfo = opts.ipInfoChecker
		? async (args: {ip: string}) => opts.ipInfoChecker!(args.ip)
		: async (args: {ip: string}) => unavailableIpInfoAnonymousResult(args.ip, 'IPInfo not configured (no API key)');
	const lookupReverseDns = opts.reverseDnsLookup
		? async (args: {ip: string}) => opts.reverseDnsLookup!(args.ip)
		: async (args: {ip: string}) => ({
				ip: args.ip,
				hostname: null,
				classification: 'unknown' as const,
			});
	return {
		analyzeEmailSyntax: async (args) => analyzeEmailSyntax(args),
		checkDomainDisposable,
		checkMx,
		checkDomainAge,
		lookupGeoIpCity,
		lookupGeoIpAsn,
		lookupIpInfo,
		lookupReverseDns,
		getSuspiciousIp: async (args) => opts.suspiciousIpRepository.findActiveByIp(args.ip),
		getRegistrationsByIp: velocity.getRegistrationsByIp,
		getRegistrationsBySubnet: velocity.getRegistrationsBySubnet,
		getRegistrationsByEmailDomain: velocity.getRegistrationsByEmailDomain,
		getRegistrationsByPlusAddressBase: velocity.getRegistrationsByPlusAddressBase,
		getHistoricalOutcomesByIp: historicalOutcomes.getHistoricalOutcomesByIp,
		getHistoricalOutcomesBySubnet: historicalOutcomes.getHistoricalOutcomesBySubnet,
		getHistoricalOutcomesByEmailDomain: historicalOutcomes.getHistoricalOutcomesByEmailDomain,
		getHistoricalOutcomesByAsn: historicalOutcomes.getHistoricalOutcomesByAsn,
		checkGeoVsLocale: async (args) => checkGeoVsLocale(args),
		analyzeUserAgent: async (args) => analyzeUserAgent(args),
		analyzeRegistrationTiming: async (args) => analyzeRegistrationTiming(args),
	};
}
