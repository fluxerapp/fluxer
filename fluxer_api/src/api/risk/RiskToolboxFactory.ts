// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IpInfoService} from '@pkgs/geoip/src/IpInfoService';
import type {IAdminRepository} from '../admin/IAdminRepository';
import {createDisposableDomainChecker} from './adapters/DisposableDomainChecker';
import {createDnsMxChecker, type MxResolver, NodeDnsMxResolver} from './adapters/DnsMxChecker';
import {createDomainAgeChecker} from './adapters/DomainAgeChecker';
import {analyzeEmailSyntax} from './adapters/EmailSyntaxAnalyzer';
import {createGeoIpAsnAdapter, createGeoIpCityAdapter} from './adapters/GeoIpAdapters';
import {createHistoricalOutcomeAdapter} from './adapters/HistoricalOutcomeAdapter';
import {checkGeoVsLocale} from './adapters/LocaleGeoMatcher';
import {analyzeRegistrationTiming} from './adapters/RegistrationTimingAnalyzer';
import {analyzeUserAgent} from './adapters/UserAgentAnalyzer';
import {createVelocityAdapter, type IRegistrationEventsRepository} from './adapters/VelocityAdapter';
import type {IRiskHistoryRepository} from './HistoricalOutcomeRepository';
import type {RiskToolbox} from './RiskToolbox';
import type {IpInfoAnonymousResult, ReverseDnsResult} from './RiskTypes';
import type {ISuspiciousIpRepository} from './SuspiciousIpRepository';

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
}

export function createRiskToolbox(opts: RiskToolboxFactoryOptions): RiskToolbox {
	const checkDomainDisposable = createDisposableDomainChecker({adminRepository: opts.adminRepository});
	const lookupGeoIpCity = createGeoIpCityAdapter({ipInfoService: opts.ipInfoService});
	const lookupGeoIpAsn = createGeoIpAsnAdapter({ipInfoService: opts.ipInfoService});
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
		: async (args: {ip: string}) =>
				({
					ip: args.ip,
					available: false,
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
					connectionType: 'unknown',
					percentDaysSeen: null,
					riskNote: 'IPInfo not configured (no API key)',
				}) as IpInfoAnonymousResult;
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
