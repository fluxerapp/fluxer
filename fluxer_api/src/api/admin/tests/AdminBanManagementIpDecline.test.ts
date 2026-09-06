// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';
import type {IpInfoLookupResult, IpInfoService} from '@pkgs/geoip/src/IpInfoService';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';
import type {ApiContext} from '../../ApiContext';
import {createUserID} from '../../BrandedTypes';
import {getConfig} from '../../Config';
import {ipBanCache} from '../../middleware/IpBanMiddleware';
import {resetIpBanExemptionsForTesting} from '../../risk/IpBanExemptions';
import type {ISuspiciousIpRepository} from '../../risk/SuspiciousIpRepository';
import type {IAdminRepository} from '../IAdminRepository';
import type {AdminAuditService} from '../services/AdminAuditService';
import {AdminBanManagementService} from '../services/AdminBanManagementService';

const ADMIN_ID = createUserID(42n);
const EXEMPT_IP = '10.0.0.1';
const CARRIER_IP = '198.51.100.7';
const LOOKUP_FAILURE_IP = '203.0.113.9';

interface AuditCall {
	action: string;
	metadata: Map<string, string> | undefined;
}

function ipInfoResult(overrides: Partial<IpInfoLookupResult> = {}): IpInfoLookupResult {
	return {
		ip: CARRIER_IP,
		available: true,
		riskNote: 'test',
		geo: {
			countryCode: 'US',
			countryName: 'United States',
			continent: 'North America',
			continentCode: 'NA',
			region: null,
			regionCode: null,
			city: null,
			postalCode: null,
			timezone: null,
			latitude: null,
			longitude: null,
			accuracyRadiusKm: null,
		},
		asn: {
			asn: 'AS64500',
			number: 64500,
			name: 'Test Carrier',
			domain: null,
			type: null,
		},
		mobile: {
			name: null,
			mcc: null,
			mnc: null,
		},
		anonymous: {
			isAnonymous: false,
			providerName: null,
			isVpn: false,
			isProxy: false,
			isResidentialProxy: false,
			isTor: false,
			isRelay: false,
			percentDaysSeen: null,
		},
		flags: {
			isAnycast: false,
			isHosting: false,
			isMobile: false,
			isSatellite: false,
		},
		...overrides,
	};
}

function createBanManagementService(lookup: (ip: string) => Promise<IpInfoLookupResult>) {
	const bannedIps: Array<string> = [];
	const auditCalls: Array<AuditCall> = [];
	const adminRepository = {
		banIp: async (ip: string) => {
			bannedIps.push(ip);
		},
	};
	const auditService = {
		createAuditLog: async ({action, metadata}: AuditCall) => {
			auditCalls.push({action, metadata});
		},
	};
	const ipInfoService = {lookup: (ip: string) => lookup(ip)};
	const apiContext = {
		services: {
			cache: {
				publish: async () => {},
			},
		},
	};
	const service = new AdminBanManagementService({
		apiContext: apiContext as unknown as ApiContext,
		adminRepository: adminRepository as unknown as IAdminRepository,
		auditService: auditService as unknown as AdminAuditService,
		ipInfoService: ipInfoService as unknown as IpInfoService,
		suspiciousIpRepository: {} as unknown as ISuspiciousIpRepository,
	});
	return {service, bannedIps, auditCalls};
}

describe('AdminBanManagementService banIp guards', () => {
	let originalExemptIps: Array<string>;

	beforeEach(() => {
		const config = getConfig();
		originalExemptIps = config.ipBanExemptIps;
		config.ipBanExemptIps = [EXEMPT_IP];
		resetIpBanExemptionsForTesting();
	});

	afterEach(() => {
		ipBanCache.unban(LOOKUP_FAILURE_IP);
		getConfig().ipBanExemptIps = originalExemptIps;
		resetIpBanExemptionsForTesting();
	});

	it('refuses an exempt address with IP_BAN_DECLINED and writes no ban row', async () => {
		const {service, bannedIps, auditCalls} = createBanManagementService(async () => ipInfoResult());

		const error = await service.banIp({ip: EXEMPT_IP}, ADMIN_ID, null).then(
			() => null,
			(caught: unknown) => caught,
		);

		expect(error).toBeInstanceOf(BadRequestError);
		expect((error as BadRequestError).code).toBe(APIErrorCodes.IP_BAN_DECLINED);
		expect((error as BadRequestError).status).toBe(400);
		expect(bannedIps).toEqual([]);
		expect(auditCalls.map((call) => call.action)).toEqual(['ban_ip_skipped_exempt']);
		expect(auditCalls[0].metadata?.get('ip')).toBe(EXEMPT_IP);
	});

	it('refuses a high blast-radius carrier address with IP_BAN_DECLINED and writes no ban row', async () => {
		const {service, bannedIps, auditCalls} = createBanManagementService(async () =>
			ipInfoResult({
				mobile: {name: 'Example Mobile', mcc: '001', mnc: '01'},
				flags: {isAnycast: false, isHosting: false, isMobile: true, isSatellite: false},
			}),
		);

		const error = await service.banIp({ip: CARRIER_IP}, ADMIN_ID, null).then(
			() => null,
			(caught: unknown) => caught,
		);

		expect(error).toBeInstanceOf(BadRequestError);
		expect((error as BadRequestError).code).toBe(APIErrorCodes.IP_BAN_DECLINED);
		expect((error as BadRequestError).status).toBe(400);
		expect(bannedIps).toEqual([]);
		expect(auditCalls.map((call) => call.action)).toEqual(['ban_ip_skipped_cgnat']);
		expect(auditCalls[0].metadata?.get('ip')).toBe(CARRIER_IP);
	});

	it('still writes the ban when the IPInfo lookup fails', async () => {
		const {service, bannedIps, auditCalls} = createBanManagementService(async () => {
			throw new Error('ipinfo is unreachable');
		});

		await expect(service.banIp({ip: LOOKUP_FAILURE_IP}, ADMIN_ID, null)).resolves.toBeUndefined();

		expect(bannedIps).toEqual([LOOKUP_FAILURE_IP]);
		expect(auditCalls.map((call) => call.action)).toEqual(['ban_ip']);
	});
});
