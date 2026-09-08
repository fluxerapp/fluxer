// SPDX-License-Identifier: AGPL-3.0-or-later

import {getSameIpDecisionKey} from '@fluxer/ip_utils/src/IpAddress';
import type {IpInfoLookupResult, IpInfoService} from '@pkgs/geoip/src/IpInfoService';
import {parseIpBanEntry} from '../utils/IpRangeUtils';
import {isTrustedCommercialPrivacyProvider} from './TrustedPrivacyProviders';

const VERDICT_CACHE_TTL_MS = 60 * 60 * 1000;

interface IpBanBlastRadiusVerdict {
	cgnat: boolean;
	sharedAccess: boolean;
}

interface CachedVerdict {
	expiresAtMs: number;
	verdict: IpBanBlastRadiusVerdict;
}

const verdictCache = new Map<string, CachedVerdict>();

function isAnonymousAccess(result: IpInfoLookupResult): boolean {
	return (
		result.anonymous.isAnonymous ||
		result.anonymous.isVpn ||
		result.anonymous.isProxy ||
		result.anonymous.isResidentialProxy ||
		result.anonymous.isTor ||
		result.anonymous.isRelay
	);
}

export function isHighCgnatBlastRadiusRisk(result: IpInfoLookupResult): boolean {
	if (!result.available || result.flags.isHosting || isAnonymousAccess(result)) {
		return false;
	}
	const asnType = result.asn.type?.trim().toLowerCase() ?? null;
	return result.flags.isMobile || result.mobile.name !== null || asnType === 'mobile';
}

type SuspiciousIpSkipReason = 'ipinfo_unavailable' | 'trusted_commercial_privacy_provider' | 'high_blast_radius';

export function getSuspiciousIpSkipReason(result: IpInfoLookupResult): SuspiciousIpSkipReason | null {
	if (!result.available) {
		return 'ipinfo_unavailable';
	}
	if (isTrustedCommercialPrivacyProvider(result.anonymous.providerName)) {
		return 'trusted_commercial_privacy_provider';
	}
	if (isHighCgnatBlastRadiusRisk(result) || isHighSharedAccessBlastRadiusRisk(result)) {
		return 'high_blast_radius';
	}
	return null;
}

export function isHighSharedAccessBlastRadiusRisk(result: IpInfoLookupResult): boolean {
	if (result.flags.isHosting || isAnonymousAccess(result)) {
		return false;
	}
	const asnType = result.asn.type?.trim().toLowerCase() ?? null;
	return result.flags.isAnycast || result.flags.isSatellite || asnType === 'education';
}

export function isSingleIpBanCandidate(value: string): boolean {
	return parseIpBanEntry(value)?.type === 'single';
}

export async function getIpBanBlastRadiusVerdict(
	ip: string,
	ipInfoService: IpInfoService,
	context: {
		source: string;
		reason: string;
	},
): Promise<IpBanBlastRadiusVerdict> {
	const now = Date.now();
	const cacheKey = getSameIpDecisionKey(ip) ?? ip;
	const cached = verdictCache.get(cacheKey);
	if (cached && cached.expiresAtMs > now) {
		return cached.verdict;
	}
	const result = await ipInfoService.lookup(ip, {
		source: context.source,
		reason: context.reason,
		metadata: {policy: 'ip_ban_cgnat_guard'},
	});
	const verdict: IpBanBlastRadiusVerdict = {
		cgnat: isHighCgnatBlastRadiusRisk(result),
		sharedAccess: isHighSharedAccessBlastRadiusRisk(result),
	};
	verdictCache.set(cacheKey, {
		verdict,
		expiresAtMs: now + VERDICT_CACHE_TTL_MS,
	});
	return verdict;
}
