// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	classifyAccountPolicyReverseDnsHostname,
	isAccountPolicyEducationOrganizationName,
	isAccountPolicyTrustedCommercialPrivacyProvider,
} from './AccountPolicyService';

const ASN_ENTRY_REGEX = /^\d+$/u;

export interface LocalIpIntel {
	countryIso: string | null;
	asn: number | null;
	asnOrg: string | null;
}

export interface IpInfoPrescreenOptions {
	enabled: boolean;
	allowedAsns: ReadonlySet<number>;
}

export type IpInfoPrescreenVerdict = 'consult' | 'skip';

export function prescreenIpInfoLookup(local: LocalIpIntel, opts: IpInfoPrescreenOptions): IpInfoPrescreenVerdict {
	if (!opts.enabled) return 'consult';
	if (opts.allowedAsns.size === 0) return 'consult';
	if (local.countryIso === null) return 'consult';
	if (local.asn === null) return 'consult';
	if (!opts.allowedAsns.has(local.asn)) return 'consult';
	if (isAccountPolicyTrustedCommercialPrivacyProvider(local.asnOrg)) return 'consult';
	if (isAccountPolicyEducationOrganizationName(local.asnOrg)) return 'consult';
	if (classifyAccountPolicyReverseDnsHostname(local.asnOrg) === 'cellular') return 'consult';
	return 'skip';
}

export function ipInfoPrescreenOptionsFromEnv(): IpInfoPrescreenOptions {
	return {
		enabled: booleanFromEnv(process.env.FLUXER_RISK_IPINFO_PRESCREEN_ENABLED),
		allowedAsns: asnSetFromEnv(process.env.FLUXER_RISK_IPINFO_PRESCREEN_ALLOW_ASNS),
	};
}

function booleanFromEnv(rawValue: string | undefined): boolean {
	if (!rawValue) return false;
	const normalized = rawValue.trim().toLowerCase();
	return normalized === '1' || normalized === 'true';
}

function asnSetFromEnv(rawValue: string | undefined): ReadonlySet<number> {
	const allowedAsns = new Set<number>();
	if (!rawValue) return allowedAsns;
	for (const entry of rawValue.split(',')) {
		const trimmed = entry.trim();
		if (!ASN_ENTRY_REGEX.test(trimmed)) continue;
		const asn = Number.parseInt(trimmed, 10);
		if (!Number.isSafeInteger(asn)) continue;
		allowedAsns.add(asn);
	}
	return allowedAsns;
}
