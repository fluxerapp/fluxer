// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {type GeoipAsnResult, lookupAsnByIp} from '@pkgs/geoip/src/GeoipLookup';

const ASN_ENTRY_REGEX = /^\d+$/u;

type AsnLookup = (ip: string) => Promise<GeoipAsnResult>;

let exemptAsns: ReadonlySet<number> | null = null;
let injectedAsnLookup: AsnLookup | undefined;

function getExemptAsns(): ReadonlySet<number> {
	if (exemptAsns) {
		return exemptAsns;
	}
	const asns = new Set<number>();
	const rawValue = process.env.FLUXER_ABUSE_EXEMPT_ASNS;
	if (rawValue) {
		for (const entry of rawValue.split(',')) {
			const trimmed = entry.trim();
			if (!ASN_ENTRY_REGEX.test(trimmed)) continue;
			const asn = Number.parseInt(trimmed, 10);
			if (Number.isSafeInteger(asn) && asn > 0) asns.add(asn);
		}
	}
	exemptAsns = asns;
	return asns;
}

function resolveAsn(ip: string): Promise<GeoipAsnResult> {
	if (injectedAsnLookup) {
		return injectedAsnLookup(ip);
	}
	return lookupAsnByIp(ip, Config.geoip.maxmindAsnDbPath);
}

export async function isAutoBanExemptAsn(ip: string): Promise<boolean> {
	const asns = getExemptAsns();
	if (asns.size === 0) return false;
	const result = await resolveAsn(ip);
	return result.asn !== null && asns.has(result.asn);
}

export function setInjectedAutoBanAsnLookup(lookup: AsnLookup | undefined): void {
	injectedAsnLookup = lookup;
}

export function resetAutoBanAsnExemptionsForTesting(): void {
	exemptAsns = null;
	injectedAsnLookup = undefined;
}
