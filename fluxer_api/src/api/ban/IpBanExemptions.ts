// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {parseIpBanEntry, tryParseSingleIp} from '@app/api/utils/IpRangeUtils';
import type {IpAddressFamily} from '@fluxer/ip_utils/src/IpAddress';
import {getSameIpDecisionKey} from '@fluxer/ip_utils/src/IpAddress';

interface ExemptRange {
	family: IpAddressFamily;
	start: bigint;
	end: bigint;
}

interface IpBanExemptions {
	decisionKeys: ReadonlySet<string>;
	ranges: ReadonlyArray<ExemptRange>;
}

let exemptions: IpBanExemptions | null = null;

function getExemptions(): IpBanExemptions {
	if (exemptions) {
		return exemptions;
	}
	const decisionKeys = new Set<string>();
	const ranges: Array<ExemptRange> = [];
	for (const entry of Config.ipBanExemptIps) {
		if (entry.includes('/')) {
			const range = parseIpBanEntry(entry);
			if (range?.type !== 'range') {
				throw new Error(`Invalid IP ban exemption in API config: ${entry}`);
			}
			ranges.push({family: range.family, start: range.start, end: range.end});
			continue;
		}
		const key = getSameIpDecisionKey(entry);
		if (!key) {
			throw new Error(`Invalid IP ban exemption in API config: ${entry}`);
		}
		decisionKeys.add(key);
	}
	exemptions = {decisionKeys, ranges};
	return exemptions;
}

export function isIpBanExempt(ip: string | null | undefined): boolean {
	if (!ip) {
		return false;
	}
	const {decisionKeys, ranges} = getExemptions();
	const key = getSameIpDecisionKey(ip);
	if (key !== null && decisionKeys.has(key)) {
		return true;
	}
	if (ranges.length === 0) {
		return false;
	}
	const parsed = tryParseSingleIp(ip);
	if (!parsed) {
		return false;
	}
	return ranges.some(
		(range) => range.family === parsed.family && parsed.value >= range.start && parsed.value <= range.end,
	);
}

export function resetIpBanExemptionsForTesting(): void {
	exemptions = null;
}
