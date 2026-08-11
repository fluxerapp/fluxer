// SPDX-License-Identifier: AGPL-3.0-or-later

import {getSameIpDecisionKey} from '@fluxer/ip_utils/src/IpAddress';
import {Config} from '../Config';

let exemptDecisionKeys: ReadonlySet<string> | null = null;

function getExemptDecisionKeys(): ReadonlySet<string> {
	if (exemptDecisionKeys) {
		return exemptDecisionKeys;
	}
	const keys = new Set<string>();
	for (const ip of Config.ipBanExemptIps) {
		const key = getSameIpDecisionKey(ip);
		if (!key) {
			throw new Error(`Invalid IP ban exemption in API config: ${ip}`);
		}
		keys.add(key);
	}
	exemptDecisionKeys = keys;
	return keys;
}

export function isIpBanExempt(ip: string | null | undefined): boolean {
	if (!ip) {
		return false;
	}
	const key = getSameIpDecisionKey(ip);
	return key !== null && getExemptDecisionKeys().has(key);
}
