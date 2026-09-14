// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	classifyAccountPolicyEmailTld,
	isAccountPolicyBlockedRegistrationEmailDomain,
	isAccountPolicyLowRiskEmailTld,
} from '@app/api/risk/AccountPolicyService';

export enum AbuseProneEmailTldRisk {
	High = 'high',
}

export function classifyAbuseProneEmailTld(tld: string | null | undefined): AbuseProneEmailTldRisk | null {
	return classifyAccountPolicyEmailTld(tld) === 'high' ? AbuseProneEmailTldRisk.High : null;
}

export function isBlockedRegistrationEmailDomain(domain: string | null | undefined): boolean {
	return isAccountPolicyBlockedRegistrationEmailDomain(domain);
}

export function isLowRiskEmailTld(tld: string | null | undefined): boolean {
	return isAccountPolicyLowRiskEmailTld(tld);
}
