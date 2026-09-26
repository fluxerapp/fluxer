// SPDX-License-Identifier: AGPL-3.0-or-later

import {phoneRequiresInboundVerification} from '@app/api/risk/AbusePolicy';

export function requiresInboundPhoneVerification(phone: string, prefixes?: ReadonlyArray<string>): boolean {
	return phoneRequiresInboundVerification(phone, prefixes);
}
