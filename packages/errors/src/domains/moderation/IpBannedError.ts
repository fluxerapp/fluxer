// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ForbiddenError} from '@fluxer/errors/src/domains/core/ForbiddenError';

type GlobalIpBanKind = 'permanent' | 'temporary_24h';

interface IpBannedErrorOptions {
	ipAddress: string;
	kind: GlobalIpBanKind;
	expiresAt?: Date | null;
}

const SUPPORT_EMAIL = 'support@fluxer.com';

export class IpBannedError extends ForbiddenError {
	constructor(options: IpBannedErrorOptions) {
		const isTemporary = options.kind === 'temporary_24h';
		super({
			code: isTemporary ? APIErrorCodes.GLOBAL_IP_TEMPORARILY_BANNED : APIErrorCodes.GLOBAL_IP_BANNED,
			data: {
				ip_address: options.ipAddress,
				appeal_email: SUPPORT_EMAIL,
				appeals_supported: true,
				ban_kind: options.kind,
				expires_at: options.expiresAt?.toISOString() ?? null,
			},
			messageVariables: {
				ipAddress: options.ipAddress,
			},
		});
	}
}
