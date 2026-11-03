// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ForbiddenError} from '@fluxer/errors/src/domains/core/ForbiddenError';
import type {NetworkBlock} from '@fluxer/schema/src/domains/auth/EnforcementNoticeSchemas';

interface IpBannedErrorOptions {
	ipAddress: string;
	kind: NetworkBlock['ban_kind'];
	expiresAt: Date | null;
	expiresAtText: string | null;
	appealEmail: string | null;
	appealsSupported: boolean;
	reason: NetworkBlock['reason'];
	productName: string;
}

export class IpBannedError extends ForbiddenError {
	constructor(options: IpBannedErrorOptions) {
		const isTemporary = options.kind === 'temporary_24h';
		const data: NetworkBlock = {
			ip_address: options.ipAddress,
			appeal_email: options.appealEmail,
			appeals_supported: options.appealsSupported,
			ban_kind: options.kind,
			expires_at: options.expiresAt?.toISOString() ?? null,
			reason: options.reason,
			product_name: options.productName,
		};
		super({
			code: isTemporary ? APIErrorCodes.GLOBAL_IP_TEMPORARILY_BANNED : APIErrorCodes.GLOBAL_IP_BANNED,
			data,
			messageVariables: {
				ipAddress: options.ipAddress,
				appealEmail: options.appealEmail,
				expiresAt: options.expiresAtText ?? '',
				reason: options.reason ? 'shown' : 'none',
				product_name: options.productName,
			},
		});
	}
}
