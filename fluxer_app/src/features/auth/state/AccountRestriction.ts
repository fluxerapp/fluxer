// SPDX-License-Identifier: AGPL-3.0-or-later

import {HttpError} from '@app/features/platform/types/EndpointError';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {isObjectLike as isRecord} from '@fluxer/record_utils/src/RecordGuards';
import {EnforcementNotice, NetworkBlock} from '@fluxer/schema/src/domains/auth/EnforcementNoticeSchemas';

export type AccountRestriction =
	| {type: 'notice'; notice: EnforcementNotice}
	| {type: 'network_blocked'; block: NetworkBlock};

const NOTICE_CODES: ReadonlySet<unknown> = new Set([
	APIErrorCodes.ACCOUNT_SUSPENDED_PERMANENTLY,
	APIErrorCodes.ACCOUNT_SUSPENDED_TEMPORARILY,
]);

const NETWORK_BLOCK_CODES: ReadonlySet<unknown> = new Set([
	APIErrorCodes.GLOBAL_IP_BANNED,
	APIErrorCodes.GLOBAL_IP_TEMPORARILY_BANNED,
]);

export function accountRestrictionFromBody(body: unknown): AccountRestriction | null {
	if (!isRecord(body)) return null;
	if (NOTICE_CODES.has(body.code)) {
		const notice = EnforcementNotice.safeParse(body.notice);
		return notice.success ? {type: 'notice', notice: notice.data} : null;
	}
	if (NETWORK_BLOCK_CODES.has(body.code)) {
		const block = NetworkBlock.safeParse(body);
		return block.success ? {type: 'network_blocked', block: block.data} : null;
	}
	return null;
}

export function parseAccountRestriction(error: unknown): AccountRestriction | null {
	if (!(error instanceof HttpError) || error.status !== HttpStatus.FORBIDDEN) return null;
	return accountRestrictionFromBody(error.body);
}
