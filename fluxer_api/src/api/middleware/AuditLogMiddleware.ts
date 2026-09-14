// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HonoEnv} from '@app/api/types/HonoEnv';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {AuditLogReasonType} from '@fluxer/schema/src/primitives/ChannelValidators';
import {createMiddleware} from 'hono/factory';

export const AuditLogMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const auditLogReasonHeader = ctx.req.header('X-Audit-Log-Reason');
	if (auditLogReasonHeader) {
		const result = AuditLogReasonType.safeParse(auditLogReasonHeader);
		if (!result.success) {
			throw InputValidationError.fromCode('X-Audit-Log-Reason', ValidationErrorCodes.INVALID_AUDIT_LOG_REASON);
		}
		ctx.set('auditLogReason', result.data);
	} else {
		ctx.set('auditLogReason', null);
	}
	await next();
});
