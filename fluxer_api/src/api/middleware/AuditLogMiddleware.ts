// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HonoEnv} from '@app/api/types/HonoEnv';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {AuditLogReasonType} from '@fluxer/schema/src/primitives/ChannelValidators';
import {createMiddleware} from 'hono/factory';

const utf8Decoder = new TextDecoder('utf-8', {fatal: true});

function hasOnlyLatin1WithHighBytes(value: string): boolean {
	let high = false;
	for (let index = 0; index < value.length; index++) {
		const code = value.charCodeAt(index);
		if (code > 0xff) return false;
		if (code >= 0x80) high = true;
	}
	return high;
}

export function decodeHeaderUtf8(value: string): string {
	if (!hasOnlyLatin1WithHighBytes(value)) {
		return value;
	}
	try {
		return utf8Decoder.decode(Buffer.from(value, 'latin1'));
	} catch {
		return value;
	}
}

export const AuditLogMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const auditLogReasonHeader = ctx.req.header('X-Audit-Log-Reason');
	if (auditLogReasonHeader) {
		const result = AuditLogReasonType.safeParse(decodeHeaderUtf8(auditLogReasonHeader));
		if (!result.success) {
			throw InputValidationError.fromCode('X-Audit-Log-Reason', ValidationErrorCodes.INVALID_AUDIT_LOG_REASON);
		}
		ctx.set('auditLogReason', result.data);
	} else {
		ctx.set('auditLogReason', null);
	}
	await next();
});
