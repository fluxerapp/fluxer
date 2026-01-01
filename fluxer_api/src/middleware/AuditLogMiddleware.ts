/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {createMiddleware} from 'hono/factory';
import type {HonoEnv} from '~/App';
import {InputValidationError} from '~/Errors';
import {AuditLogReasonType} from '~/Schema';

export const AuditLogMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const auditLogReasonHeader = ctx.req.header('X-Audit-Log-Reason');

	if (auditLogReasonHeader) {
		const result = AuditLogReasonType.safeParse(auditLogReasonHeader);
		if (!result.success) {
			throw InputValidationError.create(
				'X-Audit-Log-Reason',
				result.error.issues[0]?.message ?? 'Invalid audit log reason',
			);
		}
		ctx.set('auditLogReason', result.data);
	}

	await next();
});
