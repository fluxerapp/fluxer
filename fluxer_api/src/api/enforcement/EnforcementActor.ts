// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import type {Context} from 'hono';

export interface EnforcementActor {
	adminUserId: UserID;
	acls: ReadonlySet<string>;
	internalNote: string | null;
}

export function enforcementActor(ctx: Context<HonoEnv>): EnforcementActor {
	return {
		adminUserId: ctx.get('adminUserId'),
		acls: ctx.get('adminUserAcls'),
		internalNote: ctx.get('auditLogReason'),
	};
}
