// SPDX-License-Identifier: AGPL-3.0-or-later

import {createVisitorRequestContextMiddleware} from '@app/api/guild/visits/GuildVisitorAccess';
import {shouldEnforceRateLimits} from '@app/api/middleware/RateLimitMiddleware';
import {UserFlags} from '@fluxer/constants/src/UserConstants';

export const GuildVisitorRequestContextMiddleware = createVisitorRequestContextMiddleware((ctx) => {
	const user = ctx.get('user');
	if (user && (user.flags & UserFlags.RATE_LIMIT_BYPASS) !== 0n) return false;
	return shouldEnforceRateLimits(ctx);
});
