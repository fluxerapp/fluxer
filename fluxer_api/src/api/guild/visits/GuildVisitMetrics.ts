// SPDX-License-Identifier: AGPL-3.0-or-later

import {registerCounter} from '@fluxer/hono/src/middleware/Metrics';

export const guildVisitStartTotal = registerCounter(
	'fluxer_api_guild_visit_start_total',
	'Guild visits started, by source and account tier',
);
export const guildVisitEndedTotal = registerCounter(
	'fluxer_api_guild_visit_end_total',
	'Guild visits ended, by reason',
);
export const guildVisitJoinTotal = registerCounter(
	'fluxer_api_guild_visit_join_total',
	'Guild visits converted into membership',
);
export const guildVisitorRateLimitedTotal = registerCounter(
	'fluxer_api_guild_visitor_rate_limited_total',
	'Visitor reads refused by a visitor bucket, by key kind',
);
export const guildVisitHistoryWindowTotal = registerCounter(
	'fluxer_api_guild_visit_history_window_total',
	'Visitor reads that hit the history window for accounts without a confirmed email',
);
