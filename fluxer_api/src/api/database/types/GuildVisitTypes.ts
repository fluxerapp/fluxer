// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';

export interface GuildVisitRow {
	user_id: UserID;
	guild_id: GuildID;
	source: number;
	invite_code: string | null;
	invite_expires_at: Date | null;
	started_at: Date;
	opened_at: Date;
}

export const GUILD_VISIT_COLUMNS = [
	'user_id',
	'guild_id',
	'source',
	'invite_code',
	'invite_expires_at',
	'started_at',
	'opened_at',
] as const satisfies ReadonlyArray<keyof GuildVisitRow>;
