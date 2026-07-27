// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayCustomStatusPayload} from '@app/features/user/state/CustomStatus';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export interface ActivityTimestamps {
	readonly start?: number;
	readonly end?: number;
}

export interface ActivityAssets {
	readonly large_image?: string | null;
	readonly large_text?: string | null;
	readonly small_image?: string | null;
	readonly small_text?: string | null;
}

export interface ActivityPayload {
	readonly name: string;
	readonly type: number;
	readonly url?: string | null;
	readonly created_at?: number;
	readonly timestamps?: ActivityTimestamps | null;
	readonly application_id?: string | null;
	readonly details?: string | null;
	readonly state?: string | null;
	readonly assets?: ActivityAssets | null;
}

export interface PresenceRecord {
	readonly guild_id?: string | null;
	readonly user: UserPartial;
	readonly status?: string | null;
	readonly afk?: boolean;
	readonly mobile?: boolean;
	readonly custom_status?: GatewayCustomStatusPayload | null;
	readonly activities?: readonly ActivityPayload[] | null;
}

export type Presence = PresenceRecord;
