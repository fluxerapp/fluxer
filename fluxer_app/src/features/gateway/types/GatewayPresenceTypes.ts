// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayCustomStatusPayload} from '@app/features/user/state/CustomStatus';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export type GatewayPresenceActivityType = 'game' | 'music' | 'software';

export interface GatewayPresenceActivity {
	readonly type: GatewayPresenceActivityType;
	readonly name: string;
	readonly state?: string;
	readonly details?: string;
	readonly started_at?: number;
}

export interface PresenceRecord {
	readonly guild_id?: string | null;
	readonly user: UserPartial;
	readonly status?: string | null;
	readonly afk?: boolean;
	readonly mobile?: boolean;
	readonly custom_status?: GatewayCustomStatusPayload | null;
	readonly activities?: ReadonlyArray<GatewayPresenceActivity>;
}

export type Presence = PresenceRecord;
