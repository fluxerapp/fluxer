// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayCustomStatusPayload} from '@app/features/user/state/CustomStatus';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export interface GatewayActivityPayload {
	readonly id: string;
	readonly type: 'application';
	readonly name: string;
	readonly icon?: string;
}

export interface PresenceRecord {
	readonly guild_id?: string | null;
	readonly user: UserPartial;
	readonly status?: string | null;
	readonly afk?: boolean;
	readonly mobile?: boolean;
	readonly custom_status?: GatewayCustomStatusPayload | null;
	readonly activities?: ReadonlyArray<GatewayActivityPayload>;
}

export type Presence = PresenceRecord;
