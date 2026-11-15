// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	SnapshotGuildMemberKey,
	SnapshotGuildRow,
	SnapshotPresenceRow,
	SnapshotReadStateRow,
	SnapshotUnavailableGuildRow,
	SnapshotUserRow,
} from '@app/features/gateway/snapshot/SnapshotEntities';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import type {Channel as WireChannel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';

export interface ReducerRowMap<K, V> extends ReadonlyMap<K, V> {
	set(key: K, value: V): void;
	delete(key: K): boolean;
	clear(): void;
}

export interface SnapshotReducerBacking {
	readonly guilds: ReducerRowMap<string, SnapshotGuildRow>;
	readonly unavailableGuilds: ReducerRowMap<string, SnapshotUnavailableGuildRow>;
	readonly guildMembers: ReducerRowMap<SnapshotGuildMemberKey, GuildMemberData>;
	readonly channels: ReducerRowMap<string, WireChannel>;
	readonly readStates: ReducerRowMap<string, SnapshotReadStateRow>;
	readonly presences: ReducerRowMap<string, SnapshotPresenceRow>;
	readonly users: ReducerRowMap<string, SnapshotUserRow>;
	readonly voiceStates: ReducerRowMap<string, ReadonlyArray<VoiceState>>;
}

export function createResidentSnapshotReducerBacking(): SnapshotReducerBacking {
	return {
		guilds: new Map(),
		unavailableGuilds: new Map(),
		guildMembers: new Map(),
		channels: new Map(),
		readStates: new Map(),
		presences: new Map(),
		users: new Map(),
		voiceStates: new Map(),
	};
}
