// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {noteText} from '@app/features/theme/fonts/ScriptFontLoader';
import type {GuildRole as WireGuildRole} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';

interface GuildRoleRecordOptions {
	instanceId?: string;
}

export class GuildRole {
	readonly instanceId: string;
	readonly id: string;
	readonly guildId: string;
	readonly name: string;
	readonly color: number;
	readonly position: number;
	readonly hoistPosition: number | null;
	readonly permissions: bigint;
	readonly hoist: boolean;
	readonly mentionable: boolean;

	constructor(guildId: string, guildRole: WireGuildRole, options?: GuildRoleRecordOptions) {
		this.instanceId = options?.instanceId ?? RuntimeConfig.localInstanceDomain;
		this.id = guildRole.id;
		this.guildId = guildId;
		this.name = guildRole.name;
		this.color = guildRole.color;
		this.position = guildRole.position;
		this.hoistPosition = guildRole.hoist_position ?? null;
		this.permissions = BigInt(guildRole.permissions);
		this.hoist = guildRole.hoist;
		this.mentionable = guildRole.mentionable;
		noteText(this.name);
	}

	get effectiveHoistPosition(): number {
		return this.hoistPosition ?? this.position;
	}

	withUpdates(updates: Partial<WireGuildRole>): GuildRole {
		return new GuildRole(
			this.guildId,
			{
				id: this.id,
				name: updates.name ?? this.name,
				color: updates.color ?? this.color,
				position: updates.position ?? this.position,
				hoist_position: updates.hoist_position !== undefined ? updates.hoist_position : this.hoistPosition,
				permissions: updates.permissions ?? this.permissions.toString(),
				hoist: updates.hoist ?? this.hoist,
				mentionable: updates.mentionable ?? this.mentionable,
			},
			{instanceId: this.instanceId},
		);
	}

	get isEveryone(): boolean {
		return this.id === this.guildId;
	}

	equals(other: GuildRole): boolean {
		return (
			this.instanceId === other.instanceId &&
			this.id === other.id &&
			this.guildId === other.guildId &&
			this.name === other.name &&
			this.color === other.color &&
			this.position === other.position &&
			this.hoistPosition === other.hoistPosition &&
			this.permissions === other.permissions &&
			this.hoist === other.hoist &&
			this.mentionable === other.mentionable
		);
	}

	toJSON(): WireGuildRole {
		return {
			id: this.id,
			name: this.name,
			color: this.color,
			position: this.position,
			hoist_position: this.hoistPosition,
			permissions: this.permissions.toString(),
			hoist: this.hoist,
			mentionable: this.mentionable,
		};
	}
}
