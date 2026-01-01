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

export interface GuildRole {
	readonly id: string;
	readonly name: string;
	readonly color: number;
	readonly position: number;
	readonly hoist_position?: number | null;
	readonly permissions: string;
	readonly hoist: boolean;
	readonly mentionable: boolean;
}

export class GuildRoleRecord {
	readonly id: string;
	readonly guildId: string;
	readonly name: string;
	readonly color: number;
	readonly position: number;
	readonly hoistPosition: number | null;
	readonly permissions: bigint;
	readonly hoist: boolean;
	readonly mentionable: boolean;

	constructor(guildId: string, guildRole: GuildRole) {
		this.id = guildRole.id;
		this.guildId = guildId;
		this.name = guildRole.name;
		this.color = guildRole.color;
		this.position = guildRole.position;
		this.hoistPosition = guildRole.hoist_position ?? null;
		this.permissions = BigInt(guildRole.permissions);
		this.hoist = guildRole.hoist;
		this.mentionable = guildRole.mentionable;
	}

	get effectiveHoistPosition(): number {
		return this.hoistPosition ?? this.position;
	}

	withUpdates(updates: Partial<GuildRole>): GuildRoleRecord {
		return new GuildRoleRecord(this.guildId, {
			id: this.id,
			name: updates.name ?? this.name,
			color: updates.color ?? this.color,
			position: updates.position ?? this.position,
			hoist_position: updates.hoist_position !== undefined ? updates.hoist_position : this.hoistPosition,
			permissions: updates.permissions ?? this.permissions.toString(),
			hoist: updates.hoist ?? this.hoist,
			mentionable: updates.mentionable ?? this.mentionable,
		});
	}

	get isEveryone(): boolean {
		return this.id === this.guildId;
	}

	equals(other: GuildRoleRecord): boolean {
		return (
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

	toJSON(): GuildRole {
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
