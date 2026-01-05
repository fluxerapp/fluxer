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

export type AuthSession = Readonly<{
	id: string;
	approx_last_used_at: string | null;
	client_os: string;
	client_platform: string;
	client_location: string | null;
}>;

export class AuthSessionRecord {
	readonly id: string;
	readonly approxLastUsedAt: Date | null;
	readonly clientOs: string;
	readonly clientPlatform: string;
	readonly clientLocation: string | null;

	constructor(data: AuthSession) {
		this.id = data.id;
		this.approxLastUsedAt = data.approx_last_used_at ? new Date(data.approx_last_used_at) : null;
		this.clientOs = data.client_os;
		this.clientPlatform = data.client_platform;
		this.clientLocation = data.client_location;
	}

	toJSON(): AuthSession {
		return {
			id: this.id,
			approx_last_used_at: this.approxLastUsedAt?.toISOString() ?? null,
			client_os: this.clientOs,
			client_platform: this.clientPlatform,
			client_location: this.clientLocation,
		};
	}

	equals(other: AuthSessionRecord): boolean {
		return JSON.stringify(this) === JSON.stringify(other);
	}
}
