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

export interface PackSummary {
	id: string;
	name: string;
	description: string | null;
	type: 'emoji' | 'sticker';
	creator_id: string;
	created_at: string;
	updated_at: string;
	installed_at?: string;
}

export interface PackDashboardSection {
	installed_limit: number;
	created_limit: number;
	installed: Array<PackSummary>;
	created: Array<PackSummary>;
}

export interface PackDashboardResponse {
	emoji: PackDashboardSection;
	sticker: PackDashboardSection;
}

export interface PackInviteMetadata {
	code: string;
	type: 'emoji_pack' | 'sticker_pack';
	pack: PackSummary & {creator: {id: string; username: string; discriminator: string}};
	inviter: {id: string; username: string; discriminator: string} | null;
	expires_at: string | null;
	temporary: boolean;
	created_at: string;
	uses: number;
	max_uses: number;
}
