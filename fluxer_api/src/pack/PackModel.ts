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

import type {ExpressionPack} from '~/models/ExpressionPack';
import type {PackType} from './PackRepository';

export interface PackSummary {
	id: string;
	name: string;
	description: string | null;
	type: PackType;
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

export const mapPackToSummary = (pack: ExpressionPack, installedAt?: Date | null): PackSummary => {
	const summary: PackSummary = {
		id: pack.id.toString(),
		name: pack.name,
		description: pack.description,
		type: pack.type,
		creator_id: pack.creatorId.toString(),
		created_at: pack.createdAt.toISOString(),
		updated_at: pack.updatedAt.toISOString(),
	};
	if (installedAt) {
		summary.installed_at = installedAt.toISOString();
	}
	return summary;
};
