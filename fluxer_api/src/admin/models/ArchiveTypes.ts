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

import {Int64Type, z} from '~/Schema';

export const TriggerUserArchiveRequest = z.object({
	user_id: Int64Type,
});

export type TriggerUserArchiveRequest = z.infer<typeof TriggerUserArchiveRequest>;

export const TriggerGuildArchiveRequest = z.object({
	guild_id: Int64Type,
});

export type TriggerGuildArchiveRequest = z.infer<typeof TriggerGuildArchiveRequest>;

export const ListArchivesRequest = z.object({
	subject_type: z.enum(['user', 'guild', 'all']).default('all'),
	subject_id: Int64Type.optional(),
	requested_by: Int64Type.optional(),
	limit: z.number().min(1).max(200).default(50),
	include_expired: z.boolean().default(false),
});

export type ListArchivesRequest = z.infer<typeof ListArchivesRequest>;
