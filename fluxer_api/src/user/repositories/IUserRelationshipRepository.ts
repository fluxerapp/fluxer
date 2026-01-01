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

import type {UserID} from '~/BrandedTypes';
import type {RelationshipRow} from '~/database/CassandraTypes';
import type {Relationship, UserNote} from '~/Models';

export interface IUserRelationshipRepository {
	listRelationships(sourceUserId: UserID): Promise<Array<Relationship>>;
	getRelationship(sourceUserId: UserID, targetUserId: UserID, type: number): Promise<Relationship | null>;
	upsertRelationship(relationship: RelationshipRow): Promise<Relationship>;
	deleteRelationship(sourceUserId: UserID, targetUserId: UserID, type: number): Promise<void>;
	deleteAllRelationships(userId: UserID): Promise<void>;

	getUserNote(sourceUserId: UserID, targetUserId: UserID): Promise<UserNote | null>;
	getUserNotes(sourceUserId: UserID): Promise<Map<UserID, string>>;
	upsertUserNote(sourceUserId: UserID, targetUserId: UserID, note: string): Promise<UserNote>;
	clearUserNote(sourceUserId: UserID, targetUserId: UserID): Promise<void>;
	deleteAllNotes(userId: UserID): Promise<void>;
}
