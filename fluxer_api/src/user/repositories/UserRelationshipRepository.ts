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

import {createUserID, type UserID} from '~/BrandedTypes';
import {BatchBuilder, Db, deleteOneOrMany, executeVersionedUpdate, fetchMany, fetchOne} from '~/database/Cassandra';
import type {NoteRow, RelationshipRow} from '~/database/CassandraTypes';
import {Relationship, UserNote} from '~/Models';
import {Notes, Relationships} from '~/Tables';
import type {IUserRelationshipRepository} from './IUserRelationshipRepository';

const FETCH_ALL_NOTES_CQL = Notes.selectCql({
	where: Notes.where.eq('source_user_id'),
});

const FETCH_NOTE_CQL = Notes.selectCql({
	where: [Notes.where.eq('source_user_id'), Notes.where.eq('target_user_id')],
	limit: 1,
});

const FETCH_RELATIONSHIPS_CQL = Relationships.selectCql({
	where: Relationships.where.eq('source_user_id'),
});

const FETCH_RELATIONSHIP_CQL = Relationships.selectCql({
	where: [
		Relationships.where.eq('source_user_id'),
		Relationships.where.eq('target_user_id'),
		Relationships.where.eq('type'),
	],
	limit: 1,
});

const FETCH_ALL_NOTES_FOR_DELETE_QUERY = Notes.selectCql({
	columns: ['source_user_id', 'target_user_id'],
	limit: 10000,
});

const FETCH_RELATIONSHIPS_FOR_DELETE_CQL = Relationships.selectCql({
	columns: ['source_user_id', 'target_user_id', 'type'],
	where: Relationships.where.eq('source_user_id', 'user_id'),
});

export class UserRelationshipRepository implements IUserRelationshipRepository {
	async clearUserNote(sourceUserId: UserID, targetUserId: UserID): Promise<void> {
		await deleteOneOrMany(
			Notes.deleteByPk({
				source_user_id: sourceUserId,
				target_user_id: targetUserId,
			}),
		);
	}

	async deleteAllNotes(userId: UserID): Promise<void> {
		await deleteOneOrMany(
			Notes.deleteCql({
				where: Notes.where.eq('source_user_id', 'user_id'),
			}),
			{user_id: userId},
		);

		const allNotes = await fetchMany<{source_user_id: bigint; target_user_id: bigint}>(
			FETCH_ALL_NOTES_FOR_DELETE_QUERY,
			{},
		);

		const batch = new BatchBuilder();
		for (const note of allNotes) {
			if (note.target_user_id === BigInt(userId)) {
				batch.addPrepared(
					Notes.deleteByPk({
						source_user_id: createUserID(note.source_user_id),
						target_user_id: createUserID(note.target_user_id),
					}),
				);
			}
		}
		if (batch) {
			await batch.execute();
		}
	}

	async deleteAllRelationships(userId: UserID): Promise<void> {
		await deleteOneOrMany(
			Relationships.deleteCql({
				where: Relationships.where.eq('source_user_id', 'user_id'),
			}),
			{user_id: userId},
		);

		const relationships = await fetchMany<{source_user_id: bigint; target_user_id: bigint; type: number}>(
			FETCH_RELATIONSHIPS_FOR_DELETE_CQL,
			{user_id: userId},
		);

		const batch = new BatchBuilder();
		for (const rel of relationships) {
			if (rel.target_user_id === BigInt(userId)) {
				batch.addPrepared(
					Relationships.deleteByPk({
						source_user_id: createUserID(rel.source_user_id),
						target_user_id: createUserID(rel.target_user_id),
						type: rel.type,
					}),
				);
			}
		}
		await batch.execute();
	}

	async deleteRelationship(sourceUserId: UserID, targetUserId: UserID, type: number): Promise<void> {
		await deleteOneOrMany(
			Relationships.deleteByPk({
				source_user_id: sourceUserId,
				target_user_id: targetUserId,
				type,
			}),
		);
	}

	async getRelationship(sourceUserId: UserID, targetUserId: UserID, type: number): Promise<Relationship | null> {
		const relationship = await fetchOne<RelationshipRow>(FETCH_RELATIONSHIP_CQL, {
			source_user_id: sourceUserId,
			target_user_id: targetUserId,
			type,
		});
		return relationship ? new Relationship(relationship) : null;
	}

	async getUserNote(sourceUserId: UserID, targetUserId: UserID): Promise<UserNote | null> {
		const note = await fetchOne<NoteRow>(FETCH_NOTE_CQL, {
			source_user_id: sourceUserId,
			target_user_id: targetUserId,
		});
		return note ? new UserNote(note) : null;
	}

	async getUserNotes(sourceUserId: UserID): Promise<Map<UserID, string>> {
		const notes = await fetchMany<NoteRow>(FETCH_ALL_NOTES_CQL, {source_user_id: sourceUserId});
		const noteMap = new Map<UserID, string>();
		for (const note of notes) {
			noteMap.set(note.target_user_id, note.note);
		}
		return noteMap;
	}

	async listRelationships(sourceUserId: UserID): Promise<Array<Relationship>> {
		const relationships = await fetchMany<RelationshipRow>(FETCH_RELATIONSHIPS_CQL, {
			source_user_id: sourceUserId,
		});
		return relationships.map((rel) => new Relationship(rel));
	}

	async upsertRelationship(relationship: RelationshipRow): Promise<Relationship> {
		const result = await executeVersionedUpdate<RelationshipRow, 'source_user_id' | 'target_user_id' | 'type'>(
			() =>
				fetchOne(FETCH_RELATIONSHIP_CQL, {
					source_user_id: relationship.source_user_id,
					target_user_id: relationship.target_user_id,
					type: relationship.type,
				}),
			(current) => ({
				pk: {
					source_user_id: relationship.source_user_id,
					target_user_id: relationship.target_user_id,
					type: relationship.type,
				},
				patch: {
					nickname: Db.set(relationship.nickname),
					since: Db.set(relationship.since),
					version: Db.set((current?.version ?? 0) + 1),
				},
			}),
			Relationships,
			{onFailure: 'log'},
		);
		return new Relationship({
			...relationship,
			version: result.finalVersion ?? 1,
		});
	}

	async upsertUserNote(sourceUserId: UserID, targetUserId: UserID, note: string): Promise<UserNote> {
		const result = await executeVersionedUpdate<NoteRow, 'source_user_id' | 'target_user_id'>(
			() => fetchOne(FETCH_NOTE_CQL, {source_user_id: sourceUserId, target_user_id: targetUserId}),
			(current) => ({
				pk: {source_user_id: sourceUserId, target_user_id: targetUserId},
				patch: {note: Db.set(note), version: Db.set((current?.version ?? 0) + 1)},
			}),
			Notes,
			{onFailure: 'log'},
		);
		return new UserNote({
			source_user_id: sourceUserId,
			target_user_id: targetUserId,
			note,
			version: result.finalVersion ?? 0,
		});
	}
}
