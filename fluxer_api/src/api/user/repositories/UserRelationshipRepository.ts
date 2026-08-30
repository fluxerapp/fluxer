// SPDX-License-Identifier: AGPL-3.0-or-later

import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {createUserID, type UserID} from '../../BrandedTypes';
import {BatchBuilder, deleteOneOrMany, fetchMany, fetchOne} from '../../database/CassandraQueryExecution';
import {Db, nextVersion} from '../../database/CassandraTypes';
import {executeVersionedUpdate} from '../../database/CassandraVersionedUpdate';
import type {NoteRow, RelationshipRow} from '../../database/types/UserTypes';
import {Relationship} from '../../models/Relationship';
import {UserNote} from '../../models/UserNote';
import {Notes, Relationships, RelationshipsByTarget} from '../../Tables';
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
const FETCH_RELATIONSHIP_TARGET_TYPES_CQL = Relationships.selectCql({
	columns: ['target_user_id', 'type'],
	where: Relationships.where.eq('source_user_id'),
});
const FETCH_RELATIONSHIPS_BY_TARGET_CQL = RelationshipsByTarget.selectCql({
	where: RelationshipsByTarget.where.eq('target_user_id'),
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
		const allNotes = await fetchMany<{
			source_user_id: bigint;
			target_user_id: bigint;
		}>(FETCH_ALL_NOTES_FOR_DELETE_QUERY, {});
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
		const [relationshipsFromUser, relationshipsPointingToUser] = await Promise.all([
			fetchMany<RelationshipRow>(FETCH_RELATIONSHIPS_CQL, {source_user_id: userId}),
			fetchMany<RelationshipRow>(FETCH_RELATIONSHIPS_BY_TARGET_CQL, {target_user_id: userId}),
		]);
		const batch = new BatchBuilder();
		for (const rel of relationshipsFromUser) {
			batch.addPrepared(
				Relationships.deleteByPk({
					source_user_id: rel.source_user_id,
					target_user_id: rel.target_user_id,
					type: rel.type,
				}),
			);
			batch.addPrepared(
				RelationshipsByTarget.deleteByPk({
					target_user_id: rel.target_user_id,
					source_user_id: rel.source_user_id,
					type: rel.type,
				}),
			);
		}
		for (const rel of relationshipsPointingToUser) {
			batch.addPrepared(
				Relationships.deleteByPk({
					source_user_id: rel.source_user_id,
					target_user_id: rel.target_user_id,
					type: rel.type,
				}),
			);
			batch.addPrepared(
				RelationshipsByTarget.deleteByPk({
					target_user_id: rel.target_user_id,
					source_user_id: rel.source_user_id,
					type: rel.type,
				}),
			);
		}
		await batch.execute();
	}

	async deleteRelationship(sourceUserId: UserID, targetUserId: UserID, type: number): Promise<void> {
		await Promise.all([
			deleteOneOrMany(
				Relationships.deleteByPk({
					source_user_id: sourceUserId,
					target_user_id: targetUserId,
					type,
				}),
			),
			deleteOneOrMany(
				RelationshipsByTarget.deleteByPk({
					target_user_id: targetUserId,
					source_user_id: sourceUserId,
					type,
				}),
			),
		]);
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

	async listBlockedUserIds(sourceUserId: UserID): Promise<Array<UserID>> {
		const rows = await fetchMany<Pick<RelationshipRow, 'target_user_id' | 'type'>>(
			FETCH_RELATIONSHIP_TARGET_TYPES_CQL,
			{source_user_id: sourceUserId},
		);
		return rows.filter((row) => row.type === RelationshipTypes.BLOCKED).map((row) => row.target_user_id);
	}

	async listIncomingRequests(userId: UserID): Promise<Array<Relationship>> {
		const relationships = await this.listRelationships(userId);
		return relationships.filter((rel) => rel.type === RelationshipTypes.INCOMING_REQUEST);
	}

	async hasReachedRelationshipLimit(sourceUserId: UserID, limit: number): Promise<boolean> {
		const relationships = await fetchMany<RelationshipRow>(
			Relationships.select({
				where: Relationships.where.eq('source_user_id'),
				limit: limit + 1,
			}).bind({source_user_id: sourceUserId}),
		);
		return relationships.length >= limit;
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
					share_voice_activity: Db.set(relationship.share_voice_activity ?? null),
					version: Db.set(nextVersion(current?.version)),
				},
			}),
			Relationships,
		);
		const finalRelationship: RelationshipRow = {
			...relationship,
			version: result.finalVersion ?? 1,
		};
		await executeVersionedUpdate<RelationshipRow, 'target_user_id' | 'source_user_id' | 'type'>(
			() =>
				fetchOne(
					RelationshipsByTarget.selectCql({
						where: [
							RelationshipsByTarget.where.eq('target_user_id'),
							RelationshipsByTarget.where.eq('source_user_id'),
							RelationshipsByTarget.where.eq('type'),
						],
						limit: 1,
					}),
					{
						target_user_id: relationship.target_user_id,
						source_user_id: relationship.source_user_id,
						type: relationship.type,
					},
				),
			(current) => ({
				pk: {
					target_user_id: relationship.target_user_id,
					source_user_id: relationship.source_user_id,
					type: relationship.type,
				},
				patch: {
					nickname: Db.set(relationship.nickname),
					since: Db.set(relationship.since),
					share_voice_activity: Db.set(relationship.share_voice_activity ?? null),
					version: Db.set(nextVersion(current?.version)),
				},
			}),
			RelationshipsByTarget,
		);
		return new Relationship(finalRelationship);
	}

	async bulkUpdateFriendShareVoiceActivity(sourceUserId: UserID, value: boolean): Promise<Array<Relationship>> {
		const all = await this.listRelationships(sourceUserId);
		const friends = all.filter((rel) => rel.type === RelationshipTypes.FRIEND);
		const updated: Array<Relationship> = [];
		for (const friend of friends) {
			if (friend.shareVoiceActivity === value) {
				updated.push(friend);
				continue;
			}
			const newRow: RelationshipRow = {
				...friend.toRow(),
				share_voice_activity: value,
			};
			const next = await this.upsertRelationship(newRow);
			updated.push(next);
		}
		return updated;
	}

	async upsertUserNote(sourceUserId: UserID, targetUserId: UserID, note: string): Promise<UserNote> {
		const result = await executeVersionedUpdate<NoteRow, 'source_user_id' | 'target_user_id'>(
			() => fetchOne(FETCH_NOTE_CQL, {source_user_id: sourceUserId, target_user_id: targetUserId}),
			(current) => ({
				pk: {source_user_id: sourceUserId, target_user_id: targetUserId},
				patch: {note: Db.set(note), version: Db.set(nextVersion(current?.version))},
			}),
			Notes,
		);
		return new UserNote({
			source_user_id: sourceUserId,
			target_user_id: targetUserId,
			note,
			version: result.finalVersion ?? 1,
		});
	}
}
