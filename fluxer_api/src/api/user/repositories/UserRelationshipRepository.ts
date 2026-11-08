// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {
	BatchBuilder,
	deleteOneOrMany,
	executeGroupedBatches,
	fetchMany,
	fetchManyInChunks,
	fetchOne,
} from '@app/api/database/CassandraQueryExecution';
import {Db, nextVersion} from '@app/api/database/CassandraTypes';
import {executeVersionedUpdate} from '@app/api/database/CassandraVersionedUpdate';
import type {NoteRow, RelationshipRow} from '@app/api/database/types/UserTypes';
import {Relationship} from '@app/api/models/Relationship';
import {UserNote} from '@app/api/models/UserNote';
import {Notes, Relationships, RelationshipsByTarget} from '@app/api/Tables';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';

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
const FETCH_RELATIONSHIPS_BY_TARGET_AND_SOURCES_CQL = RelationshipsByTarget.selectCql({
	columns: ['source_user_id', 'type'],
	where: [
		RelationshipsByTarget.where.eq('target_user_id'),
		RelationshipsByTarget.where.in('source_user_id', 'source_user_ids'),
	],
});
const FETCH_RELATIONSHIPS_FROM_SOURCES_TO_TARGET_CQL = Relationships.selectCql({
	where: [Relationships.where.in('source_user_id', 'source_user_ids'), Relationships.where.eq('target_user_id')],
});
const FETCH_RELATIONSHIP_CQL = Relationships.selectCql({
	where: [
		Relationships.where.eq('source_user_id'),
		Relationships.where.eq('target_user_id'),
		Relationships.where.eq('type'),
	],
	limit: 1,
});
const FETCH_RELATIONSHIP_BY_TARGET_CQL = RelationshipsByTarget.selectCql({
	columns: ['version'],
	where: [
		RelationshipsByTarget.where.eq('target_user_id'),
		RelationshipsByTarget.where.eq('source_user_id'),
		RelationshipsByTarget.where.eq('type'),
	],
	limit: 1,
});
const FETCH_ALL_NOTES_FOR_DELETE_QUERY = Notes.selectCql({
	columns: ['source_user_id', 'target_user_id'],
	limit: 10000,
});

export interface RelationshipKey {
	source_user_id: UserID;
	target_user_id: UserID;
	type: number;
}

function sameRelationshipKey(a: RelationshipKey, b: RelationshipKey): boolean {
	return a.source_user_id === b.source_user_id && a.target_user_id === b.target_user_id && a.type === b.type;
}

export class UserRelationshipRepository {
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
		await executeGroupedBatches(
			allNotes
				.filter((note) => note.target_user_id === BigInt(userId))
				.map((note) => [
					Notes.deleteByPk({
						source_user_id: createUserID(note.source_user_id),
						target_user_id: createUserID(note.target_user_id),
					}),
				]),
		);
	}

	async deleteAllRelationships(userId: UserID): Promise<void> {
		const [relationshipsFromUser, relationshipsPointingToUser] = await Promise.all([
			fetchMany<RelationshipRow>(FETCH_RELATIONSHIPS_CQL, {source_user_id: userId}),
			fetchMany<RelationshipRow>(FETCH_RELATIONSHIPS_BY_TARGET_CQL, {target_user_id: userId}),
		]);
		const counterpartIds = [...new Set(relationshipsFromUser.map((rel) => rel.target_user_id))];
		const reverseRelationships = await fetchManyInChunks<RelationshipRow>(
			FETCH_RELATIONSHIPS_FROM_SOURCES_TO_TARGET_CQL,
			counterpartIds,
			(chunk) => ({source_user_ids: chunk, target_user_id: userId}),
		);
		const rowsByKey = new Map<string, RelationshipRow>();
		for (const rel of [...relationshipsFromUser, ...relationshipsPointingToUser, ...reverseRelationships]) {
			rowsByKey.set(`${rel.source_user_id}:${rel.target_user_id}:${rel.type}`, rel);
		}
		await executeGroupedBatches(
			[...rowsByKey.values()].map((rel) => [
				Relationships.deleteByPk({
					source_user_id: rel.source_user_id,
					target_user_id: rel.target_user_id,
					type: rel.type,
				}),
				RelationshipsByTarget.deleteByPk({
					target_user_id: rel.target_user_id,
					source_user_id: rel.source_user_id,
					type: rel.type,
				}),
			]),
		);
	}

	async deleteRelationship(sourceUserId: UserID, targetUserId: UserID, type: number): Promise<void> {
		await this.writeRelationships({
			deletes: [{source_user_id: sourceUserId, target_user_id: targetUserId, type}],
		});
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

	async listUserIdsBlockingTarget(targetUserId: UserID, sourceUserIds: Array<UserID>): Promise<Set<UserID>> {
		const rows = await fetchManyInChunks<Pick<RelationshipRow, 'source_user_id' | 'type'>>(
			FETCH_RELATIONSHIPS_BY_TARGET_AND_SOURCES_CQL,
			sourceUserIds,
			(chunk) => ({target_user_id: targetUserId, source_user_ids: chunk}),
		);
		return new Set(rows.filter((row) => row.type === RelationshipTypes.BLOCKED).map((row) => row.source_user_id));
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
		const [written] = await this.writeRelationships({upserts: [relationship]});
		return written!;
	}

	async writeRelationships({
		upserts = [],
		deletes = [],
	}: {
		upserts?: ReadonlyArray<RelationshipRow>;
		deletes?: ReadonlyArray<RelationshipKey>;
	}): Promise<Array<Relationship>> {
		const currentVersions = await Promise.all(
			upserts.map((row) =>
				Promise.all([
					fetchOne<Pick<RelationshipRow, 'version'>>(FETCH_RELATIONSHIP_CQL, {
						source_user_id: row.source_user_id,
						target_user_id: row.target_user_id,
						type: row.type,
					}),
					fetchOne<Pick<RelationshipRow, 'version'>>(FETCH_RELATIONSHIP_BY_TARGET_CQL, {
						target_user_id: row.target_user_id,
						source_user_id: row.source_user_id,
						type: row.type,
					}),
				]),
			),
		);
		const batch = new BatchBuilder();
		for (const key of deletes) {
			if (upserts.some((row) => sameRelationshipKey(row, key))) continue;
			batch.addPrepared(Relationships.deleteByPk(key));
			batch.addPrepared(
				RelationshipsByTarget.deleteByPk({
					target_user_id: key.target_user_id,
					source_user_id: key.source_user_id,
					type: key.type,
				}),
			);
		}
		const written = upserts.map((row, index) => {
			const [current, currentByTarget] = currentVersions[index]!;
			const version = nextVersion(current?.version);
			const fields = {
				nickname: Db.set(row.nickname),
				since: Db.set(row.since),
				share_voice_activity: Db.set(row.share_voice_activity ?? null),
			};
			batch.addPrepared(
				Relationships.patchByPk(
					{source_user_id: row.source_user_id, target_user_id: row.target_user_id, type: row.type},
					{...fields, version: Db.set(version)},
				),
			);
			batch.addPrepared(
				RelationshipsByTarget.patchByPk(
					{target_user_id: row.target_user_id, source_user_id: row.source_user_id, type: row.type},
					{...fields, version: Db.set(nextVersion(currentByTarget?.version))},
				),
			);
			return new Relationship({...row, version});
		});
		await batch.execute();
		return written;
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
