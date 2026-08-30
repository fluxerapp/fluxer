// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '../../BrandedTypes';
import type {RelationshipRow} from '../../database/types/UserTypes';
import type {Relationship} from '../../models/Relationship';
import type {UserNote} from '../../models/UserNote';

export interface IUserRelationshipRepository {
	listRelationships(sourceUserId: UserID): Promise<Array<Relationship>>;
	listBlockedUserIds(sourceUserId: UserID): Promise<Array<UserID>>;
	listIncomingRequests(userId: UserID): Promise<Array<Relationship>>;
	hasReachedRelationshipLimit(sourceUserId: UserID, limit: number): Promise<boolean>;
	getRelationship(sourceUserId: UserID, targetUserId: UserID, type: number): Promise<Relationship | null>;
	upsertRelationship(relationship: RelationshipRow): Promise<Relationship>;
	bulkUpdateFriendShareVoiceActivity(sourceUserId: UserID, value: boolean): Promise<Array<Relationship>>;
	deleteRelationship(sourceUserId: UserID, targetUserId: UserID, type: number): Promise<void>;
	deleteAllRelationships(userId: UserID): Promise<void>;
	getUserNote(sourceUserId: UserID, targetUserId: UserID): Promise<UserNote | null>;
	getUserNotes(sourceUserId: UserID): Promise<Map<UserID, string>>;
	upsertUserNote(sourceUserId: UserID, targetUserId: UserID, note: string): Promise<UserNote>;
	clearUserNote(sourceUserId: UserID, targetUserId: UserID): Promise<void>;
	deleteAllNotes(userId: UserID): Promise<void>;
}
