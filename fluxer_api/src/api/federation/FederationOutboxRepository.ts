// SPDX-License-Identifier: AGPL-3.0-or-later

import {deleteOneOrMany, executeConditional, fetchMany, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import type {FederationOutboxRow} from '@app/api/database/types/FederationTypes';
import {FederationOutbox} from '@app/api/Tables';

export interface FederationOutboxAttempt {
	targetInstanceId: string;
	eventId: string;
	attempts: number;
	nextAttemptAt: Date;
	lastAttemptAt: Date;
	lastError: string | null;
}

export interface IFederationOutboxRepository {
	insertIfAbsent(row: FederationOutboxRow): Promise<boolean>;
	listPendingForPeer(targetInstanceId: string, limit: number): Promise<Array<FederationOutboxRow>>;
	markAttempt(attempt: FederationOutboxAttempt): Promise<void>;
	delete(targetInstanceId: string, eventId: string): Promise<void>;
}

export class FederationOutboxRepository implements IFederationOutboxRepository {
	async insertIfAbsent(row: FederationOutboxRow): Promise<boolean> {
		return executeConditional(FederationOutbox.insertIfNotExists(row));
	}

	async listPendingForPeer(targetInstanceId: string, limit: number): Promise<Array<FederationOutboxRow>> {
		const query = FederationOutbox.select({
			where: FederationOutbox.where.eq('target_instance_id'),
			limit,
		});
		return fetchMany<FederationOutboxRow>(query.bind({target_instance_id: targetInstanceId}));
	}

	async markAttempt(attempt: FederationOutboxAttempt): Promise<void> {
		await upsertOne(
			FederationOutbox.patchByPk(
				{target_instance_id: attempt.targetInstanceId, event_id: attempt.eventId},
				{
					attempts: Db.set(attempt.attempts),
					next_attempt_at: Db.set(attempt.nextAttemptAt),
					last_attempt_at: Db.set(attempt.lastAttemptAt),
					last_error: attempt.lastError === null ? Db.clear() : Db.set(attempt.lastError),
				},
			),
		);
	}

	async delete(targetInstanceId: string, eventId: string): Promise<void> {
		await deleteOneOrMany(FederationOutbox.deleteByPk({target_instance_id: targetInstanceId, event_id: eventId}));
	}
}
