// SPDX-License-Identifier: AGPL-3.0-or-later

import {executeConditional, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import type {FederationInboxRow} from '@app/api/database/types/FederationTypes';
import {FederationInbox} from '@app/api/Tables';

export interface IFederationInboxRepository {
	insertIfAbsent(row: FederationInboxRow): Promise<boolean>;
	get(origin: string, eventId: string): Promise<FederationInboxRow | null>;
	listPendingForOrigin(origin: string, limit: number): Promise<Array<FederationInboxRow>>;
	markApplied(origin: string, eventId: string, appliedAt: Date): Promise<void>;
	markRejected(origin: string, eventId: string, reason: string): Promise<void>;
}

export class FederationInboxRepository implements IFederationInboxRepository {
	async insertIfAbsent(row: FederationInboxRow): Promise<boolean> {
		return executeConditional(FederationInbox.insertIfNotExists(row));
	}

	async get(origin: string, eventId: string): Promise<FederationInboxRow | null> {
		const query = FederationInbox.select({
			where: [FederationInbox.where.eq('origin'), FederationInbox.where.eq('event_id')],
			limit: 1,
		});
		return fetchOne<FederationInboxRow>(query.bind({origin, event_id: eventId}));
	}

	async listPendingForOrigin(origin: string, limit: number): Promise<Array<FederationInboxRow>> {
		const query = FederationInbox.select({
			where: FederationInbox.where.eq('origin'),
			orderBy: {col: 'event_id', direction: 'ASC'},
			limit,
		});
		const rows = await fetchMany<FederationInboxRow>(query.bind({origin}));
		return rows.filter((row) => row.status === 'received');
	}

	async markApplied(origin: string, eventId: string, appliedAt: Date): Promise<void> {
		await upsertOne(
			FederationInbox.patchByPk(
				{origin, event_id: eventId},
				{
					status: Db.set('applied'),
					applied_at: Db.set(appliedAt),
					error_reason: Db.clear(),
				},
			),
		);
	}

	async markRejected(origin: string, eventId: string, reason: string): Promise<void> {
		await upsertOne(
			FederationInbox.patchByPk(
				{origin, event_id: eventId},
				{
					status: Db.set('rejected'),
					error_reason: Db.set(reason),
				},
			),
		);
	}
}
