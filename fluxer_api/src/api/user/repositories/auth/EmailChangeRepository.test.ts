// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {createUserID} from '@app/api/BrandedTypes';
import {
	type CassandraQueryExecutorForTesting,
	setCassandraQueryExecutorForTesting,
} from '@app/api/database/CassandraQueryExecution';
import type {CassandraParams, KvQueryMeta, PreparedQuery} from '@app/api/database/CassandraTypes';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {EmailChangeRepository} from '@app/api/user/repositories/auth/EmailChangeRepository';
import {beforeEach, describe, expect, it} from 'vitest';

const USER_ID = createUserID(4242n);
const ONE_DAY_SECONDS = 24 * 60 * 60;

interface RecordedWrite {
	table: string;
	ttlSeconds: unknown;
}

class WriteRecorder implements CassandraQueryExecutorForTesting {
	readonly writes: Array<RecordedWrite> = [];

	constructor(private readonly inner: CassandraQueryExecutorForTesting) {}

	async executeQuery<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
		query: PreparedQuery<P>,
	): Promise<Array<T>> {
		this.record(query.kvMeta, query.params);
		return this.inner.executeQuery<T, P>(query);
	}

	async executeBatch(
		queries: Array<{query: string; params: object; meta?: KvQueryMeta}>,
		atomic?: boolean,
	): Promise<void> {
		for (const entry of queries) this.record(entry.meta, entry.params as CassandraParams);
		await this.inner.executeBatch(queries, atomic);
	}

	private record(meta: KvQueryMeta | undefined, params: CassandraParams): void {
		if (meta?.action !== 'upsert') return;
		this.writes.push({
			table: meta.table.name,
			ttlSeconds: meta.ttlParamName ? params[meta.ttlParamName] : null,
		});
	}
}

describe('EmailChangeRepository retention', () => {
	let recorder: WriteRecorder;

	beforeEach(() => {
		recorder = new WriteRecorder(new InMemoryCassandraQueryExecutor());
		setCassandraQueryExecutorForTesting(recorder);
	});

	it('writes every ticket and token with a one day TTL so finished and abandoned changes expire', async () => {
		const repo = new EmailChangeRepository();
		const now = new Date();
		const ticket = {
			ticket: randomUUID(),
			user_id: USER_ID,
			require_original: false,
			original_email: 'old@example.com',
			original_verified: true,
			original_proof: randomUUID(),
			original_code: null,
			original_code_sent_at: null,
			original_code_expires_at: null,
			new_email: null,
			new_code: null,
			new_code_sent_at: null,
			new_code_expires_at: null,
			status: 'pending_new',
			created_at: now,
			updated_at: now,
		};
		await repo.createTicket(ticket);
		await repo.updateTicket({...ticket, new_email: 'new@example.com', status: 'completed'});
		await repo.createToken({
			token_: randomUUID(),
			user_id: USER_ID,
			new_email: 'new@example.com',
			expires_at: new Date(now.getTime() + 30 * 60 * 1000),
			created_at: now,
		});

		expect((await repo.findTicket(ticket.ticket))?.status).toBe('completed');
		expect(recorder.writes).toEqual([
			{table: 'email_change_tickets', ttlSeconds: ONE_DAY_SECONDS},
			{table: 'email_change_tickets', ttlSeconds: ONE_DAY_SECONDS},
			{table: 'email_change_tokens', ttlSeconds: ONE_DAY_SECONDS},
		]);
	});
});
