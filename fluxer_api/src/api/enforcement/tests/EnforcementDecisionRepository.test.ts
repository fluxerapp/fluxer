// SPDX-License-Identifier: AGPL-3.0-or-later

import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import {PostgresKvQueryExecutor} from '@app/api/database/PostgresKvQueryExecutor';
import type {EnforcementDecisionRow} from '@app/api/database/types/EnforcementTypes';
import type {IARMessageContextRow} from '@app/api/database/types/ReportTypes';
import {EnforcementDecisionRepository} from '@app/api/enforcement/EnforcementDecisionRepository';
import {
	EnforcementDecisionEvidence,
	EnforcementDecisionNotes,
	EnforcementDecisions,
	EnforcementDecisionsBySubject,
} from '@app/api/Tables';
import {dockerAvailable, startPostgresKv, stopPostgresKv} from '@app/api/test/DockerTestContainer';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import type {IPostgresClient} from '@pkgs/postgres/src/Client';
import {seconds} from 'itty-time';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const KV_TABLE = 'kv_enforcement_repository';

function decisionRow(decisionId: bigint, overrides: Partial<EnforcementDecisionRow> = {}): EnforcementDecisionRow {
	return {
		decision_id: decisionId,
		subject_type: 'user',
		subject_id: 40n,
		subject_name: null,
		recipient_user_id: 40n,
		kind: 'suspension',
		reason_code: 3,
		source: 'report',
		automation: 'none',
		statement: 'Stop sending invites',
		rights_holder: null,
		ends_at: new Date('2026-11-01T00:00:00.000Z'),
		deletion_at: null,
		feature_limit: null,
		profile_fields: null,
		removed_message_count: null,
		removed_all_messages: null,
		payment_at: null,
		report_ids: new Set([7n, 8n]),
		decided_by: 1n,
		created_at: new Date('2026-10-14T10:00:00.000Z'),
		status: 'active',
		end_outcome: null,
		ended_at: null,
		ended_by: null,
		end_statement: null,
		notice_status: 'pending',
		notice_channels: null,
		notice_locale: null,
		notice_text: null,
		notice_sent_at: null,
		...overrides,
	};
}

const evidenceMessage: IARMessageContextRow = {
	message_id: 30n,
	channel_id: 20n,
	author_id: 40n,
	webhook_id: null,
	author_username: 'target',
	author_discriminator: 1,
	author_avatar_hash: null,
	content: 'buy now',
	timestamp: new Date('2026-10-01T10:00:00.000Z'),
	edited_timestamp: null,
	type: 0,
	flags: 0,
	mention_everyone: false,
	mention_users: null,
	mention_roles: null,
	mention_channels: null,
	attachments: null,
	embeds: null,
	sticker_items: null,
	missing_attachments: null,
};

async function create(
	repository: EnforcementDecisionRepository,
	decision: EnforcementDecisionRow,
	withEvidence = true,
) {
	await repository.create({
		decision,
		evidence: withEvidence
			? {
					decision_id: decision.decision_id,
					messages: [evidenceMessage],
					message_locations: new Map([[evidenceMessage.message_id, 'channel' as const]]),
					profile_values: null,
				}
			: null,
		firstNote: {
			decision_id: decision.decision_id,
			note_id: decision.decision_id + 1n,
			author_id: 1n,
			body: 'Internal note',
			created_at: decision.created_at,
		},
	});
}

function describeRepository(backend: string, setup: () => Promise<void>, teardown: () => Promise<void>) {
	describe(`EnforcementDecisionRepository on ${backend}`, () => {
		const repository = new EnforcementDecisionRepository();

		beforeEach(setup);
		afterAll(teardown);

		it('round-trips a decision with its subject index, evidence and first note', async () => {
			await create(repository, decisionRow(100n));
			expect(await repository.get(100n)).toEqual(decisionRow(100n));
			expect((await repository.getEvidence(100n))?.messages).toEqual([evidenceMessage]);
			expect((await repository.listNotes(100n)).map((note) => note.body)).toEqual(['Internal note']);
			expect(await repository.getEvidence(101n)).toBeNull();
			expect(await repository.get(101n)).toBeNull();
		});

		it('lists a subject newest first and pages before a decision', async () => {
			for (const id of [200n, 210n, 220n]) {
				await create(repository, decisionRow(id), false);
			}
			await create(repository, decisionRow(230n, {subject_id: 41n}), false);
			const ids = async (limit: number, before?: bigint) =>
				(await repository.listBySubject('user', 40n, limit, before)).map((row) => row.decision_id);
			expect(await ids(10)).toEqual([220n, 210n, 200n]);
			expect(await ids(2)).toEqual([220n, 210n]);
			expect(await ids(10, 220n)).toEqual([210n, 200n]);
			expect(await ids(10, 200n)).toEqual([]);
			expect((await repository.getMany([210n, 999n, 200n])).map((row) => row.decision_id)).toEqual([210n, 200n]);
		});

		it('records the notice, notes and the end', async () => {
			await create(repository, decisionRow(300n));
			const sentAt = new Date('2026-10-14T10:01:00.000Z');
			await repository.setNotice(300n, {
				status: 'sent',
				channels: new Set(['email']),
				locale: 'sv-SE',
				text: 'Hello',
				sentAt,
			});
			await repository.addNote({decision_id: 300n, note_id: 305n, author_id: 2n, body: 'Second', created_at: sentAt});
			await repository.end(300n, {outcome: 'reversed', endedAt: sentAt, endedBy: 2n, statement: null});
			expect(await repository.get(300n)).toEqual(
				decisionRow(300n, {
					notice_status: 'sent',
					notice_channels: new Set(['email']),
					notice_locale: 'sv-SE',
					notice_text: 'Hello',
					notice_sent_at: sentAt,
					status: 'ended',
					end_outcome: 'reversed',
					ended_at: sentAt,
					ended_by: 2n,
				}),
			);
			expect((await repository.listNotes(300n)).map((note) => note.body)).toEqual(['Internal note', 'Second']);
		});

		it('deletes every row it created', async () => {
			const decision = decisionRow(400n);
			await create(repository, decision);
			await repository.delete(decision);
			expect(await repository.get(400n)).toBeNull();
			expect(await repository.getEvidence(400n)).toBeNull();
			expect(await repository.listNotes(400n)).toEqual([]);
			expect(await repository.listBySubject('user', 40n, 10)).toEqual([]);
		});
	});
}

describeRepository(
	'the in-memory executor',
	async () => {
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
	},
	async () => {
		setCassandraQueryExecutorForTesting(null);
	},
);

describe.skipIf(!dockerAvailable)('Postgres KV', () => {
	let raw: IPostgresClient;
	let container: string;

	beforeAll(async () => {
		const started = await startPostgresKv({namePrefix: 'fluxer-enforcementkv', kvTable: KV_TABLE, maxConnections: 8});
		container = started.container;
		raw = started.client;
	}, 900_000);

	afterAll(async () => {
		setCassandraQueryExecutorForTesting(null);
		await stopPostgresKv(container);
	});

	describeRepository(
		'the Postgres KV backend',
		async () => {
			setCassandraQueryExecutorForTesting(new PostgresKvQueryExecutor(raw));
			await raw.query(`DELETE FROM ${KV_TABLE}`);
		},
		async () => {},
	);

	it('gives evidence and notes a one year expiry and keeps decisions', async () => {
		setCassandraQueryExecutorForTesting(new PostgresKvQueryExecutor(raw));
		await raw.query(`DELETE FROM ${KV_TABLE}`);
		await create(new EnforcementDecisionRepository(), decisionRow(500n));
		const rows = await raw.query<{table_name: string; expires_at: Date | null}>(
			`SELECT table_name, expires_at FROM ${KV_TABLE}`,
		);
		const expiry = (table: string) => rows.rows.filter((row) => row.table_name === table).map((row) => row.expires_at);
		for (const table of [EnforcementDecisionEvidence.name, EnforcementDecisionNotes.name]) {
			const [expiresAt] = expiry(table);
			const remaining = (expiresAt!.getTime() - Date.now()) / 1000;
			expect(remaining).toBeGreaterThan(seconds('365 days') - 60);
			expect(remaining).toBeLessThanOrEqual(seconds('365 days'));
		}
		expect(expiry(EnforcementDecisions.name)).toEqual([null]);
		expect(expiry(EnforcementDecisionsBySubject.name)).toEqual([null]);
	});
});
