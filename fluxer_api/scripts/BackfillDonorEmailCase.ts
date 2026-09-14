// SPDX-License-Identifier: AGPL-3.0-or-later

import {initializeConfig} from '@app/api/Config';
import {
	BatchBuilder,
	fetchOne,
	fetchPage,
	type PagedQueryResult,
	setDatabaseQueryExecutor,
	upsertOne,
} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import {ensurePostgresKvSchema, PostgresKvQueryExecutor} from '@app/api/database/PostgresKvQueryExecutor';
import type {
	DonorByStripeCustomerIdRow,
	DonorByStripeSubscriptionIdRow,
	DonorMagicLinkTokenByEmailRow,
	DonorMagicLinkTokenRow,
	DonorRow,
} from '@app/api/database/types/DonationTypes';
import {
	DonorMagicLinkTokens,
	DonorMagicLinkTokensByEmail,
	Donors,
	DonorsByStripeCustomerId,
	DonorsByStripeSubscriptionId,
} from '@app/api/donation/DonationTables';
import {initializeLogger} from '@app/api/Logger';
import {Config} from '@app/Config';
import {Logger} from '@app/Logger';
import {BACKGROUND_READ_TIMEOUT_MS, initCassandra, shutdownCassandra} from '@pkgs/cassandra/src/Client';
import {getDefaultPostgresClient, initPostgres, shutdownPostgres} from '@pkgs/postgres/src/Client';

const PAGE_SIZE = 500;

const SCAN_DONORS_QUERY = Donors.selectCql({});
const FETCH_DONOR_BY_EMAIL_QUERY = Donors.selectCql({
	where: Donors.where.eq('email'),
	limit: 1,
});
const SCAN_DONORS_BY_STRIPE_CUSTOMER_ID_QUERY = DonorsByStripeCustomerId.selectCql({});
const SCAN_DONORS_BY_STRIPE_SUBSCRIPTION_ID_QUERY = DonorsByStripeSubscriptionId.selectCql({});
const SCAN_MAGIC_LINK_TOKENS_QUERY = DonorMagicLinkTokens.selectCql({});
const SCAN_MAGIC_LINK_TOKENS_BY_EMAIL_QUERY = DonorMagicLinkTokensByEmail.selectCql({});

function normalizeEmail(email: string): string {
	return email.trim().toLowerCase();
}

async function scanTable<Row>(query: string): Promise<Array<Row>> {
	const rows: Array<Row> = [];
	let pageState: string | null = null;
	do {
		const page: PagedQueryResult<Row> = await fetchPage<Row>(query, {}, {pageSize: PAGE_SIZE, pageState});
		rows.push(...page.rows);
		pageState = page.pageState;
	} while (pageState !== null);
	return rows;
}

async function backfillDonors(apply: boolean): Promise<Set<string>> {
	const rows = await scanTable<DonorRow>(SCAN_DONORS_QUERY);
	const conflicted = new Set<string>();
	let moved = 0;
	for (const row of rows) {
		const email = normalizeEmail(row.email);
		if (email === row.email) continue;
		const existing = await fetchOne<DonorRow>(FETCH_DONOR_BY_EMAIL_QUERY, {email});
		if (existing) {
			conflicted.add(row.email);
			Logger.warn(
				{email, original: row.email},
				'Donor already exists under the normalized email, leaving both rows and every row that points at them in place',
			);
			continue;
		}
		if (apply) {
			const batch = new BatchBuilder();
			batch.addPrepared(Donors.upsertAll({...row, email}));
			batch.addPrepared(Donors.deleteByPk({email: row.email}));
			await batch.execute();
		}
		moved += 1;
	}
	Logger.info({scanned: rows.length, moved, conflicts: conflicted.size}, 'Donor rows processed');
	return conflicted;
}

async function backfillDonorsByStripeCustomerId(apply: boolean, conflicted: Set<string>): Promise<void> {
	const rows = await scanTable<DonorByStripeCustomerIdRow>(SCAN_DONORS_BY_STRIPE_CUSTOMER_ID_QUERY);
	let moved = 0;
	let skipped = 0;
	for (const row of rows) {
		const email = normalizeEmail(row.email);
		if (email === row.email) continue;
		if (conflicted.has(row.email)) {
			skipped += 1;
			continue;
		}
		if (apply) {
			const batch = new BatchBuilder();
			batch.addPrepared(DonorsByStripeCustomerId.upsertAll({stripe_customer_id: row.stripe_customer_id, email}));
			batch.addPrepared(
				DonorsByStripeCustomerId.deleteByPk({stripe_customer_id: row.stripe_customer_id, email: row.email}),
			);
			await batch.execute();
		}
		moved += 1;
	}
	Logger.info({scanned: rows.length, moved, skipped}, 'Donor stripe customer index rows processed');
}

async function backfillDonorsByStripeSubscriptionId(apply: boolean, conflicted: Set<string>): Promise<void> {
	const rows = await scanTable<DonorByStripeSubscriptionIdRow>(SCAN_DONORS_BY_STRIPE_SUBSCRIPTION_ID_QUERY);
	let moved = 0;
	let skipped = 0;
	for (const row of rows) {
		const email = normalizeEmail(row.email);
		if (email === row.email) continue;
		if (conflicted.has(row.email)) {
			skipped += 1;
			continue;
		}
		if (apply) {
			const batch = new BatchBuilder();
			batch.addPrepared(
				DonorsByStripeSubscriptionId.upsertAll({stripe_subscription_id: row.stripe_subscription_id, email}),
			);
			batch.addPrepared(
				DonorsByStripeSubscriptionId.deleteByPk({
					stripe_subscription_id: row.stripe_subscription_id,
					email: row.email,
				}),
			);
			await batch.execute();
		}
		moved += 1;
	}
	Logger.info({scanned: rows.length, moved, skipped}, 'Donor stripe subscription index rows processed');
}

async function backfillMagicLinkTokens(apply: boolean, conflicted: Set<string>): Promise<void> {
	const rows = await scanTable<DonorMagicLinkTokenRow>(SCAN_MAGIC_LINK_TOKENS_QUERY);
	let updated = 0;
	let skipped = 0;
	for (const row of rows) {
		const donorEmail = normalizeEmail(row.donor_email);
		if (donorEmail === row.donor_email) continue;
		if (conflicted.has(row.donor_email)) {
			skipped += 1;
			continue;
		}
		if (apply) {
			await upsertOne(DonorMagicLinkTokens.patchByPk({token_: row.token_}, {donor_email: Db.set(donorEmail)}));
		}
		updated += 1;
	}
	Logger.info({scanned: rows.length, updated, skipped}, 'Donor magic link token rows processed');
}

async function backfillMagicLinkTokensByEmail(apply: boolean, conflicted: Set<string>): Promise<void> {
	const rows = await scanTable<DonorMagicLinkTokenByEmailRow>(SCAN_MAGIC_LINK_TOKENS_BY_EMAIL_QUERY);
	let moved = 0;
	let skipped = 0;
	for (const row of rows) {
		const donorEmail = normalizeEmail(row.donor_email);
		if (donorEmail === row.donor_email) continue;
		if (conflicted.has(row.donor_email)) {
			skipped += 1;
			continue;
		}
		if (apply) {
			const batch = new BatchBuilder();
			batch.addPrepared(DonorMagicLinkTokensByEmail.upsertAll({donor_email: donorEmail, token_: row.token_}));
			batch.addPrepared(DonorMagicLinkTokensByEmail.deleteByPk({donor_email: row.donor_email, token_: row.token_}));
			await batch.execute();
		}
		moved += 1;
	}
	Logger.info({scanned: rows.length, moved, skipped}, 'Donor magic link token index rows processed');
}

async function main(): Promise<void> {
	initializeConfig(Config);
	initializeLogger(Logger);
	const apply = process.argv.includes('--apply');
	let cassandraInitialized = false;
	let postgresInitialized = false;
	if (Config.database.backend === 'postgres') {
		await initPostgres(Config.postgres, (diagnostic) => Logger.error({diagnostic}, 'Postgres connection error'));
		postgresInitialized = true;
		const postgres = getDefaultPostgresClient();
		await ensurePostgresKvSchema(postgres);
		setDatabaseQueryExecutor(new PostgresKvQueryExecutor(postgres));
	}
	if (Config.database.backend === 'cassandra') {
		await initCassandra({
			hosts: Config.cassandra.hosts.split(',').filter(Boolean),
			port: Config.cassandra.port,
			keyspace: Config.cassandra.keyspace,
			localDc: Config.cassandra.localDc,
			username: Config.cassandra.username || undefined,
			password: Config.cassandra.password || undefined,
			readTimeoutMs: BACKGROUND_READ_TIMEOUT_MS,
		});
		cassandraInitialized = true;
	}
	Logger.info({apply}, apply ? 'Backfilling donor email casing' : 'Inspecting donor email casing (dry run)');
	try {
		const conflicted = await backfillDonors(apply);
		await backfillDonorsByStripeCustomerId(apply, conflicted);
		await backfillDonorsByStripeSubscriptionId(apply, conflicted);
		await backfillMagicLinkTokens(apply, conflicted);
		await backfillMagicLinkTokensByEmail(apply, conflicted);
		if (conflicted.size > 0) {
			Logger.warn({addresses: [...conflicted]}, 'Addresses left untouched, merge them by hand before a rerun');
		}
	} finally {
		if (cassandraInitialized) {
			await shutdownCassandra();
		}
		if (postgresInitialized) {
			setDatabaseQueryExecutor(null);
			await shutdownPostgres();
		}
	}
}

main().catch((error) => {
	Logger.fatal({error}, 'Failed to backfill donor email casing');
	process.exit(1);
});
