// SPDX-License-Identifier: AGPL-3.0-or-later

import '@app/Instrument';
import {initializeConfig} from '@app/api/Config';
import {setDatabaseQueryExecutor} from '@app/api/database/CassandraQueryExecution';
import {ensurePostgresKvSchema, PostgresKvQueryExecutor} from '@app/api/database/PostgresKvQueryExecutor';
import {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {ExpressionAssetPurger} from '@app/api/guild/services/content/ExpressionAssetPurger';
import {AssetDeletionQueue} from '@app/api/infrastructure/AssetDeletionQueue';
import {initializeLogger} from '@app/api/Logger';
import {
	ExpressionPackResidueCleanup,
	formatExpressionPackResidueReport,
} from '@app/api/maintenance/ExpressionPackResidueCleanup';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {Config} from '@app/Config';
import {Logger} from '@app/Logger';
import {BACKGROUND_READ_TIMEOUT_MS, initCassandra, shutdownCassandra} from '@pkgs/cassandra/src/Client';
import {getDefaultPostgresClient, initPostgres, shutdownPostgres} from '@pkgs/postgres/src/Client';

const USAGE = `Usage: fluxer-api expression-pack-residue [--apply]

Deletes the guild_emojis and guild_stickers rows that the removed expression
packs feature wrote with guild_id set to a pack id, and queues their CDN and S3
assets for purge through the asset deletion queue.

Rows are only ever read from the guild_emojis and guild_stickers partitions
named by a pack id in expression_packs, so a real guild's expressions are never
read and never deleted. A pack id that also names a live guild is reported and
skipped. Running it again after a successful run is a no-op.

Options:
  --apply   Delete the rows and queue their assets. Without it the tool reports
            what it would delete and changes nothing.
  --help    Show this message.`;

interface Options {
	apply: boolean;
	help: boolean;
}

function parseArgs(): Options {
	const options: Options = {apply: false, help: false};
	for (const arg of process.argv.slice(2)) {
		if (arg === '--apply') {
			options.apply = true;
		} else if (arg === '--help' || arg === '-h') {
			options.help = true;
		} else {
			throw new Error(`Unknown argument "${arg}". Run with --help for usage.`);
		}
	}
	return options;
}

async function initializeDatabase(): Promise<() => Promise<void>> {
	if (Config.database.backend === 'postgres') {
		await initPostgres(Config.postgres);
		const postgres = getDefaultPostgresClient();
		await ensurePostgresKvSchema(postgres);
		setDatabaseQueryExecutor(new PostgresKvQueryExecutor(postgres));
		return async () => {
			setDatabaseQueryExecutor(null);
			await shutdownPostgres();
		};
	}
	await initCassandra({
		hosts: Config.cassandra.hosts.split(',').filter(Boolean),
		port: Config.cassandra.port,
		keyspace: Config.cassandra.keyspace,
		localDc: Config.cassandra.localDc,
		username: Config.cassandra.username || undefined,
		password: Config.cassandra.password || undefined,
		readTimeoutMs: BACKGROUND_READ_TIMEOUT_MS,
	});
	return async () => {
		await shutdownCassandra();
	};
}

async function main(): Promise<void> {
	const options = parseArgs();
	if (options.help) {
		console.log(USAGE);
		return;
	}
	initializeConfig(Config);
	initializeLogger(Logger);
	const shutdownDatabase = await initializeDatabase();
	const kvClient = getKVClient();
	try {
		const cleanup = new ExpressionPackResidueCleanup(
			new GuildRepository(),
			new ExpressionAssetPurger(new AssetDeletionQueue(kvClient)),
		);
		const report = await cleanup.run({apply: options.apply});
		console.log(formatExpressionPackResidueReport(report));
	} finally {
		await shutdownDatabase();
	}
}

main()
	.then(() => {
		process.exit(0);
	})
	.catch((error) => {
		Logger.fatal({error}, 'Expression pack residue cleanup failed');
		process.exit(1);
	});
