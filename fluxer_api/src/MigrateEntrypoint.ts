// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	MIGRATE_EXIT,
	type MigrateExitCode,
	type MigrationOutput,
	MigrationUsageError,
} from '@app/api/migrate/MigrationCli';
import {
	parseS3FilesystemArgs,
	runS3FilesystemMigration,
	STORAGE_MIGRATION_USAGE,
} from '@app/api/migrate/S3FilesystemMigration';
import {
	KV_MIGRATION_USAGE,
	parseValkeyPostgresArgs,
	runValkeyPostgresMigration,
} from '@app/api/migrate/ValkeyPostgresMigration';

const USAGE = `usage: MigrateEntrypoint <kv|storage> [flags]

${KV_MIGRATION_USAGE}

${STORAGE_MIGRATION_USAGE}`;

async function initialize(): Promise<void> {
	const [{initializeConfig}, {initializeLogger}, {Config}, {Logger}] = await Promise.all([
		import('@app/api/Config'),
		import('@app/api/Logger'),
		import('@app/Config'),
		import('@app/Logger'),
	]);
	initializeConfig(Config);
	initializeLogger(Logger);
}

const output: MigrationOutput = {
	info: (message) => process.stderr.write(`${message}\n`),
	report: (report) => process.stdout.write(`${JSON.stringify(report)}\n`),
};

async function main(argv: ReadonlyArray<string>): Promise<MigrateExitCode> {
	const [command, ...rest] = argv;
	try {
		switch (command) {
			case 'kv': {
				const options = parseValkeyPostgresArgs(rest);
				await initialize();
				const report = await runValkeyPostgresMigration(options, output);
				output.report(report);
				return report.verified ? MIGRATE_EXIT.ok : MIGRATE_EXIT.mismatch;
			}
			case 'storage': {
				const options = parseS3FilesystemArgs(rest, process.env);
				await initialize();
				const report = await runS3FilesystemMigration(options, output);
				output.report(report);
				return report.verified ? MIGRATE_EXIT.ok : MIGRATE_EXIT.mismatch;
			}
			default:
				process.stderr.write(`${USAGE}\n`);
				return MIGRATE_EXIT.usage;
		}
	} catch (error) {
		if (error instanceof MigrationUsageError) {
			process.stderr.write(`${error.message}\n\n${USAGE}\n`);
			return MIGRATE_EXIT.usage;
		}
		process.stderr.write(`${error instanceof Error ? error.message : String(error)}\n`);
		return MIGRATE_EXIT.failed;
	}
}

process.exitCode = await main(process.argv.slice(2));
