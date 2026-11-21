// SPDX-License-Identifier: AGPL-3.0-or-later

import {parseArgs} from 'node:util';
import {MigrationUsageError, positiveInteger} from '@app/api/migrate/MigrationCli';

const USAGE = `usage: CaptureEntrypoint <replay|settle|cdc> [flags]

replay [--consumer lim1-replay] [--images-consumer lim1-settle] [--port 9101]
  Applies captured Cassandra writes and settle images to the Cassandra in FLUXER_CASSANDRA_*.

settle [--final] [--verify <keys>]
  Reads settle keys from the Cassandra in FLUXER_CASSANDRA_* and publishes their images.

cdc [setup|check|compare|repair|views] [flags], configured only by CDC_* variables
  cdc                        Applies CASSANDRA_CDC to ClickHouse, bootstrapping fluxer_cdc on its first run.
  cdc setup                  Creates the stream and the consumer, or prints how existing ones differ.
  cdc check                  Prints how the stream and the consumer differ, creating nothing.
  cdc compare --since <time> [--sync-database fluxer] [--sample 2000]
                             Compares the CDC views with the sync.py tables written after <time>.
  cdc repair --all | <table>...
                             Marks tables for a repair at bootstrap pace.
  cdc views --database <db> [--replace-tables]
                             Creates the views of every CDC table in <db>.`;

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

async function runCdcCommand(argv: ReadonlyArray<string>): Promise<number> {
	const [first, ...flags] = argv;
	const command = first === undefined || first.startsWith('-') ? 'run' : first;
	const args = command === 'run' ? [...argv] : flags;
	const [{loadCdcConfig, loadCdcNatsConfig}, cdc] = await Promise.all([
		import('@app/api/capture/cdc/CdcConfig'),
		import('@app/api/capture/cdc/CdcMain'),
	]);
	if (command === 'setup' || command === 'check') {
		parseArgs({args, strict: true, options: {}});
		return cdc.runCdcSetup(loadCdcNatsConfig(process.env), command === 'setup');
	}
	const config = loadCdcConfig(process.env);
	switch (command) {
		case 'run':
			parseArgs({args, strict: true, options: {}});
			return cdc.runCdc(config);
		case 'compare': {
			const {values} = parseArgs({
				args,
				strict: true,
				options: {
					since: {type: 'string'},
					'sync-database': {type: 'string', default: 'fluxer'},
					sample: {type: 'string'},
				},
			});
			const sinceMs = Date.parse(values.since ?? '');
			if (!Number.isFinite(sinceMs)) throw new MigrationUsageError('--since needs the start time of a sync.py run');
			return cdc.runCdcCompare(config, {
				syncDatabase: values['sync-database'],
				sinceUs: sinceMs * 1000,
				sample: positiveInteger('sample', values.sample, 2000),
			});
		}
		case 'repair': {
			const {values, positionals} = parseArgs({
				args,
				strict: true,
				allowPositionals: true,
				options: {all: {type: 'boolean', default: false}},
			});
			if (values.all === positionals.length > 0) throw new MigrationUsageError('pass --all or table names');
			return cdc.runCdcRepairMark(config, values.all ? 'all' : positionals);
		}
		case 'views': {
			const {values} = parseArgs({
				args,
				strict: true,
				options: {database: {type: 'string'}, 'replace-tables': {type: 'boolean', default: false}},
			});
			if (values.database === undefined) throw new MigrationUsageError('--database is required');
			return cdc.runCdcViews(config, values.database, values['replace-tables']);
		}
		default:
			throw new MigrationUsageError(`unknown cdc command ${command}`);
	}
}

async function main(argv: ReadonlyArray<string>): Promise<number> {
	const [command, ...rest] = argv;
	try {
		switch (command) {
			case 'replay': {
				const {values} = parseArgs({
					args: rest,
					strict: true,
					options: {
						consumer: {type: 'string', default: 'lim1-replay'},
						'images-consumer': {type: 'string', default: 'lim1-settle'},
						port: {type: 'string'},
					},
				});
				await initialize();
				const {runReplay} = await import('@app/api/capture/ReplayMain');
				await runReplay({
					consumer: values.consumer,
					imagesConsumer: values['images-consumer'],
					port: positiveInteger('port', values.port, 9101),
				});
				return 0;
			}
			case 'settle': {
				const {values} = parseArgs({
					args: rest,
					strict: true,
					options: {final: {type: 'boolean', default: false}, verify: {type: 'string'}},
				});
				await initialize();
				const {runSettle} = await import('@app/api/capture/SettleMain');
				await runSettle({
					final: values.final,
					verify: values.verify === undefined ? null : positiveInteger('verify', values.verify, 1),
				});
				return 0;
			}
			case 'cdc':
				return await runCdcCommand(rest);
			default:
				process.stderr.write(`${USAGE}\n`);
				return 2;
		}
	} catch (error) {
		const usage = error instanceof MigrationUsageError || (error as {code?: string}).code?.startsWith('ERR_PARSE_ARGS');
		process.stderr.write(`${error instanceof Error ? error.message : String(error)}\n${usage ? `\n${USAGE}\n` : ''}`);
		return usage ? 2 : 1;
	}
}

process.exitCode = await main(process.argv.slice(2));
