// SPDX-License-Identifier: AGPL-3.0-or-later

import {parseArgs} from 'node:util';
import {MigrationUsageError, positiveInteger} from '@app/api/migrate/MigrationCli';

const USAGE = `usage: CaptureEntrypoint <replay|settle> [flags]

replay [--consumer lim1-replay] [--images-consumer lim1-settle] [--port 9101]
  Applies captured Cassandra writes and settle images to the Cassandra in FLUXER_CASSANDRA_*.

settle [--final] [--verify <keys>]
  Reads settle keys from the Cassandra in FLUXER_CASSANDRA_* and publishes their images.`;

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
