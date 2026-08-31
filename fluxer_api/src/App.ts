// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAPIApp} from '@app/api/App';
import {buildAPIServerOptions, initializeConfig} from '@app/api/Config';
import {initializeLogger} from '@app/api/Logger';
import {Config} from '@app/Config';
import {shutdownInstrumentation} from '@app/Instrument';
import {Logger} from '@app/Logger';
import {createServer, setupGracefulShutdown} from '@fluxer/hono/src/Server';

async function closeHttpServer(server: {close: (callback: (error?: Error) => void) => void}): Promise<void> {
	await new Promise<void>((resolve, reject) => {
		server.close((error) => {
			if (error !== undefined) {
				reject(error);
				return;
			}
			resolve();
		});
	});
}

async function main(): Promise<void> {
	initializeConfig(Config);
	initializeLogger(Logger);
	const {app, initialize, shutdown} = await createAPIApp({
		config: Config,
		logger: Logger,
	});
	await initialize();
	process.on('uncaughtException', (error) => {
		Logger.error({error}, 'Uncaught exception');
	});
	process.on('unhandledRejection', (reason) => {
		Logger.error({reason}, 'Unhandled rejection (suppressed)');
	});
	const server = createServer(app, buildAPIServerOptions(Config));
	Logger.info({port: Config.port}, `Starting Fluxer API on port ${Config.port}`);
	setupGracefulShutdown(
		async () => {
			await closeHttpServer(server);
			await shutdown();
			await shutdownInstrumentation();
		},
		{logger: Logger, timeoutMs: 30000},
	);
}

main().catch((err) => {
	Logger.fatal({error: err}, 'Failed to start Fluxer API');
	process.exit(1);
});
