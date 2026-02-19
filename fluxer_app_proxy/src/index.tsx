/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Config} from '@app/Config';
import {shutdownInstrumentation} from '@app/Instrument';
import {Logger} from '@app/Logger';
import {createAppProxyApp} from '@fluxer/app_proxy/src/App';
import {buildFluxerCSPOptions} from '@fluxer/app_proxy/src/app_server/utils/CSP';
import {createServiceTelemetry} from '@fluxer/hono/src/middleware/TelemetryAdapters';
import {createServer, setupGracefulShutdown} from '@fluxer/hono/src/Server';

const telemetry = createServiceTelemetry({
	serviceName: 'fluxer-app-proxy',
	skipPaths: ['/_health'],
});

async function main(): Promise<void> {
	const cspDirectives = buildFluxerCSPOptions({sentryDsn: Config.sentry_dsn});

	const {app, shutdown} = await createAppProxyApp({
		config: Config,
		cspDirectives,
		logger: Logger,
		metricsCollector: telemetry.metricsCollector,
		staticCDNEndpoint: Config.static_cdn_endpoint,
		staticDir: Config.assets_dir,
		tracing: telemetry.tracing,
	});

	const port = Config.port;
	Logger.info({port}, 'Starting Fluxer App Proxy');

	const server = createServer(app, {port});

	setupGracefulShutdown(
		async () => {
			await shutdown();
			await shutdownInstrumentation();
			await new Promise<void>((resolve) => {
				server.close(() => resolve());
			});
		},
		{logger: Logger},
	);
}

main().catch((err) => {
	Logger.fatal({error: err}, 'fatal error');
	process.exit(1);
});
