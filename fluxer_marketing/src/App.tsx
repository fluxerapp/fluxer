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
import {Logger} from '@app/Logger';
import type {ICacheService} from '@fluxer/cache/src/ICacheService';
import {KVCacheProvider} from '@fluxer/cache/src/providers/KVCacheProvider';
import {createServiceTelemetry} from '@fluxer/hono/src/middleware/TelemetryAdapters';
import type {IKVProvider} from '@fluxer/kv_client/src/IKVProvider';
import {KVClient} from '@fluxer/kv_client/src/KVClient';
import {createMarketingApp} from '@fluxer/marketing/src/App';
import {resolveMarketingPublicDir} from '@fluxer/marketing/src/PublicDir';
import {throwKVRequiredError} from '@fluxer/rate_limit/src/KVRequiredError';
import {RateLimitService} from '@fluxer/rate_limit/src/RateLimitService';
import type {Context, Hono} from 'hono';

export interface MarketingApp {
	app: Hono;
	shutdown: () => void;
}

export function createApp(): MarketingApp {
	let rateLimitService: RateLimitService | null = null;

	if (Config.rateLimit) {
		if (!Config.kvUrl) {
			throwKVRequiredError({
				serviceName: 'fluxer_marketing',
				configPath: 'Config.kvUrl',
			});
		}

		const kvProvider = createKVProvider(Config.kvUrl, Logger);
		const cacheService: ICacheService = new KVCacheProvider({client: kvProvider});
		rateLimitService = new RateLimitService(cacheService);
	}

	const telemetry = createServiceTelemetry({
		serviceName: 'fluxer-marketing',
		skipPaths: ['/_health', '/static'],
	});

	const {app, shutdown} = createMarketingApp({
		config: {
			env: Config.env,
			port: Config.port,
			host: Config.host,
			secretKeyBase: Config.secretKeyBase,
			basePath: Config.basePath,
			apiEndpoint: Config.apiEndpoint,
			appEndpoint: Config.appEndpoint,
			staticCdnEndpoint: Config.staticCdnEndpoint,
			marketingEndpoint: Config.marketingEndpoint,
			geoipDbPath: Config.geoipDbPath,
			trustCfConnectingIp: Config.trustCfConnectingIp,
			releaseChannel: Config.releaseChannel,
			buildTimestamp: Config.buildTimestamp,
			rateLimit: Config.rateLimit,
		},
		logger: Logger,
		publicDir: resolveMarketingPublicDir(),
		rateLimitService: rateLimitService ?? undefined,
		metricsCollector: telemetry.metricsCollector,
		tracing: telemetry.tracing,
	});

	app.get('/_health', (c: Context) => c.json({status: 'ok'}));

	return {app, shutdown};
}

function createKVProvider(kvUrl: string, _logger: typeof Logger): IKVProvider {
	return new KVClient({url: kvUrl});
}
