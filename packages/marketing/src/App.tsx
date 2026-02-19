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

/** @jsxRuntime automatic */
/** @jsxImportSource hono/jsx */

import {resolve} from 'node:path';
import {fileURLToPath} from 'node:url';
import {normalizeEndpointOrigin, validateOutboundEndpointUrl} from '@fluxer/hono/src/security/OutboundEndpoint';
import type {MetricsCollector} from '@fluxer/hono_types/src/MetricsTypes';
import type {TracingOptions} from '@fluxer/hono_types/src/TracingTypes';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import {createMarketingContextFactory} from '@fluxer/marketing/src/app/MarketingContextFactory';
import {applyMarketingMiddlewareStack} from '@fluxer/marketing/src/app/MarketingMiddlewareStack';
import {registerMarketingRoutes} from '@fluxer/marketing/src/app/MarketingRouteRegistrar';
import {applyMarketingStaticAssets} from '@fluxer/marketing/src/app/MarketingStaticAssets';
import type {MarketingConfig} from '@fluxer/marketing/src/MarketingConfig';
import {createMarketingMetricsMiddleware} from '@fluxer/marketing/src/MarketingTelemetry';
import {initializeMarketingCsrf} from '@fluxer/marketing/src/middleware/Csrf';
import {normalizeBasePath} from '@fluxer/marketing/src/UrlUtils';
import type {IRateLimitService} from '@fluxer/rate_limit/src/IRateLimitService';
import {Hono} from 'hono';

export interface CreateMarketingAppOptions {
	config: MarketingConfig;
	logger: LoggerInterface;
	publicDir?: string;
	rateLimitService?: IRateLimitService | null;
	metricsCollector?: MetricsCollector;
	tracing?: TracingOptions;
}

export interface MarketingAppResult {
	app: Hono;
	shutdown: () => void;
}

export function createMarketingApp(options: CreateMarketingAppOptions): MarketingAppResult {
	const {logger, publicDir: publicDirOption, rateLimitService = null, metricsCollector, tracing} = options;

	const config = normalizeMarketingSecurityConfig(options.config);
	const publicDir = resolve(publicDirOption ?? fileURLToPath(new URL('../public', import.meta.url)));
	const app = new Hono();

	const contextFactory = createMarketingContextFactory({
		config,
		publicDir,
	});

	initializeMarketingCsrf(config.secretKeyBase, config.env === 'production');

	app.use('*', createMarketingMetricsMiddleware(config));

	applyMarketingStaticAssets({
		app,
		publicDir,
		basePath: config.basePath,
		logger,
	});

	applyMarketingMiddlewareStack({
		app,
		config,
		logger,
		rateLimitService,
		metricsCollector,
		tracing,
	});

	registerMarketingRoutes({
		app,
		config,
		contextFactory,
	});

	const shutdown = (): void => {
		logger.info('Marketing app shutting down');
	};

	return {app, shutdown};
}

function normalizeMarketingSecurityConfig(rawConfig: MarketingConfig): MarketingConfig {
	const basePath = normalizeBasePath(rawConfig.basePath);
	const isProduction = rawConfig.env === 'production';
	const apiEndpoint = validateOutboundEndpointUrl(rawConfig.apiEndpoint, {
		name: 'marketing.apiEndpoint',
		allowHttp: !isProduction,
		allowLocalhost: !isProduction,
		allowPrivateIpLiterals: !isProduction,
	});

	return {
		...rawConfig,
		basePath,
		apiEndpoint: normalizeEndpointOrigin(apiEndpoint),
	};
}
