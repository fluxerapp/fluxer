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

import type {AppProxyHonoEnv, CreateAppProxyAppOptions} from '@fluxer/app_proxy/src/AppProxyTypes';
import {applyAppProxyMiddleware} from '@fluxer/app_proxy/src/app_proxy/AppProxyMiddleware';
import {registerAppProxyRoutes} from '@fluxer/app_proxy/src/app_proxy/AppProxyRoutes';
import {Hono} from 'hono';

export interface AppProxyResult {
	app: Hono<AppProxyHonoEnv>;
	shutdown: () => Promise<void>;
}

export async function createAppProxyApp(options: CreateAppProxyAppOptions): Promise<AppProxyResult> {
	const {
		assetsPath = '/assets',
		cspDirectives,
		customMiddleware = [],
		logger,
		metricsCollector,
		staticCDNEndpoint,
		staticDir,
		tracing,
	} = options;

	const app = new Hono<AppProxyHonoEnv>({strict: true});

	applyAppProxyMiddleware({
		app,
		customMiddleware,
		logger,
		metricsCollector,
		tracing,
	});

	registerAppProxyRoutes({
		app,
		assetsPath,
		cspDirectives,
		logger,
		staticCDNEndpoint,
		staticDir,
	});

	const shutdown = async (): Promise<void> => {
		logger.info('App Proxy shutting down');
	};

	return {app, shutdown};
}
