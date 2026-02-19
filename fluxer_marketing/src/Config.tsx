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

import {loadConfig} from '@fluxer/config/src/ConfigLoader';
import {
	extractBaseServiceConfig,
	extractBuildInfoConfig,
	extractKVClientConfig,
} from '@fluxer/config/src/ServiceConfigSlices';
import {normalizeBasePath} from '@fluxer/marketing/src/UrlUtils';

const master = await loadConfig();
const marketingConfig = master.services.marketing;
if (!marketingConfig) {
	throw new Error('services.marketing configuration is required for standalone marketing service');
}

export const Config = {
	...extractBaseServiceConfig(master),
	...extractKVClientConfig(master),
	...extractBuildInfoConfig(),
	port: marketingConfig.port,
	host: marketingConfig.host,
	secretKeyBase: marketingConfig.secret_key_base,
	basePath: normalizeBasePath(marketingConfig.base_path),
	apiEndpoint: master.endpoints.api,
	appEndpoint: master.endpoints.app,
	staticCdnEndpoint: master.endpoints.static_cdn,
	marketingEndpoint: stripPath(master.endpoints.marketing),
	geoipDbPath: master.geoip.maxmind_db_path,
	trustCfConnectingIp: master.proxy.trust_cf_connecting_ip,
	rateLimit: null,
};

export type Config = typeof Config;

function stripPath(value: string): string {
	const url = new URL(value);
	url.pathname = '';
	url.search = '';
	url.hash = '';
	return url.toString().replace(/\/$/, '');
}
