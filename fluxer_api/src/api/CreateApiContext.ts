// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext, ApiServices, RequestScope} from '@app/api/ApiContext';
import {Config} from '@app/api/Config';
import {
	getGatewayService,
	getKVClient,
	getMediaService,
	getSnowflakeService,
	getWorkerService,
} from '@app/api/middleware/ServiceRegistry';
import {
	getBotMfaMirrorService,
	getCacheService,
	getContactChangeLogService,
	getEmailDnsValidationService,
	getEmailService,
	getRateLimitService,
	getUserActivityBuffer,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';

let cachedServices: ApiServices | null = null;
let cachedConfigRef: typeof Config | null = null;

function buildApiServices(): ApiServices {
	if (cachedServices && cachedConfigRef === Config) {
		return cachedServices;
	}
	cachedServices = {
		users: getUserRepository(),
		cache: getCacheService(),
		gateway: getGatewayService(),
		kv: getKVClient(),
		media: getMediaService(),
		email: getEmailService(),
		emailDnsValidation: getEmailDnsValidationService(),
		worker: getWorkerService(),
		snowflake: getSnowflakeService(),
		rateLimit: getRateLimitService(),
		contactChangeLog: getContactChangeLogService(),
		botMfaMirror: getBotMfaMirrorService(),
		userActivityBuffer: getUserActivityBuffer(),
		config: Config,
	};
	cachedConfigRef = Config;
	return cachedServices;
}

export function resetApiServicesForTesting(): void {
	cachedServices = null;
	cachedConfigRef = null;
}

const ANONYMOUS_REQUEST_SCOPE: RequestScope = {
	requestId: 'anonymous',
	clientIp: null,
	userAgent: null,
};

export function createApiContext(scope: RequestScope = ANONYMOUS_REQUEST_SCOPE): ApiContext {
	return {
		services: buildApiServices(),
		request: scope,
	};
}
