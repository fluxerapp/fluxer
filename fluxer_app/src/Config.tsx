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

import * as v from 'valibot';

const envSchema = v.object({
	PUBLIC_BUILD_SHA: v.optional(v.string(), 'dev'),
	PUBLIC_BUILD_NUMBER: v.optional(v.pipe(v.string(), v.transform(Number), v.number()), '0'),
	PUBLIC_BUILD_TIMESTAMP: v.optional(
		v.pipe(v.string(), v.transform(Number), v.number()),
		`${Math.floor(Date.now() / 1000)}`,
	),
	PUBLIC_PROJECT_ENV: v.optional(v.picklist(['stable', 'canary', 'development']), 'development'),
	PUBLIC_SENTRY_DSN: v.optional(v.nullable(v.string()), null),
	PUBLIC_SENTRY_PROJECT_ID: v.optional(v.nullable(v.string()), null),
	PUBLIC_SENTRY_PUBLIC_KEY: v.optional(v.nullable(v.string()), null),
	PUBLIC_SENTRY_PROXY_PATH: v.optional(v.string(), '/error-reporting-proxy'),
	PUBLIC_API_VERSION: v.optional(v.pipe(v.string(), v.transform(Number), v.number()), '1'),
	PUBLIC_BOOTSTRAP_API_ENDPOINT: v.optional(v.string(), '/api'),
	PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT: v.optional(v.string()),
});

const env = v.parse(envSchema, {
	PUBLIC_BUILD_SHA: import.meta.env.PUBLIC_BUILD_SHA,
	PUBLIC_BUILD_NUMBER: import.meta.env.PUBLIC_BUILD_NUMBER,
	PUBLIC_BUILD_TIMESTAMP: import.meta.env.PUBLIC_BUILD_TIMESTAMP,
	PUBLIC_PROJECT_ENV: import.meta.env.PUBLIC_PROJECT_ENV,
	PUBLIC_SENTRY_DSN: import.meta.env.PUBLIC_SENTRY_DSN,
	PUBLIC_SENTRY_PROJECT_ID: import.meta.env.PUBLIC_SENTRY_PROJECT_ID,
	PUBLIC_SENTRY_PUBLIC_KEY: import.meta.env.PUBLIC_SENTRY_PUBLIC_KEY,
	PUBLIC_SENTRY_PROXY_PATH: import.meta.env.PUBLIC_SENTRY_PROXY_PATH,
	PUBLIC_API_VERSION: import.meta.env.PUBLIC_API_VERSION,
	PUBLIC_BOOTSTRAP_API_ENDPOINT: import.meta.env.PUBLIC_BOOTSTRAP_API_ENDPOINT,
	PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT: import.meta.env.PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT,
});

export default {
	PUBLIC_BUILD_SHA: env.PUBLIC_BUILD_SHA,
	PUBLIC_BUILD_NUMBER: env.PUBLIC_BUILD_NUMBER,
	PUBLIC_BUILD_TIMESTAMP: env.PUBLIC_BUILD_TIMESTAMP,
	PUBLIC_PROJECT_ENV: env.PUBLIC_PROJECT_ENV,
	PUBLIC_SENTRY_DSN: env.PUBLIC_SENTRY_DSN,
	PUBLIC_SENTRY_PROJECT_ID: env.PUBLIC_SENTRY_PROJECT_ID,
	PUBLIC_SENTRY_PUBLIC_KEY: env.PUBLIC_SENTRY_PUBLIC_KEY,
	PUBLIC_SENTRY_PROXY_PATH: env.PUBLIC_SENTRY_PROXY_PATH,
	PUBLIC_API_VERSION: env.PUBLIC_API_VERSION,
	PUBLIC_BOOTSTRAP_API_ENDPOINT: env.PUBLIC_BOOTSTRAP_API_ENDPOINT,
	PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT: env.PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT ?? env.PUBLIC_BOOTSTRAP_API_ENDPOINT,
};
