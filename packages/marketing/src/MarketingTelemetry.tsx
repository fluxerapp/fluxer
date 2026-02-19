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

import type {LocaleCode} from '@fluxer/constants/src/Locales';
import {parseSession} from '@fluxer/hono/src/Session';
import {getLocaleFromCode, parseAcceptLanguage} from '@fluxer/locale/src/LocaleService';
import {detectArchitecture, detectPlatform} from '@fluxer/marketing/src/DeviceUtils';
import {getCountryCode} from '@fluxer/marketing/src/GeoIp';
import type {MarketingConfig} from '@fluxer/marketing/src/MarketingConfig';
import type {MarketingArchitecture, MarketingPlatform} from '@fluxer/marketing/src/MarketingContext';
import {recordCounter, recordHistogram} from '@fluxer/telemetry/src/Metrics';
import type {Context, MiddlewareHandler} from 'hono';
import {getCookie} from 'hono/cookie';
import {createMiddleware} from 'hono/factory';

export interface MarketingRequestInfo {
	locale: LocaleCode;
	platform: MarketingPlatform;
	architecture: MarketingArchitecture;
	countryCode: string;
	referrerDomain: string;
}

const LOCALE_COOKIE_MAX_AGE_SECONDS = 60 * 60 * 24 * 365;
const REQUEST_INFO_KEY = 'marketing.requestInfo';

interface LocaleCookieSession {
	locale: string;
}

export async function getMarketingRequestInfo(c: Context, config: MarketingConfig): Promise<MarketingRequestInfo> {
	const existing = c.get(REQUEST_INFO_KEY);
	if (isMarketingRequestInfo(existing)) {
		return existing;
	}

	const locale = getRequestLocale(c, config);
	const userAgent = c.req.header('user-agent') ?? '';
	const platform = detectPlatform(userAgent);
	const architecture = detectArchitecture(userAgent, platform);
	const countryCode = await getCountryCode(c.req.raw, {
		geoipDbPath: config.geoipDbPath,
		trustCfConnectingIp: config.trustCfConnectingIp,
	});
	const referrerDomain = extractReferrerDomain(c.req.header('referer') ?? c.req.header('referrer'));

	const info = {
		locale,
		platform,
		architecture,
		countryCode,
		referrerDomain,
	};
	c.set(REQUEST_INFO_KEY, info);
	return info;
}

export function createMarketingMetricsMiddleware(config: MarketingConfig): MiddlewareHandler {
	return createMiddleware(async (c, next) => {
		const startTime = Date.now();
		const method = c.req.method;
		const path = c.req.path;

		await next();

		const durationMs = Date.now() - startTime;
		const status = c.res.status;
		const requestInfo = await getMarketingRequestInfo(c, config);

		const dimensions: Record<string, string> = {
			method,
			path,
			status: status.toString(),
			release_channel: config.releaseChannel,
			base_path: config.basePath,
			platform: requestInfo.platform,
			architecture: requestInfo.architecture,
			country_code: requestInfo.countryCode,
			locale: requestInfo.locale,
			referrer_domain: requestInfo.referrerDomain,
		};

		recordHistogram({
			name: 'marketing.request.latency',
			valueMs: durationMs,
			dimensions,
		});

		recordCounter({
			name: 'marketing.request.count',
			value: 1,
			dimensions,
		});

		recordCounter({
			name: 'marketing.request.outcome',
			value: 1,
			dimensions: {
				...dimensions,
				outcome: status >= 400 ? 'failure' : 'success',
			},
		});
	});
}

function getRequestLocale(c: Context, config: MarketingConfig): LocaleCode {
	const localeCookie = getCookie(c, 'locale');
	if (localeCookie) {
		const session = parseLocaleCookie(localeCookie, config.secretKeyBase) ?? localeCookie;
		const locale = getLocaleFromCode(session);
		if (locale) return locale;
	}

	const header = c.req.header('accept-language');
	if (header) {
		return parseAcceptLanguage(header);
	}

	return 'en-US';
}

function parseLocaleCookie(cookieValue: string, secretKeyBase: string): string | null {
	const session = parseSession<LocaleCookieSession>(cookieValue, secretKeyBase, LOCALE_COOKIE_MAX_AGE_SECONDS);
	return session?.locale ?? null;
}

function extractReferrerDomain(value?: string): string {
	if (!value) return 'direct';
	try {
		const url = new URL(value);
		return url.hostname || 'unknown';
	} catch {
		return 'unknown';
	}
}

function isMarketingRequestInfo(value: unknown): value is MarketingRequestInfo {
	if (!value || typeof value !== 'object') return false;
	const record = value as Record<string, unknown>;
	return (
		typeof record['locale'] === 'string' &&
		typeof record['platform'] === 'string' &&
		typeof record['architecture'] === 'string' &&
		typeof record['countryCode'] === 'string' &&
		typeof record['referrerDomain'] === 'string'
	);
}
