// SPDX-License-Identifier: AGPL-3.0-or-later

import Bowser from 'bowser';
import {Logger} from '../Logger';
import {parseJsonRecord} from './JsonBoundaryUtils';

export type SessionDeviceClass = 'mobile' | 'desktop';

interface SessionClientSignals {
	userAgent: string | null;
	reportedOs: string | null;
	productName: string;
}

export interface SessionClientInfo {
	platform: string | null;
	os: string | null;
	browser: string | null;
	device: SessionDeviceClass;
}

type OsToken = 'android' | 'ios' | 'macos' | 'windows' | 'linux';

const OS_DISPLAY: Record<OsToken, string> = {
	android: 'Android',
	ios: 'iOS',
	macos: 'macOS',
	windows: 'Windows',
	linux: 'Linux',
};

const BOWSER_OS_TO_TOKEN: Record<string, OsToken> = {
	macOS: 'macos',
	Windows: 'windows',
	Linux: 'linux',
	iOS: 'ios',
	Android: 'android',
};

const NATIVE_UA_REGEX = /^Fluxer (Android|iOS|Linux|Desktop|Client)(?=[/ ]|$)/;
const ELECTRON_UA_REGEX = /\bElectron\/\d+(?:\.\d+)*/;
const PRODUCT_TOKEN_OS: Record<string, OsToken | null> = {
	Android: 'android',
	iOS: 'ios',
	Linux: 'linux',
	Desktop: null,
	Client: null,
};
const CLIENT_PROPERTIES_HEADER_MAX_LENGTH = 4096;
const MOBILE_PLATFORM_TYPES = new Set(['mobile', 'tablet']);

function narrowOsToken(value: string | null): OsToken | null {
	if (value === null) return null;
	return Object.hasOwn(OS_DISPLAY, value) ? (value as OsToken) : null;
}

function parseUserAgent(userAgent: string): Bowser.Parser.Parser | null {
	try {
		return Bowser.getParser(userAgent);
	} catch (error) {
		Logger.warn({error}, 'Failed to parse user agent');
		return null;
	}
}

function bowserOsToken(userAgent: string): OsToken | null {
	if (!userAgent) return null;
	return narrowOsToken(BOWSER_OS_TO_TOKEN[parseUserAgent(userAgent)?.getOSName() ?? ''] ?? null);
}

export function isFluxerNativeUserAgent(userAgent: string | null): boolean {
	return NATIVE_UA_REGEX.test(userAgent?.trim() ?? '');
}

export function parseReportedClientOs(headerValue: string | null): OsToken | null {
	if (!headerValue) return null;
	const trimmed = headerValue.trim();
	if (!trimmed || trimmed.length > CLIENT_PROPERTIES_HEADER_MAX_LENGTH) return null;
	let decoded: string;
	try {
		decoded = Buffer.from(trimmed, 'base64').toString('utf8');
	} catch {
		return null;
	}
	const record = parseJsonRecord(decoded);
	if (!record) return null;
	const os = record.os;
	return typeof os === 'string' ? narrowOsToken(os) : null;
}

export function resolveSessionClientInfo({
	userAgent,
	reportedOs,
	productName,
}: SessionClientSignals): SessionClientInfo {
	const ua = userAgent?.trim() ?? '';
	const nativeMatch = NATIVE_UA_REGEX.exec(ua);

	if (nativeMatch) {
		const productToken = nativeMatch[1] as keyof typeof PRODUCT_TOKEN_OS;
		const osToken = narrowOsToken(reportedOs) ?? PRODUCT_TOKEN_OS[productToken] ?? bowserOsToken(ua);
		const osDisplay = osToken ? OS_DISPLAY[osToken] : null;
		const mobile = osToken === 'ios' || osToken === 'android';
		const platform = osDisplay
			? mobile
				? `${productName} ${osDisplay}`
				: `${productName} Lite ${osDisplay}`
			: `${productName} Lite`;
		return {platform, os: osDisplay, browser: null, device: mobile ? 'mobile' : 'desktop'};
	}

	if (ELECTRON_UA_REGEX.test(ua)) {
		const osToken = bowserOsToken(ua);
		const osDisplay = osToken ? OS_DISPLAY[osToken] : null;
		const mobile = osToken === 'ios' || osToken === 'android';
		return {
			platform: osDisplay ? `${productName} ${osDisplay}` : productName,
			os: osDisplay,
			browser: null,
			device: mobile ? 'mobile' : 'desktop',
		};
	}

	const parser = ua ? parseUserAgent(ua) : null;
	const browser = parser?.getBrowserName() || null;
	const os = parser?.getOSName() || null;
	const platformType = parser?.getPlatformType(true) ?? '';
	return {
		platform: browser ?? os,
		os,
		browser,
		device: MOBILE_PLATFORM_TYPES.has(platformType) ? 'mobile' : 'desktop',
	};
}
