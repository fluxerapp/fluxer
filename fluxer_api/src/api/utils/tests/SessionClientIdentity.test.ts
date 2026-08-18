// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {
	isFluxerNativeUserAgent,
	parseReportedClientOs,
	resolveSessionClientInfo,
	type SessionClientInfo,
} from '../SessionClientIdentity';

const ELECTRON_MAC_UA =
	'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) FluxerStable/2026.614.83512 Chrome/126.0.0.0 Electron/31.0.0 Safari/537.36';
const ELECTRON_WINDOWS_UA =
	'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) FluxerCanary/2026.614.83512 Chrome/126.0.0.0 Electron/31.0.0 Safari/537.36';
const CHROME_MAC_UA =
	'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36';
const SAFARI_IPHONE_UA =
	'Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Mobile/15E148 Safari/604.1';

const resolve = (userAgent: string | null, reportedOs: string | null, productName = 'Fluxer'): SessionClientInfo =>
	resolveSessionClientInfo({userAgent, reportedOs, productName});

describe('resolveSessionClientInfo', () => {
	it('labels the Flutter mobile clients by their operating system', () => {
		expect(resolve('Fluxer iOS/1.4.2 (stable)', 'ios')).toEqual({
			platform: 'Fluxer iOS',
			os: 'iOS',
			browser: null,
			device: 'mobile',
		});
		expect(resolve('Fluxer Android/1.4.2 (stable)', 'android')).toEqual({
			platform: 'Fluxer Android',
			os: 'Android',
			browser: null,
			device: 'mobile',
		});
	});

	it('accepts the versionless user agent emitted before package info loads', () => {
		expect(resolve('Fluxer iOS (stable)', 'ios')).toEqual({
			platform: 'Fluxer iOS',
			os: 'iOS',
			browser: null,
			device: 'mobile',
		});
	});

	it('labels Flutter desktop builds as Lite and distinguishes them by reported operating system', () => {
		expect(resolve('Fluxer Desktop/1.4.2 (stable)', 'macos')).toEqual({
			platform: 'Fluxer Lite macOS',
			os: 'macOS',
			browser: null,
			device: 'desktop',
		});
		expect(resolve('Fluxer Desktop/1.4.2 (stable)', 'windows')).toEqual({
			platform: 'Fluxer Lite Windows',
			os: 'Windows',
			browser: null,
			device: 'desktop',
		});
		expect(resolve('Fluxer Desktop/1.4.2 (stable)', 'linux')).toEqual({
			platform: 'Fluxer Lite Linux',
			os: 'Linux',
			browser: null,
			device: 'desktop',
		});
	});

	it('treats a narrow Linux window as a desktop because the product token cannot carry form factor', () => {
		expect(resolve('Fluxer Linux/1.4.2 (stable)', 'linux')).toEqual({
			platform: 'Fluxer Lite Linux',
			os: 'Linux',
			browser: null,
			device: 'desktop',
		});
	});

	it('resolves legacy rows that predate the reported operating system', () => {
		expect(resolve('Fluxer iOS/1.4.2 (stable)', null)).toEqual({
			platform: 'Fluxer iOS',
			os: 'iOS',
			browser: null,
			device: 'mobile',
		});
		expect(resolve('Fluxer Desktop/1.4.2 (stable)', null)).toEqual({
			platform: 'Fluxer Lite',
			os: null,
			browser: null,
			device: 'desktop',
		});
	});

	it('ignores a corrupt reported operating system instead of rendering it', () => {
		expect(resolve('Fluxer Desktop/1.4.2 (stable)', 'Windows 11')).toEqual({
			platform: 'Fluxer Lite',
			os: null,
			browser: null,
			device: 'desktop',
		});
	});

	it('labels the desktop application by operating system without naming a browser', () => {
		expect(resolve(ELECTRON_MAC_UA, null)).toEqual({
			platform: 'Fluxer macOS',
			os: 'macOS',
			browser: null,
			device: 'desktop',
		});
		expect(resolve(ELECTRON_WINDOWS_UA, null)).toEqual({
			platform: 'Fluxer Windows',
			os: 'Windows',
			browser: null,
			device: 'desktop',
		});
	});

	it('keeps browser sessions reporting their browser', () => {
		expect(resolve(CHROME_MAC_UA, null)).toEqual({
			platform: 'Chrome',
			os: 'macOS',
			browser: 'Chrome',
			device: 'desktop',
		});
		expect(resolve(SAFARI_IPHONE_UA, null)).toEqual({
			platform: 'Safari',
			os: 'iOS',
			browser: 'Safari',
			device: 'mobile',
		});
	});

	it('keeps the operating system when the browser cannot be named', () => {
		expect(resolve('SomeBot (Windows NT 10.0; Win64; x64)', null)).toEqual({
			platform: 'Windows',
			os: 'Windows',
			browser: null,
			device: 'desktop',
		});
	});

	it('rejects a product token that only prefixes a real one', () => {
		const resolved = resolve('Fluxer iOS-not-really/6.6.6', 'ios');
		expect(resolved.platform).not.toBe('Fluxer iOS');
		expect(resolved.browser).toBe(resolved.platform);
	});

	it('never emits an Unknown literal when the user agent is absent', () => {
		expect(resolve(null, null)).toEqual({platform: null, os: null, browser: null, device: 'desktop'});
		expect(resolve('', null)).toEqual({platform: null, os: null, browser: null, device: 'desktop'});
	});

	it('uses instance branding for the product word', () => {
		expect(resolve('Fluxer iOS/1.4.2 (stable)', 'ios', 'Acme').platform).toBe('Acme iOS');
		expect(resolve('Fluxer Desktop/1.4.2 (stable)', 'windows', 'Acme').platform).toBe('Acme Lite Windows');
		expect(resolve(ELECTRON_MAC_UA, null, 'Acme').platform).toBe('Acme macOS');
	});
});

describe('isFluxerNativeUserAgent', () => {
	it('matches only the Fluxer native product tokens', () => {
		expect(isFluxerNativeUserAgent('Fluxer iOS/1.4.2 (stable)')).toBe(true);
		expect(isFluxerNativeUserAgent('Fluxer Desktop (canary)')).toBe(true);
		expect(isFluxerNativeUserAgent(ELECTRON_MAC_UA)).toBe(false);
		expect(isFluxerNativeUserAgent(CHROME_MAC_UA)).toBe(false);
		expect(isFluxerNativeUserAgent(null)).toBe(false);
	});
});

describe('parseReportedClientOs', () => {
	const encode = (value: unknown): string => Buffer.from(JSON.stringify(value), 'utf8').toString('base64');

	it('reads the operating system from the client properties header', () => {
		expect(parseReportedClientOs(encode({os: 'macos', device: 'mobile'}))).toBe('macos');
		expect(parseReportedClientOs(encode({os: 'ios'}))).toBe('ios');
	});

	it('rejects anything outside the known operating systems', () => {
		expect(parseReportedClientOs(null)).toBeNull();
		expect(parseReportedClientOs('')).toBeNull();
		expect(parseReportedClientOs('not base64 $$$')).toBeNull();
		expect(parseReportedClientOs(encode([1, 2]))).toBeNull();
		expect(parseReportedClientOs(encode({os: 'solaris'}))).toBeNull();
		expect(parseReportedClientOs(encode({os: 42}))).toBeNull();
		expect(parseReportedClientOs(encode({}))).toBeNull();
	});

	it('rejects an oversized header without decoding it', () => {
		expect(parseReportedClientOs('a'.repeat(4097))).toBeNull();
	});
});
