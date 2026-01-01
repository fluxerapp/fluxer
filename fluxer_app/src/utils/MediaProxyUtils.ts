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

interface MediaProxyOptions {
	width?: number;
	height?: number;
	format?: string;
	quality?: 'high' | 'low' | 'lossless';
	animated?: boolean;
}

const CSP_ALLOWED_RESOURCE_HOSTS = new Set(['fluxerusercontent.com', 'fluxerstatic.com', 'i.ytimg.com']);
const CSP_ALLOWED_RESOURCE_SUFFIXES = ['.fluxer.app', '.fluxer.media', '.youtube.com'];

function hasURLGlobals(): boolean {
	return typeof URL !== 'undefined' && typeof URLSearchParams !== 'undefined';
}

function getElectronMediaProxyBase(): URL | null {
	if (!hasURLGlobals()) return null;
	if (typeof window.electron?.getMediaProxyUrl !== 'function') return null;

	const raw = window.electron.getMediaProxyUrl();
	if (!raw) return null;

	try {
		return new URL(raw);
	} catch {
		return null;
	}
}

function isAllowedByDefaultCsp(hostname: string): boolean {
	if (CSP_ALLOWED_RESOURCE_HOSTS.has(hostname)) return true;
	return CSP_ALLOWED_RESOURCE_SUFFIXES.some((suffix) => hostname.endsWith(suffix));
}

function unwrapElectronMediaProxyUrl(url: string): {base: URL; target: string} | null {
	if (!hasURLGlobals()) return null;

	const base = getElectronMediaProxyBase();
	if (!base) return null;

	let parsed: URL;
	try {
		parsed = new URL(url);
	} catch {
		return null;
	}

	if (parsed.origin !== base.origin) return null;
	if (parsed.pathname !== base.pathname) return null;

	const target = parsed.searchParams.get('target');
	if (!target) return null;

	return {base, target};
}

function shouldWrapWithElectronMediaProxy(targetUrl: string): boolean {
	const base = getElectronMediaProxyBase();
	if (!base) return false;

	try {
		const parsed = new URL(targetUrl);
		if (parsed.protocol === 'blob:' || parsed.protocol === 'data:') return false;
		return !isAllowedByDefaultCsp(parsed.hostname);
	} catch {
		return false;
	}
}

function wrapWithElectronMediaProxy(targetUrl: string, base: URL): string {
	const proxied = new URL(base.toString());
	proxied.searchParams.set('target', targetUrl);
	return proxied.toString();
}

function appendMediaProxyParams(proxyURL: string, options: MediaProxyOptions): string {
	const {width, height, format, quality, animated} = options;

	if (!width && !height && !format && !quality && !animated) {
		return proxyURL;
	}

	const params = new URLSearchParams();

	if (format) {
		params.append('format', format);
	}
	if (width !== undefined) {
		params.append('width', width.toString());
	}
	if (height !== undefined) {
		params.append('height', height.toString());
	}
	if (quality) {
		params.append('quality', quality);
	}
	if (animated !== undefined) {
		params.append('animated', animated.toString());
	}

	const separator = proxyURL.includes('?') ? '&' : '?';
	return `${proxyURL}${separator}${params.toString()}`;
}

export function buildMediaProxyURL(proxyURL: string, options: MediaProxyOptions = {}): string {
	if (!proxyURL) return proxyURL;

	const unwrapped = unwrapElectronMediaProxyUrl(proxyURL);
	const base = unwrapped?.base ?? getElectronMediaProxyBase();
	const rawUrl = unwrapped ? unwrapped.target : proxyURL;

	const updated = appendMediaProxyParams(rawUrl, options);

	if (unwrapped && base) {
		return wrapWithElectronMediaProxy(updated, base);
	}

	if (base && shouldWrapWithElectronMediaProxy(updated)) {
		return wrapWithElectronMediaProxy(updated, base);
	}

	return updated;
}

export function stripMediaProxyParams(proxyURL: string): string {
	const unwrapped = unwrapElectronMediaProxyUrl(proxyURL);
	const base = unwrapped?.base ?? getElectronMediaProxyBase();
	const rawUrl = unwrapped ? unwrapped.target : proxyURL;

	const url = new URL(rawUrl);
	url.searchParams.delete('width');
	url.searchParams.delete('height');
	url.searchParams.delete('format');
	url.searchParams.delete('quality');
	url.searchParams.delete('animated');

	const stripped = url.toString();

	if (unwrapped && base) {
		return wrapWithElectronMediaProxy(stripped, base);
	}

	if (base && shouldWrapWithElectronMediaProxy(stripped)) {
		return wrapWithElectronMediaProxy(stripped, base);
	}

	return stripped;
}
