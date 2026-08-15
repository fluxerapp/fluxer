// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	TRACKING_URL_RULES,
	type TrackingURLAMPRule,
	TrackingURLEncoding,
	TrackingURLHandler,
	type TrackingURLRule,
} from '@app/features/messaging/utils/TrackingUrlRules';

const MAX_REWRITE_DEPTH = 2;
const ABSOLUTE_HTTP_URL_PATTERN = /^https?:\/\//i;
const YOUTUBE_HOST_PATTERN = /(?:^|\.)(?:youtube\.com|youtu\.be|youtube-nocookie\.com)$/i;
const YOUTUBE_TRACKING_PARAMS = ['si', 'pp'] as const;

interface DecodeContext {
	readonly originalURL: string;
	readonly decoded: string;
	readonly lastPath: string;
	readonly searchParams: URLSearchParams;
}

interface AMPRewrite {
	rewrittenURL: string;
	resolvedTarget: boolean;
}

function matches(pattern: RegExp, value: string): boolean {
	pattern.lastIndex = 0;
	return pattern.test(value);
}

function parseHTTPURL(value: string): URL | null {
	try {
		const parsedURL = new URL(value);
		if (parsedURL.protocol === 'http:' || parsedURL.protocol === 'https:') {
			return parsedURL;
		}
		return null;
	} catch {
		return null;
	}
}

function hostMatchesRule(hosts: ReadonlyArray<string>, hostname: string): boolean {
	return hosts.some((host) => hostname === host || hostname.endsWith(`.${host}`));
}

function findRules(parsedURL: URL): ReadonlyArray<TrackingURLRule> {
	const hostname = parsedURL.hostname.toLowerCase();
	return TRACKING_URL_RULES.filter((rule) => {
		if (rule.matchURL === true) return matches(rule.match, parsedURL.href);
		if (rule.hosts != null) return hostMatchesRule(rule.hosts, hostname);
		return matches(rule.match, hostname);
	});
}

function findParamCaseInsensitive(params: URLSearchParams, target: string): string | null {
	const normalizedTarget = target.toLowerCase();
	for (const [key, value] of params) {
		if (key.toLowerCase() === normalizedTarget) return value;
	}
	return null;
}

function removeTrackingParams(parsedURL: URL, rules: ReadonlyArray<TrackingURLRule>): boolean {
	const allowed = new Set<string>();
	const removable = new Set<string>();
	for (const rule of rules) {
		if (rule.allowParams != null) {
			for (const param of rule.allowParams) allowed.add(param);
		}
		if (rule.removeParams != null) {
			for (const param of rule.removeParams) removable.add(param);
		}
	}
	if (YOUTUBE_HOST_PATTERN.test(parsedURL.hostname)) {
		for (const param of YOUTUBE_TRACKING_PARAMS) removable.add(param);
	}
	let changed = false;
	for (const param of removable) {
		if (allowed.has(param) || !parsedURL.searchParams.has(param)) continue;
		parsedURL.searchParams.delete(param);
		changed = true;
	}
	return changed;
}

function removeTrackingPathParts(parsedURL: URL, rules: ReadonlyArray<TrackingURLRule>): boolean {
	let pathname = parsedURL.pathname;
	for (const rule of rules) {
		if (rule.removeFromPath == null) continue;
		for (const pattern of rule.removeFromPath) {
			pattern.lastIndex = 0;
			pathname = pathname.replace(pattern, '');
		}
	}
	if (pathname === parsedURL.pathname) return false;
	parsedURL.pathname = pathname;
	return true;
}

function decodeBase64(value: string): string {
	try {
		return atob(value);
	} catch {
		return value;
	}
}

function decodeURIComponentSafely(value: string): string | null {
	try {
		return decodeURIComponent(value);
	} catch {
		return null;
	}
}

function decodeHex(value: string): string {
	if (value.length % 2 !== 0 || !/^[\da-f]+$/i.test(value)) return value;
	let result = '';
	for (let index = 0; index < value.length; index += 2) {
		result += String.fromCharCode(Number.parseInt(value.slice(index, index + 2), 16));
	}
	return result;
}

function decodeValue(value: string, encoding: TrackingURLEncoding | undefined): string {
	let resolvedEncoding: TrackingURLEncoding = TrackingURLEncoding.BASE64;
	if (encoding != null) {
		resolvedEncoding = encoding;
	}
	try {
		switch (resolvedEncoding) {
			case TrackingURLEncoding.BASE64:
				return decodeBase64(value);
			case TrackingURLEncoding.URL:
				return decodeURI(value);
			case TrackingURLEncoding.URL_COMPONENT:
				return decodeURIComponent(value);
			case TrackingURLEncoding.HEX:
				return decodeHex(value);
			case TrackingURLEncoding.BASE32:
			case TrackingURLEncoding.BASE45:
			case TrackingURLEncoding.BINARY:
				return value;
		}
	} catch {
		return value;
	}
}

function decodeJSONTarget(decoded: string, key: string): string | null {
	try {
		const value: unknown = JSON.parse(decoded);
		if (value == null || typeof value !== 'object') return null;
		const target = (value as Record<string, unknown>)[key];
		if (typeof target === 'string') {
			return target;
		}
		return null;
	} catch {
		return null;
	}
}

function decodePatchbotTarget(context: DecodeContext): string | null {
	const target = context.decoded.replaceAll('%3D', '=').split('|')[2];
	if (target == null || target.length === 0) {
		return null;
	}
	return decodeURIComponentSafely(target);
}

function decodeProofpointURLDefenseTarget(context: DecodeContext): string | null {
	const target = context.searchParams.get('u');
	if (target == null || target.length === 0) {
		return null;
	}
	return decodeURIComponent(target.replaceAll('-', '%')).replaceAll('_', '/').replaceAll('%2F', '/');
}

function decodeRedditMailTarget(context: DecodeContext): string | null {
	const match = /https:\/\/click\.redditmail\.com\/CL0\/(.*?)\//i.exec(context.originalURL);
	if (match == null) {
		return null;
	}
	const target = match[1];
	if (target == null || target.length === 0) {
		return null;
	}
	return decodeURIComponentSafely(target);
}

function decodeRedirectingAtTarget(context: DecodeContext): string | null {
	const [host, target] = context.originalURL.split('?id');
	if (host !== 'https://go.redirectingat.com/') {
		return context.originalURL;
	}
	if (target == null || target.length === 0) {
		return context.originalURL;
	}
	return new URL(`${host}?id=${decodeURIComponent(target)}`).searchParams.get('url');
}

function decodeTwitchEmailTarget(context: DecodeContext): string | null {
	const data: unknown = JSON.parse(context.decoded);
	if (data == null || typeof data !== 'object') {
		return null;
	}
	const record = data as Record<string, unknown>;
	if (record.name === 'twitch_favorite_up' && typeof record.channel === 'string') {
		return `https://www.twitch.tv/${record.channel}`;
	}
	return null;
}

function decodeHandlerTarget(handler: TrackingURLHandler, context: DecodeContext): string | null {
	try {
		switch (handler) {
			case TrackingURLHandler.PATCHBOT:
				return decodePatchbotTarget(context);
			case TrackingURLHandler.PROOFPOINT_URL_DEFENSE:
				return decodeProofpointURLDefenseTarget(context);
			case TrackingURLHandler.STARDOCK_ENTERTAINMENT: {
				const target = decodeBase64(context.lastPath);
				return target.replace('watch>v=', 'watch?v=');
			}
			case TrackingURLHandler.STEAM: {
				const steamTarget = context.originalURL.split('%3Eutm_')[0];
				if (steamTarget == null) {
					return null;
				}
				return steamTarget;
			}
			case TrackingURLHandler.MJT:
			case TrackingURLHandler.DOMINOS_NEW_ZEALAND:
				return decodeBase64(context.lastPath);
			case TrackingURLHandler.REDDIT_MAIL:
				return decodeRedditMailTarget(context);
			case TrackingURLHandler.REDIRECTING_AT:
				return decodeRedirectingAtTarget(context);
			case TrackingURLHandler.TWITCH_EMAIL:
				return decodeTwitchEmailTarget(context);
		}
	} catch {
		return null;
	}
}

function resolveRedirect(parsedURL: URL, rules: ReadonlyArray<TrackingURLRule>): string | null {
	for (const rule of rules) {
		if (rule.redirectParam == null || rule.redirectParam.length === 0) continue;
		const target = findParamCaseInsensitive(parsedURL.searchParams, rule.redirectParam);
		if (target != null && target.length > 0 && parseHTTPURL(target) != null) return target;
		let decoded: string | null;
		if (target != null && target.length > 0) {
			decoded = decodeURIComponentSafely(target);
		} else {
			decoded = null;
		}
		if (decoded != null && decoded.length > 0 && parseHTTPURL(decoded) != null) return decoded;
	}
	return null;
}

function resolveEncodedDecodeTarget(parsedURL: URL, rule: TrackingURLRule, lastPath: string): string | null {
	if (rule.decode == null) {
		return null;
	}
	if (rule.decode.targetPath === true) {
		return lastPath;
	}
	if (rule.decode.param != null) {
		return parsedURL.searchParams.get(rule.decode.param);
	}
	return null;
}

function resolveDecodedURL(parsedURL: URL, rules: ReadonlyArray<TrackingURLRule>): string | null {
	const poppedPath = parsedURL.pathname.split('/').pop();
	let lastPath = '';
	if (poppedPath != null) {
		lastPath = poppedPath;
	}
	for (const rule of rules) {
		if (rule.decode == null) continue;
		const encoded = resolveEncodedDecodeTarget(parsedURL, rule, lastPath);
		if (encoded == null) continue;
		const decoded = decodeValue(encoded, rule.decode.encoding);
		let target: string | null;
		if (rule.decode.handler != null) {
			target = decodeHandlerTarget(rule.decode.handler, {
				originalURL: parsedURL.toString(),
				decoded,
				lastPath,
				searchParams: parsedURL.searchParams,
			});
		} else if (rule.decode.jsonKey != null) {
			target = decodeJSONTarget(decoded, rule.decode.jsonKey);
		} else {
			target = decoded;
		}
		if (target != null && target.length > 0 && parseHTTPURL(target) != null) return target;
	}
	return null;
}

function resolveAMPCandidateURL(decoded: string): string {
	if (ABSOLUTE_HTTP_URL_PATTERN.test(decoded)) {
		return decoded;
	}
	return `https://${decoded}`;
}

function resolveAMPRegexTarget(regex: RegExp, value: string): string | null {
	regex.lastIndex = 0;
	const match = regex.exec(value);
	if (match == null) {
		return null;
	}
	const target = match[1];
	if (target == null || target.length === 0) {
		return null;
	}
	const decoded = decodeURIComponentSafely(target);
	if (decoded == null || decoded.length === 0) {
		return null;
	}
	const candidate = resolveAMPCandidateURL(decoded);
	if (parseHTTPURL(candidate) == null) {
		return null;
	}
	return candidate;
}

function applyAMPRewrite(value: string, AMP: TrackingURLAMPRule): AMPRewrite {
	let result = value;
	let resolvedTarget = false;
	if (AMP.replace != null) {
		let replacement = '';
		if (AMP.replace.with != null) {
			replacement = AMP.replace.with;
		}
		result = result.replace(AMP.replace.text, replacement);
	}
	if (AMP.regex != null) {
		const target = resolveAMPRegexTarget(AMP.regex, result);
		if (target != null) {
			result = target;
			resolvedTarget = true;
		}
	}
	if (AMP.sliceTrailing != null && AMP.sliceTrailing.length > 0 && result.endsWith(AMP.sliceTrailing)) {
		result = result.slice(0, -AMP.sliceTrailing.length);
	}
	if (result.endsWith('%3Famp')) result = result.slice(0, -6);
	if (result.endsWith('amp/')) result = result.slice(0, -4);
	return {rewrittenURL: result, resolvedTarget};
}

function removeAMPWrappers(value: string, rules: ReadonlyArray<TrackingURLRule>): AMPRewrite {
	let result = value;
	let resolvedTarget = false;
	for (const rule of rules) {
		const AMP = rule.AMP;
		if (AMP == null) continue;
		const rewrite = applyAMPRewrite(result, AMP);
		result = rewrite.rewrittenURL;
		if (rewrite.resolvedTarget) resolvedTarget = true;
	}
	return {rewrittenURL: result, resolvedTarget};
}

function preserveSourceHash(target: string, source: URL): string {
	const hasEmptyHash = source.hash.length === 0 && source.href.endsWith('#');
	if (source.hash.length === 0 && !hasEmptyHash) return target;
	const parsed = parseHTTPURL(target);
	if (parsed == null) return target;
	if (source.hash.length > 0) parsed.hash = source.hash;
	const result = parsed.toString();
	if (hasEmptyHash && parsed.hash.length === 0 && !result.endsWith('#')) {
		return `${result}#`;
	}
	return result;
}

function isExcludedByRules(rules: ReadonlyArray<TrackingURLRule>, sourceURL: URL): boolean {
	const target = sourceURL.toString();
	for (const rule of rules) {
		if (rule.exclude == null) continue;
		if (rule.exclude.some((pattern) => matches(pattern, target))) return true;
	}
	return false;
}

function sanitizeTrackingURLAtDepth(value: string, depth: number): string {
	const sourceURL = parseHTTPURL(value);
	if (sourceURL == null) return value;
	const rules = findRules(sourceURL);
	if (isExcludedByRules(rules, sourceURL)) return value;
	const redirect = resolveRedirect(sourceURL, rules);
	if (redirect != null && redirect.length > 0 && depth < MAX_REWRITE_DEPTH) {
		const sanitized = sanitizeTrackingURLAtDepth(preserveSourceHash(redirect, sourceURL), depth + 1);
		if (sanitized.length <= value.length) {
			return sanitized;
		}
		return value;
	}
	let changed = removeTrackingParams(sourceURL, rules);
	changed = removeTrackingPathParts(sourceURL, rules) || changed;
	const AMPRewrite = removeAMPWrappers(sourceURL.toString(), rules);
	let result = AMPRewrite.rewrittenURL;
	changed = result !== sourceURL.toString() || changed;
	if (AMPRewrite.resolvedTarget && depth < MAX_REWRITE_DEPTH) {
		const sanitizedAMPTarget = sanitizeTrackingURLAtDepth(preserveSourceHash(result, sourceURL), depth + 1);
		changed = sanitizedAMPTarget !== result || changed;
		result = sanitizedAMPTarget;
	}
	const AMPURL = parseHTTPURL(result);
	if (AMPURL == null) return value;
	const decoded = resolveDecodedURL(AMPURL, rules);
	if (decoded != null && decoded.length > 0 && depth < MAX_REWRITE_DEPTH) {
		result = sanitizeTrackingURLAtDepth(preserveSourceHash(decoded, sourceURL), depth + 1);
		changed = true;
	}
	if (rules.some((rule) => rule.removeEmptyParamValues === true)) {
		const withoutEmptyValues = result.replace(/=(?=&|$)/g, '');
		changed = withoutEmptyValues !== result || changed;
		result = withoutEmptyValues;
	}
	if (!changed || result.length > value.length) return value;
	return result;
}

export function sanitizeTrackingURL(value: string): string {
	return sanitizeTrackingURLAtDepth(value, 0);
}
