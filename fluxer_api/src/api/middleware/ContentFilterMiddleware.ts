// SPDX-License-Identifier: AGPL-3.0-or-later

import {ContentBlockedError} from '@fluxer/errors/src/domains/content/ContentBlockedError';
import {createMiddleware} from 'hono/factory';
import {Logger} from '../Logger';
import {extractUrlCandidates} from '../utils/UrlNormalizer';
import {phraseBlocklistCache} from './PhraseBlocklistCache';
import {urlBlocklistCache} from './UrlBlocklistCache';

const SKIP_FIELDS = new Set([
	'acls',
	'allow',
	'avatar',
	'banner',
	'challenge',
	'channels',
	'content_type',
	'date_of_birth',
	'deny',
	'email_token',
	'encryption_key',
	'endpoint',
	'embed_splash',
	'flags',
	'hashes',
	'icon',
	'id',
	'ids',
	'image',
	'keys',
	'locale',
	'mfa_code',
	'new_password',
	'nonce',
	'old_endpoint',
	'original_proof',
	'password',
	'permission_overwrites',
	'permissions',
	'recipients',
	'response',
	'roles',
	'secret',
	'splash',
	'state',
	'synced_preferences',
	'thumbnail',
	'ticket',
	'token',
	'traits',
	'upload_filename',
	'upload_id',
	'users',
	'verification_proof',
	'waveform',
	'webauthn_challenge',
	'webauthn_response',
]);
const SKIP_FIELD_SUFFIXES = [
	'_challenge',
	'_flags',
	'_hash',
	'_hashes',
	'_id',
	'_ids',
	'_key',
	'_keys',
	'_proof',
	'_secret',
	'_token',
	'_tokens',
] as const;
const SKIP_CONTENT_FILTER_PATH_PARTS = [
	'/admin/audit-logs/search/',
	'/admin/bans/phrase/',
	'/admin/guilds/search/',
	'/admin/messages/search/',
	'/admin/reports/search/',
	'/admin/users/lookup/',
	'/admin/users/search/',
	'/auth/',
	'/oauth2/',
	'/reports/dsa/email/',
	'/users/@me/authorized-ips',
	'/users/@me/email-change/',
	'/users/@me/mfa/',
	'/users/@me/password-change/',
	'/users/@me/phone/',
	'/users/@me/sudo/',
	'/webhooks/',
] as const;

export function shouldSkipContentFilterPath(path: string): boolean {
	return SKIP_CONTENT_FILTER_PATH_PARTS.some((part) => path.includes(part));
}

function shouldSkipField(key: string): boolean {
	if (SKIP_FIELDS.has(key)) {
		return true;
	}
	return SKIP_FIELD_SUFFIXES.some((suffix) => key.endsWith(suffix));
}

export function extractStringValues(body: unknown, depth = 0): Array<string> {
	if (depth > 10) return [];
	const strings: Array<string> = [];
	if (typeof body === 'string') {
		strings.push(body);
	} else if (Array.isArray(body)) {
		for (const item of body) {
			strings.push(...extractStringValues(item, depth + 1));
		}
	} else if (body && typeof body === 'object') {
		for (const [key, value] of Object.entries(body as Record<string, unknown>)) {
			if (shouldSkipField(key)) continue;
			if (typeof value === 'string' && value.length > 0) {
				strings.push(value);
			} else if (typeof value === 'object' && value !== null) {
				strings.push(...extractStringValues(value, depth + 1));
			}
		}
	}
	return strings;
}

const ContentFilterMiddleware = createMiddleware(async (ctx, next) => {
	const method = ctx.req.method;
	if (method !== 'POST' && method !== 'PUT' && method !== 'PATCH') {
		return next();
	}
	const path = ctx.req.path;
	if (shouldSkipContentFilterPath(path)) {
		return next();
	}
	const contentType = ctx.req.header('content-type') ?? '';
	if (!contentType.includes('application/json')) {
		return next();
	}
	let body: unknown;
	try {
		body = await ctx.req.json();
	} catch {
		return next();
	}
	const strings = extractStringValues(body);
	if (strings.length === 0) {
		return next();
	}
	const userId = ctx.get('user')?.id ?? null;
	for (const text of strings) {
		if (text.length < 3) continue;
		if (phraseBlocklistCache.containsBannedPhrase(text)) {
			Logger.warn(
				{surface: 'global_filter', userId: userId?.toString(), path},
				'content_moderation.block phrase match in request body',
			);
			throw new ContentBlockedError();
		}
		const urls = extractUrlCandidates(text);
		for (const url of urls) {
			if (urlBlocklistCache.isUrlOrDomainBanned(url)) {
				Logger.warn(
					{surface: 'global_filter', userId: userId?.toString(), path},
					'content_moderation.block url match in request body',
				);
				throw new ContentBlockedError();
			}
		}
	}
	return next();
});

export default ContentFilterMiddleware;
