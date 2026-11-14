// SPDX-License-Identifier: AGPL-3.0-or-later

import {makeSignedAttachmentCdnUrl, signAttachmentUrl} from '@app/api/attachment/AttachmentUrls';
import {
	type AttachmentID,
	type ChannelID,
	createAttachmentID,
	createChannelID,
	createMessageID,
	type MessageID,
} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {isApiProxiedAttachments} from '@app/api/instance/AttachmentAccessMode';
import {stripApiPrefix} from '@app/api/utils/RequestPathUtils';
import {
	attachmentStorageKeyFromUrl,
	isSignatureParameterName,
	percentDecodeStorageKey,
} from '@pkgs/media_proxy_utils/src/AttachmentUrlSignature';

export const SNOWFLAKE_REGEX = /^[0-9]{1,20}$/u;
export const TRAILING_SLASHES_REGEX = /\/+$/u;
const LEADING_SLASHES_REGEX = /^\/+/u;
const VERSION_PREFIX_REGEX = /^\/v[0-9]+(?=\/)/u;
const API_ATTACHMENT_PATH_REGEX = /^\/channels\/([0-9]{1,20})\/attachments\/([0-9]{1,20})\/(.+)$/u;
const RESERVED_QUERY_NAMES = new Set(['mid', 'mt']);
const ENCODED_SEPARATOR_REGEX = /%(?:2e|2f|5c)/iu;

export interface ApiAttachmentLocation {
	channelId: ChannelID;
	attachmentId: AttachmentID;
	filename: string;
	messageId: MessageID | null;
}

function apiAttachmentBase(): string {
	return Config.endpoints.apiClient.replace(TRAILING_SLASHES_REGEX, '');
}

function splitQuery(url: string): {head: string; query: string} {
	const withoutFragment = url.split('#', 1)[0] ?? '';
	const queryIndex = withoutFragment.indexOf('?');
	return queryIndex < 0
		? {head: withoutFragment, query: ''}
		: {head: withoutFragment.slice(0, queryIndex), query: withoutFragment.slice(queryIndex + 1)};
}

function rawPathOf(url: string): string {
	const {head} = splitQuery(url);
	const authorityStart = head.indexOf('://');
	if (authorityStart < 0) return '';
	const pathStart = head.indexOf('/', authorityStart + 3);
	return pathStart < 0 ? '' : head.slice(pathStart);
}

function queryFieldName(field: string): string {
	const separator = field.indexOf('=');
	return separator < 0 ? field : field.slice(0, separator);
}

function preservedQueryFields(query: string): Array<string> {
	return query
		.split('&')
		.filter((field) => field.length > 0 && field !== '=')
		.filter((field) => {
			const name = queryFieldName(field);
			return !isSignatureParameterName(name) && !RESERVED_QUERY_NAMES.has(name);
		});
}

function isSafeFilename(filename: string): boolean {
	return filename
		.split('/')
		.every((segment) => segment.length > 0 && segment !== '.' && segment !== '..' && !segment.includes('\0'));
}

export function makeApiAttachmentUrl(
	channelId: ChannelID,
	attachmentId: AttachmentID | bigint,
	rawFilename: string,
	messageId: MessageID | null,
	preservedQuery: ReadonlyArray<string> = [],
): string {
	const fields = [...(messageId === null ? [] : [`mid=${messageId}`]), ...preservedQuery];
	const url = `${apiAttachmentBase()}/channels/${channelId}/attachments/${attachmentId}/${rawFilename}`;
	return fields.length === 0 ? url : `${url}?${fields.join('&')}`;
}

export function parseApiAttachmentRoute(routePath: string, mid: string | null): ApiAttachmentLocation | null {
	const match = API_ATTACHMENT_PATH_REGEX.exec(routePath.replace(VERSION_PREFIX_REGEX, ''));
	if (!match || ENCODED_SEPARATOR_REGEX.test(match[3]!)) return null;
	const filename = percentDecodeStorageKey(match[3]!);
	if (filename === null || !isSafeFilename(filename)) return null;
	if (mid !== null && !SNOWFLAKE_REGEX.test(mid)) return null;
	return {
		channelId: createChannelID(BigInt(match[1]!)),
		attachmentId: createAttachmentID(BigInt(match[2]!)),
		filename,
		messageId: mid === null ? null : createMessageID(BigInt(mid)),
	};
}

export function parseApiAttachmentUrl(url: string): ApiAttachmentLocation | null {
	let target: URL;
	let base: URL;
	try {
		target = new URL(url);
		base = new URL(apiAttachmentBase());
	} catch {
		return null;
	}
	if (target.origin !== base.origin) return null;
	const basePath = rawPathOf(apiAttachmentBase()).replace(TRAILING_SLASHES_REGEX, '');
	const rawPath = rawPathOf(url);
	if (!rawPath.startsWith(`${basePath}/`)) return null;
	return parseApiAttachmentRoute(rawPath.slice(basePath.length), target.searchParams.get('mid'));
}

export function isAttachmentMediaRequest(method: string, path: string): boolean {
	return (
		isApiProxiedAttachments() &&
		(method === 'GET' || method === 'HEAD') &&
		API_ATTACHMENT_PATH_REGEX.test(stripApiPrefix(path))
	);
}

export function parseMediaAttachmentUrl(url: string): ApiAttachmentLocation | null {
	const storageKey = attachmentStorageKeyFromUrl(url, Config.endpoints.media.replace(TRAILING_SLASHES_REGEX, ''));
	if (storageKey === null) return null;
	const [prefix, channelSegment, attachmentSegment, ...filenameSegments] = storageKey.split('/');
	const filename = filenameSegments.join('/');
	if (
		prefix !== 'attachments' ||
		!SNOWFLAKE_REGEX.test(channelSegment ?? '') ||
		!SNOWFLAKE_REGEX.test(attachmentSegment ?? '') ||
		!isSafeFilename(filename)
	) {
		return null;
	}
	return {
		channelId: createChannelID(BigInt(channelSegment!)),
		attachmentId: createAttachmentID(BigInt(attachmentSegment!)),
		filename,
		messageId: null,
	};
}

export function ownAttachmentLocation(url: string): ApiAttachmentLocation | null {
	return parseApiAttachmentUrl(url) ?? parseMediaAttachmentUrl(url);
}

export function isGuardedAttachmentUrl(url: string): boolean {
	return isApiProxiedAttachments() && ownAttachmentLocation(url) !== null;
}

export interface AttachmentUrlOwner {
	channelId: ChannelID;
	messageId: MessageID;
	storageChannelId: ChannelID;
}

export function apiFormOfMediaUrl(url: string, owner: AttachmentUrlOwner | null = null): string | null {
	const mediaEndpoint = Config.endpoints.media.replace(TRAILING_SLASHES_REGEX, '');
	const storageKey = attachmentStorageKeyFromUrl(url, mediaEndpoint);
	if (storageKey === null) return null;
	const [, channelSegment, attachmentSegment] = storageKey.split('/');
	if (!SNOWFLAKE_REGEX.test(channelSegment ?? '') || !SNOWFLAKE_REGEX.test(attachmentSegment ?? '')) return null;
	const {query} = splitQuery(url);
	const endpointPath = rawPathOf(mediaEndpoint).replace(TRAILING_SLASHES_REGEX, '');
	const rawKeyPath = rawPathOf(url).slice(endpointPath.length).replace(LEADING_SLASHES_REGEX, '');
	const rawFilename = rawKeyPath.split('/').slice(3).join('/');
	if (rawFilename.length === 0) return null;
	const keyChannelId = createChannelID(BigInt(channelSegment!));
	const ownedHere = owner !== null && owner.storageChannelId === keyChannelId;
	return makeApiAttachmentUrl(
		ownedHere ? owner.channelId : keyChannelId,
		BigInt(attachmentSegment!),
		rawFilename,
		ownedHere ? owner.messageId : null,
		preservedQueryFields(query),
	);
}

export function publicAttachmentUrl(url: string): string {
	if (!isApiProxiedAttachments()) return signAttachmentUrl(url);
	return apiFormOfMediaUrl(url) ?? url;
}

export function makePublicAttachmentUrl(
	channelId: ChannelID,
	attachmentId: AttachmentID | bigint,
	filename: string,
): string {
	if (!isApiProxiedAttachments()) return makeSignedAttachmentCdnUrl(channelId, attachmentId, filename);
	return makeApiAttachmentUrl(channelId, attachmentId, filename, null);
}
