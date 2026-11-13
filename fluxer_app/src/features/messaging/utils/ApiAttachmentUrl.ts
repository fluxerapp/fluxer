// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {parseEndpoint} from '@app/features/messaging/utils/AttachmentCdnUrl';

const ACCESS_TOKEN_PARAMETER = 'mt';
const MESSAGE_PARAMETER = 'mid';
const VERSION_PREFIX_PATTERN = /^\/v[0-9]+(?=\/)/u;
const ATTACHMENT_ROUTE_PATTERN = /^\/channels\/[0-9]+\/attachments\/[0-9]+\/[^/]/u;
const ENCODED_SEPARATOR_PATTERN = /%(?:2e|2f|5c)/iu;

interface ParsedApiAttachmentUrl {
	url: URL;
	routePath: string;
}

export function isApiProxiedAttachmentsEnabled(): boolean {
	return RuntimeConfig.getSnapshotOrNull()?.features.proxy_attachments_through_api === true;
}

function parseApiAttachmentUrl(url: string): ParsedApiAttachmentUrl | null {
	if (!isApiProxiedAttachmentsEnabled()) return null;
	const apiEndpoint = RuntimeConfig.getSnapshotOrNull()?.apiEndpoint;
	const endpoint = apiEndpoint ? parseEndpoint(apiEndpoint) : null;
	if (endpoint === null) return null;
	let parsed: URL;
	try {
		parsed = new URL(url);
	} catch {
		return null;
	}
	if (parsed.origin !== endpoint.origin || parsed.username !== '' || parsed.password !== '') return null;
	const basePath = endpoint.basePath === '/' ? '' : endpoint.basePath;
	if (!parsed.pathname.startsWith(`${basePath}/`)) return null;
	const routePath = parsed.pathname.slice(basePath.length).replace(VERSION_PREFIX_PATTERN, '');
	return ATTACHMENT_ROUTE_PATTERN.test(routePath) && !ENCODED_SEPARATOR_PATTERN.test(routePath)
		? {url: parsed, routePath}
		: null;
}

export function isApiAttachmentUrl(url: string): boolean {
	return parseApiAttachmentUrl(url) !== null;
}

export function apiAttachmentRequestPath(url: string): string | null {
	const parsed = parseApiAttachmentUrl(url);
	if (parsed === null) return null;
	parsed.url.searchParams.delete(ACCESS_TOKEN_PARAMETER);
	return `${parsed.routePath}${parsed.url.search}`;
}

export function apiAttachmentFileKey(url: string): string | null {
	const parsed = parseApiAttachmentUrl(url);
	if (parsed === null) return null;
	const messageIds = parsed.url.searchParams.getAll(MESSAGE_PARAMETER);
	parsed.url.search = '';
	for (const messageId of messageIds) parsed.url.searchParams.append(MESSAGE_PARAMETER, messageId);
	parsed.url.hash = '';
	return parsed.url.toString();
}

export function readAttachmentToken(url: string): string | null {
	return parseApiAttachmentUrl(url)?.url.searchParams.get(ACCESS_TOKEN_PARAMETER) ?? null;
}

export function withoutAttachmentToken(url: string): string {
	const parsed = parseApiAttachmentUrl(url);
	if (parsed === null || !parsed.url.searchParams.has(ACCESS_TOKEN_PARAMETER)) return url;
	parsed.url.searchParams.delete(ACCESS_TOKEN_PARAMETER);
	return parsed.url.toString();
}

export function withAttachmentToken(url: string, token: string): string {
	const parsed = parseApiAttachmentUrl(url);
	if (parsed === null) return url;
	parsed.url.searchParams.set(ACCESS_TOKEN_PARAMETER, token);
	return parsed.url.toString();
}
