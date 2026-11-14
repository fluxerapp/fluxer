// SPDX-License-Identifier: AGPL-3.0-or-later

export const DESKTOP_API_ATTACHMENT_AUTHORIZATION_CHANNEL = 'desktop-api-attachment-authorization:set';

const MAX_ENDPOINT_LENGTH = 2048;
const MAX_TOKEN_LENGTH = 4096;
const TOKEN_PATTERN = /^[\x21-\x7e]+$/u;

export interface DesktopApiAttachmentAuthorization {
	readonly apiEndpoint: string;
	readonly token: string | null;
}

function readApiEndpoint(value: unknown): string | null {
	if (typeof value !== 'string' || value.length === 0 || value.length > MAX_ENDPOINT_LENGTH) return null;
	let url: URL;
	try {
		url = new URL(value);
	} catch {
		return null;
	}
	if (url.protocol !== 'https:' && url.protocol !== 'http:') return null;
	if (url.username !== '' || url.password !== '' || url.search !== '' || url.hash !== '') return null;
	return url.toString();
}

function readToken(value: unknown): string | null | undefined {
	if (value === null) return null;
	if (typeof value !== 'string' || value.length > MAX_TOKEN_LENGTH || !TOKEN_PATTERN.test(value)) return undefined;
	return value;
}

export function readDesktopApiAttachmentAuthorization(value: unknown): DesktopApiAttachmentAuthorization | null {
	if (value == null || typeof value !== 'object' || Array.isArray(value)) return null;
	const record = value as Record<string, unknown>;
	const apiEndpoint = readApiEndpoint(record.apiEndpoint);
	const token = readToken(record.token);
	if (apiEndpoint === null || token === undefined) return null;
	return {apiEndpoint, token};
}
