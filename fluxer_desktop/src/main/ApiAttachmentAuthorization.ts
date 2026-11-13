// SPDX-License-Identifier: AGPL-3.0-or-later

const AUTHORIZATION_HEADER = 'Authorization';
const VERSION_PREFIX_PATTERN = /^\/v[0-9]+(?=\/)/u;
const ATTACHMENT_ROUTE_PATTERN = /^\/channels\/[0-9]+\/attachments\/[0-9]+\/[^/]/u;
const READ_METHODS: ReadonlySet<string> = new Set(['GET', 'HEAD']);
const TRAILING_SLASHES_PATTERN = /\/+$/u;

interface ApiAttachmentGrant {
	readonly origin: string;
	readonly basePath: string;
	readonly token: string;
	readonly grantedAt: number;
}

export interface ApiAttachmentRequest {
	readonly webContentsId: number | null;
	readonly fromSubFrame: boolean;
	readonly method: string;
	readonly url: string;
}

function parseHttpURL(value: string): URL | null {
	try {
		const url = new URL(value);
		if (url.protocol !== 'https:' && url.protocol !== 'http:') return null;
		if (url.username !== '' || url.password !== '') return null;
		return url;
	} catch {
		return null;
	}
}

function isAttachmentTarget(grant: ApiAttachmentGrant, method: string, value: string): boolean {
	if (!READ_METHODS.has(method.toUpperCase())) return false;
	const url = parseHttpURL(value);
	if (url === null || url.origin !== grant.origin) return false;
	if (!url.pathname.startsWith(`${grant.basePath}/`)) return false;
	const routePath = url.pathname.slice(grant.basePath.length).replace(VERSION_PREFIX_PATTERN, '');
	return ATTACHMENT_ROUTE_PATTERN.test(routePath);
}

function hasAuthorizationHeader(headers: Record<string, string>): boolean {
	return Object.keys(headers).some((name) => name.toLowerCase() === AUTHORIZATION_HEADER.toLowerCase());
}

export class ApiAttachmentAuthorization {
	private readonly grants = new Map<number, ApiAttachmentGrant>();
	private readonly listeners = new Set<() => void>();
	private grantCount = 0;

	grant(webContentsId: number, apiEndpoint: string, token: string): boolean {
		const url = parseHttpURL(apiEndpoint);
		if (url === null) return false;
		this.grantCount += 1;
		this.grants.set(webContentsId, {
			origin: url.origin,
			basePath: url.pathname.replace(TRAILING_SLASHES_PATTERN, ''),
			token,
			grantedAt: this.grantCount,
		});
		this.notify();
		return true;
	}

	revoke(webContentsId: number): void {
		if (this.grants.delete(webContentsId)) this.notify();
	}

	urlPatterns(): Array<string> {
		const patterns = new Set<string>();
		for (const grant of this.grants.values()) {
			const url = new URL(grant.origin);
			patterns.add(`${url.protocol}//${url.hostname}/*`);
		}
		return [...patterns].sort();
	}

	onChange(listener: () => void): () => void {
		this.listeners.add(listener);
		return () => {
			this.listeners.delete(listener);
		};
	}

	applyRequestHeaders(request: ApiAttachmentRequest, headers: Record<string, string>): Record<string, string> {
		if (request.webContentsId === null || request.fromSubFrame || hasAuthorizationHeader(headers)) return headers;
		const grant = this.grants.get(request.webContentsId);
		if (grant === undefined || !isAttachmentTarget(grant, request.method, request.url)) return headers;
		return {...headers, [AUTHORIZATION_HEADER]: grant.token};
	}

	authorizationForProxiedRequest(method: string, url: string): string | null {
		let latest: ApiAttachmentGrant | null = null;
		for (const grant of this.grants.values()) {
			if (!isAttachmentTarget(grant, method, url)) continue;
			if (latest === null || grant.grantedAt > latest.grantedAt) latest = grant;
		}
		return latest?.token ?? null;
	}

	private notify(): void {
		for (const listener of this.listeners) listener();
	}
}

let instance: ApiAttachmentAuthorization | null = null;

export function getApiAttachmentAuthorization(): ApiAttachmentAuthorization {
	instance ??= new ApiAttachmentAuthorization();
	return instance;
}
