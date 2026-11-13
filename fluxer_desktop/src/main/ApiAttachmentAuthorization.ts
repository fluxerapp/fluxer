// SPDX-License-Identifier: AGPL-3.0-or-later

const AUTHORIZATION_HEADER = 'Authorization';
export const ATTACHMENT_WINDOW_HEADER = 'X-Fluxer-Desktop-Attachment-Window';
const ENCODED_SEPARATOR_PATTERN = /%(?:2e|2f|5c)/iu;
const VERSION_PREFIX_PATTERN = /^\/v[0-9]+(?=\/)/u;
const ATTACHMENT_ROUTE_PATTERN = /^\/channels\/[0-9]+\/attachments\/[0-9]+\/[^/]/u;
const READ_METHODS: ReadonlySet<string> = new Set(['GET', 'HEAD']);
const TRAILING_SLASHES_PATTERN = /\/+$/u;

interface ApiAttachmentGrant {
	readonly origin: string;
	readonly basePath: string;
	readonly token: string;
}

export interface ApiAttachmentRequest {
	readonly topFrameWebContentsId: number | null;
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
	return ATTACHMENT_ROUTE_PATTERN.test(routePath) && !ENCODED_SEPARATOR_PATTERN.test(routePath);
}

function hasAuthorizationHeader(headers: Record<string, string>): boolean {
	return Object.keys(headers).some((name) => name.toLowerCase() === AUTHORIZATION_HEADER.toLowerCase());
}

export class ApiAttachmentAuthorization {
	private readonly grants = new Map<number, ApiAttachmentGrant>();
	private readonly childWindows = new Map<number, number>();
	private readonly listeners = new Set<() => void>();

	grant(webContentsId: number, apiEndpoint: string, token: string): boolean {
		const url = parseHttpURL(apiEndpoint);
		if (url === null) return false;
		this.grants.set(webContentsId, {
			origin: url.origin,
			basePath: url.pathname.replace(TRAILING_SLASHES_PATTERN, ''),
			token,
		});
		this.notify();
		return true;
	}

	revoke(webContentsId: number): void {
		if (this.grants.delete(webContentsId)) this.notify();
	}

	linkChildWindow(childWebContentsId: number, parentWebContentsId: number): void {
		this.childWindows.set(childWebContentsId, parentWebContentsId);
	}

	unlinkChildWindow(childWebContentsId: number): void {
		this.childWindows.delete(childWebContentsId);
	}

	hasGrants(): boolean {
		return this.grants.size > 0;
	}

	hasGrantFor(webContentsId: number): boolean {
		return this.grantFor(webContentsId) !== undefined;
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
		if (request.topFrameWebContentsId === null || hasAuthorizationHeader(headers)) return headers;
		const grant = this.grantFor(request.topFrameWebContentsId);
		if (grant === undefined || !isAttachmentTarget(grant, request.method, request.url)) return headers;
		return {...headers, [AUTHORIZATION_HEADER]: grant.token};
	}

	authorizationForProxiedRequest(windowHeader: string | null, method: string, url: string): string | null {
		if (windowHeader === null || !/^[0-9]+$/u.test(windowHeader)) return null;
		const grant = this.grantFor(Number(windowHeader));
		return grant !== undefined && isAttachmentTarget(grant, method, url) ? grant.token : null;
	}

	private grantFor(webContentsId: number): ApiAttachmentGrant | undefined {
		const parentId = this.childWindows.get(webContentsId);
		return this.grants.get(webContentsId) ?? (parentId === undefined ? undefined : this.grants.get(parentId));
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
