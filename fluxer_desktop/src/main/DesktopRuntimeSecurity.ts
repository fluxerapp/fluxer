// SPDX-License-Identifier: AGPL-3.0-or-later

import {DESKTOP_APP_SCHEME, STABLE_APP_URL} from '@electron/common/Constants';
import {isPlainRecord} from '@electron/common/PlainRecord';
import type {ApiAttachmentAuthorization} from '@electron/main/ApiAttachmentAuthorization';

const SUB_FRAME_RESOURCE_TYPE = 'subFrame';

const REFERER_REQUEST_HEADER = 'Referer';
const REFERER_HEADER_NAME_LOWERCASE = 'referer';
const YOUTUBE_EMBED_FRAME_ORIGIN = 'https://www.youtube.com';
const YOUTUBE_EMBED_FRAME_PATH_PREFIX = '/embed/';
const DESKTOP_RUNTIME_SECURITY_REQUEST_URL_PATTERNS = [
	`${DESKTOP_APP_SCHEME}://*/*`,
	`${YOUTUBE_EMBED_FRAME_ORIGIN}${YOUTUBE_EMBED_FRAME_PATH_PREFIX}*`,
];

interface DesktopRuntimeSecurityLogger {
	error: (...args: Array<unknown>) => void;
}

interface DesktopLocalAppRequestAuthorization {
	readonly applyRequestHeaders: (details: unknown, headers: Record<string, string>) => Record<string, string>;
}

interface DesktopRuntimeSecurityDependencies {
	readonly logger: DesktopRuntimeSecurityLogger;
	readonly localAppAuthorization?: DesktopLocalAppRequestAuthorization;
	readonly attachmentAuthorization?: ApiAttachmentAuthorization;
}

class InvalidWebRequestDetailsError extends TypeError {
	constructor() {
		super('Electron web request details have an invalid shape');
		this.name = 'InvalidWebRequestDetailsError';
	}
}

class InvalidWebRequestCallbackError extends TypeError {
	constructor() {
		super('Electron web request callback must be a function');
		this.name = 'InvalidWebRequestCallbackError';
	}
}

export class DesktopRuntimeSecurity {
	private readonly logger: DesktopRuntimeSecurityLogger;
	private readonly localAppAuthorization: DesktopLocalAppRequestAuthorization | null;
	private readonly attachmentAuthorization: ApiAttachmentAuthorization | null;
	private readonly installedSessions = new Set<Electron.Session>();

	constructor(dependencies: DesktopRuntimeSecurityDependencies) {
		this.logger = dependencies.logger;
		this.localAppAuthorization = dependencies.localAppAuthorization ?? null;
		this.attachmentAuthorization = dependencies.attachmentAuthorization ?? null;
		this.attachmentAuthorization?.onChange(() => {
			for (const session of [...this.installedSessions]) {
				try {
					this.listen(session);
				} catch (error) {
					this.logger.error('Failed to update desktop runtime security listeners', error);
					this.uninstall(session);
				}
			}
		});
	}

	install(session: Electron.Session): void {
		if (this.installedSessions.has(session)) {
			return;
		}
		this.installedSessions.add(session);
		try {
			this.listen(session);
		} catch (installationError) {
			this.uninstall(session);
			throw installationError;
		}
	}

	private listen(session: Electron.Session): void {
		session.webRequest.onBeforeSendHeaders(
			{
				urls: [
					...DESKTOP_RUNTIME_SECURITY_REQUEST_URL_PATTERNS,
					...(this.attachmentAuthorization?.urlPatterns() ?? []),
				],
			},
			(details: unknown, callback: unknown) => {
				this.settleRequestHeaders(details, callback);
			},
		);
	}

	private uninstall(session: Electron.Session): void {
		this.installedSessions.delete(session);
		try {
			session.webRequest.onBeforeSendHeaders(null);
		} catch (error) {
			this.logger.error('Failed to remove desktop runtime security listeners', error);
		}
	}

	private settleRequestHeaders(detailsValue: unknown, callbackValue: unknown): void {
		let settlement: Record<string, unknown> = {};
		try {
			settlement = this.rewriteRequestHeaders(detailsValue);
		} catch (error) {
			this.logger.error('Failed to rewrite desktop runtime security request headers', error);
			settlement = {};
		}
		try {
			invokeCallback(callbackValue, settlement);
		} catch (error) {
			this.logger.error('Failed to settle desktop runtime security request headers', error);
		}
	}

	private rewriteRequestHeaders(detailsValue: unknown): Record<string, unknown> {
		return injectEmbedFrameReferrer(
			detailsValue,
			this.authorizeAttachmentRequest(detailsValue, this.authorizeLocalAppRequest(detailsValue)),
		);
	}

	private authorizeAttachmentRequest(
		detailsValue: unknown,
		settlement: Record<string, unknown>,
	): Record<string, unknown> {
		const authorization = this.attachmentAuthorization;
		if (authorization == null) {
			return settlement;
		}
		const details = readWebRequestDetails(detailsValue);
		const headers = readRequestHeaderRecord(settlement.requestHeaders ?? details.requestHeadersValue);
		if (headers == null) {
			return settlement;
		}
		const next = authorization.applyRequestHeaders(
			{
				webContentsId: details.webContentsId,
				fromSubFrame: details.fromSubFrame,
				method: details.method,
				url: details.url,
			},
			headers,
		);
		return next === headers ? settlement : {...settlement, requestHeaders: next};
	}

	private authorizeLocalAppRequest(detailsValue: unknown): Record<string, unknown> {
		const authorization = this.localAppAuthorization;
		if (authorization == null) {
			return {};
		}
		const details = readWebRequestDetails(detailsValue);
		const headers = readRequestHeaderRecord(details.requestHeadersValue);
		if (headers == null) {
			return {};
		}
		const next = authorization.applyRequestHeaders(detailsValue, headers);
		if (haveEqualHeaderRecords(headers, next)) {
			return {};
		}
		return {requestHeaders: next};
	}
}

function injectEmbedFrameReferrer(detailsValue: unknown, settlement: Record<string, unknown>): Record<string, unknown> {
	const details = readWebRequestDetails(detailsValue);
	if (details.resourceType !== SUB_FRAME_RESOURCE_TYPE || !isYouTubeEmbedFrameURL(details.url)) {
		return settlement;
	}
	const headers = readRequestHeaderRecord(settlement.requestHeaders ?? details.requestHeadersValue);
	if (headers == null) {
		return settlement;
	}
	const next: Record<string, string> = Object.create(null) as Record<string, string>;
	for (const [name, value] of Object.entries(headers)) {
		if (name.toLowerCase() !== REFERER_HEADER_NAME_LOWERCASE) {
			next[name] = value;
		}
	}
	next[REFERER_REQUEST_HEADER] = embedFrameReferrer();
	return {...settlement, requestHeaders: next};
}

function embedFrameReferrer(): string {
	return STABLE_APP_URL.endsWith('/') ? STABLE_APP_URL : `${STABLE_APP_URL}/`;
}

function isYouTubeEmbedFrameURL(value: string): boolean {
	const url = parseURL(value);
	if (url == null || originOf(value) !== YOUTUBE_EMBED_FRAME_ORIGIN) {
		return false;
	}
	return url.pathname.startsWith(YOUTUBE_EMBED_FRAME_PATH_PREFIX);
}

function originOf(value: string): string | null {
	const url = parseURL(value);
	if (url == null) {
		return null;
	}
	if (url.protocol !== 'http:' && url.protocol !== 'https:') {
		return null;
	}
	return `${url.protocol}//${url.host}`;
}

function parseURL(value: string): URL | null {
	if (typeof value !== 'string' || value.trim().length === 0) {
		return null;
	}
	try {
		const url = new URL(value.trim());
		return url.host.length === 0 ? null : url;
	} catch {
		return null;
	}
}

interface WebRequestDetails {
	readonly resourceType: string;
	readonly url: string;
	readonly method: string;
	readonly webContentsId: number | null;
	readonly fromSubFrame: boolean;
	readonly requestHeadersValue: unknown;
}

function isSubFrame(value: unknown): boolean {
	if (value == null || typeof value !== 'object') {
		return false;
	}
	try {
		return Reflect.get(value, 'parent') != null;
	} catch {
		return true;
	}
}

function readWebRequestDetails(value: unknown): WebRequestDetails {
	if (!isPlainRecord(value)) {
		throw new InvalidWebRequestDetailsError();
	}
	const resourceType = value.resourceType;
	const url = value.url;
	if (typeof resourceType !== 'string' || typeof url !== 'string') {
		throw new InvalidWebRequestDetailsError();
	}
	const webContentsId = value.webContentsId;
	return {
		resourceType,
		url,
		method: typeof value.method === 'string' ? value.method : '',
		webContentsId: typeof webContentsId === 'number' && Number.isInteger(webContentsId) ? webContentsId : null,
		fromSubFrame: resourceType === SUB_FRAME_RESOURCE_TYPE || isSubFrame(value.frame),
		requestHeadersValue: value.requestHeaders ?? null,
	};
}

function isHeaderName(name: unknown): name is string {
	return typeof name === 'string' && name.length > 0 && /^[!#$%&'*+\-.^_`|~0-9A-Za-z]+$/.test(name);
}

function readRequestHeaderRecord(value: unknown): Record<string, string> | null {
	if (!isPlainRecord(value)) {
		return null;
	}
	const headers: Record<string, string> = Object.create(null) as Record<string, string>;
	for (const [name, headerValue] of Object.entries(value)) {
		if (!isHeaderName(name) || typeof headerValue !== 'string') {
			return null;
		}
		headers[name] = headerValue;
	}
	return headers;
}

function haveEqualHeaderRecords(left: Record<string, string>, right: Record<string, string>): boolean {
	const leftNames = Object.keys(left);
	if (leftNames.length !== Object.keys(right).length) {
		return false;
	}
	for (const name of leftNames) {
		if (!Object.hasOwn(right, name) || right[name] !== left[name]) {
			return false;
		}
	}
	return true;
}

function invokeCallback(callbackValue: unknown, settlement: Record<string, unknown>): void {
	if (typeof callbackValue !== 'function') {
		throw new InvalidWebRequestCallbackError();
	}
	Reflect.apply(callbackValue, null, [settlement]);
}
