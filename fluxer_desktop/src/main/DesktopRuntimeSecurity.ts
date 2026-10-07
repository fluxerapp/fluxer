// SPDX-License-Identifier: AGPL-3.0-or-later

import {STABLE_APP_URL} from '@electron/common/Constants';
import {isPlainRecord} from '@electron/common/PlainRecord';

const SUB_FRAME_RESOURCE_TYPE = 'subFrame';

const REFERER_REQUEST_HEADER = 'Referer';
const REFERER_HEADER_NAME_LOWERCASE = 'referer';
const YOUTUBE_EMBED_FRAME_ORIGIN = 'https://www.youtube.com';
const YOUTUBE_EMBED_FRAME_PATH_PREFIX = '/embed/';

interface DesktopRuntimeSecurityLogger {
	error: (...args: Array<unknown>) => void;
}

interface DesktopLocalAppRequestAuthorization {
	readonly applyRequestHeaders: (details: unknown, headers: Record<string, string>) => Record<string, string>;
}

interface DesktopRuntimeSecurityDependencies {
	readonly logger: DesktopRuntimeSecurityLogger;
	readonly localAppAuthorization?: DesktopLocalAppRequestAuthorization;
}

class DesktopRuntimeSecurityClosedError extends Error {
	constructor() {
		super('Desktop runtime security policy is closed');
		this.name = 'DesktopRuntimeSecurityClosedError';
	}
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
	private readonly installedSessions = new Set<Electron.Session>();
	private acceptingChanges = true;

	constructor(dependencies: DesktopRuntimeSecurityDependencies) {
		this.logger = dependencies.logger;
		this.localAppAuthorization = dependencies.localAppAuthorization ?? null;
	}

	install(session: Electron.Session): void {
		this.requireAcceptingChanges();
		if (this.installedSessions.has(session)) {
			return;
		}
		this.installedSessions.add(session);
		try {
			session.webRequest.onBeforeSendHeaders((details: unknown, callback: unknown) => {
				this.settleRequestHeaders(details, callback);
			});
		} catch (installationError) {
			this.uninstall(session);
			throw installationError;
		}
	}

	cleanup(): void {
		this.acceptingChanges = false;
		for (const session of this.installedSessions) {
			this.uninstall(session);
		}
	}

	private uninstall(session: Electron.Session): void {
		this.installedSessions.delete(session);
		try {
			session.webRequest.onBeforeSendHeaders(null);
		} catch (error) {
			this.logger.error('Failed to remove desktop runtime security listeners', error);
		}
	}

	private requireAcceptingChanges(): void {
		if (!this.acceptingChanges) {
			throw new DesktopRuntimeSecurityClosedError();
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
		return injectEmbedFrameReferrer(detailsValue, this.authorizeLocalAppRequest(detailsValue));
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
	readonly requestHeadersValue: unknown;
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
	return {
		resourceType,
		url,
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
