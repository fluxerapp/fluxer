// SPDX-License-Identifier: AGPL-3.0-or-later

import {isDesktopLocalAppDocument, isDesktopLocalAppOrigin} from '@app/features/platform/DesktopLocalAppRuntime';
import {
	AccountScopedWork,
	type AccountScopedWorkSuspension,
	type AccountScopedWorkTicket,
	accountScopedWorkAbortError,
} from '@app/features/platform/state/AccountScopedWork';
import {ClientInstallationId} from '@app/features/platform/state/ClientInstallationId';
import {HttpError, type HttpErrorDetail} from '@app/features/platform/types/EndpointError';
import type {
	HttpMethod,
	MultipartBody,
	RestAuthMode,
	RestClientHooks,
	RestInterceptor,
	RestRequestHandle,
	RestRequestOptions,
	RestResponse,
	RestResponseFormat,
	SudoBindings,
} from '@app/features/platform/types/TransportTypes';
import {resolveDocumentURLFromRoot} from '@app/features/platform/URLOriginUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Headers} from '@fluxer/constants/src/Headers';
import type {DesktopLocalAppUploadProgress} from '@fluxer/desktop_ipc/src/LocalAppRouteContract';
import {
	DESKTOP_LOCAL_APP_HOST,
	LOCAL_APP_API_PATH_PREFIX,
	LOCAL_APP_UPLOAD_ID_HEADER,
} from '@fluxer/desktop_ipc/src/LocalAppRouteContract';
import {i18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const TOO_MANY_REQUESTS_DESCRIPTOR = msg({message: 'Too many requests. Try again later.'});
const log = new Logger('RestClient');
const RETRY_BACKOFF_BASE_MS = 1000;
const RETRY_BACKOFF_CAP_MS = 30_000;
const RETRY_BACKOFF_FACTOR = 2;
const RETRY_BACKOFF_JITTER = 0.25;
const RETRYABLE_STATUSES: ReadonlySet<number> = new Set([502, 504, 507, 522, 523, 524, 598, 599]);
const SUDO_HEADER = 'x-fluxer-sudo-mode-jwt';
const SUDO_VERIFICATION_ERROR_FIELDS: ReadonlySet<string> = new Set([
	'password',
	'mfa_method',
	'mfa_code',
	'webauthn_response',
	'webauthn_challenge',
]);

type RestMode = NonNullable<RestRequestOptions['mode']>;
type PlanOptions = RestRequestOptions & {skipIntercept?: boolean};
type BodyShape =
	| {tag: 'empty'}
	| {tag: 'json'; payload: string}
	| {tag: 'urlencoded'; payload: string}
	| {tag: 'form'; payload: FormData}
	| {tag: 'opaque'; payload: XMLHttpRequestBodyInit};

interface PacingEntry {
	until: number;
	note?: string;
	code?: string;
}

class UnscopedDesktopLocalApiEndpointError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'UnscopedDesktopLocalApiEndpointError';
	}
}

interface RuntimeState {
	routing: RestRuntimeRouting | null;
	defaultTimeoutMs: number;
	defaultRetries: number;
	authProvider: () => string | null;
	sudo: SudoBindings | null;
	globalIntercept?: RestInterceptor;
	pacing: Map<string, PacingEntry>;
}

interface RestRuntimeRouting {
	readonly baseUrl: string;
	readonly canonicalBaseUrl: string;
	readonly apiVersion: number;
}

export class RestRuntimeUnavailableError extends Error {
	constructor() {
		super('A relative REST request requires an active instance runtime');
		this.name = 'RestRuntimeUnavailableError';
	}
}

interface Plan {
	method: HttpMethod;
	path: string;
	routing: RestRuntimeRouting | null;
	url: string;
	rateLimitKey: string;
	body: BodyShape;
	headers: Record<string, string>;
	parse: RestResponseFormat;
	timeoutMs: number;
	retries: number;
	mode: RestMode;
	suppressContentBlockedModal: boolean;
	sudoApplied: boolean;
	signal?: AbortSignal;
	onProgress?: (event: ProgressEvent) => void;
	options: PlanOptions;
}

type TransportOutcome =
	| {status: 'reply'; reply: RestResponse; receivedSudoToken: string | null | undefined}
	| {status: 'transport-error'; error: Error}
	| {status: 'aborted'; error: DOMException};
type AttemptDecision =
	| {next: 'deliver'; reply: RestResponse}
	| {next: 'retry-after'; delayMs: number; mode: 'backoff' | 'fixed'}
	| {next: 'fail'; error: unknown};

interface OnlineWaiter {
	resolve: () => void;
	signal: AbortSignal | undefined;
	onAbort: () => void;
}

const strippedAuthorizationOrigins = new Set<string>();

const onlineWaiters = new Set<OnlineWaiter>();
let onlineListenerActive = false;

function createRequestAbortError(): DOMException {
	return new DOMException('Request aborted', 'AbortError');
}

function removeOnlineListener(): void {
	if (!onlineListenerActive) return;
	window.removeEventListener('online', resolveOnlineWaiters);
	onlineListenerActive = false;
}

function releaseOnlineWaiter(waiter: OnlineWaiter): boolean {
	if (!onlineWaiters.delete(waiter)) return false;
	waiter.signal?.removeEventListener('abort', waiter.onAbort);
	if (onlineWaiters.size === 0) removeOnlineListener();
	return true;
}

function resolveOnlineWaiters(): void {
	const pending = Array.from(onlineWaiters);
	onlineWaiters.clear();
	removeOnlineListener();
	for (const waiter of pending) {
		waiter.signal?.removeEventListener('abort', waiter.onAbort);
		waiter.resolve();
	}
}

function waitUntilOnline(signal?: AbortSignal): Promise<void> {
	if (signal?.aborted) return Promise.reject(createRequestAbortError());
	if (navigator.onLine) return Promise.resolve();
	return new Promise<void>((resolve, reject) => {
		const waiter: OnlineWaiter = {
			resolve,
			signal,
			onAbort: () => {
				if (releaseOnlineWaiter(waiter)) reject(createRequestAbortError());
			},
		};
		onlineWaiters.add(waiter);
		if (signal) signal.addEventListener('abort', waiter.onAbort, {once: true});
		if (!onlineListenerActive) {
			window.addEventListener('online', resolveOnlineWaiters);
			onlineListenerActive = true;
		}
		if (navigator.onLine) queueMicrotask(resolveOnlineWaiters);
	});
}

function withoutTrailingSlashes(value: string): string {
	return value.replace(/\/+$/u, '');
}

export class RestClient {
	private readonly state: RuntimeState = {
		routing: null,
		defaultTimeoutMs: 0,
		defaultRetries: 0,
		authProvider: () => null,
		sudo: null,
		pacing: new Map(),
	};

	configure(options: {baseUrl: string; canonicalBaseUrl?: string; apiVersion: number}): void {
		this.state.routing = {
			baseUrl: options.baseUrl,
			canonicalBaseUrl: options.canonicalBaseUrl ?? options.baseUrl,
			apiVersion: options.apiVersion,
		};
	}

	clearRuntime(): void {
		this.state.routing = null;
	}

	matchesConfiguredRouting(baseUrl: string, apiVersion: number): boolean {
		const routing = this.state.routing;
		return (
			routing !== null &&
			routing.apiVersion === apiVersion &&
			withoutTrailingSlashes(routing.canonicalBaseUrl) === withoutTrailingSlashes(baseUrl)
		);
	}

	installAuth(provider: () => string | null): void {
		this.state.authProvider = provider;
	}

	installSudo(bindings: SudoBindings): void {
		this.state.sudo = bindings;
	}

	installHooks(hooks: RestClientHooks): void {
		this.state.globalIntercept = hooks.intercept;
	}

	hasAuthorization(): boolean {
		const routing = this.state.routing;
		if (routing === null) return false;
		return !isOffOrigin(resolveUrl(routing, '/', undefined));
	}

	dispatch<T = unknown>(method: HttpMethod, path: string, options: RestRequestOptions = {}): Promise<RestResponse<T>> {
		return this.dispatchAccountScoped(method, path, options, () => AccountScopedWork.begin());
	}

	dispatchWithinAccountTransition<T = unknown>(
		suspension: AccountScopedWorkSuspension,
		method: HttpMethod,
		path: string,
		options: RestRequestOptions = {},
	): Promise<RestResponse<T>> {
		return this.dispatchAccountScoped(method, path, options, () => AccountScopedWork.beginWithinSuspension(suspension));
	}

	private dispatchAccountScoped<T>(
		method: HttpMethod,
		path: string,
		options: RestRequestOptions,
		beginTicket: () => AccountScopedWorkTicket,
	): Promise<RestResponse<T>> {
		const routing = this.state.routing;
		if (!looksAbsolute(path) && routing === null) {
			return Promise.reject(new RestRuntimeUnavailableError());
		}
		let ticket: AccountScopedWorkTicket;
		try {
			ticket = beginTicket();
		} catch (error) {
			return Promise.reject(error);
		}
		return runWithSudoEscalation<T>(this.state, routing, ticket, method, path, options, 'fresh')
			.catch((error: unknown) => {
				throw toAccountScopedFailure(ticket, error);
			})
			.finally(() => ticket.dispose());
	}

	clearPacing(): void {
		this.state.pacing.clear();
	}

	get<T = unknown>(path: string, options?: RestRequestOptions): Promise<RestResponse<T>> {
		return this.dispatch<T>('GET', path, options);
	}

	post<T = unknown>(path: string, options?: RestRequestOptions): Promise<RestResponse<T>> {
		return this.dispatch<T>('POST', path, options);
	}

	put<T = unknown>(path: string, options?: RestRequestOptions): Promise<RestResponse<T>> {
		return this.dispatch<T>('PUT', path, options);
	}

	patch<T = unknown>(path: string, options?: RestRequestOptions): Promise<RestResponse<T>> {
		return this.dispatch<T>('PATCH', path, options);
	}

	delete<T = unknown>(path: string, options?: RestRequestOptions): Promise<RestResponse<T>> {
		return this.dispatch<T>('DELETE', path, options);
	}
}

type SudoPhase = 'fresh' | 'reissued' | 'reissued-twice';

async function runWithSudoEscalation<T>(
	state: RuntimeState,
	routing: RestRuntimeRouting | null,
	ticket: AccountScopedWorkTicket,
	method: HttpMethod,
	path: string,
	options: RestRequestOptions,
	phase: SudoPhase,
	overrideOptions?: RestRequestOptions,
): Promise<RestResponse<T>> {
	const effective = overrideOptions ?? options;
	const sudoApplied = phase !== 'fresh';
	try {
		return await runRetryLoop<T>(state, routing, ticket, method, path, effective, sudoApplied, 0);
	} catch (err) {
		const sudo = state.sudo;
		const sudoRequired = isSudoRequiredFailure(err);
		const sudoVerificationFailed = sudoApplied && isSudoVerificationFailure(err);
		if (!sudo || (!sudoRequired && !sudoVerificationFailed)) {
			if (sudoApplied) sudo?.onFailure(err);
			throw err;
		}
		if (sudoVerificationFailed) {
			sudo.onFailure(err);
			const merged = await awaitAccountScopedInteraction(ticket, sudo.prompt(method, path, err));
			if (!merged) throw err;
			return runWithSudoEscalation<T>(
				state,
				routing,
				ticket,
				method,
				path,
				options,
				'reissued',
				mergeBody(options, merged),
			);
		}
		switch (phase) {
			case 'fresh': {
				sudo.invalidate();
				const merged = await awaitAccountScopedInteraction(ticket, sudo.prompt(method, path, err));
				if (!merged) throw err;
				return runWithSudoEscalation<T>(
					state,
					routing,
					ticket,
					method,
					path,
					options,
					'reissued',
					mergeBody(options, merged),
				);
			}
			case 'reissued': {
				sudo.onFailure(err);
				const second = await awaitAccountScopedInteraction(ticket, sudo.prompt(method, path, err));
				if (!second) throw err;
				return runWithSudoEscalation<T>(
					state,
					routing,
					ticket,
					method,
					path,
					options,
					'reissued-twice',
					mergeBody(options, second),
				);
			}
			case 'reissued-twice': {
				sudo.onFailure(err);
				throw err;
			}
		}
	}
}

function isSudoRequiredFailure(err: unknown): boolean {
	if (!(err instanceof HttpError) || err.status !== 403) return false;
	const body = err.body;
	return typeof body === 'object' && body !== null && (body as Record<string, unknown>).code === 'SUDO_MODE_REQUIRED';
}

function isSudoVerificationFailure(err: unknown): boolean {
	if (!(err instanceof HttpError) || err.status !== 400) return false;
	const body = err.body;
	if (!isRecord(body) || body.code !== APIErrorCodes.INVALID_FORM_BODY) return false;
	const errors = body.errors;
	if (!Array.isArray(errors)) return false;
	return errors.some((error) => {
		if (!isRecord(error)) return false;
		const path = typeof error.path === 'string' ? error.path : typeof error.field === 'string' ? error.field : null;
		return path !== null && SUDO_VERIFICATION_ERROR_FIELDS.has(path);
	});
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null;
}

async function runRetryLoop<T>(
	state: RuntimeState,
	routing: RestRuntimeRouting | null,
	ticket: AccountScopedWorkTicket,
	method: HttpMethod,
	path: string,
	options: PlanOptions,
	sudoApplied: boolean,
	attempt: number,
): Promise<RestResponse<T>> {
	ticket.assertCurrent();
	const plan = composePlan(state, routing, method, path, options, sudoApplied);
	const {handle, unlink} = createHandle(plan.signal, ticket.signal);
	try {
		if (plan.retries > 0) await waitUntilOnline(handle.abortController.signal);
		const pacingHit = consultPacing(state.pacing, plan.rateLimitKey);
		if (pacingHit) {
			if (plan.mode === 'auto-retry') {
				await sleepUntil(pacingHit.until, handle.abortController.signal);
			} else {
				const synthesized = synthesizePacingReply<T>(plan, pacingHit);
				if (plan.mode === 'silent') {
					return synthesized;
				}
				throw new HttpError(failureDetail(plan, synthesized));
			}
			ticket.assertCurrent();
		}
		const outcome = await performTransport(plan, handle);
		ticket.assertCurrent();
		const decision = await reactToOutcome(state, ticket, plan, outcome);
		ticket.assertCurrent();
		switch (decision.next) {
			case 'deliver':
				return decision.reply as RestResponse<T>;
			case 'retry-after': {
				if (attempt >= plan.retries) {
					return finalizeAfterRetriesExhausted<T>(state, plan, outcome);
				}
				const wait = decision.mode === 'backoff' ? computeBackoffMs(attempt) : decision.delayMs;
				if (outcome.status === 'transport-error' && !navigator.onLine) {
					await waitUntilOnline(handle.abortController.signal);
				} else {
					await delay(wait, handle.abortController.signal);
				}
				return runRetryLoop<T>(state, routing, ticket, method, path, options, sudoApplied, attempt + 1);
			}
			case 'fail':
				throw decision.error;
		}
	} finally {
		unlink();
	}
}

function composePlan(
	state: RuntimeState,
	routing: RestRuntimeRouting | null,
	method: HttpMethod,
	path: string,
	options: PlanOptions,
	sudoApplied: boolean,
): Plan {
	const url = resolveUrl(routing, path, options.query);
	const body = encodeBody(options);
	const targetsApiBase = !looksAbsolute(path);
	const apiOrigin = targetsApiBase ? originOf(url) : null;
	const sameOrigin = targetsApiBase && (apiOrigin === null || apiOrigin === window.location.origin);
	if (apiOrigin !== null && !sameOrigin) {
		reportStrippedAuthorization(state, apiOrigin, options.auth);
	}
	const headers = assembleHeaders({
		state,
		callerHeaders: options.headers,
		body,
		reason: options.reason,
		auth: options.auth,
		sameOrigin,
	});
	return {
		method,
		path,
		routing,
		url,
		rateLimitKey: `${method} ${path}`,
		body,
		headers,
		parse: options.parse ?? 'auto',
		timeoutMs: options.timeoutMs ?? state.defaultTimeoutMs,
		retries: options.retries ?? state.defaultRetries,
		mode: options.mode ?? 'strict',
		suppressContentBlockedModal: options.suppressContentBlockedModal === true,
		sudoApplied,
		signal: options.signal,
		onProgress: options.onProgress,
		options,
	};
}

function resolveUrl(routing: RestRuntimeRouting | null, path: string, query: RestRequestOptions['query']): string {
	let seed: string;
	if (looksAbsolute(path)) {
		seed = path;
	} else {
		if (routing === null) {
			throw new RestRuntimeUnavailableError();
		}
		assertScopedDesktopLocalApiBase(routing.baseUrl);
		seed = `${routing.baseUrl}/v${routing.apiVersion}${path}`;
	}
	const url = resolveDocumentURLFromRoot(seed);
	if (query instanceof URLSearchParams) {
		query.forEach((value, key) => url.searchParams.set(key, value));
	} else if (query) {
		for (const [key, raw] of Object.entries(query)) {
			if (raw === null || raw === undefined) continue;
			url.searchParams.set(key, String(raw));
		}
	}
	return url.toString();
}

function assertScopedDesktopLocalApiBase(baseUrl: string): void {
	if (!isDesktopLocalAppDocument()) return;
	if (!isUnscopedDesktopLocalApiBase(baseUrl)) return;
	throw new UnscopedDesktopLocalApiEndpointError('Desktop local REST requests require a scoped runtime API endpoint');
}

function isUnscopedDesktopLocalApiBase(baseUrl: string): boolean {
	let parsedUrl: URL;
	try {
		parsedUrl = resolveDocumentURLFromRoot(baseUrl);
	} catch {
		return false;
	}
	if (!isDesktopLocalAppOrigin(parsedUrl.protocol, parsedUrl.hostname)) return false;
	if (parsedUrl.host !== DESKTOP_LOCAL_APP_HOST) return false;
	return !parsedUrl.pathname.startsWith(`${LOCAL_APP_API_PATH_PREFIX}/`);
}

function looksAbsolute(path: string): boolean {
	return path.startsWith('//') || /^[a-z][a-z0-9+.-]*:\/\//i.test(path);
}

function originOf(url: string): string | null {
	try {
		return new URL(url).origin;
	} catch {
		return null;
	}
}

function isOffOrigin(url: string): boolean {
	const origin = originOf(url);
	return origin !== null && origin !== window.location.origin;
}

function reportStrippedAuthorization(state: RuntimeState, apiOrigin: string, auth: RestAuthMode | undefined): void {
	if (auth === 'none' || strippedAuthorizationOrigins.has(apiOrigin)) return;
	if (!state.authProvider()) return;
	strippedAuthorizationOrigins.add(apiOrigin);
	log.warn(`authorization withheld from off-origin api base: ${apiOrigin} (page ${window.location.origin})`);
}

function encodeBody(options: RestRequestOptions): BodyShape {
	if (options.multipart) {
		return {tag: 'form', payload: buildFormData(options.multipart)};
	}
	if (options.raw !== undefined) return classifyOpaque(options.raw);
	const raw = options.body;
	if (raw === undefined || raw === null) return {tag: 'empty'};
	if (typeof raw === 'string') return {tag: 'opaque', payload: raw};
	if (raw instanceof FormData) return {tag: 'form', payload: raw};
	if (raw instanceof URLSearchParams) return {tag: 'urlencoded', payload: raw.toString()};
	if (raw instanceof Blob) return {tag: 'opaque', payload: raw};
	if (raw instanceof ArrayBuffer) return {tag: 'opaque', payload: raw};
	if (ArrayBuffer.isView(raw)) return {tag: 'opaque', payload: copyArrayBufferView(raw)};
	return {tag: 'json', payload: JSON.stringify(raw)};
}

function classifyOpaque(raw: BodyInit): BodyShape {
	if (raw instanceof FormData) return {tag: 'form', payload: raw};
	if (raw instanceof URLSearchParams) return {tag: 'urlencoded', payload: raw.toString()};
	return {tag: 'opaque', payload: toXmlHttpRequestBody(raw)};
}

function toXmlHttpRequestBody(raw: BodyInit): XMLHttpRequestBodyInit {
	if (typeof raw === 'string' || raw instanceof Blob || raw instanceof FormData || raw instanceof URLSearchParams) {
		return raw;
	}
	if (raw instanceof ArrayBuffer) return raw;
	if (ArrayBuffer.isView(raw)) return copyArrayBufferView(raw);
	throw new TypeError('Unsupported raw request body for XMLHttpRequest transport');
}

function copyArrayBufferView(raw: ArrayBufferView): ArrayBuffer {
	const buffer = new ArrayBuffer(raw.byteLength);
	new Uint8Array(buffer).set(new Uint8Array(raw.buffer, raw.byteOffset, raw.byteLength));
	return buffer;
}

function buildFormData(payload: MultipartBody): FormData {
	const form = new FormData();
	const fields = payload.fields ?? {};
	for (const [name, value] of Object.entries(fields)) {
		form.append(name, value);
	}
	const files = payload.files ?? [];
	for (const part of files) {
		form.append(part.name, part.file, part.filename);
	}
	return form;
}

interface AssembleHeadersInput {
	state: RuntimeState;
	callerHeaders: Record<string, string> | undefined;
	body: BodyShape;
	reason: string | undefined;
	auth: RestAuthMode | undefined;
	sameOrigin: boolean;
}

function assembleHeaders(input: AssembleHeadersInput): Record<string, string> {
	const accumulator: Record<string, string> = {};
	if (input.sameOrigin) {
		accumulator[Headers.X_FLUXER_FEATURES] = 'view_channel_members_permission,channel_threads';
		accumulator[Headers.X_FLUXER_CLIENT_INSTALLATION_ID] = ClientInstallationId.get();
	}
	const contentType = inferContentType(input.body);
	if (contentType) accumulator[Headers.CONTENT_TYPE] = contentType;
	if (input.reason) accumulator[Headers.X_AUDIT_LOG_REASON] = encodeURIComponent(input.reason);
	if (input.auth !== 'none' && input.sameOrigin) {
		const token = input.state.authProvider();
		if (token) accumulator[Headers.AUTHORIZATION] = token;
	}
	const sudoToken = input.state.sudo?.tokenProvider() ?? null;
	if (sudoToken && input.sameOrigin) accumulator[SUDO_HEADER] = sudoToken;
	if (input.callerHeaders) {
		for (const [name, value] of Object.entries(input.callerHeaders)) {
			accumulator[name] = value;
		}
	}
	return accumulator;
}

function inferContentType(body: BodyShape): string | null {
	switch (body.tag) {
		case 'json':
			return 'application/json';
		case 'urlencoded':
			return 'application/x-www-form-urlencoded;charset=UTF-8';
		case 'form':
		case 'opaque':
		case 'empty':
			return null;
	}
}

function consultPacing(pacing: Map<string, PacingEntry>, key: string): PacingEntry | null {
	const entry = pacing.get(key);
	if (!entry) return null;
	if (entry.until <= Date.now()) {
		pacing.delete(key);
		return null;
	}
	return entry;
}

function recordPacing(
	pacing: Map<string, PacingEntry>,
	key: string,
	retryAfterSeconds: number | null,
	headerMs: number | null,
	note?: string,
	code?: string,
): void {
	const fallbackMs = 1000;
	const ms =
		headerMs !== null && headerMs > 0
			? headerMs
			: retryAfterSeconds !== null && retryAfterSeconds > 0
				? retryAfterSeconds * 1000
				: fallbackMs;
	pacing.set(key, {until: Date.now() + ms, note, code});
}

function synthesizePacingReply<T>(_plan: Plan, hit: PacingEntry): RestResponse<T> {
	const remaining = Math.max(0, hit.until - Date.now());
	const headers: Record<string, string> = {
		'retry-after': String(Math.ceil(remaining / 1000)),
		'content-type': 'application/json',
	};
	const payload = {
		message: hit.note ?? i18n._(TOO_MANY_REQUESTS_DESCRIPTOR),
		retry_after: remaining / 1000,
		global: false,
		...(hit.code !== undefined ? {code: hit.code} : {}),
	};
	return {
		ok: false,
		status: 429,
		statusText: 'Too Many Requests',
		headers,
		body: payload as T,
		text: JSON.stringify(payload),
	};
}

const UPLOAD_ID_RANDOM_BYTES = 16;

function newLocalUploadId(): string {
	const bytes = new Uint8Array(UPLOAD_ID_RANDOM_BYTES);
	globalThis.crypto.getRandomValues(bytes);
	return Array.from(bytes, (byte) => byte.toString(16).padStart(2, '0')).join('');
}

function isDesktopLocalAppRequest(url: string): boolean {
	if (!isDesktopLocalAppDocument()) return false;
	let parsedUrl: URL;
	try {
		parsedUrl = resolveDocumentURLFromRoot(url);
	} catch {
		return false;
	}
	return isDesktopLocalAppOrigin(parsedUrl.protocol, parsedUrl.hostname);
}

function attachLocalUploadProgress(
	xhr: XMLHttpRequest,
	url: string,
	onProgress: (event: ProgressEvent) => void,
): boolean {
	if (!isDesktopLocalAppRequest(url)) {
		return false;
	}
	const subscribe = getElectronAPI()?.localAppUpload?.subscribe;
	if (subscribe == null) {
		return false;
	}
	const uploadId = newLocalUploadId();
	xhr.setRequestHeader(LOCAL_APP_UPLOAD_ID_HEADER, uploadId);
	const unsubscribe = subscribe((progress: DesktopLocalAppUploadProgress) => {
		if (progress.uploadId !== uploadId || progress.failed) {
			return;
		}
		onProgress(
			new ProgressEvent('progress', {
				lengthComputable: progress.total !== null,
				loaded: progress.loaded,
				total: progress.total ?? 0,
			}),
		);
	});
	xhr.addEventListener('loadend', unsubscribe, {once: true});
	return true;
}

function performTransport(plan: Plan, handle: RestRequestHandle): Promise<TransportOutcome> {
	return new Promise<TransportOutcome>((resolve) => {
		const xhr = new XMLHttpRequest();
		const finishWithReply = () => {
			const headers = parseHeaderBlock(xhr.getAllResponseHeaders());
			const reply = decodeReply(xhr, plan.parse, headers);
			const sudoToken = headers[SUDO_HEADER];
			const receivedSudoToken = sudoToken !== undefined ? sudoToken : plan.sudoApplied ? null : undefined;
			resolve({status: 'reply', reply, receivedSudoToken});
		};
		xhr.open(plan.method, plan.url);
		if (plan.parse === 'binary') xhr.responseType = 'blob';
		if (plan.timeoutMs > 0) xhr.timeout = plan.timeoutMs;
		for (const [name, value] of Object.entries(plan.headers)) {
			xhr.setRequestHeader(name, value);
		}
		const externalSignal = handle.abortController.signal;
		const onExternalAbort = () => xhr.abort();
		if (externalSignal.aborted) {
			queueMicrotask(() => xhr.abort());
		} else {
			externalSignal.addEventListener('abort', onExternalAbort, {once: true});
		}
		xhr.addEventListener('loadend', () => {
			externalSignal.removeEventListener('abort', onExternalAbort);
		});
		if (plan.onProgress) {
			if (!attachLocalUploadProgress(xhr, plan.url, plan.onProgress)) {
				xhr.upload.addEventListener('progress', plan.onProgress);
			}
		}
		xhr.addEventListener('load', finishWithReply);
		xhr.addEventListener('error', () => {
			resolve({status: 'transport-error', error: new Error('Network error during request')});
		});
		xhr.addEventListener('abort', () => {
			resolve({status: 'aborted', error: new DOMException('Request aborted', 'AbortError')});
		});
		xhr.addEventListener('timeout', () => {
			resolve({
				status: 'transport-error',
				error: Object.assign(new Error('Request timeout'), {name: 'TimeoutError'}),
			});
		});
		xhr.send(materializeBody(plan.body));
	});
}

function materializeBody(body: BodyShape): XMLHttpRequestBodyInit | null {
	switch (body.tag) {
		case 'empty':
			return null;
		case 'json':
		case 'urlencoded':
			return body.payload;
		case 'form':
			return body.payload;
		case 'opaque':
			return body.payload as XMLHttpRequestBodyInit;
	}
}

function decodeReply(xhr: XMLHttpRequest, parse: RestResponseFormat, headers: Record<string, string>): RestResponse {
	const ok = xhr.status >= 200 && xhr.status < 300;
	const base = {ok, status: xhr.status, statusText: xhr.statusText, headers};
	if (parse === 'none' || xhr.status === 204) {
		return {...base, body: undefined};
	}
	if (parse === 'binary') {
		return {...base, body: xhr.response as unknown};
	}
	const text = xhr.responseType === '' || xhr.responseType === 'text' ? xhr.responseText : '';
	const wantsJson =
		parse === 'json' || (parse === 'auto' && (headers['content-type'] ?? '').includes('application/json'));
	if (!wantsJson) {
		return {...base, body: text, text};
	}
	if (!text) {
		return {...base, body: undefined, text: ''};
	}
	try {
		const body: unknown = JSON.parse(text);
		return {...base, body, text};
	} catch {
		return {...base, body: text, text};
	}
}

function parseHeaderBlock(raw: string | null): Record<string, string> {
	const out: Record<string, string> = {};
	if (!raw) return out;
	const lines = raw.split(/\r?\n/);
	for (const line of lines) {
		if (!line) continue;
		const sep = line.indexOf(':');
		if (sep < 0) continue;
		const name = line.slice(0, sep).trim().toLowerCase();
		out[name] = line.slice(sep + 1).trim();
	}
	return out;
}

async function reactToOutcome(
	state: RuntimeState,
	ticket: AccountScopedWorkTicket,
	plan: Plan,
	outcome: TransportOutcome,
): Promise<AttemptDecision> {
	if (outcome.status === 'aborted') {
		return {next: 'fail', error: outcome.error};
	}
	if (outcome.status === 'transport-error') {
		return {next: 'retry-after', delayMs: 0, mode: 'backoff'};
	}
	const {reply, receivedSudoToken} = outcome;
	if (reply.status === 429) {
		return reactToRateLimit(state, plan, reply);
	}
	const interceptor = plan.options.skipIntercept ? undefined : (plan.options.intercept ?? state.globalIntercept);
	if (interceptor) {
		const intercepted = await invokeInterceptor(state, ticket, plan, interceptor, reply);
		if (intercepted) return intercepted;
	}
	if (RETRYABLE_STATUSES.has(reply.status)) {
		return {next: 'retry-after', delayMs: 0, mode: 'backoff'};
	}
	if (reply.ok) {
		propagateSudoToken(state, receivedSudoToken);
		return {next: 'deliver', reply};
	}
	if (reply.status === 403 && hasContentBlockedCode(reply.body) && !plan.suppressContentBlockedModal) {
		void import('@app/features/auth/components/ContentBlockedHandler').then((m) => m.showContentBlockedModal());
	}
	if (plan.mode === 'silent') {
		propagateSudoToken(state, receivedSudoToken);
		return {next: 'deliver', reply};
	}
	return {
		next: 'fail',
		error: new HttpError(failureDetail(plan, reply)),
	};
}

function reactToRateLimit(state: RuntimeState, plan: Plan, reply: RestResponse): AttemptDecision {
	const retryAfterSeconds = readRetryAfter(reply.headers['retry-after']);
	const headerMs = readNumericHeader(reply.headers['x-ratelimit-reset-after']);
	const note = extractMessage(reply.body);
	const code = extractCode(reply.body);
	recordPacing(state.pacing, plan.rateLimitKey, retryAfterSeconds, headerMs, note, code);
	if (plan.mode === 'silent') {
		return {next: 'deliver', reply};
	}
	if (plan.mode === 'strict') {
		return {next: 'fail', error: new HttpError(failureDetail(plan, reply))};
	}
	const entry = state.pacing.get(plan.rateLimitKey);
	const delayMs = entry ? Math.max(0, entry.until - Date.now()) : (retryAfterSeconds ?? 1) * 1000;
	return {next: 'retry-after', delayMs, mode: 'fixed'};
}

function readRetryAfter(raw: string | undefined): number | null {
	if (!raw) return null;
	const numeric = Number(raw);
	if (Number.isFinite(numeric)) return numeric;
	const date = Date.parse(raw);
	if (!Number.isFinite(date)) return null;
	return Math.max(0, (date - Date.now()) / 1000);
}

function readNumericHeader(raw: string | undefined): number | null {
	if (!raw) return null;
	const value = Number(raw);
	return Number.isFinite(value) ? value * 1000 : null;
}

function extractCode(body: unknown): string | undefined {
	if (typeof body !== 'object' || body === null) return undefined;
	const c = (body as Record<string, unknown>).code;
	return typeof c === 'string' ? c : undefined;
}

function extractMessage(body: unknown): string | undefined {
	if (typeof body !== 'object' || body === null) return undefined;
	const m = (body as Record<string, unknown>).message;
	return typeof m === 'string' ? m : undefined;
}

async function invokeInterceptor(
	state: RuntimeState,
	ticket: AccountScopedWorkTicket,
	plan: Plan,
	interceptor: RestInterceptor,
	reply: RestResponse,
): Promise<AttemptDecision | null> {
	const retry = (extra: Record<string, string>): Promise<RestResponse> =>
		runRetryLoop(
			state,
			plan.routing,
			ticket,
			plan.method,
			plan.path,
			{...plan.options, headers: {...(plan.options.headers ?? {}), ...extra}, skipIntercept: true},
			plan.sudoApplied,
			0,
		);
	try {
		const finalReply = await awaitAccountScopedInteraction(ticket, Promise.resolve(interceptor(reply, retry)));
		return finalReply === undefined ? null : {next: 'deliver', reply: finalReply};
	} catch (err) {
		return {next: 'fail', error: err};
	}
}

function toAccountScopedFailure(ticket: AccountScopedWorkTicket, error: unknown): unknown {
	if (ticket.isStale && error instanceof DOMException && error.name === 'AbortError') {
		return accountScopedWorkAbortError();
	}
	return error;
}

function awaitAccountScopedInteraction<T>(ticket: AccountScopedWorkTicket, operation: Promise<T>): Promise<T> {
	ticket.assertCurrent();
	const signal = ticket.signal;
	if (signal.aborted) {
		return Promise.reject(signal.reason);
	}
	return new Promise<T>((resolve, reject) => {
		const handleAbort = (): void => {
			reject(signal.reason);
		};
		signal.addEventListener('abort', handleAbort, {once: true});
		operation.then(resolve, reject).finally(() => {
			signal.removeEventListener('abort', handleAbort);
		});
	});
}

function hasContentBlockedCode(body: unknown): boolean {
	return typeof body === 'object' && body !== null && (body as Record<string, unknown>).code === 'CONTENT_BLOCKED';
}

function propagateSudoToken(state: RuntimeState, received: string | null | undefined): void {
	if (received === undefined) return;
	state.sudo?.tokenListener(received);
}

function failureDetail(plan: Plan, reply: RestResponse): HttpErrorDetail {
	return {
		method: plan.method,
		path: plan.path,
		status: reply.status,
		body: reply.body,
		rawText: reply.text,
		responseHeaders: reply.headers,
	};
}

async function finalizeAfterRetriesExhausted<T>(
	state: RuntimeState,
	plan: Plan,
	outcome: TransportOutcome,
): Promise<RestResponse<T>> {
	if (outcome.status === 'transport-error') {
		log.warn(`transport gave up after retries: ${plan.method} ${plan.path}`);
		throw outcome.error;
	}
	if (outcome.status === 'aborted') {
		throw outcome.error;
	}
	const reply = outcome.reply;
	if (reply.ok || plan.mode === 'silent') {
		propagateSudoToken(state, outcome.receivedSudoToken);
		return reply as RestResponse<T>;
	}
	throw new HttpError(failureDetail(plan, reply));
}

function computeBackoffMs(retryIndex: number): number {
	const exponent = Math.min(retryIndex, 16);
	const target = Math.min(RETRY_BACKOFF_BASE_MS * RETRY_BACKOFF_FACTOR ** exponent, RETRY_BACKOFF_CAP_MS);
	const jitterRange = target * RETRY_BACKOFF_JITTER;
	const offset = (Math.random() * 2 - 1) * jitterRange;
	return Math.max(0, Math.floor(target + offset));
}

function delay(ms: number, signal?: AbortSignal): Promise<void> {
	if (ms <= 0) return Promise.resolve();
	return new Promise<void>((resolve, reject) => {
		const timer = setTimeout(() => {
			signal?.removeEventListener('abort', onAbort);
			resolve();
		}, ms);
		const onAbort = () => {
			clearTimeout(timer);
			reject(new DOMException('Request aborted', 'AbortError'));
		};
		if (signal) {
			if (signal.aborted) {
				clearTimeout(timer);
				reject(new DOMException('Request aborted', 'AbortError'));
				return;
			}
			signal.addEventListener('abort', onAbort, {once: true});
		}
	});
}

function sleepUntil(deadlineMs: number, signal?: AbortSignal): Promise<void> {
	return delay(Math.max(0, deadlineMs - Date.now()), signal);
}

function createHandle(
	external: AbortSignal | undefined,
	accountScoped: AbortSignal,
): {handle: RestRequestHandle; unlink: () => void} {
	const controller = new AbortController();
	const forward = () => controller.abort();
	const linked: Array<AbortSignal> = [];
	for (const signal of external === undefined ? [accountScoped] : [accountScoped, external]) {
		if (signal.aborted) {
			controller.abort();
			continue;
		}
		signal.addEventListener('abort', forward, {once: true});
		linked.push(signal);
	}
	return {
		handle: {
			abortController: controller,
			abort: forward,
		},
		unlink: () => {
			for (const signal of linked) {
				signal.removeEventListener('abort', forward);
			}
		},
	};
}

function mergeBody(options: RestRequestOptions, augmentation: Record<string, unknown>): RestRequestOptions {
	if (options.multipart !== undefined || options.raw !== undefined) {
		throw new Error('RestClient: cannot fold sudo payload into a multipart or raw body');
	}
	const {body: existing, multipart: _m, raw: _r, ...rest} = options;
	if (existing === undefined || existing === null) {
		return {...rest, body: augmentation};
	}
	if (
		typeof existing !== 'object' ||
		existing instanceof Blob ||
		existing instanceof ArrayBuffer ||
		existing instanceof FormData ||
		existing instanceof URLSearchParams
	) {
		throw new Error('RestClient: cannot fold sudo payload into a non-plain-object body');
	}
	return {...rest, body: {...(existing as Record<string, unknown>), ...augmentation}};
}

export const http = new RestClient();

AccountScopedWork.registerCancellation(() => http.clearPacing());
