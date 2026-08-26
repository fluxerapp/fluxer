// SPDX-License-Identifier: AGPL-3.0-or-later

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
import {Logger} from '@app/features/platform/utils/AppLogger';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';

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
type BodyShape =
	| {tag: 'empty'}
	| {tag: 'json'; payload: string}
	| {tag: 'urlencoded'; payload: string}
	| {tag: 'form'; payload: FormData}
	| {tag: 'opaque'; payload: XMLHttpRequestBodyInit};

interface RuntimeState {
	baseUrl: string;
	apiVersion: number;
	defaultTimeoutMs: number;
	defaultRetries: number;
	authProvider: () => string | null;
	sudo: SudoBindings | null;
	prepare?: RestClientHooks['prepareRequest'];
	globalIntercept?: RestInterceptor;
	pacing: Map<string, {until: number; note?: string}>;
}

interface Plan {
	method: HttpMethod;
	path: string;
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
	options: RestRequestOptions;
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

export class RestClient {
	private readonly state: RuntimeState = {
		baseUrl: '/api',
		apiVersion: 1,
		defaultTimeoutMs: 0,
		defaultRetries: 0,
		authProvider: () => null,
		sudo: null,
		pacing: new Map(),
	};

	configure(options: {baseUrl?: string; apiVersion?: number; timeoutMs?: number; retries?: number}): void {
		const s = this.state;
		if (options.baseUrl !== undefined) s.baseUrl = options.baseUrl;
		if (options.apiVersion !== undefined) s.apiVersion = options.apiVersion;
		if (options.timeoutMs !== undefined) s.defaultTimeoutMs = options.timeoutMs;
		if (options.retries !== undefined) s.defaultRetries = options.retries;
	}

	installAuth(provider: () => string | null): void {
		this.state.authProvider = provider;
	}

	installSudo(bindings: SudoBindings): void {
		this.state.sudo = bindings;
	}

	installHooks(hooks: RestClientHooks): void {
		this.state.prepare = hooks.prepareRequest;
		this.state.globalIntercept = hooks.intercept;
	}

	dispatch<T = unknown>(method: HttpMethod, path: string, options: RestRequestOptions = {}): Promise<RestResponse<T>> {
		return runWithSudoEscalation<T>(this.state, method, path, options, 'fresh');
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
	method: HttpMethod,
	path: string,
	options: RestRequestOptions,
	phase: SudoPhase,
	overrideOptions?: RestRequestOptions,
): Promise<RestResponse<T>> {
	const effective = overrideOptions ?? options;
	const sudoApplied = phase !== 'fresh';
	try {
		return await runRetryLoop<T>(state, method, path, effective, sudoApplied, 0);
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
			const merged = await sudo.prompt(method, path, err);
			if (!merged) throw err;
			return runWithSudoEscalation<T>(state, method, path, options, 'reissued', mergeBody(options, merged));
		}
		switch (phase) {
			case 'fresh': {
				sudo.invalidate();
				const merged = await sudo.prompt(method, path, err);
				if (!merged) throw err;
				return runWithSudoEscalation<T>(state, method, path, options, 'reissued', mergeBody(options, merged));
			}
			case 'reissued': {
				sudo.onFailure(err);
				const second = await sudo.prompt(method, path, err);
				if (!second) throw err;
				return runWithSudoEscalation<T>(state, method, path, options, 'reissued-twice', mergeBody(options, second));
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
	method: HttpMethod,
	path: string,
	options: RestRequestOptions,
	sudoApplied: boolean,
	attempt: number,
): Promise<RestResponse<T>> {
	const plan = composePlan(state, method, path, options, sudoApplied);
	if (plan.retries > 0) await waitUntilOnline(plan.signal);
	const pacingHit = consultPacing(state.pacing, plan.rateLimitKey);
	if (pacingHit) {
		if (plan.mode === 'auto-retry') {
			await sleepUntil(pacingHit.until, plan.signal);
		} else {
			const synthesized = synthesizePacingReply<T>(plan, pacingHit);
			if (plan.mode === 'silent') {
				return synthesized;
			}
			throw new HttpError(failureDetail(plan, synthesized));
		}
	}
	const handle = createHandle(plan.signal);
	state.prepare?.(handle);
	const outcome = await performTransport(plan, handle);
	const decision = await reactToOutcome(state, plan, outcome);
	switch (decision.next) {
		case 'deliver':
			return decision.reply as RestResponse<T>;
		case 'retry-after': {
			if (attempt >= plan.retries) {
				return finalizeAfterRetriesExhausted<T>(state, plan, outcome);
			}
			const wait = decision.mode === 'backoff' ? computeBackoffMs(attempt) : decision.delayMs;
			if (outcome.status === 'transport-error' && !navigator.onLine) {
				await waitUntilOnline(plan.signal);
			} else {
				await delay(wait, plan.signal);
			}
			return runRetryLoop<T>(state, method, path, options, sudoApplied, attempt + 1);
		}
		case 'fail':
			throw decision.error;
	}
}

function composePlan(
	state: RuntimeState,
	method: HttpMethod,
	path: string,
	options: RestRequestOptions,
	sudoApplied: boolean,
): Plan {
	const url = resolveUrl(state, path, options.query);
	const body = encodeBody(options);
	const sameOrigin = !looksAbsolute(path) && !isOffOrigin(url);
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
		url,
		rateLimitKey: path,
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

function resolveUrl(state: RuntimeState, path: string, query: RestRequestOptions['query']): string {
	const seed = looksAbsolute(path) ? path : `${state.baseUrl}/v${state.apiVersion}${path}`;
	const url = new URL(seed, window.location.origin);
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

function looksAbsolute(path: string): boolean {
	return path.startsWith('//') || /^[a-z][a-z0-9+.-]*:\/\//i.test(path);
}

function isOffOrigin(url: string): boolean {
	try {
		return new URL(url).origin !== window.location.origin;
	} catch {
		return false;
	}
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
		accumulator['X-Fluxer-Features'] = 'view_channel_members_permission';
	}
	const contentType = inferContentType(input.body);
	if (contentType) accumulator['Content-Type'] = contentType;
	if (input.reason) accumulator['X-Audit-Log-Reason'] = encodeURIComponent(input.reason);
	if (input.auth !== 'none' && input.sameOrigin) {
		const token = input.state.authProvider();
		if (token) accumulator['Authorization'] = token;
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

function consultPacing(
	pacing: Map<string, {until: number; note?: string}>,
	key: string,
): {until: number; note?: string} | null {
	const entry = pacing.get(key);
	if (!entry) return null;
	if (entry.until <= Date.now()) {
		pacing.delete(key);
		return null;
	}
	return entry;
}

function recordPacing(
	pacing: Map<string, {until: number; note?: string}>,
	key: string,
	retryAfterSeconds: number | null,
	headerMs: number | null,
	note?: string,
): void {
	const fallbackMs = 1000;
	const ms =
		headerMs !== null && headerMs > 0
			? headerMs
			: retryAfterSeconds !== null && retryAfterSeconds > 0
				? retryAfterSeconds * 1000
				: fallbackMs;
	pacing.set(key, {until: Date.now() + ms, note});
}

function synthesizePacingReply<T>(_plan: Plan, hit: {until: number; note?: string}): RestResponse<T> {
	const remaining = Math.max(0, hit.until - Date.now());
	const headers: Record<string, string> = {
		'retry-after': String(Math.ceil(remaining / 1000)),
		'content-type': 'application/json',
	};
	const payload = {
		message: hit.note ?? 'You are being rate limited.',
		retry_after: remaining / 1000,
		global: false,
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
			xhr.upload.addEventListener('progress', plan.onProgress);
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

async function reactToOutcome(state: RuntimeState, plan: Plan, outcome: TransportOutcome): Promise<AttemptDecision> {
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
	const interceptor = plan.options.intercept ?? state.globalIntercept;
	if (interceptor) {
		const intercepted = await invokeInterceptor(state, plan, interceptor, reply);
		if (intercepted.next !== 'passthrough') {
			return intercepted.decision;
		}
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
	recordPacing(state.pacing, plan.rateLimitKey, retryAfterSeconds, headerMs, note);
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

function extractMessage(body: unknown): string | undefined {
	if (typeof body !== 'object' || body === null) return undefined;
	const m = (body as Record<string, unknown>).message;
	return typeof m === 'string' ? m : undefined;
}

type InterceptorFold = {next: 'passthrough'} | {next: 'used'; decision: AttemptDecision};

async function invokeInterceptor(
	state: RuntimeState,
	plan: Plan,
	interceptor: RestInterceptor,
	reply: RestResponse,
): Promise<InterceptorFold> {
	let captured: Error | null = null;
	let chained: Promise<RestResponse> | null = null;
	const retry = (extra: Record<string, string>): Promise<RestResponse> => {
		const augmented: RestRequestOptions = {
			...plan.options,
			headers: {...(plan.options.headers ?? {}), ...extra},
		};
		chained = runRetryLoop(state, plan.method, plan.path, augmented, plan.sudoApplied, 0);
		return chained;
	};
	const reject = (err: Error) => {
		captured = err;
	};
	let result: boolean | undefined | Promise<RestResponse | undefined>;
	try {
		result = interceptor(reply, retry, reject);
	} catch (err) {
		return {next: 'used', decision: {next: 'fail', error: err}};
	}
	if (captured) {
		return {next: 'used', decision: {next: 'fail', error: captured}};
	}
	if (result instanceof Promise) {
		try {
			const finalReply = await result;
			if (captured) {
				return {next: 'used', decision: {next: 'fail', error: captured}};
			}
			if (finalReply === undefined && chained) {
				return {next: 'used', decision: {next: 'deliver', reply: await chained}};
			}
			if (finalReply === undefined) {
				return {next: 'passthrough'};
			}
			return {next: 'used', decision: {next: 'deliver', reply: finalReply}};
		} catch (err) {
			return {next: 'used', decision: {next: 'fail', error: err}};
		}
	}
	if (result === true && chained) {
		try {
			return {next: 'used', decision: {next: 'deliver', reply: await chained}};
		} catch (err) {
			return {next: 'used', decision: {next: 'fail', error: err}};
		}
	}
	return {next: 'passthrough'};
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

function createHandle(external?: AbortSignal): RestRequestHandle {
	const controller = new AbortController();
	if (external) {
		if (external.aborted) controller.abort();
		else external.addEventListener('abort', () => controller.abort(), {once: true});
	}
	return {
		abortController: controller,
		abort: () => controller.abort(),
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
