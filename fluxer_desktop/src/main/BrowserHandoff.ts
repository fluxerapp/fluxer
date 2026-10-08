// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Buffer} from 'node:buffer';
import {parseDesktopHTTPTarget, requireDesktopHTTPOrigin} from '@electron/main/DesktopOutboundHTTP';
import type {RendererDocumentOwnerFactory} from '@electron/main/RendererDocumentOwner';
import type {
	RendererDocumentIpcEvent,
	RendererDocumentIpcRoutes,
	RendererDocumentOwner,
	RendererDocumentOwnerWatcher,
} from '@electron/main/RendererDocumentOwnership';
import type {DesktopSelectedInstanceClient} from '@electron/main/SelectedInstanceFetch';
import {Headers} from '@fluxer/constants/src/Headers';
import {HttpStatus, MimeType} from '@fluxer/constants/src/HttpConstants';
import type {
	DesktopHandoffInstance,
	DesktopHandoffSession,
	DesktopHandoffStatusResult,
	DesktopHandoffUser,
} from '@fluxer/desktop_ipc/src/BrowserHandoffContract';
import {
	DESKTOP_HANDOFF_CHANNELS,
	DesktopHandoffReturnMethod,
	DesktopHandoffStatus,
} from '@fluxer/desktop_ipc/src/BrowserHandoffContract';

const RENDERER_CONTEXT = 'Browser sign-in handoff';
const INITIATION_CONTEXT = 'Desktop handoff initiation';
const STATUS_CONTEXT = 'Desktop handoff status';
const HANDOFF_HTTP_TIMEOUT_MS = 15_000;
const HANDOFF_INITIATE_PATH = '/auth/handoff/initiate';
const HANDOFF_RESPONSE_MAX_BYTES = 64 * 1024;
const HANDOFF_ENDPOINT_MAX_LENGTH = 2048;
const HANDOFF_FIELD_MAX_LENGTH = 512;
const HANDOFF_TOKEN_MAX_LENGTH = 4096;
const HANDOFF_API_VERSION_MAX = 1000;
const HANDOFF_CODE_PATTERN = /^[A-Za-z0-9-]{1,64}$/u;
const HANDOFF_POLL_SECRET_PATTERN = /^[A-Za-z0-9_-]{1,256}$/u;
const HANDOFF_CODE_SEPARATOR_PATTERN = /[^A-Za-z0-9]/gu;
const TRAILING_SLASH_PATTERN = /\/+$/u;

interface BrowserHandoffLogger {
	warn: (message: string, ...args: Array<unknown>) => void;
}

interface DesktopBrowserHandoffDependencies {
	readonly logger: BrowserHandoffLogger;
	readonly rendererDocumentOwners: RendererDocumentOwnerFactory;
	readonly selectedInstanceClient: DesktopSelectedInstanceClient;
	readonly returnUri: () => string | null;
}

interface ActiveHandoffSession {
	readonly code: string;
	readonly expiresAtMs: number;
	readonly instance: DesktopHandoffInstance;
	readonly origin: string;
	readonly owner: RendererDocumentOwner;
	readonly watcher: RendererDocumentOwnerWatcher;
	inFlight: Promise<DesktopHandoffStatusResult> | null;
	pollSecret: string | null;
	grant: string | null;
}

interface HandoffJSONRequest {
	readonly body?: unknown;
	readonly instance: DesktopHandoffInstance;
	readonly method: 'GET' | 'POST';
	readonly origin: string;
	readonly path: string;
}

interface HandoffJSONResponse {
	readonly payload: Record<string, unknown> | null;
	readonly status: number;
}

class InvalidDesktopHandoffRequestError extends TypeError {
	public constructor(field: string) {
		super(`Desktop handoff ${field} is invalid`);
		this.name = 'InvalidDesktopHandoffRequestError';
	}
}

class InactiveDesktopHandoffSessionError extends Error {
	public constructor() {
		super('Desktop handoff session is not active for this renderer document');
		this.name = 'InactiveDesktopHandoffSessionError';
	}
}

class DesktopHandoffHTTPStatusError extends Error {
	public constructor(context: string, status: number) {
		super(`${context} failed with HTTP ${status}`);
		this.name = 'DesktopHandoffHTTPStatusError';
	}
}

class DesktopHandoffResponseSchemaError extends Error {
	public constructor(context: string) {
		super(`${context} response did not match the expected shape`);
		this.name = 'DesktopHandoffResponseSchemaError';
	}
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function boundedString(value: unknown, maxLength: number): string | null {
	if (typeof value !== 'string' || value.length === 0 || value.length > maxLength) {
		return null;
	}
	return value;
}

function requireEndpoint(value: unknown, field: string): string {
	const raw = boundedString(value, HANDOFF_ENDPOINT_MAX_LENGTH);
	if (raw == null) {
		throw new InvalidDesktopHandoffRequestError(field);
	}
	const endpoint = raw.replace(TRAILING_SLASH_PATTERN, '');
	if (parseDesktopHTTPTarget(endpoint) == null) {
		throw new InvalidDesktopHandoffRequestError(field);
	}
	return endpoint;
}

function requireApiVersion(value: unknown): number {
	if (typeof value !== 'number' || !Number.isSafeInteger(value) || value < 1 || value > HANDOFF_API_VERSION_MAX) {
		throw new InvalidDesktopHandoffRequestError('instance.apiVersion');
	}
	return value;
}

function requireDesktopHandoffInstance(value: unknown): DesktopHandoffInstance {
	if (!isRecord(value)) {
		throw new InvalidDesktopHandoffRequestError('instance');
	}
	return {
		apiEndpoint: requireEndpoint(value.apiEndpoint, 'instance.apiEndpoint'),
		apiVersion: requireApiVersion(value.apiVersion),
		webAppEndpoint: requireEndpoint(value.webAppEndpoint, 'instance.webAppEndpoint'),
	};
}

function requireHandoffCode(value: unknown): string {
	if (typeof value !== 'string' || !HANDOFF_CODE_PATTERN.test(value)) {
		throw new InvalidDesktopHandoffRequestError('code');
	}
	return value;
}

function normalizeHandoffCode(value: string): string {
	return value.replace(HANDOFF_CODE_SEPARATOR_PATTERN, '').toUpperCase();
}

function readHandoffUser(value: unknown): DesktopHandoffUser | null {
	if (!isRecord(value)) {
		return null;
	}
	const username = boundedString(value.username, HANDOFF_FIELD_MAX_LENGTH);
	const discriminator = boundedString(value.discriminator, HANDOFF_FIELD_MAX_LENGTH);
	if (username == null || discriminator == null) {
		return null;
	}
	const user: DesktopHandoffUser = {
		username,
		discriminator,
		global_name: boundedString(value.global_name, HANDOFF_FIELD_MAX_LENGTH),
		avatar: boundedString(value.avatar, HANDOFF_FIELD_MAX_LENGTH),
	};
	if (value.email === undefined) {
		return user;
	}
	return {...user, email: boundedString(value.email, HANDOFF_FIELD_MAX_LENGTH)};
}

function readHandoffStatusResult(payload: Record<string, unknown> | null): DesktopHandoffStatusResult {
	if (payload == null) {
		throw new DesktopHandoffResponseSchemaError(STATUS_CONTEXT);
	}
	if (payload.status === DesktopHandoffStatus.EXPIRED) {
		return {status: DesktopHandoffStatus.EXPIRED};
	}
	if (payload.status === DesktopHandoffStatus.DENIED) {
		return {status: DesktopHandoffStatus.DENIED};
	}
	if (payload.status !== DesktopHandoffStatus.COMPLETED) {
		return {status: DesktopHandoffStatus.PENDING};
	}
	const token = boundedString(payload.token, HANDOFF_TOKEN_MAX_LENGTH);
	const userId = boundedString(payload.user_id, HANDOFF_FIELD_MAX_LENGTH);
	if (token == null || userId == null) {
		throw new DesktopHandoffResponseSchemaError(STATUS_CONTEXT);
	}
	return {status: DesktopHandoffStatus.COMPLETED, token, userId, user: readHandoffUser(payload.user)};
}

export class DesktopBrowserHandoff {
	private readonly dependencies: DesktopBrowserHandoffDependencies;
	private readonly sessions = new Map<Electron.WebFrameMain, ActiveHandoffSession>();

	public constructor(dependencies: DesktopBrowserHandoffDependencies) {
		this.dependencies = dependencies;
	}

	public ipcRoutes(): RendererDocumentIpcRoutes {
		return Object.freeze({
			[DESKTOP_HANDOFF_CHANNELS.initiate]: (event, instance) => this.initiate(event, instance),
			[DESKTOP_HANDOFF_CHANNELS.status]: (event, code) => this.status(event, code),
		});
	}

	public acceptReturnLink(url: URL): void {
		const code = url.searchParams.get('code');
		const grant = url.searchParams.get('grant');
		if (code == null || !HANDOFF_CODE_PATTERN.test(code) || grant == null || !HANDOFF_POLL_SECRET_PATTERN.test(grant)) {
			this.dependencies.logger.warn('[BrowserHandoff] Ignored a malformed sign-in return link');
			return;
		}
		const normalizedCode = normalizeHandoffCode(code);
		const session = [...this.sessions.values()].find(
			(candidate) => normalizeHandoffCode(candidate.code) === normalizedCode,
		);
		if (session == null || session.pollSecret == null) {
			this.dependencies.logger.warn(
				'[BrowserHandoff] Ignored a sign-in return link for a request this app is not waiting on',
			);
			return;
		}
		session.grant = grant;
	}

	public cleanup(): void {
		for (const frame of [...this.sessions.keys()]) {
			this.releaseFrame(frame);
		}
	}

	private async initiate(event: RendererDocumentIpcEvent, request: unknown): Promise<DesktopHandoffSession> {
		const owner = this.dependencies.rendererDocumentOwners.capture(event, RENDERER_CONTEXT);
		const instance = requireDesktopHandoffInstance(request);
		const origin = requireDesktopHTTPOrigin(new URL(instance.apiEndpoint).origin);
		const returnUri = this.dependencies.returnUri();
		const response = await this.requestJSON({
			...(returnUri == null ? {} : {body: {return_uri: returnUri}}),
			instance,
			origin,
			method: 'POST',
			path: HANDOFF_INITIATE_PATH,
		});
		if (response.status !== HttpStatus.OK && response.status !== HttpStatus.CREATED) {
			throw new DesktopHandoffHTTPStatusError(INITIATION_CONTEXT, response.status);
		}
		if (response.payload == null) {
			throw new DesktopHandoffResponseSchemaError(INITIATION_CONTEXT);
		}
		const code = requireHandoffCode(response.payload.code);
		const expiresAt = boundedString(response.payload.expires_at, HANDOFF_FIELD_MAX_LENGTH);
		const expiresAtMs = expiresAt == null ? Number.NaN : Date.parse(expiresAt);
		if (expiresAt == null || !Number.isFinite(expiresAtMs)) {
			throw new DesktopHandoffResponseSchemaError(INITIATION_CONTEXT);
		}
		const pollSecret = boundedString(response.payload.poll_secret, HANDOFF_FIELD_MAX_LENGTH);
		const usablePollSecret = pollSecret != null && HANDOFF_POLL_SECRET_PATTERN.test(pollSecret) ? pollSecret : null;
		const returnMethod =
			returnUri != null &&
			usablePollSecret != null &&
			response.payload.return_method === DesktopHandoffReturnMethod.DEEP_LINK
				? DesktopHandoffReturnMethod.DEEP_LINK
				: DesktopHandoffReturnMethod.CODE;
		owner.requireCurrent(RENDERER_CONTEXT);
		this.rememberSession(owner, {code, expiresAtMs, instance, origin, pollSecret: usablePollSecret, grant: null});
		return {instance, code, expiresAt, returnMethod};
	}

	private async status(event: RendererDocumentIpcEvent, codeValue: unknown): Promise<DesktopHandoffStatusResult> {
		const code = requireHandoffCode(codeValue);
		const frame = event.senderFrame ?? null;
		const session = frame == null ? undefined : this.sessions.get(frame);
		if (frame == null || session == null || session.code !== code || !session.owner.matchesEvent(event)) {
			throw new InactiveDesktopHandoffSessionError();
		}
		if (session.expiresAtMs <= Date.now()) {
			this.releaseFrame(frame);
			return {status: DesktopHandoffStatus.EXPIRED};
		}
		if (session.inFlight == null) {
			const operation = this.readStatus(frame, session);
			const clear = (): void => {
				if (session.inFlight === operation) {
					session.inFlight = null;
				}
			};
			session.inFlight = operation;
			operation.then(clear, clear);
		}
		return session.inFlight;
	}

	private async readStatus(
		frame: Electron.WebFrameMain,
		session: ActiveHandoffSession,
	): Promise<DesktopHandoffStatusResult> {
		session.owner.requireCurrent(RENDERER_CONTEXT);
		const path = `/auth/handoff/${encodeURIComponent(session.code)}/status`;
		let response: HandoffJSONResponse | null = null;
		if (session.pollSecret != null) {
			const attempt = await this.requestJSON({
				body:
					session.grant == null
						? {poll_secret: session.pollSecret}
						: {poll_secret: session.pollSecret, grant: session.grant},
				instance: session.instance,
				method: 'POST',
				origin: session.origin,
				path,
			});
			if (attempt.status === HttpStatus.NOT_FOUND || attempt.status === HttpStatus.METHOD_NOT_ALLOWED) {
				session.pollSecret = null;
			} else {
				response = attempt;
			}
		}
		response ??= await this.requestJSON({instance: session.instance, method: 'GET', origin: session.origin, path});
		if (response.status !== HttpStatus.OK) {
			throw new DesktopHandoffHTTPStatusError(STATUS_CONTEXT, response.status);
		}
		const result = readHandoffStatusResult(response.payload);
		if (result.status !== DesktopHandoffStatus.PENDING && this.sessions.get(frame) === session) {
			this.releaseFrame(frame);
		}
		return result;
	}

	private async requestJSON({body, instance, method, origin, path}: HandoffJSONRequest): Promise<HandoffJSONResponse> {
		const headers: Record<string, string> = {[Headers.ACCEPT]: MimeType.JSON};
		if (method === 'POST') {
			headers[Headers.ORIGIN] = new URL(instance.webAppEndpoint).origin;
		}
		let encoded: Uint8Array | null = null;
		if (body !== undefined) {
			headers[Headers.CONTENT_TYPE] = MimeType.JSON;
			encoded = new TextEncoder().encode(JSON.stringify(body));
		}
		const response = await this.dependencies.selectedInstanceClient.fetch({
			body: encoded,
			expectedOrigin: origin,
			headers,
			method,
			timeoutMs: HANDOFF_HTTP_TIMEOUT_MS,
			url: `${instance.apiEndpoint}/v${instance.apiVersion}${path}`,
		});
		return {payload: this.readPayload(response.body), status: response.status};
	}

	private readPayload(body: Buffer | null): Record<string, unknown> | null {
		if (body == null || body.byteLength > HANDOFF_RESPONSE_MAX_BYTES) {
			return null;
		}
		try {
			const parsed: unknown = JSON.parse(body.toString('utf8'));
			return isRecord(parsed) ? parsed : null;
		} catch (error) {
			this.dependencies.logger.warn('[BrowserHandoff] Discarded a handoff response that was not JSON', error);
			return null;
		}
	}

	private rememberSession(
		owner: RendererDocumentOwner,
		session: Omit<ActiveHandoffSession, 'inFlight' | 'owner' | 'watcher'>,
	): void {
		const frame = owner.frame;
		this.releaseFrame(frame);
		this.sessions.set(frame, {
			...session,
			inFlight: null,
			owner,
			watcher: owner.watchInvalidation(() => this.releaseFrame(frame)),
		});
	}

	private releaseFrame(frame: Electron.WebFrameMain): void {
		const session = this.sessions.get(frame);
		if (session == null) {
			return;
		}
		this.sessions.delete(frame);
		try {
			session.watcher.dispose();
		} catch (error) {
			this.dependencies.logger.warn('[BrowserHandoff] Failed to dispose a renderer document watcher', error);
		}
	}
}
