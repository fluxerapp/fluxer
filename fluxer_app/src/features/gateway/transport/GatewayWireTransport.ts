// SPDX-License-Identifier: AGPL-3.0-or-later

import {isDesktopLocalAppDocument} from '@app/features/platform/DesktopLocalAppRuntime';
import {takePrebootGatewaySocket} from '@app/features/platform/state/PrebootNetworkHandoff';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {randomUuid} from '@app/features/platform/utils/RandomUuid';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {
	NATIVE_GATEWAY_TRANSPORT_PROXIED_MESSAGE,
	type NativeGatewayTransportAPI,
	type NativeGatewayTransportCloseRequest,
	type NativeGatewayTransportEvent,
	NativeGatewayTransportEventKind,
	NativeGatewayTransportMode,
	type NativeGatewayTransportSendBinaryRequest,
	type NativeGatewayTransportSendTextRequest,
} from '@fluxer/desktop_ipc/src/GatewayTransportContract';

const log = new Logger('GatewayWireTransport');

const WEBSOCKET_NORMAL_CLOSURE = 1000;
const WEBSOCKET_ABNORMAL_CLOSURE = 1006;
const DESKTOP_CONNECTION_ID_PREFIX = 'gateway-renderer-';
const DESKTOP_RETIRED_CONNECTION_ID_LIMIT = 256;
const DESKTOP_ROUTER_GLOBAL_KEY = '__fluxerDesktopGatewayTransport';
const DESKTOP_ROUTER_ID_PATTERN = /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/;
const NATIVE_GATEWAY_TRANSPORT_METHODS = ['create', 'sendText', 'sendBinary', 'close', 'dispose', 'onEvent'] as const;
const NATIVE_GATEWAY_FAILURES_BEFORE_BROWSER_FALLBACK = 3;
const NATIVE_GATEWAY_SERVER_RESPONSE_ERROR_PATTERN = /HTTP error:/u;

export const GatewayWireReadyState = Object.freeze({
	CONNECTING: 0,
	OPEN: 1,
	CLOSING: 2,
	CLOSED: 3,
} as const);

export type GatewayWireReadyState = (typeof GatewayWireReadyState)[keyof typeof GatewayWireReadyState];

export const GatewayWireTransportKind = Object.freeze({
	BROWSER: 'browser',
	DESKTOP_NATIVE: 'desktop-native',
} as const);

export type GatewayWireTransportKind = (typeof GatewayWireTransportKind)[keyof typeof GatewayWireTransportKind];

export interface GatewayWireOpenEvent {
	target: GatewayWireTransport;
}

export interface GatewayWireMessageEvent {
	target: GatewayWireTransport;
	data: string | ArrayBuffer | Blob;
}

export interface GatewayWireCloseEvent {
	target: GatewayWireTransport;
	code: number;
	reason: string;
	wasClean: boolean;
}

export interface GatewayWireErrorEvent {
	target: GatewayWireTransport;
	error: unknown;
}

export interface GatewayWireTransportEventMap {
	open: GatewayWireOpenEvent;
	message: GatewayWireMessageEvent;
	close: GatewayWireCloseEvent;
	error: GatewayWireErrorEvent;
}

export type GatewayWireTransportEvents = {
	[K in keyof GatewayWireTransportEventMap]: (event: GatewayWireTransportEventMap[K]) => void;
};

export interface GatewayWireTransportOptions {
	readonly binaryType: BinaryType;
	readonly adoptPrebootSocket?: boolean;
}

export interface GatewayWireTransport {
	readonly kind: GatewayWireTransportKind;
	readonly readyState: number;
	start(): void;
	send(data: string | Uint8Array): void;
	close(code: number, reason: string): void;
	dispose(): void;
	on<K extends keyof GatewayWireTransportEvents>(event: K, callback: GatewayWireTransportEvents[K]): () => void;
}

class DesktopNativeGatewayTransportNotOpenError extends Error {
	constructor() {
		super('Desktop native gateway transport is not open');
		this.name = 'DesktopNativeGatewayTransportNotOpenError';
	}
}

export class InvalidDesktopGatewayTransportRouterStateError extends Error {
	constructor(reason: string) {
		super(`Desktop gateway transport router state is invalid: ${reason}`);
		this.name = 'InvalidDesktopGatewayTransportRouterStateError';
	}
}

type ListenerMap = {
	[K in keyof GatewayWireTransportEvents]: Set<GatewayWireTransportEvents[K]>;
};

abstract class BaseGatewayWireTransport implements GatewayWireTransport {
	private readonly listeners: ListenerMap = {
		open: new Set(),
		message: new Set(),
		close: new Set(),
		error: new Set(),
	};

	abstract readonly kind: GatewayWireTransportKind;
	abstract readonly readyState: number;
	abstract start(): void;
	abstract send(data: string | Uint8Array): void;
	abstract close(code: number, reason: string): void;
	abstract dispose(): void;

	on<K extends keyof GatewayWireTransportEvents>(event: K, callback: GatewayWireTransportEvents[K]): () => void {
		this.listeners[event].add(callback);
		return () => {
			this.listeners[event].delete(callback);
		};
	}

	protected emit<K extends keyof GatewayWireTransportEvents>(event: K, payload: GatewayWireTransportEventMap[K]): void {
		for (const callback of [...this.listeners[event]]) {
			(callback as (event: GatewayWireTransportEventMap[K]) => void)(payload);
		}
	}

	protected clearListeners(): void {
		for (const listeners of Object.values(this.listeners)) {
			listeners.clear();
		}
	}
}

function toArrayBufferCopy(data: Uint8Array): ArrayBuffer {
	const copy = new Uint8Array(data.byteLength);
	copy.set(data);
	return copy.buffer;
}

function isBrowserSocketActive(socket: WebSocket): boolean {
	return socket.readyState === WebSocket.CONNECTING || socket.readyState === WebSocket.OPEN;
}

function describeError(error: unknown): string {
	return error instanceof Error ? error.message : String(error);
}

class BrowserGatewayWireTransport extends BaseGatewayWireTransport {
	readonly kind = GatewayWireTransportKind.BROWSER;
	private socket: WebSocket | null = null;
	private state: number = GatewayWireReadyState.CONNECTING;
	private disposed = false;

	constructor(
		private readonly url: string,
		private readonly options: GatewayWireTransportOptions,
	) {
		super();
	}

	get readyState(): number {
		return this.socket?.readyState ?? this.state;
	}

	start(): void {
		if (this.disposed) return;
		if (this.socket != null) return;
		const preboot = this.options.adoptPrebootSocket === true ? takePrebootGatewaySocket(this.url) : null;
		const socket = preboot?.socket ?? new WebSocket(this.url);
		this.socket = socket;
		socket.binaryType = this.options.binaryType;
		socket.addEventListener('open', this.handleOpen);
		socket.addEventListener('message', this.handleMessage);
		socket.addEventListener('close', this.handleClose);
		socket.addEventListener('error', this.handleError);
		if (preboot == null) return;
		log.info(`Adopted the preboot gateway socket with ${preboot.messages.length} buffered message(s)`);
		if (socket.readyState === WebSocket.OPEN) {
			this.handleOpen();
		}
		for (const message of preboot.messages) {
			if (this.socket !== socket) return;
			this.handleMessage(message);
		}
	}

	send(data: string | Uint8Array): void {
		const socket = this.socket;
		if (socket == null || socket.readyState !== WebSocket.OPEN) {
			throw new Error('Browser gateway transport is not open');
		}
		if (typeof data === 'string') {
			socket.send(data);
			return;
		}
		socket.send(toArrayBufferCopy(data));
	}

	close(code: number, reason: string): void {
		if (this.socket == null) {
			this.state = GatewayWireReadyState.CLOSED;
			return;
		}
		this.socket.close(code, reason);
	}

	dispose(): void {
		if (this.disposed) return;
		this.disposed = true;
		const socket = this.socket;
		this.socket = null;
		this.state = GatewayWireReadyState.CLOSED;
		if (socket != null) {
			socket.removeEventListener('open', this.handleOpen);
			socket.removeEventListener('message', this.handleMessage);
			socket.removeEventListener('close', this.handleClose);
			socket.removeEventListener('error', this.handleError);
			if (isBrowserSocketActive(socket)) {
				socket.close(WEBSOCKET_NORMAL_CLOSURE, 'Disposing stale socket');
			}
		}
		this.clearListeners();
	}

	private handleOpen = (): void => {
		this.state = GatewayWireReadyState.OPEN;
		this.emit('open', {target: this});
	};

	private handleMessage = (event: MessageEvent): void => {
		this.emit('message', {target: this, data: event.data as string | ArrayBuffer | Blob});
	};

	private handleClose = (event: CloseEvent): void => {
		this.state = GatewayWireReadyState.CLOSED;
		this.emit('close', {target: this, code: event.code, reason: event.reason, wasClean: event.wasClean});
	};

	private handleError = (event: Event): void => {
		this.emit('error', {target: this, error: event});
	};
}

interface PendingDesktopClose {
	code: number;
	reason: string;
}

interface DesktopNativeGatewayDisposal {
	readonly connectionId: string;
	readonly completion: Promise<void>;
}

class DesktopNativeGatewayWireTransport extends BaseGatewayWireTransport {
	readonly kind = GatewayWireTransportKind.DESKTOP_NATIVE;
	private connectionId: string | null = null;
	private state: number = GatewayWireReadyState.CONNECTING;
	private unsubscribe: (() => void) | null = null;
	private disposed = false;
	private started = false;
	private nativeCreateCompleted = false;
	private pendingClose: PendingDesktopClose | null = null;
	private nativeDisposal: DesktopNativeGatewayDisposal | null = null;
	private opened = false;
	private serverRespondedBeforeOpen = false;

	constructor(
		private readonly url: string,
		private readonly router: DesktopGatewayTransportRouter,
	) {
		super();
	}

	get readyState(): number {
		return this.state;
	}

	start(): void {
		if (this.disposed) return;
		if (this.started) return;
		this.started = true;
		this.open();
	}

	send(data: string | Uint8Array): void {
		const connectionId = this.connectionId;
		if (this.state !== GatewayWireReadyState.OPEN || connectionId == null || !this.nativeCreateCompleted) {
			throw new DesktopNativeGatewayTransportNotOpenError();
		}
		const delivered =
			typeof data === 'string'
				? this.router.sendText({connectionId, payload: data})
				: this.router.sendBinary({connectionId, payload: toArrayBufferCopy(data)});
		delivered.catch((error: unknown) => {
			if (this.disposed) return;
			this.emit('error', {target: this, error});
		});
	}

	close(code: number, reason: string): void {
		if (this.disposed) return;
		if (this.state === GatewayWireReadyState.CLOSED) return;
		const connectionId = this.connectionId;
		const closeRequest: PendingDesktopClose = {code, reason};
		this.pendingClose = closeRequest;
		this.state = GatewayWireReadyState.CLOSING;
		if (connectionId == null || !this.nativeCreateCompleted) return;
		this.router.close({...closeRequest, connectionId}).catch((error: unknown) => {
			this.failClose(error);
		});
	}

	dispose(): void {
		if (this.disposed) return;
		this.disposed = true;
		const connectionId = this.connectionId;
		const nativeCreateCompleted = this.nativeCreateCompleted;
		this.connectionId = null;
		this.nativeCreateCompleted = false;
		this.pendingClose = null;
		this.state = GatewayWireReadyState.CLOSED;
		this.detachRouterListener();
		if (connectionId != null && nativeCreateCompleted) {
			this.disposeNativeConnection(connectionId).catch((error: unknown) => {
				log.warn('Failed to dispose the desktop native gateway connection', error);
			});
		}
		this.clearListeners();
	}

	private disposeNativeConnection(connectionId: string): Promise<void> {
		const existing = this.nativeDisposal;
		if (existing != null) {
			if (existing.connectionId !== connectionId) {
				throw new Error(
					`Desktop native gateway transport cannot dispose ${connectionId} after disposing ${existing.connectionId}`,
				);
			}
			return existing.completion;
		}
		const completion = this.router.dispose(connectionId);
		this.nativeDisposal = {connectionId, completion};
		return completion;
	}

	private detachRouterListener(): void {
		const unsubscribe = this.unsubscribe;
		this.unsubscribe = null;
		unsubscribe?.();
	}

	private open(): void {
		let creation: DesktopGatewayTransportCreation;
		try {
			creation = this.router.create({
				url: this.url,
				callback: (event) => {
					if (this.disposed) return;
					this.handleRouterEvent(event);
				},
			});
		} catch (error) {
			this.failOpen(error);
			return;
		}
		this.connectionId = creation.connectionId;
		this.unsubscribe = creation.unsubscribe;
		creation.created
			.then(() => this.finishOpen(creation.connectionId))
			.catch((error: unknown) => {
				this.failOpen(error);
			});
	}

	private async finishOpen(connectionId: string): Promise<void> {
		if (this.disposed) {
			await this.disposeNativeConnection(connectionId);
			return;
		}
		if (this.connectionId !== connectionId) return;
		if (this.state === GatewayWireReadyState.CLOSED) return;
		this.nativeCreateCompleted = true;
		const pendingClose = this.pendingClose;
		if (pendingClose == null) return;
		this.pendingClose = null;
		this.state = GatewayWireReadyState.CLOSED;
		try {
			await this.disposeNativeConnection(connectionId);
		} catch (error) {
			this.failClose(error);
			return;
		}
		if (this.disposed) return;
		if (this.connectionId === connectionId) {
			this.connectionId = null;
		}
		this.nativeCreateCompleted = false;
		this.unsubscribe = null;
		this.emit('close', {target: this, code: pendingClose.code, reason: pendingClose.reason, wasClean: true});
	}

	private abandonNativeConnection(): string | null {
		const connectionId = this.connectionId;
		this.state = GatewayWireReadyState.CLOSED;
		this.connectionId = null;
		this.nativeCreateCompleted = false;
		this.pendingClose = null;
		this.detachRouterListener();
		return connectionId;
	}

	private emitTransportFailure(error: unknown): void {
		this.emit('error', {target: this, error});
		this.emit('close', {
			target: this,
			code: WEBSOCKET_ABNORMAL_CLOSURE,
			reason: describeError(error),
			wasClean: false,
		});
	}

	private failOpen(error: unknown): void {
		if (this.disposed) return;
		if (describeError(error).includes(NATIVE_GATEWAY_TRANSPORT_PROXIED_MESSAGE)) {
			switchProxiedGatewayToBrowserTransport();
		} else {
			recordNativeGatewayFailureWithoutOpen();
		}
		this.abandonNativeConnection();
		this.emitTransportFailure(error);
	}

	private failClose(error: unknown): void {
		if (this.disposed) return;
		const connectionId = this.abandonNativeConnection();
		if (connectionId != null) {
			this.disposeNativeConnection(connectionId).catch((disposeError: unknown) => {
				log.warn('Failed to dispose the desktop native gateway connection after a close failure', disposeError);
			});
		}
		this.emitTransportFailure(error);
	}

	private handleRouterEvent(event: NativeGatewayTransportEvent): void {
		switch (event.kind) {
			case NativeGatewayTransportEventKind.OPEN:
				if (this.state === GatewayWireReadyState.CLOSING) return;
				this.state = GatewayWireReadyState.OPEN;
				this.opened = true;
				recordNativeGatewayOpen();
				this.emit('open', {target: this});
				return;
			case NativeGatewayTransportEventKind.MESSAGE:
				if (this.state !== GatewayWireReadyState.OPEN) return;
				if (event.data == null) {
					this.emit('error', {
						target: this,
						error: new Error('Desktop native gateway transport delivered a text frame without a payload'),
					});
					return;
				}
				this.emit('message', {target: this, data: event.data});
				return;
			case NativeGatewayTransportEventKind.BINARY:
				if (this.state !== GatewayWireReadyState.OPEN) return;
				if (event.binary == null) {
					this.emit('error', {
						target: this,
						error: new Error('Desktop native gateway transport delivered a binary frame without a payload'),
					});
					return;
				}
				this.emit('message', {target: this, data: event.binary});
				return;
			case NativeGatewayTransportEventKind.CLOSE:
				if (!this.opened && this.state !== GatewayWireReadyState.CLOSING && !this.serverRespondedBeforeOpen) {
					recordNativeGatewayFailureWithoutOpen();
				}
				this.abandonNativeConnection();
				this.emit('close', {
					target: this,
					code: event.code ?? WEBSOCKET_ABNORMAL_CLOSURE,
					reason: event.reason ?? '',
					wasClean: event.wasClean ?? false,
				});
				return;
			case NativeGatewayTransportEventKind.ERROR:
				if (!this.opened && NATIVE_GATEWAY_SERVER_RESPONSE_ERROR_PATTERN.test(event.message ?? '')) {
					this.serverRespondedBeforeOpen = true;
				}
				this.emit('error', {
					target: this,
					error: new Error(event.message ?? 'Desktop native gateway transport reported an error'),
				});
				return;
		}
	}
}

type DesktopGatewayTransportEventCallback = (event: NativeGatewayTransportEvent) => void;

interface DesktopGatewayTransportCreationRequest {
	readonly url: string;
	readonly callback: DesktopGatewayTransportEventCallback;
}

interface DesktopGatewayTransportCreation {
	readonly connectionId: string;
	readonly created: Promise<void>;
	readonly unsubscribe: () => void;
}

interface DesktopGatewayTransportRouter {
	create(request: DesktopGatewayTransportCreationRequest): DesktopGatewayTransportCreation;
	sendText(request: NativeGatewayTransportSendTextRequest): Promise<void>;
	sendBinary(request: NativeGatewayTransportSendBinaryRequest): Promise<void>;
	close(request: NativeGatewayTransportCloseRequest): Promise<void>;
	dispose(connectionId: string): Promise<void>;
	shutdown(): void;
}

interface DesktopGatewayTransportRouterState {
	rendererRouterId: string;
	api: NativeGatewayTransportAPI | null;
	router: DesktopGatewayTransportRouter | null;
	nativeFailuresWithoutOpen: number;
}

function requireRouterState(value: unknown): DesktopGatewayTransportRouterState {
	if (typeof value !== 'object' || value == null || Array.isArray(value)) {
		throw new InvalidDesktopGatewayTransportRouterStateError('value must be a plain state object');
	}
	const state = value as Record<string, unknown>;
	if (typeof state.rendererRouterId !== 'string' || !DESKTOP_ROUTER_ID_PATTERN.test(state.rendererRouterId)) {
		throw new InvalidDesktopGatewayTransportRouterStateError('renderer identity must be a canonical UUID v4');
	}
	if (!Object.hasOwn(state, 'api') || !Object.hasOwn(state, 'router')) {
		throw new InvalidDesktopGatewayTransportRouterStateError('API and router ownership fields are required');
	}
	if (state.api != null && typeof state.api !== 'object') {
		throw new InvalidDesktopGatewayTransportRouterStateError('API owner must be an object or null');
	}
	if (state.router != null && typeof state.router !== 'object') {
		throw new InvalidDesktopGatewayTransportRouterStateError('router owner must be an object or null');
	}
	if (
		typeof state.nativeFailuresWithoutOpen !== 'number' ||
		!Number.isSafeInteger(state.nativeFailuresWithoutOpen) ||
		state.nativeFailuresWithoutOpen < 0
	) {
		throw new InvalidDesktopGatewayTransportRouterStateError('native failure count must be a non-negative integer');
	}
	return value as DesktopGatewayTransportRouterState;
}

function getRouterState(): DesktopGatewayTransportRouterState {
	const existing: unknown = Reflect.get(globalThis, DESKTOP_ROUTER_GLOBAL_KEY);
	if (existing != null) {
		return requireRouterState(existing);
	}
	const state: DesktopGatewayTransportRouterState = {
		rendererRouterId: randomUuid(),
		api: null,
		router: null,
		nativeFailuresWithoutOpen: 0,
	};
	if (!Reflect.set(globalThis, DESKTOP_ROUTER_GLOBAL_KEY, state)) {
		throw new InvalidDesktopGatewayTransportRouterStateError('state installation failed');
	}
	return state;
}

function isNativeGatewayFallbackActive(state: DesktopGatewayTransportRouterState): boolean {
	return state.nativeFailuresWithoutOpen >= NATIVE_GATEWAY_FAILURES_BEFORE_BROWSER_FALLBACK;
}

function recordNativeGatewayOpen(): void {
	const state = getRouterState();
	if (isNativeGatewayFallbackActive(state)) return;
	state.nativeFailuresWithoutOpen = 0;
}

function recordNativeGatewayFailureWithoutOpen(): void {
	if (globalThis.navigator?.onLine === false) return;
	const state = getRouterState();
	if (isNativeGatewayFallbackActive(state)) return;
	state.nativeFailuresWithoutOpen += 1;
	if (isNativeGatewayFallbackActive(state)) {
		log.warn(
			`Desktop native gateway transport failed ${state.nativeFailuresWithoutOpen} times before opening, using the browser transport`,
		);
	}
}

function switchProxiedGatewayToBrowserTransport(): void {
	const state = getRouterState();
	if (isNativeGatewayFallbackActive(state)) return;
	state.nativeFailuresWithoutOpen = NATIVE_GATEWAY_FAILURES_BEFORE_BROWSER_FALLBACK;
	log.warn('Desktop native gateway transport cannot use the configured proxy, using the browser transport');
}

function createDesktopConnectionId(): string {
	return `${DESKTOP_CONNECTION_ID_PREFIX}${getRouterState().rendererRouterId}:${randomUuid()}`;
}

function isConnectionIdFromThisDocument(connectionId: string): boolean {
	return connectionId.startsWith(`${DESKTOP_CONNECTION_ID_PREFIX}${getRouterState().rendererRouterId}:`);
}

class DesktopGatewayTransportEventRouter implements DesktopGatewayTransportRouter {
	private readonly listeners = new Map<string, DesktopGatewayTransportEventCallback>();
	private readonly retiredConnectionIds = new Set<string>();
	private removeNativeListener: (() => void) | null = null;

	constructor(private readonly api: NativeGatewayTransportAPI) {}

	create({url, callback}: DesktopGatewayTransportCreationRequest): DesktopGatewayTransportCreation {
		this.ensureListening();
		const connectionId = createDesktopConnectionId();
		this.registerConnection(connectionId, callback);
		try {
			const created = this.api
				.create({connectionId, url, mode: NativeGatewayTransportMode.GATEWAY})
				.then((result) => {
					if (result.connectionId !== connectionId) {
						throw new Error('Desktop native gateway transport returned a mismatched connection id');
					}
				})
				.catch((error: unknown) => {
					this.releaseConnection(connectionId);
					throw error;
				});
			return {connectionId, created, unsubscribe: () => this.releaseConnection(connectionId)};
		} catch (error) {
			this.releaseConnection(connectionId);
			throw error;
		}
	}

	sendText(request: NativeGatewayTransportSendTextRequest): Promise<void> {
		return this.api.sendText(request);
	}

	sendBinary(request: NativeGatewayTransportSendBinaryRequest): Promise<void> {
		return this.api.sendBinary(request);
	}

	close(request: NativeGatewayTransportCloseRequest): Promise<void> {
		return this.api.close(request);
	}

	async dispose(connectionId: string): Promise<void> {
		this.releaseConnection(connectionId);
		await this.api.dispose(connectionId);
	}

	shutdown(): void {
		this.removeNativeListener?.();
		this.removeNativeListener = null;
		this.listeners.clear();
		this.retiredConnectionIds.clear();
	}

	private ensureListening(): void {
		if (this.removeNativeListener != null) return;
		this.removeNativeListener = this.api.onEvent((event) => this.handleNativeEvent(event));
	}

	private registerConnection(connectionId: string, callback: DesktopGatewayTransportEventCallback): void {
		if (this.listeners.has(connectionId)) {
			throw new Error(`Desktop native gateway transport connection ${connectionId} is already registered`);
		}
		this.retiredConnectionIds.delete(connectionId);
		this.listeners.set(connectionId, callback);
	}

	private handleNativeEvent(event: NativeGatewayTransportEvent): void {
		const listener = this.listeners.get(event.connectionId);
		if (listener == null) {
			if (!this.retiredConnectionIds.has(event.connectionId) && isConnectionIdFromThisDocument(event.connectionId)) {
				log.error(`Desktop native gateway event ${event.kind} has no registered connection`);
			}
			return;
		}
		listener(event);
		if (event.kind === NativeGatewayTransportEventKind.CLOSE) {
			this.releaseConnection(event.connectionId);
		}
	}

	private releaseConnection(connectionId: string): void {
		if (!this.listeners.delete(connectionId)) return;
		this.retiredConnectionIds.delete(connectionId);
		this.retiredConnectionIds.add(connectionId);
		while (this.retiredConnectionIds.size > DESKTOP_RETIRED_CONNECTION_ID_LIMIT) {
			const oldest = this.retiredConnectionIds.values().next().value;
			if (oldest == null) return;
			this.retiredConnectionIds.delete(oldest);
		}
	}
}

function resolveNativeGatewayTransportAPI(): NativeGatewayTransportAPI | null {
	if (!isDesktopLocalAppDocument()) return null;
	const api = getElectronAPI()?.nativeGatewayTransport;
	if (api == null) return null;
	if (api.isAvailable !== true) return null;
	for (const method of NATIVE_GATEWAY_TRANSPORT_METHODS) {
		if (typeof api[method] !== 'function') {
			log.warn(`Desktop native gateway transport is missing ${method}, using the browser transport`);
			return null;
		}
	}
	if (isNativeGatewayFallbackActive(getRouterState())) return null;
	return api;
}

function getDesktopGatewayTransportRouter(api: NativeGatewayTransportAPI): DesktopGatewayTransportRouter {
	const state = getRouterState();
	if (state.router != null && state.api !== api) {
		state.router.shutdown();
		state.router = null;
		state.api = null;
	}
	if (state.router == null) {
		state.router = new DesktopGatewayTransportEventRouter(api);
		state.api = api;
	}
	return state.router;
}

export function isDesktopNativeGatewayTransportAvailable(): boolean {
	return resolveNativeGatewayTransportAPI() != null;
}

export function createGatewayWireTransport(url: string, options: GatewayWireTransportOptions): GatewayWireTransport {
	const api = resolveNativeGatewayTransportAPI();
	if (api != null) {
		return new DesktopNativeGatewayWireTransport(url, getDesktopGatewayTransportRouter(api));
	}
	return new BrowserGatewayWireTransport(url, options);
}
