// SPDX-License-Identifier: AGPL-3.0-or-later

import {getDesktopOutboundHTTP} from '@electron/main/DesktopOutboundHTTP';
import type {GatewayOriginRegistry} from '@electron/main/GatewayOriginRegistry';
import {
	createGatewaySocketBoundary,
	type GatewaySocketBoundary,
	warmGatewaySocketTransport,
} from '@electron/main/GatewaySocketNativeBoundary';
import {parseNativeGatewayTransportEvent} from '@electron/main/NativeGatewayTransportEventParser';
import {
	requireNativeGatewayCloseRequest,
	requireNativeGatewayConnectionId,
	requireNativeGatewayCreateRequest,
	requireNativeGatewaySendBinaryRequest,
	requireNativeGatewaySendTextRequest,
} from '@electron/main/NativeGatewayTransportRequestBoundary';
import type {RendererDocumentOwnerFactory} from '@electron/main/RendererDocumentOwner';
import {
	isSameRendererDocument,
	type RendererDocumentIpcEvent,
	type RendererDocumentIpcRoutes,
	type RendererDocumentOwner,
	type RendererDocumentOwnerWatcher,
} from '@electron/main/RendererDocumentOwnership';
import {websocketHTTPOrigin} from '@electron/main/WebSocketOrigin';
import {
	NATIVE_GATEWAY_TRANSPORT_CHANNELS,
	NATIVE_GATEWAY_TRANSPORT_EVENT_CHANNEL,
	NATIVE_GATEWAY_TRANSPORT_MAX_CONNECTIONS_PER_DOCUMENT,
	type NativeGatewayTransportCreateResult,
	type NativeGatewayTransportEvent,
	NativeGatewayTransportEventKind,
} from '@fluxer/desktop_ipc/src/GatewayTransportContract';
import type {NativeGatewayConnection} from '@fluxer/gateway-socket';

const RENDERER_CONTEXT = 'Native gateway transport';
const ABNORMAL_CLOSURE = 1006;

interface NativeGatewayTransportLogger {
	info: (message: string, ...args: Array<unknown>) => void;
	warn: (message: string, ...args: Array<unknown>) => void;
	error: (message: string, ...args: Array<unknown>) => void;
}

interface DesktopNativeGatewayTransportDependencies {
	readonly boundary: GatewaySocketBoundary | null;
	readonly logger: NativeGatewayTransportLogger;
	readonly originRegistry: GatewayOriginRegistry;
	readonly rendererDocumentOwners: RendererDocumentOwnerFactory;
	readonly requireCleartextGatewayAddress: (httpOrigin: string) => Promise<string>;
	readonly resolveProxy: (event: RendererDocumentIpcEvent, url: string) => Promise<string>;
}

class NativeGatewayTransportUnavailableError extends Error {
	public constructor() {
		super('The native gateway transport addon is unavailable');
		this.name = 'NativeGatewayTransportUnavailableError';
	}
}

class NativeGatewayTransportProxiedError extends Error {
	public constructor() {
		super('The native gateway transport cannot reach a gateway that resolves through a proxy');
		this.name = 'NativeGatewayTransportProxiedError';
	}
}

class NativeGatewayTransportShutdownError extends Error {
	public constructor() {
		super('The native gateway transport is shutting down');
		this.name = 'NativeGatewayTransportShutdownError';
	}
}

class NativeGatewayTransportConnectionNotFoundError extends Error {
	public constructor(connectionId: string) {
		super(`Native gateway transport connection ${connectionId} does not exist`);
		this.name = 'NativeGatewayTransportConnectionNotFoundError';
	}
}

class DuplicateNativeGatewayTransportConnectionError extends Error {
	public constructor(connectionId: string) {
		super(`Native gateway transport connection ${connectionId} already exists`);
		this.name = 'DuplicateNativeGatewayTransportConnectionError';
	}
}

class NativeGatewayTransportConnectionOwnerMismatchError extends Error {
	public constructor() {
		super('Native gateway transport connection is owned by another renderer document');
		this.name = 'NativeGatewayTransportConnectionOwnerMismatchError';
	}
}

class NativeGatewayTransportConnectionLimitError extends Error {
	public constructor() {
		super('Native gateway transport connection limit reached for this renderer document');
		this.name = 'NativeGatewayTransportConnectionLimitError';
	}
}

class NativeGatewayTransportInvalidTargetError extends TypeError {
	public constructor() {
		super('Native gateway transport target has no HTTP origin');
		this.name = 'NativeGatewayTransportInvalidTargetError';
	}
}

interface ConnectionRecord {
	readonly id: string;
	readonly owner: RendererDocumentOwner;
	native: NativeGatewayConnection | null;
	disposing: boolean;
}

function requireNativeConnection(record: ConnectionRecord): NativeGatewayConnection {
	if (record.native == null) {
		throw new NativeGatewayTransportConnectionNotFoundError(record.id);
	}
	return record.native;
}

export class DesktopNativeGatewayTransport {
	private accepting = true;
	private readonly connections = new Map<string, ConnectionRecord>();
	private readonly reservedConnections = new Map<string, RendererDocumentOwner>();
	private readonly ownerWatchers = new Map<Electron.WebFrameMain, RendererDocumentOwnerWatcher>();

	private readonly dependencies: DesktopNativeGatewayTransportDependencies;

	public constructor(dependencies: DesktopNativeGatewayTransportDependencies) {
		this.dependencies = dependencies;
	}

	public ipcRoutes(): RendererDocumentIpcRoutes {
		return Object.freeze({
			[NATIVE_GATEWAY_TRANSPORT_CHANNELS.isAvailable]: async (): Promise<boolean> =>
				this.accepting && this.dependencies.boundary != null,
			[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create]: (event, request) => this.handleCreate(event, request),
			[NATIVE_GATEWAY_TRANSPORT_CHANNELS.sendText]: async (event, request) => {
				this.requireAccepting();
				const {connectionId, payload} = requireNativeGatewaySendTextRequest(request);
				requireNativeConnection(this.requireOwnedConnection(event, connectionId)).sendText(payload);
			},
			[NATIVE_GATEWAY_TRANSPORT_CHANNELS.sendBinary]: async (event, request) => {
				this.requireAccepting();
				const {connectionId, payload} = requireNativeGatewaySendBinaryRequest(request);
				requireNativeConnection(this.requireOwnedConnection(event, connectionId)).sendBinary(payload);
			},
			[NATIVE_GATEWAY_TRANSPORT_CHANNELS.close]: async (event, request) => {
				this.requireAccepting();
				const {connectionId, code, reason} = requireNativeGatewayCloseRequest(request);
				requireNativeConnection(this.requireOwnedConnection(event, connectionId)).close(code, reason);
			},
			[NATIVE_GATEWAY_TRANSPORT_CHANNELS.dispose]: async (event, connectionId) => {
				this.requireAccepting();
				this.disposeOwnedConnection(event, requireNativeGatewayConnectionId(connectionId));
			},
		});
	}

	public cleanup(): void {
		this.accepting = false;
		for (const id of [...this.connections.keys()]) {
			this.cleanupConnection(id);
		}
		this.reservedConnections.clear();
		for (const [frame, watcher] of [...this.ownerWatchers]) {
			this.ownerWatchers.delete(frame);
			try {
				watcher.dispose();
			} catch (error) {
				this.dependencies.logger.warn('[NativeGateway] Failed to dispose a renderer document watcher', error);
			}
		}
		this.dependencies.originRegistry.cleanup();
	}

	private requireAccepting(): void {
		if (!this.accepting) {
			throw new NativeGatewayTransportShutdownError();
		}
	}

	private requireOwnedConnection(event: RendererDocumentIpcEvent, connectionId: string): ConnectionRecord {
		const record = this.connections.get(connectionId);
		if (record == null) {
			throw new NativeGatewayTransportConnectionNotFoundError(connectionId);
		}
		if (!record.owner.matchesEvent(event)) {
			throw new NativeGatewayTransportConnectionOwnerMismatchError();
		}
		return record;
	}

	private disposeOwnedConnection(event: RendererDocumentIpcEvent, connectionId: string): void {
		const record = this.connections.get(connectionId);
		if (record == null) return;
		if (!record.owner.matchesEvent(event)) {
			throw new NativeGatewayTransportConnectionOwnerMismatchError();
		}
		this.cleanupConnection(connectionId);
	}

	private connectionCountForOwner(owner: RendererDocumentOwner): number {
		let count = 0;
		for (const record of this.connections.values()) {
			if (isSameRendererDocument(record.owner, owner)) count += 1;
		}
		for (const [id, reservedOwner] of this.reservedConnections) {
			if (this.connections.has(id)) continue;
			if (isSameRendererDocument(reservedOwner, owner)) count += 1;
		}
		return count;
	}

	private async handleCreate(
		event: RendererDocumentIpcEvent,
		request: unknown,
	): Promise<NativeGatewayTransportCreateResult> {
		this.requireAccepting();
		const boundary = this.dependencies.boundary;
		if (boundary == null) {
			throw new NativeGatewayTransportUnavailableError();
		}
		const owner = this.dependencies.rendererDocumentOwners.capture(event, RENDERER_CONTEXT);
		const {connectionId, url} = requireNativeGatewayCreateRequest(request);
		if (this.connections.has(connectionId) || this.reservedConnections.has(connectionId)) {
			throw new DuplicateNativeGatewayTransportConnectionError(connectionId);
		}
		if (this.connectionCountForOwner(owner) >= NATIVE_GATEWAY_TRANSPORT_MAX_CONNECTIONS_PER_DOCUMENT) {
			throw new NativeGatewayTransportConnectionLimitError();
		}
		this.reservedConnections.set(connectionId, owner);
		try {
			await Promise.all([
				this.dependencies.originRegistry.admit({documentKey: owner.frame, url}),
				this.requireDirectRoute(event, url),
			]);
			const address = await this.pinnedAddressFor(url);
			this.requireAccepting();
			owner.requireCurrent(RENDERER_CONTEXT);
			const record: ConnectionRecord = {id: connectionId, owner, native: null, disposing: false};
			this.connections.set(connectionId, record);
			this.watchOwner(owner);
			record.native = boundary.connect({
				url,
				address,
				onEvent: (nativeEvent) => this.handleNativeEvent(connectionId, nativeEvent),
				onTerminalEvent: (nativeEvent) => this.handleNativeEvent(connectionId, nativeEvent),
			});
			return {connectionId};
		} catch (error) {
			this.cleanupConnection(connectionId);
			this.dependencies.logger.warn('[NativeGateway] Refused a native gateway connection', error);
			throw error;
		} finally {
			this.reservedConnections.delete(connectionId);
		}
	}

	private async requireDirectRoute(event: RendererDocumentIpcEvent, url: string): Promise<void> {
		const route = await this.dependencies.resolveProxy(event, url.replace(/^ws/u, 'http'));
		if (route.trim() !== 'DIRECT') {
			throw new NativeGatewayTransportProxiedError();
		}
	}

	private async pinnedAddressFor(url: string): Promise<string | null> {
		if (new URL(url).protocol !== 'ws:') return null;
		const httpOrigin = websocketHTTPOrigin(url);
		if (httpOrigin == null) {
			throw new NativeGatewayTransportInvalidTargetError();
		}
		return await this.dependencies.requireCleartextGatewayAddress(httpOrigin);
	}

	private watchOwner(owner: RendererDocumentOwner): void {
		if (this.ownerWatchers.has(owner.frame)) return;
		this.ownerWatchers.set(
			owner.frame,
			owner.watchInvalidation((reason) => this.releaseOwner(owner, reason)),
		);
	}

	private unwatchOwnerIfUnused(owner: RendererDocumentOwner): void {
		for (const record of this.connections.values()) {
			if (isSameRendererDocument(record.owner, owner)) return;
		}
		const watcher = this.ownerWatchers.get(owner.frame);
		if (watcher == null) return;
		this.ownerWatchers.delete(owner.frame);
		try {
			watcher.dispose();
		} catch (error) {
			this.dependencies.logger.warn('[NativeGateway] Failed to dispose a renderer document watcher', error);
		}
	}

	private releaseOwner(owner: RendererDocumentOwner, reason: string): void {
		this.dependencies.logger.info('[NativeGateway] Releasing renderer document connections', reason);
		this.ownerWatchers.delete(owner.frame);
		this.dependencies.originRegistry.releaseDocument(owner.frame);
		for (const [id, record] of [...this.connections]) {
			if (!isSameRendererDocument(record.owner, owner)) continue;
			this.cleanupConnection(id);
		}
	}

	private cleanupConnection(id: string): void {
		const record = this.connections.get(id);
		if (record == null) return;
		if (record.disposing) return;
		record.disposing = true;
		this.connections.delete(id);
		const native = record.native;
		record.native = null;
		if (native != null) {
			try {
				native.dispose();
			} catch (error) {
				this.dependencies.logger.warn('[NativeGateway] Failed to dispose a native gateway connection', error);
			}
		}
		this.unwatchOwnerIfUnused(record.owner);
	}

	private handleNativeEvent(connectionId: string, nativeEvent: unknown): void {
		const record = this.connections.get(connectionId);
		if (record == null || record.disposing) return;
		let parsed: NativeGatewayTransportEvent;
		try {
			parsed = parseNativeGatewayTransportEvent(connectionId, nativeEvent);
		} catch (error) {
			this.dependencies.logger.error('[NativeGateway] Rejected a malformed native gateway event', error);
			this.failConnection(
				record,
				'Native gateway transport emitted an invalid event',
				'Invalid native transport event',
			);
			return;
		}
		if (!this.sendConnectionEvent(record.owner, parsed)) {
			this.cleanupConnection(connectionId);
			return;
		}
		if (parsed.kind === NativeGatewayTransportEventKind.CLOSE) {
			this.cleanupConnection(connectionId);
		}
	}

	private failConnection(record: ConnectionRecord, message: string, closeReason: string): void {
		const base = {
			connectionId: record.id,
			data: null,
			binary: null,
			code: null,
			reason: null,
			wasClean: null,
			message: null,
		};
		if (this.sendConnectionEvent(record.owner, {...base, kind: NativeGatewayTransportEventKind.ERROR, message})) {
			this.sendConnectionEvent(record.owner, {
				...base,
				kind: NativeGatewayTransportEventKind.CLOSE,
				code: ABNORMAL_CLOSURE,
				reason: closeReason,
				wasClean: false,
			});
		}
		this.cleanupConnection(record.id);
	}

	private sendConnectionEvent(owner: RendererDocumentOwner, event: NativeGatewayTransportEvent): boolean {
		if (!owner.isCurrent()) return false;
		try {
			owner.frame.send(NATIVE_GATEWAY_TRANSPORT_EVENT_CHANNEL, event);
			return true;
		} catch (error) {
			this.dependencies.logger.warn('[NativeGateway] Failed to deliver a gateway event to the renderer', error);
			return false;
		}
	}
}

interface CreateDesktopNativeGatewayTransportRequest {
	readonly logger: NativeGatewayTransportLogger;
	readonly originRegistry: GatewayOriginRegistry;
	readonly rendererDocumentOwners: RendererDocumentOwnerFactory;
}

export function createDesktopNativeGatewayTransport(
	request: CreateDesktopNativeGatewayTransportRequest,
): DesktopNativeGatewayTransport {
	const boundary = createGatewaySocketBoundary();
	if (boundary != null) {
		warmGatewaySocketTransport();
	}
	return new DesktopNativeGatewayTransport({
		...request,
		boundary,
		requireCleartextGatewayAddress: (httpOrigin) =>
			getDesktopOutboundHTTP().requireCleartextTransportAddress(httpOrigin),
		resolveProxy: (event, url) => event.sender.session.resolveProxy(url),
	});
}
