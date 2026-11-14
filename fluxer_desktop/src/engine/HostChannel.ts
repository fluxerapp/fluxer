// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EngineToHostMessage, HostToEngineMessage} from '@electron/engine/HostProtocol';

export interface EngineMessagePort {
	postMessage(message: unknown): void;
	close(): void;
	onMessage(listener: (message: unknown) => void): void;
	onClose(listener: () => void): void;
}

interface RawPort {
	postMessage(message: unknown): void;
	close(): void;
	on(event: string, listener: (...args: Array<unknown>) => void): unknown;
	start?: () => void;
}

function isMessageEventLike(value: unknown): value is {data: unknown; ports: unknown} {
	return typeof value === 'object' && value !== null && 'data' in value && 'ports' in value;
}

function wrapEngineMessagePort(raw: RawPort): EngineMessagePort {
	return {
		postMessage: (message) => raw.postMessage(message),
		close: () => raw.close(),
		onMessage: (listener) => {
			raw.on('message', (event: unknown) => listener(isMessageEventLike(event) ? event.data : event));
			raw.start?.();
		},
		onClose: (listener) => {
			raw.on('close', () => listener());
		},
	};
}

export type HostMessageListener = (message: HostToEngineMessage, port: EngineMessagePort | null) => void;

interface ParentPortLike {
	postMessage(message: unknown): void;
	on(event: 'message', listener: (event: unknown) => void): unknown;
}

function extractPort(event: unknown, data: unknown): RawPort | null {
	if (isMessageEventLike(event) && Array.isArray(event.ports) && event.ports.length > 0) {
		return event.ports[0] as RawPort;
	}
	const candidate = (data as {port?: unknown} | null)?.port;
	if (typeof candidate === 'object' && candidate !== null && 'postMessage' in candidate) {
		return candidate as RawPort;
	}
	return null;
}

export class HostChannel {
	private readonly post: (message: EngineToHostMessage) => void;
	private listener: HostMessageListener | null = null;

	private constructor(
		post: (message: EngineToHostMessage) => void,
		subscribe: (deliver: (event: unknown) => void) => void,
	) {
		this.post = post;
		subscribe((event) => {
			const data = isMessageEventLike(event) ? event.data : event;
			if (typeof data !== 'object' || data === null || typeof (data as {type?: unknown}).type !== 'string') {
				return;
			}
			const raw = extractPort(event, data);
			this.listener?.(data as HostToEngineMessage, raw === null ? null : wrapEngineMessagePort(raw));
		});
	}

	static async connect(): Promise<HostChannel> {
		const electronParent = (process as unknown as {parentPort?: ParentPortLike}).parentPort;
		if (electronParent != null) {
			return new HostChannel(
				(message) => electronParent.postMessage(message),
				(deliver) => electronParent.on('message', deliver),
			);
		}
		const {parentPort} = await import('node:worker_threads');
		if (parentPort != null) {
			return new HostChannel(
				(message) => parentPort.postMessage(message),
				(deliver) => parentPort.on('message', deliver),
			);
		}
		if (typeof process.send === 'function') {
			return new HostChannel(
				(message) => process.send?.(message),
				(deliver) => process.on('message', deliver),
			);
		}
		throw new Error('The account engine has no host to talk to');
	}

	onMessage(listener: HostMessageListener): void {
		this.listener = listener;
	}

	send(message: EngineToHostMessage): void {
		this.post(message);
	}
}
