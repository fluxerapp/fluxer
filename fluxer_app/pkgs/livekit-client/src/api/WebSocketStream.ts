// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {ConnectionError} from '../room/errors.ts';
import {sleep} from '../room/utils.ts';
import TypedPromise from '../utils/TypedPromise.ts';
import {getErrorDescription} from './utils.ts';

export interface WebSocketConnection<T extends ArrayBuffer | string = ArrayBuffer | string> {
	readable: ReadableStream<T>;
	writable: WritableStream<T>;
	protocol: string;
	extensions: string;
}

export interface WebSocketCloseInfo {
	closeCode?: number;
	reason?: string;
}

export interface WebSocketStreamOptions {
	protocols?: Array<string>;
	signal?: AbortSignal;
}

type WebsocketError = ConnectionError;

export class WebSocketStream<T extends ArrayBuffer | string = ArrayBuffer | string> {
	readonly url: string;

	readonly opened: TypedPromise<WebSocketConnection<T>, WebsocketError>;

	readonly closed: TypedPromise<WebSocketCloseInfo, WebsocketError>;

	readonly close: (closeInfo?: WebSocketCloseInfo) => void;

	get readyState(): number {
		return this.ws.readyState;
	}

	private ws: WebSocket;

	constructor(url: string, options: WebSocketStreamOptions = {}) {
		if (options.signal?.aborted) {
			throw new DOMException('This operation was aborted', 'AbortError');
		}

		this.url = url;

		const ws = new WebSocket(url, options.protocols ?? []);
		ws.binaryType = 'arraybuffer';
		this.ws = ws;

		const closeWithInfo = ({closeCode: code, reason}: WebSocketCloseInfo = {}) => ws.close(code, reason);

		this.opened = new TypedPromise<WebSocketConnection<T>, WebsocketError>((resolve, reject) => {
			const rejectHandler = () => {
				reject(ConnectionError.websocket('Encountered websocket error during connection establishment'));
			};
			ws.onopen = () => {
				resolve({
					readable: new ReadableStream<T>({
						start(controller) {
							ws.onmessage = ({data}) => controller.enqueue(data);
							ws.onerror = (e) => controller.error(ConnectionError.websocket(getErrorDescription(e, 'websocket')));
							ws.onclose = (ev) => {
								if (ev.wasClean || ev.code === 1000) {
									controller.close();
								} else {
									controller.error(ConnectionError.websocket(`WS closed unexpectedly with code ${ev.code}`));
								}
							};
						},
						cancel: closeWithInfo,
					}),
					writable: new WritableStream<T>({
						write(chunk) {
							ws.send(chunk);
						},
						abort() {
							ws.close();
						},
						close: closeWithInfo,
					}),
					protocol: ws.protocol,
					extensions: ws.extensions,
				});
				ws.removeEventListener('error', rejectHandler);
			};
			ws.addEventListener('error', rejectHandler);
		});

		this.closed = new TypedPromise<WebSocketCloseInfo, WebsocketError>((resolve, reject) => {
			const rejectHandler = async () => {
				const closePromise = new TypedPromise<CloseEvent, never>((res) => {
					if (ws.readyState === WebSocket.CLOSED) return;
					else {
						ws.addEventListener(
							'close',
							(closeEv: CloseEvent) => {
								res(closeEv);
							},
							{once: true},
						);
					}
				});
				const reason = await TypedPromise.race([sleep(250), closePromise]);
				if (!reason) {
					reject(ConnectionError.websocket('Encountered unspecified websocket error without a timely close event'));
				} else {
					resolve(reason);
				}
			};
			ws.addEventListener('close', ({code, reason}) => {
				resolve({closeCode: code, reason});
				ws.removeEventListener('error', rejectHandler);
			});

			ws.addEventListener('error', rejectHandler);
		});

		if (options.signal) {
			options.signal.onabort = () => ws.close();
		}

		this.close = closeWithInfo;
	}
}
