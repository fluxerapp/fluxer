/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import http from 'node:http';
import log from 'electron-log';
import WebSocket, {WebSocketServer} from 'ws';
import {BUILD_CHANNEL} from '../common/build-channel.js';
import {CANARY_APP_URL, STABLE_APP_URL} from '../common/constants.js';

export const WS_PROXY_PORT = BUILD_CHANNEL === 'canary' ? 21866 : 21865;

const ALLOWED_ORIGINS = new Set([STABLE_APP_URL, CANARY_APP_URL]);

let server: http.Server | null = null;
let wss: WebSocketServer | null = null;

const isAllowedOrigin = (origin: string | undefined): boolean => {
	if (!origin) return false;
	return ALLOWED_ORIGINS.has(origin);
};

const isValidTargetUrl = (url: string | null): boolean => {
	if (!url) return false;
	try {
		const parsed = new URL(url);
		return parsed.protocol === 'wss:' || parsed.protocol === 'ws:';
	} catch {
		return false;
	}
};

interface ProxyConnection {
	clientSocket: WebSocket;
	targetSocket: WebSocket | null;
	targetUrl: string;
}

const activeConnections = new Map<WebSocket, ProxyConnection>();

const handleUpgrade = (request: http.IncomingMessage, socket: import('stream').Duplex, head: Buffer): void => {
	const remoteAddress = request.socket.remoteAddress;

	if (remoteAddress !== '127.0.0.1' && remoteAddress !== '::1' && remoteAddress !== '::ffff:127.0.0.1') {
		log.warn('[WS Proxy] Rejected connection from non-localhost:', remoteAddress);
		socket.write('HTTP/1.1 403 Forbidden\r\n\r\n');
		socket.destroy();
		return;
	}

	const origin = request.headers.origin;
	if (!isAllowedOrigin(origin)) {
		log.warn('[WS Proxy] Rejected connection from disallowed origin:', origin);
		socket.write('HTTP/1.1 403 Forbidden\r\n\r\n');
		socket.destroy();
		return;
	}

	const url = new URL(request.url ?? '/', `http://${request.headers.host}`);
	const targetUrl = url.searchParams.get('target');

	if (!isValidTargetUrl(targetUrl)) {
		log.warn('[WS Proxy] Invalid or missing target URL:', targetUrl);
		socket.write('HTTP/1.1 400 Bad Request\r\n\r\n');
		socket.destroy();
		return;
	}

	wss?.handleUpgrade(request, socket, head, (clientSocket: WebSocket) => {
		wss?.emit('connection', clientSocket, request, targetUrl);
	});
};

const handleConnection = (clientSocket: WebSocket, _request: http.IncomingMessage, targetUrl: string): void => {
	log.info('[WS Proxy] New connection, proxying to:', targetUrl);

	const connection: ProxyConnection = {
		clientSocket,
		targetSocket: null,
		targetUrl,
	};

	activeConnections.set(clientSocket, connection);

	const targetSocket = new WebSocket(targetUrl);
	connection.targetSocket = targetSocket;

	targetSocket.binaryType = 'arraybuffer';

	targetSocket.on('open', () => {
		log.debug('[WS Proxy] Target connection established:', targetUrl);
	});

	targetSocket.on('message', (data: Buffer | ArrayBuffer | Array<Buffer>, isBinary: boolean) => {
		if (clientSocket.readyState === WebSocket.OPEN) {
			try {
				if (isBinary) {
					if (data instanceof ArrayBuffer) {
						clientSocket.send(Buffer.from(data));
					} else if (Array.isArray(data)) {
						clientSocket.send(Buffer.concat(data));
					} else {
						clientSocket.send(data);
					}
				} else {
					clientSocket.send(data.toString());
				}
			} catch (error) {
				log.error('[WS Proxy] Error sending to client:', error);
			}
		}
	});

	targetSocket.on('close', (code: number, reason: Buffer) => {
		log.debug('[WS Proxy] Target closed:', code, reason.toString());
		if (clientSocket.readyState === WebSocket.OPEN) {
			clientSocket.close(code, reason.toString());
		}
		activeConnections.delete(clientSocket);
	});

	targetSocket.on('error', (error: Error) => {
		log.error('[WS Proxy] Target socket error:', error);
		if (clientSocket.readyState === WebSocket.OPEN) {
			clientSocket.close(1011, 'Target connection error');
		}
		activeConnections.delete(clientSocket);
	});

	clientSocket.on('message', (data: Buffer | ArrayBuffer | Array<Buffer>, isBinary: boolean) => {
		if (targetSocket.readyState === WebSocket.OPEN) {
			try {
				if (isBinary) {
					if (data instanceof ArrayBuffer) {
						targetSocket.send(Buffer.from(data));
					} else if (Array.isArray(data)) {
						targetSocket.send(Buffer.concat(data));
					} else {
						targetSocket.send(data);
					}
				} else {
					targetSocket.send(data.toString());
				}
			} catch (error) {
				log.error('[WS Proxy] Error sending to target:', error);
			}
		}
	});

	clientSocket.on('close', (code: number, reason: Buffer) => {
		log.debug('[WS Proxy] Client closed:', code, reason.toString());
		if (targetSocket.readyState === WebSocket.OPEN || targetSocket.readyState === WebSocket.CONNECTING) {
			targetSocket.close(code, reason.toString());
		}
		activeConnections.delete(clientSocket);
	});

	clientSocket.on('error', (error: Error) => {
		log.error('[WS Proxy] Client socket error:', error);
		if (targetSocket.readyState === WebSocket.OPEN || targetSocket.readyState === WebSocket.CONNECTING) {
			targetSocket.close(1011, 'Client connection error');
		}
		activeConnections.delete(clientSocket);
	});
};

export const startWsProxyServer = (): Promise<void> => {
	return new Promise((resolve, reject) => {
		if (server) {
			resolve();
			return;
		}

		server = http.createServer((_req, res) => {
			res.writeHead(426, {'Content-Type': 'text/plain'});
			res.end('WebSocket connections only');
		});

		wss = new WebSocketServer({noServer: true});

		wss.on('connection', handleConnection);

		server.on('upgrade', handleUpgrade);

		server.on('error', (error: NodeJS.ErrnoException) => {
			if (error.code === 'EADDRINUSE') {
				log.warn(`[WS Proxy] Port ${WS_PROXY_PORT} already in use, WS proxy server disabled`);
				server = null;
				wss = null;
				resolve();
			} else {
				log.error('[WS Proxy] Server error:', error);
				reject(error);
			}
		});

		server.listen(WS_PROXY_PORT, '127.0.0.1', () => {
			log.info(`[WS Proxy] Server listening on ws://127.0.0.1:${WS_PROXY_PORT}`);
			resolve();
		});
	});
};

export const stopWsProxyServer = (): Promise<void> => {
	return new Promise((resolve) => {
		for (const connection of activeConnections.values()) {
			try {
				connection.targetSocket?.close(1001, 'Server shutting down');
				connection.clientSocket.close(1001, 'Server shutting down');
			} catch {}
		}
		activeConnections.clear();

		if (wss) {
			wss.close();
			wss = null;
		}

		if (!server) {
			resolve();
			return;
		}

		server.close((err) => {
			if (err) {
				log.error('[WS Proxy] Error closing server:', err);
			}
			server = null;
			resolve();
		});
	});
};

export const getWsProxyUrl = (): string | null => {
	if (!server) return null;
	return `ws://127.0.0.1:${WS_PROXY_PORT}`;
};
