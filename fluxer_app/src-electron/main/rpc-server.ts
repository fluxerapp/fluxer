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
import {app} from 'electron';
import log from 'electron-log';
import {BUILD_CHANNEL} from '../common/build-channel.js';
import {CANARY_APP_URL, STABLE_APP_URL} from '../common/constants.js';
import {getMainWindow, showWindow} from './window.js';

export const RPC_PORT = BUILD_CHANNEL === 'canary' ? 21864 : 21863;

const ALLOWED_ORIGINS = [STABLE_APP_URL, CANARY_APP_URL];

const isAllowedOrigin = (origin?: string): boolean => {
	if (!origin) return false;
	return ALLOWED_ORIGINS.includes(origin);
};

const refererMatchesAllowedOrigin = (referer?: string): boolean => {
	if (!referer) return false;
	return ALLOWED_ORIGINS.some((allowed) => referer.startsWith(allowed));
};

let server: http.Server | null = null;

interface RpcRequest {
	method: string;
	params?: Record<string, unknown>;
}

interface RpcResponse {
	success: boolean;
	data?: unknown;
	error?: string;
}

const sendJson = (res: http.ServerResponse, status: number, data: RpcResponse) => {
	res.writeHead(status, {'Content-Type': 'application/json'});
	res.end(JSON.stringify(data));
};

const rejectIfDisallowedPage = (req: http.IncomingMessage, res: http.ServerResponse): boolean => {
	const origin = req.headers.origin;
	const referer = req.headers.referer;

	if (!origin && !referer) {
		res.writeHead(403);
		res.end();
		return true;
	}

	if (origin && !isAllowedOrigin(origin)) {
		res.writeHead(403);
		res.end();
		return true;
	}

	if (referer && !refererMatchesAllowedOrigin(referer)) {
		res.writeHead(403);
		res.end();
		return true;
	}

	if (origin && referer && !referer.startsWith(origin)) {
		res.writeHead(403);
		res.end();
		return true;
	}

	return false;
};

const handleCors = (req: http.IncomingMessage, res: http.ServerResponse): boolean => {
	const origin = req.headers.origin;

	if (origin && !isAllowedOrigin(origin)) {
		res.writeHead(403);
		res.end();
		return true;
	}

	if (origin && isAllowedOrigin(origin)) {
		res.setHeader('Access-Control-Allow-Origin', origin);
		res.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
		res.setHeader('Access-Control-Allow-Headers', 'Content-Type');
		res.setHeader('Access-Control-Max-Age', '86400');
	}

	if (req.method === 'OPTIONS') {
		res.writeHead(204);
		res.end();
		return true;
	}

	return false;
};

const parseBody = (req: http.IncomingMessage): Promise<RpcRequest | null> => {
	return new Promise((resolve) => {
		if (req.method !== 'POST') {
			resolve(null);
			return;
		}

		let body = '';
		req.on('data', (chunk) => {
			body += chunk;
			if (body.length > 1024 * 1024) {
				resolve(null);
			}
		});
		req.on('end', () => {
			try {
				resolve(JSON.parse(body) as RpcRequest);
			} catch {
				resolve(null);
			}
		});
		req.on('error', () => resolve(null));
	});
};

const handleHealth = (_req: http.IncomingMessage, res: http.ServerResponse) => {
	sendJson(res, 200, {
		success: true,
		data: {
			status: 'ok',
			channel: BUILD_CHANNEL,
			version: app.getVersion(),
			platform: process.platform,
		},
	});
};

const handleNavigate = async (req: http.IncomingMessage, res: http.ServerResponse) => {
	const body = await parseBody(req);

	if (!body?.params?.path || typeof body.params.path !== 'string') {
		sendJson(res, 400, {success: false, error: 'Missing or invalid path parameter'});
		return;
	}

	const path = body.params.path;
	const mainWindow = getMainWindow();

	if (!mainWindow || mainWindow.isDestroyed()) {
		sendJson(res, 503, {success: false, error: 'Main window not available'});
		return;
	}

	mainWindow.webContents.send('rpc-navigate', path);
	showWindow();

	sendJson(res, 200, {success: true, data: {navigated: true, path}});
};

const handleFocus = (_req: http.IncomingMessage, res: http.ServerResponse) => {
	const mainWindow = getMainWindow();

	if (!mainWindow || mainWindow.isDestroyed()) {
		sendJson(res, 503, {success: false, error: 'Main window not available'});
		return;
	}

	showWindow();
	sendJson(res, 200, {success: true, data: {focused: true}});
};

const requestHandler = async (req: http.IncomingMessage, res: http.ServerResponse) => {
	const remoteAddress = req.socket.remoteAddress;
	if (remoteAddress !== '127.0.0.1' && remoteAddress !== '::1' && remoteAddress !== '::ffff:127.0.0.1') {
		res.writeHead(403);
		res.end();
		return;
	}

	if (rejectIfDisallowedPage(req, res)) {
		return;
	}

	if (handleCors(req, res)) {
		return;
	}

	const url = req.url ?? '/';

	try {
		switch (url) {
			case '/health':
				handleHealth(req, res);
				break;
			case '/navigate':
				await handleNavigate(req, res);
				break;
			case '/focus':
				handleFocus(req, res);
				break;
			default:
				sendJson(res, 404, {success: false, error: 'Not found'});
		}
	} catch (error) {
		log.error('[RPC] Request handler error:', error);
		sendJson(res, 500, {success: false, error: 'Internal server error'});
	}
};

export const startRpcServer = (): Promise<void> => {
	return new Promise((resolve, reject) => {
		if (server) {
			resolve();
			return;
		}

		server = http.createServer(requestHandler);

		server.on('error', (error: NodeJS.ErrnoException) => {
			if (error.code === 'EADDRINUSE') {
				log.warn(`[RPC] Port ${RPC_PORT} already in use, RPC server disabled`);
				server = null;
				resolve();
			} else {
				log.error('[RPC] Server error:', error);
				reject(error);
			}
		});

		server.listen(RPC_PORT, '127.0.0.1', () => {
			log.info(`[RPC] Server listening on http://127.0.0.1:${RPC_PORT}`);
			resolve();
		});
	});
};

export const stopRpcServer = (): Promise<void> => {
	return new Promise((resolve) => {
		if (!server) {
			resolve();
			return;
		}

		server.close((err) => {
			if (err) {
				log.error('[RPC] Error closing server:', err);
			}
			server = null;
			resolve();
		});
	});
};
