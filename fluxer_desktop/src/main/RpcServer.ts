// SPDX-License-Identifier: AGPL-3.0-or-later

import http from 'node:http';
import type net from 'node:net';
import {BUILD_CHANNEL} from '@electron/common/BuildChannel';
import {DESKTOP_BUILD_VARIANT} from '@electron/common/BuildVariant';
import {CANARY_APP_URL, STABLE_APP_URL} from '@electron/common/Constants';
import {getCustomAppUrl} from '@electron/common/DesktopConfig';
import {getMainWindow, showWindow} from '@electron/main/Window';
import {app} from 'electron';
import log from 'electron-log';

const RPC_PORT = BUILD_CHANNEL === 'canary' ? 21864 : 21863;
const ALLOWED_ORIGINS = [STABLE_APP_URL, CANARY_APP_URL];

const isAllowedOrigin = (origin?: string): boolean => {
	if (!origin) return false;
	if (ALLOWED_ORIGINS.includes(origin)) return true;
	const customUrl = getCustomAppUrl();
	return customUrl != null && origin === customUrl;
};

const matchesAllowedOriginPrefix = (url: string, allowed: string): boolean => {
	const prefix = allowed.endsWith('/') ? allowed : allowed + '/';
	return url === allowed || url.startsWith(prefix);
};

const refererMatchesAllowedOrigin = (referer?: string): boolean => {
	if (!referer) return false;
	if (ALLOWED_ORIGINS.some((allowed) => matchesAllowedOriginPrefix(referer, allowed))) return true;
	const customUrl = getCustomAppUrl();
	return customUrl != null && matchesAllowedOriginPrefix(referer, customUrl);
};

let server: http.Server | null = null;
let currentActivity: unknown | null = null;
let currentActivitySocket: net.Socket | null = null;

interface RpcRequest {
	cmd?: string;
	args?: Record<string, unknown>;
	method?: string;
	params?: Record<string, unknown>;
	activity?: unknown;
}

interface RpcResponse {
	success: boolean;
	data?: unknown;
	error?: string;
}

type ParseResult =
	| { status: 'ok'; body: RpcRequest }
	| { status: 'empty'; body: null }
	| { status: 'payload_too_large' }
	| { status: 'invalid_json' };

const sendJson = (res: http.ServerResponse, status: number, data: RpcResponse) => {
	res.writeHead(status, {'Content-Type': 'application/json'});
	res.end(JSON.stringify(data));
};

const rejectIfDisallowedPage = (req: http.IncomingMessage, res: http.ServerResponse): boolean => {
	const origin = req.headers.origin;
	const referer = req.headers.referer;

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
	if (origin && referer && !matchesAllowedOriginPrefix(referer, origin)) {
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

const parseBody = (req: http.IncomingMessage): Promise<ParseResult> => {
	return new Promise((resolve) => {
		let body = '';
		let byteLength = 0;
		let destroyed = false;

		const onData = (chunk: Buffer | string) => {
			const chunkLen = typeof chunk === 'string' ? Buffer.byteLength(chunk) : chunk.length;
			byteLength += chunkLen;
			if (byteLength > 1024 * 1024) {
				destroyed = true;
				req.removeListener('data', onData);
				req.pause();
				resolve({ status: 'payload_too_large' });
			} else {
				body += chunk;
			}
		};

		req.on('data', onData);
		req.on('end', () => {
			if (destroyed) return;
			if (!body || body.trim() === '') {
				resolve({ status: 'empty', body: null });
				return;
			}
			try {
				const parsed = JSON.parse(body) as RpcRequest;
				resolve({ status: 'ok', body: parsed });
			} catch {
				resolve({ status: 'invalid_json' });
			}
		});
		req.on('error', () => {
			if (!destroyed) {
				resolve({ status: 'invalid_json' });
			}
		});
	});
};

const extractActivity = (body: RpcRequest | null): unknown | null => {
	if (!body || typeof body !== 'object') return null;
	const reqObj = body as Record<string, unknown>;

	if ('activity' in reqObj && reqObj.activity !== undefined) {
		return reqObj.activity;
	}
	if (reqObj.args && typeof reqObj.args === 'object' && reqObj.args !== null) {
		const args = reqObj.args as Record<string, unknown>;
		if ('activity' in args) {
			return args['activity'] ?? null;
		}
		if (args['name'] !== undefined || args['details'] !== undefined || args['state'] !== undefined || args['assets'] !== undefined) {
			return args;
		}
	}
	if (reqObj.params && typeof reqObj.params === 'object' && reqObj.params !== null) {
		const params = reqObj.params as Record<string, unknown>;
		if ('activity' in params) {
			return params['activity'] ?? null;
		}
		if (params['name'] !== undefined || params['details'] !== undefined || params['state'] !== undefined || params['assets'] !== undefined) {
			return params;
		}
	}
	if (reqObj['name'] !== undefined || reqObj['details'] !== undefined || reqObj['state'] !== undefined || reqObj['assets'] !== undefined) {
		return body;
	}
	return null;
};

const clearCurrentActivity = () => {
	currentActivity = null;
	currentActivitySocket = null;
	const mainWindow = getMainWindow();
	if (mainWindow && !mainWindow.isDestroyed()) {
		mainWindow.webContents.send('rpc-activity-update', null);
	}
};

const handleHealth = (_req: http.IncomingMessage, res: http.ServerResponse) => {
	sendJson(res, 200, {
		success: true,
		data: {
			status: 'ok',
			channel: BUILD_CHANNEL,
			build_variant: DESKTOP_BUILD_VARIANT,
			version: app ? app.getVersion() : '1.0.0',
			platform: process.platform,
			has_activity: currentActivity != null,
		},
	});
};

const handleNavigateWithBody = async (
	_req: http.IncomingMessage,
	res: http.ServerResponse,
	body: RpcRequest | null
) => {
	if (!body?.params?.path || typeof body.params['path'] !== 'string') {
		sendJson(res, 400, {success: false, error: 'Missing or invalid path parameter'});
		return;
	}
	const path = body.params['path'];
	const mainWindow = getMainWindow();
	if (!mainWindow || mainWindow.isDestroyed()) {
		sendJson(res, 503, {success: false, error: 'Main window not available'});
		return;
	}
	mainWindow.webContents.send('rpc-navigate', path);
	showWindow();
	sendJson(res, 200, {success: true, data: {navigated: true, path}});
};

const handleSetActivityWithBody = async (
	req: http.IncomingMessage,
	res: http.ServerResponse,
	body: RpcRequest | null
) => {
	const activity = extractActivity(body);
	currentActivity = activity;

	const mainWindow = getMainWindow();
	if (mainWindow && !mainWindow.isDestroyed()) {
		mainWindow.webContents.send('rpc-activity-update', activity);
	}

	if (activity != null) {
		const socket = req.socket;
		currentActivitySocket = socket;

		let resFinishedTime = 0;
		res.on('finish', () => {
			resFinishedTime = Date.now();
		});

		const existingListener = (socket as unknown as {_rpcCloseListener?: () => void})._rpcCloseListener;
		if (existingListener) {
			socket.removeListener('close', existingListener);
		}

		const onClose = () => {
			delete (socket as unknown as {_rpcCloseListener?: () => void})._rpcCloseListener;
			if (currentActivitySocket === socket) {
				const timeSinceFinish = resFinishedTime > 0 ? Date.now() - resFinishedTime : -1;
				if (timeSinceFinish < 0 || timeSinceFinish > 50) {
					clearCurrentActivity();
				}
			}
		};

		(socket as unknown as {_rpcCloseListener?: () => void})._rpcCloseListener = onClose;
		socket.once('close', onClose);
	} else {
		currentActivitySocket = null;
	}

	sendJson(res, 200, {success: true, data: {activity: currentActivity}});
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

	const requestUrl = req.url ?? '/';
	const urlPath = requestUrl.split('?')[0];

	try {
		if (req.method === 'GET' && urlPath === '/health') {
			handleHealth(req, res);
			return;
		}

		if (req.method === 'POST') {
			const parseResult = await parseBody(req);
			if (parseResult.status === 'payload_too_large') {
				sendJson(res, 413, {success: false, error: 'Payload Too Large'});
				res.on('finish', () => {
					req.destroy();
				});
				return;
			}
			if (parseResult.status === 'invalid_json') {
				sendJson(res, 400, {success: false, error: 'Invalid JSON'});
				return;
			}

			const body = parseResult.status === 'ok' ? parseResult.body : null;
			const cmd = (body?.cmd ?? body?.method ?? '').toUpperCase();

			if (urlPath === '/activity' || urlPath === '/rpc' || urlPath === '/' || cmd === 'SET_ACTIVITY') {
				await handleSetActivityWithBody(req, res, body);
				return;
			}
			if (urlPath === '/navigate' || cmd === 'NAVIGATE') {
				await handleNavigateWithBody(req, res, body);
				return;
			}
			if (urlPath === '/focus' || cmd === 'FOCUS') {
				handleFocus(req, res);
				return;
			}
		} else if (req.method === 'GET') {
			if (urlPath === '/focus') {
				handleFocus(req, res);
				return;
			}
		}

		sendJson(res, 404, {success: false, error: 'Not found'});
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
				log.warn('[RPC] Port ' + RPC_PORT + ' already in use, RPC server disabled');
				server = null;
				resolve();
			} else {
				log.error('[RPC] Server error:', error);
				reject(error);
			}
		});
		server.listen(RPC_PORT, '127.0.0.1', () => {
			log.info('[RPC] Server listening on http://127.0.0.1:' + RPC_PORT);
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
		clearCurrentActivity();
		server.close((err) => {
			if (err) {
				log.error('[RPC] Error closing server:', err);
			}
			server = null;
			resolve();
		});
	});
};
