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
import https from 'node:https';
import log from 'electron-log';
import {BUILD_CHANNEL} from '../common/build-channel.js';
import {CANARY_APP_URL, STABLE_APP_URL} from '../common/constants.js';

export const API_PROXY_PORT = BUILD_CHANNEL === 'canary' ? 21862 : 21861;

const PROXY_PATH = '/proxy';
const PROXY_INITIATOR_HEADER = 'x-fluxer-proxy-initiator';
const ALLOWED_ORIGINS = [STABLE_APP_URL, CANARY_APP_URL];

const isAllowedOrigin = (origin?: string): boolean => {
	if (!origin) return false;
	return ALLOWED_ORIGINS.includes(origin);
};

const refererMatchesAllowedOrigin = (referer?: string): boolean => {
	if (!referer) return false;
	return ALLOWED_ORIGINS.some((allowed) => referer.startsWith(allowed));
};

const parseOrigin = (value?: string): string | null => {
	if (!value) {
		return null;
	}

	try {
		return new URL(value).origin;
	} catch {
		return null;
	}
};

const sanitizeUpstreamHeaders = (headers: http.IncomingHttpHeaders): Record<string, string> => {
	const out: Record<string, string> = {};
	for (const [key, value] of Object.entries(headers)) {
		if (!key) continue;
		if (typeof value === 'string') {
			out[key.toLowerCase()] = value;
		} else if (Array.isArray(value)) {
			out[key.toLowerCase()] = value.join(', ');
		}
	}
	return out;
};

const buildForwardHeaders = (req: http.IncomingMessage, targetUrl: URL): Record<string, string> => {
	const headers = sanitizeUpstreamHeaders(req.headers);

	delete headers.host;
	delete headers.connection;
	delete headers.origin;
	delete headers.referer;
	delete headers['sec-fetch-site'];
	delete headers['sec-fetch-mode'];
	delete headers['sec-fetch-dest'];
	delete headers['proxy-connection'];
	delete headers[PROXY_INITIATOR_HEADER];

	headers.host = targetUrl.host;
	headers.referer = targetUrl.origin;
	return headers;
};

const setCorsHeaders = (res: http.ServerResponse, origin?: string) => {
	if (origin) {
		res.setHeader('Access-Control-Allow-Origin', origin);
		res.setHeader('Vary', 'Origin');
	}
	res.setHeader('Access-Control-Allow-Credentials', 'true');
	res.setHeader(
		'Access-Control-Allow-Headers',
		'Authorization,Content-Type,Accept,Origin,X-Requested-With,X-Fluxer-Proxy-Initiator',
	);
	res.setHeader('Access-Control-Expose-Headers', 'x-fluxer-sudo-mode-jwt,x-request-id,content-type');
};

const rejectIfDisallowedPage = (
	req: http.IncomingMessage,
	res: http.ServerResponse,
	targetUrl?: URL,
	initiator?: string,
): boolean => {
	const origin = req.headers.origin;
	const referer = req.headers.referer;
	const initiatorHeader = initiator ? initiator : undefined;
	const targetOrigin = targetUrl?.origin;

	if (!origin && !referer && !initiatorHeader) {
		res.writeHead(403);
		res.end();
		return true;
	}

	const originCandidate = origin ?? parseOrigin(initiatorHeader ?? undefined);
	if (originCandidate && !isAllowedOrigin(originCandidate) && originCandidate !== targetOrigin) {
		res.writeHead(403);
		res.end();
		return true;
	}

	const refererCandidate = referer ?? initiatorHeader;
	if (
		refererCandidate &&
		!refererMatchesAllowedOrigin(refererCandidate) &&
		!(targetOrigin && refererCandidate.startsWith(targetOrigin))
	) {
		res.writeHead(403);
		res.end();
		return true;
	}

	if (originCandidate && refererCandidate && !refererCandidate.startsWith(originCandidate)) {
		res.writeHead(403);
		res.end();
		return true;
	}

	return false;
};

const isValidTargetUrl = (raw: string | null): raw is string => {
	if (!raw) return false;
	try {
		const parsed = new URL(raw);
		return parsed.protocol === 'https:' || parsed.protocol === 'http:';
	} catch {
		return false;
	}
};

const pipeRequest = (
	req: http.IncomingMessage,
	res: http.ServerResponse,
	targetUrl: string,
	_retriesRemaining: number,
): void => {
	const parsedTarget = new URL(targetUrl);
	const agent = parsedTarget.protocol === 'https:' ? https : http;
	const method = (req.method ?? 'GET').toUpperCase();

	const requestOptions: http.RequestOptions = {
		hostname: parsedTarget.hostname,
		port: parsedTarget.port || (parsedTarget.protocol === 'https:' ? 443 : 80),
		path: parsedTarget.pathname + parsedTarget.search,
		method,
		headers: buildForwardHeaders(req, parsedTarget),
	};

	const upstreamReq = agent.request(requestOptions, (upstreamRes) => {
		const status = upstreamRes.statusCode ?? 502;

		const headers = sanitizeUpstreamHeaders(upstreamRes.headers);
		delete headers.connection;
		delete headers['proxy-connection'];
		delete headers['transfer-encoding'];

		headers['Access-Control-Allow-Origin'] = req.headers.origin ?? req.headers.referer ?? 'https://web.fluxer.app';
		headers['Access-Control-Allow-Credentials'] = 'true';
		headers['Access-Control-Expose-Headers'] = 'x-fluxer-sudo-mode-jwt,x-request-id,content-type';
		headers.Vary = 'Origin';

		res.writeHead(status, headers);
		upstreamRes.pipe(res);
	});

	upstreamReq.on('error', (error: Error) => {
		log.error('[API Proxy] Upstream request error:', error);
		if (!res.headersSent) {
			setCorsHeaders(res, req.headers.origin ?? req.headers.referer);
			res.writeHead(502, {'Content-Type': 'text/plain'});
		}
		res.end('Bad Gateway');
	});

	res.on('close', () => {
		try {
			upstreamReq.destroy();
		} catch {}
	});

	req.pipe(upstreamReq);
};

let server: http.Server | null = null;

export const startApiProxyServer = (): Promise<void> => {
	return new Promise((resolve, reject) => {
		if (server) {
			resolve();
			return;
		}

		server = http.createServer((req, res) => {
			const remoteAddress = req.socket.remoteAddress;
			if (remoteAddress !== '127.0.0.1' && remoteAddress !== '::1' && remoteAddress !== '::ffff:127.0.0.1') {
				res.writeHead(403);
				res.end();
				return;
			}

			const requestUrl = new URL(req.url ?? '/', `http://${req.headers.host}`);

			if (requestUrl.pathname !== PROXY_PATH) {
				res.writeHead(404);
				res.end();
				return;
			}

			const target = requestUrl.searchParams.get('target');
			if (!isValidTargetUrl(target)) {
				res.writeHead(400, {'Content-Type': 'text/plain'});
				res.end('Missing or invalid target');
				return;
			}

			const targetUrl = new URL(target);
			const initiator = req.headers[PROXY_INITIATOR_HEADER];
			const initiatorHeader = Array.isArray(initiator) ? initiator[0] : initiator;

			if (rejectIfDisallowedPage(req, res, targetUrl, initiatorHeader ?? undefined)) {
				return;
			}

			setCorsHeaders(res, req.headers.origin ?? initiatorHeader ?? req.headers.referer ?? undefined);

			if (req.method?.toUpperCase() === 'OPTIONS') {
				res.setHeader('Access-Control-Allow-Methods', 'GET,HEAD,POST,PUT,PATCH,DELETE,OPTIONS');
				res.writeHead(204);
				res.end();
				return;
			}

			pipeRequest(req, res, target, 5);
		});

		server.on('error', (error: NodeJS.ErrnoException) => {
			if (error.code === 'EADDRINUSE') {
				log.warn(`[API Proxy] Port ${API_PROXY_PORT} already in use, API proxy disabled`);
				server = null;
				resolve();
			} else {
				log.error('[API Proxy] Server error:', error);
				reject(error);
			}
		});

		server.listen(API_PROXY_PORT, '127.0.0.1', () => {
			log.info(`[API Proxy] Server listening on http://127.0.0.1:${API_PROXY_PORT}${PROXY_PATH}`);
			resolve();
		});
	});
};

export const stopApiProxyServer = (): Promise<void> => {
	return new Promise((resolve) => {
		if (!server) {
			resolve();
			return;
		}

		server.close((err) => {
			if (err) {
				log.error('[API Proxy] Error closing server:', err);
			}
			server = null;
			resolve();
		});
	});
};
