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

import {randomUUID} from 'node:crypto';
import http from 'node:http';
import https from 'node:https';
import log from 'electron-log';
import {BUILD_CHANNEL} from '../common/build-channel.js';
import {CANARY_APP_URL, STABLE_APP_URL} from '../common/constants.js';

export const MEDIA_PROXY_PORT = BUILD_CHANNEL === 'canary' ? 21868 : 21867;
const MEDIA_PROXY_TOKEN_PARAM = 'token';
const MEDIA_PROXY_TOKEN = randomUUID();

export const getMediaProxyToken = (): string => MEDIA_PROXY_TOKEN;

const ALLOWED_ORIGINS = [STABLE_APP_URL, CANARY_APP_URL];

const isAllowedOrigin = (origin?: string): boolean => {
	if (!origin) return false;
	return ALLOWED_ORIGINS.includes(origin);
};

const refererMatchesAllowedOrigin = (referer?: string): boolean => {
	if (!referer) return false;
	return ALLOWED_ORIGINS.some((allowed) => referer.startsWith(allowed));
};

const rejectIfDisallowedPage = (
	req: http.IncomingMessage,
	res: http.ServerResponse,
	targetUrl?: URL,
	tokenValid?: boolean,
): boolean => {
	const origin = req.headers.origin;
	const referer = req.headers.referer;
	const targetOrigin = targetUrl?.origin;

	if (tokenValid) {
		return false;
	}

	if (!origin && !referer) {
		res.writeHead(403);
		res.end();
		return true;
	}

	if (origin && !isAllowedOrigin(origin) && origin !== targetOrigin) {
		res.writeHead(403);
		res.end();
		return true;
	}

	if (referer && !refererMatchesAllowedOrigin(referer) && !(targetOrigin && referer.startsWith(targetOrigin))) {
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

const isValidTargetUrl = (raw: string | null): raw is string => {
	if (!raw) return false;
	try {
		const parsed = new URL(raw);
		return parsed.protocol === 'https:' || parsed.protocol === 'http:';
	} catch {
		return false;
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
	delete headers.origin;
	delete headers.referer;
	delete headers.connection;
	delete headers['proxy-connection'];
	delete headers['sec-fetch-site'];
	delete headers['sec-fetch-mode'];
	delete headers['sec-fetch-dest'];

	headers.host = targetUrl.host;
	return headers;
};

const pipeRequest = (
	req: http.IncomingMessage,
	res: http.ServerResponse,
	targetUrl: string,
	redirectsRemaining: number,
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

		if (
			redirectsRemaining > 0 &&
			(status === 301 || status === 302 || status === 303 || status === 307 || status === 308) &&
			typeof upstreamRes.headers.location === 'string'
		) {
			const location = upstreamRes.headers.location;
			const nextTarget = new URL(location, parsedTarget).toString();
			upstreamRes.resume();
			pipeRequest(req, res, nextTarget, redirectsRemaining - 1);
			return;
		}

		const headers = sanitizeUpstreamHeaders(upstreamRes.headers);
		delete headers.connection;
		delete headers['proxy-connection'];
		delete headers['transfer-encoding'];

		headers['cross-origin-resource-policy'] = 'cross-origin';

		res.writeHead(status, headers);
		upstreamRes.pipe(res);
	});

	upstreamReq.on('error', (error: Error) => {
		log.error('[Media Proxy] Upstream request error:', error);
		if (!res.headersSent) {
			res.writeHead(502, {'Content-Type': 'text/plain'});
		}
		res.end('Bad Gateway');
	});

	res.on('close', () => {
		try {
			upstreamReq.destroy();
		} catch {}
	});

	upstreamReq.end();
};

let server: http.Server | null = null;

const hasValidMediaProxyToken = (url: URL): boolean => {
	const value = url.searchParams.get(MEDIA_PROXY_TOKEN_PARAM);
	return value === MEDIA_PROXY_TOKEN;
};

export const startMediaProxyServer = (): Promise<void> => {
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
			const tokenValid = hasValidMediaProxyToken(requestUrl);
			if (!tokenValid) {
				res.writeHead(403);
				res.end();
				return;
			}
			requestUrl.searchParams.delete(MEDIA_PROXY_TOKEN_PARAM);

			const method = (req.method ?? 'GET').toUpperCase();
			if (method !== 'GET' && method !== 'HEAD') {
				res.writeHead(405, {'Content-Type': 'text/plain'});
				res.end('Method Not Allowed');
				return;
			}

			if (requestUrl.pathname !== '/media') {
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

			if (rejectIfDisallowedPage(req, res, targetUrl, tokenValid)) {
				return;
			}
			pipeRequest(req, res, target, 5);
		});

		server.on('error', (error: NodeJS.ErrnoException) => {
			if (error.code === 'EADDRINUSE') {
				log.warn(`[Media Proxy] Port ${MEDIA_PROXY_PORT} already in use, media proxy disabled`);
				server = null;
				resolve();
			} else {
				log.error('[Media Proxy] Server error:', error);
				reject(error);
			}
		});

		server.listen(MEDIA_PROXY_PORT, '127.0.0.1', () => {
			log.info(`[Media Proxy] Server listening on http://127.0.0.1:${MEDIA_PROXY_PORT}/media`);
			resolve();
		});
	});
};

export const stopMediaProxyServer = (): Promise<void> => {
	return new Promise((resolve) => {
		if (!server) {
			resolve();
			return;
		}

		server.close((err) => {
			if (err) {
				log.error('[Media Proxy] Error closing server:', err);
			}
			server = null;
			resolve();
		});
	});
};
