#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later

import {request} from 'node:http';

const SOCKET_PATH = process.env.FLUXER_OPERATOR_SOCKET ?? '/run/fluxer/operator.sock';
const HOST_USER_HEADER = 'X-Fluxer-Operator-Host-User';
const SCHEMA_MAJOR = 1;
const TIMEOUT_MS = 120_000;

function respond(response) {
	process.stdout.write(
		`${JSON.stringify({api_version: process.env.BUILD_VERSION ?? '', schema_major: SCHEMA_MAJOR, ...response})}\n`,
	);
}

function fail(message) {
	respond({status: 0, headers: {}, body: null, relay_error: message});
}

async function readStdin() {
	const chunks = [];
	for await (const chunk of process.stdin) chunks.push(chunk);
	return Buffer.concat(chunks).toString('utf8');
}

function buildPath(path, query) {
	const params = new URLSearchParams();
	for (const [key, value] of Object.entries(query ?? {})) params.append(key, String(value));
	const search = params.toString();
	return search ? `${path}?${search}` : path;
}

function buildBody(req, headers) {
	if (req.body_base64) return Buffer.from(req.body_base64, 'base64');
	if (req.body === undefined || req.body === null) return null;
	if (!Object.keys(headers).some((key) => key.toLowerCase() === 'content-type')) {
		headers['Content-Type'] = 'application/json';
	}
	return Buffer.from(JSON.stringify(req.body));
}

function parseBody(text, contentType) {
	if (text === '') return null;
	if (contentType.includes('json')) {
		try {
			return JSON.parse(text);
		} catch {
			return text;
		}
	}
	return text;
}

function send(req) {
	const target = req.target === 'loopback' ? 'loopback' : 'operator';
	if (req.target !== target) return Promise.reject(new Error(`unknown target ${String(req.target)}`));
	if (typeof req.path !== 'string' || !req.path.startsWith('/'))
		return Promise.reject(new Error('path must start with /'));
	if (target === 'operator' && !(req.path === '/admin' || req.path.startsWith('/admin/'))) {
		return Promise.reject(new Error('the operator target serves /admin paths only'));
	}
	const headers = {...(req.headers ?? {})};
	if (target === 'operator' && req.host_user) headers[HOST_USER_HEADER] = String(req.host_user);
	if (target === 'loopback') {
		const clientIpHeader = (process.env.FLUXER_CLIENT_IP_HEADER_NAME || 'x-forwarded-for').toLowerCase();
		if (!Object.keys(headers).some((key) => key.toLowerCase() === clientIpHeader))
			headers[clientIpHeader] = '127.0.0.1';
	}
	const body = buildBody(req, headers);
	if (body !== null) headers['Content-Length'] = String(body.length);
	const location =
		target === 'operator'
			? {socketPath: SOCKET_PATH}
			: {host: '127.0.0.1', port: Number(process.env.FLUXER_API_PORT ?? '8080')};
	return new Promise((resolve, reject) => {
		const outgoing = request(
			{...location, method: req.method ?? 'GET', path: buildPath(req.path, req.query), headers, timeout: TIMEOUT_MS},
			(incoming) => {
				const chunks = [];
				incoming.on('data', (chunk) => chunks.push(chunk));
				incoming.on('error', reject);
				incoming.on('end', () => {
					const responseHeaders = {};
					for (const [key, value] of Object.entries(incoming.headers)) {
						if (value !== undefined) responseHeaders[key] = Array.isArray(value) ? value.join(', ') : value;
					}
					resolve({
						status: incoming.statusCode ?? 0,
						headers: responseHeaders,
						body: parseBody(Buffer.concat(chunks).toString('utf8'), responseHeaders['content-type'] ?? ''),
						api_version: responseHeaders['x-fluxer-version'] ?? process.env.BUILD_VERSION ?? '',
					});
				});
			},
		);
		outgoing.on('timeout', () => outgoing.destroy(new Error('the request timed out')));
		outgoing.on('error', reject);
		outgoing.end(body ?? undefined);
	});
}

async function main() {
	let req;
	try {
		req = JSON.parse(await readStdin());
	} catch {
		fail('the request is not valid JSON');
		return;
	}
	if (req === null || typeof req !== 'object') {
		fail('the request is not a JSON object');
		return;
	}
	try {
		respond(await send(req));
	} catch (error) {
		fail(error instanceof Error ? error.message : String(error));
	}
}

main().catch((error) => {
	process.stderr.write(`${error instanceof Error ? error.message : String(error)}\n`);
	process.exit(1);
});
