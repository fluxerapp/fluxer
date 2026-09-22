import {domainToASCII} from 'node:url';
import type {KVClusterNode} from '@pkgs/kv_client/src/KVClientConfig';
import type {RedisOptions} from 'ioredis';

interface KVClusterConnection {
	nodes: Array<KVClusterNode>;
	redisOptions: RedisOptions;
}

export function resolveKVClusterConnection(url: string, nodes: ReadonlyArray<KVClusterNode>): KVClusterConnection {
	const normalizedUrl = url.trim();
	for (let index = 0; index < normalizedUrl.length; index++) {
		const code = normalizedUrl.charCodeAt(index);
		if (code <= 0x1f || code === 0x7f) {
			throw new Error('KV cluster URL must not contain control characters');
		}
	}
	const parsed = parseClusterUrl(normalizedUrl);
	const authorityStart = normalizedUrl.indexOf('://');
	if (authorityStart === -1) {
		throw new Error('Invalid KV cluster URL');
	}
	const authorityAndPath = normalizedUrl.slice(authorityStart + 3).split(/[?#]/, 1)[0];
	const pathStart = authorityAndPath.indexOf('/');
	const authority = pathStart === -1 ? authorityAndPath : authorityAndPath.slice(0, pathStart);
	const path = pathStart === -1 ? '' : authorityAndPath.slice(pathStart + 1);
	const queryNames = new Set<string>();
	for (const name of parsed.searchParams.keys()) {
		if (name !== 'username' && name !== 'password' && name !== 'db') {
			throw new Error('Unsupported KV cluster URL option');
		}
		if (queryNames.has(name)) {
			throw new Error('Duplicate KV cluster URL option');
		}
		queryNames.add(name);
	}
	if (path.length > 0) {
		validateClusterDatabase(path);
	}
	const database = parsed.searchParams.get('db');
	if (database !== null) {
		validateClusterDatabase(database);
	}
	const redisOptions = resolveClusterAuthentication(authority, parsed.searchParams);
	if (parsed.protocol === 'rediss:') {
		redisOptions.tls = {};
	}
	const host = resolveClusterHost(authority, parsed);
	const resolvedNodes = nodes.length > 0 ? [...nodes] : [{host, port: Number(parsed.port || '6379')}];
	for (const node of resolvedNodes) {
		if (node.host.trim().length === 0) {
			throw new Error('KV cluster node must include a host');
		}
		if (!Number.isInteger(node.port) || node.port < 1 || node.port > 65535) {
			throw new Error('KV cluster node port must be an integer between 1 and 65535');
		}
	}
	return {
		nodes: resolvedNodes,
		redisOptions,
	};
}

function parseClusterUrl(url: string): URL {
	let parsed: URL;
	try {
		parsed = new URL(url);
	} catch {
		throw new Error('Invalid KV cluster URL');
	}
	if (parsed.protocol !== 'redis:' && parsed.protocol !== 'rediss:') {
		throw new Error('KV cluster URL must use redis:// or rediss://');
	}
	if (!parsed.hostname) {
		throw new Error('KV cluster URL must include a host');
	}
	return parsed;
}

function resolveClusterHost(authority: string, parsed: URL): string {
	if (parsed.hostname.startsWith('[')) {
		return parsed.hostname.slice(1, -1);
	}
	const hostAndPort = authority.slice(authority.lastIndexOf('@') + 1);
	const portStart = hostAndPort.lastIndexOf(':');
	const hostname = portStart === -1 ? hostAndPort : hostAndPort.slice(0, portStart);
	if (hostname.includes('%')) {
		throw new Error('KV cluster URL must not contain a percent-encoded hostname');
	}
	const host = domainToASCII(hostname.toLowerCase());
	if (!host) {
		throw new Error('Invalid KV cluster URL hostname');
	}
	return host;
}

function validateClusterDatabase(database: string): void {
	if (!/^0+$/.test(database)) {
		throw new Error('KV cluster URL must select database 0');
	}
}

function resolveClusterAuthentication(authority: string, query: URLSearchParams): RedisOptions {
	const authEnd = authority.lastIndexOf('@');
	let auth = '';
	if (authEnd !== -1) {
		try {
			auth = decodeURIComponent(authority.slice(0, authEnd));
		} catch {
			throw new Error('Invalid KV cluster URL credentials');
		}
	}
	if (auth) {
		const separator = auth.indexOf(':');
		return {
			username: separator === -1 ? auth : auth.slice(0, separator),
			password: separator === -1 ? '' : auth.slice(separator + 1),
		};
	}
	const options: RedisOptions = {};
	const username = query.get('username');
	const password = query.get('password');
	if (username !== null) {
		options.username = username;
	}
	if (password !== null) {
		options.password = password;
	}
	return options;
}
