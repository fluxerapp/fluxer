// SPDX-License-Identifier: AGPL-3.0-or-later

import {spawn} from 'node:child_process';
import {existsSync} from 'node:fs';
import {mkdtemp, readdir, readFile, rm} from 'node:fs/promises';
import {createServer, request, type Server} from 'node:http';
import type {AddressInfo} from 'node:net';
import {tmpdir} from 'node:os';
import path from 'node:path';
import type {AdminAuditLog} from '@app/api/admin/AdminRepository';
import {OPERATOR_HOST_USER_HEADER, OPERATOR_SOCKET_PATH} from '@app/api/admin/operator/OperatorRequests';
import {createOperatorServer} from '@app/api/admin/operator/OperatorSocket';
import {createAdminApiKey} from '@app/api/admin/tests/AdminTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import {getAdminRepository, getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const API_ROOT = path.resolve(import.meta.dirname, '../../../..');
const REPO_ROOT = path.resolve(API_ROOT, '..');
const RELAY_PATH = path.join(API_ROOT, 'bin/operator-relay.mjs');
const NEW_PASSWORD = 'chosen-from-the-link-2026';

interface SocketResponse {
	status: number;
	body: unknown;
}

interface CreatedUser {
	user: {id: string; username: string; acls: Array<string>};
	password_reset: {url: string; expires_at: string};
}

function socketRequest(
	socketPath: string,
	method: string,
	requestPath: string,
	options: {body?: unknown; headers?: Record<string, string>} = {},
): Promise<SocketResponse> {
	const payload = options.body === undefined ? null : Buffer.from(JSON.stringify(options.body));
	return new Promise((resolve, reject) => {
		const outgoing = request(
			{
				socketPath,
				method,
				path: requestPath,
				headers: {
					...(payload ? {'Content-Type': 'application/json', 'Content-Length': String(payload.length)} : {}),
					...options.headers,
				},
			},
			(incoming) => {
				const chunks: Array<Buffer> = [];
				incoming.on('data', (chunk: Buffer) => chunks.push(chunk));
				incoming.on('end', () => {
					const text = Buffer.concat(chunks).toString('utf8');
					resolve({status: incoming.statusCode ?? 0, body: text ? JSON.parse(text) : null});
				});
				incoming.on('error', reject);
			},
		);
		outgoing.on('error', reject);
		outgoing.end(payload ?? undefined);
	});
}

function runRelay(
	socketPath: string,
	input: string,
	env: Record<string, string> = {},
): Promise<{code: number | null; stdout: string}> {
	return new Promise((resolve, reject) => {
		const child = spawn(process.execPath, [RELAY_PATH], {
			env: {...process.env, FLUXER_OPERATOR_SOCKET: socketPath, BUILD_VERSION: 'relay-test', ...env},
		});
		let stdout = '';
		child.stdout.on('data', (chunk: Buffer) => {
			stdout += chunk.toString('utf8');
		});
		child.on('error', reject);
		child.on('close', (code) => resolve({code, stdout}));
		child.stdin.end(input);
	});
}

async function newAuditLogs(before: Set<string>): Promise<Array<AdminAuditLog>> {
	return (await getAdminRepository().listAllAuditLogsPaginated(100000)).filter(
		(log) => !before.has(log.logId.toString()),
	);
}

async function auditLogIds(): Promise<Set<string>> {
	return new Set((await getAdminRepository().listAllAuditLogsPaginated(100000)).map((log) => log.logId.toString()));
}

async function filesUnder(directory: string): Promise<Array<string>> {
	if (!existsSync(directory)) return [];
	const files: Array<string> = [];
	for (const entry of await readdir(directory, {withFileTypes: true, recursive: true})) {
		if (entry.isFile()) files.push(path.join(entry.parentPath, entry.name));
	}
	return files;
}

describe('operator channel', () => {
	let harness: ApiTestHarness;
	let server: Server;
	let socketDir: string;
	let socketPath: string;

	beforeAll(async () => {
		harness = await createApiTestHarness();
		socketDir = await mkdtemp(path.join(tmpdir(), 'fluxer-op-'));
		socketPath = path.join(socketDir, 'operator.sock');
		server = createOperatorServer(harness.app);
		await new Promise<void>((resolve) => server.listen(socketPath, resolve));
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await new Promise<void>((resolve) => server.close(() => resolve()));
		await rm(socketDir, {recursive: true, force: true});
		await harness.shutdown();
	});

	it('acts as the system user with every admin permission on the socket', async () => {
		const response = await socketRequest(socketPath, 'GET', '/admin/users/@me');
		expect(response.status).toBe(HTTP_STATUS.OK);
		expect((response.body as {user: {id: string}}).user.id).toBe('0');
	});

	it('serves admin routes only', async () => {
		const response = await socketRequest(socketPath, 'GET', '/users/@me');
		expect(response.status).toBe(HTTP_STATUS.NOT_FOUND);
		expect((response.body as {code: string}).code).toBe(APIErrorCodes.NOT_FOUND);
	});

	it('refuses operator access off the socket', async () => {
		await createBuilderWithoutAuth(harness)
			.get('/admin/users/@me')
			.header(OPERATOR_HOST_USER_HEADER, 'alex')
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();
	});

	it('still refuses writes to system users', async () => {
		const response = await socketRequest(socketPath, 'POST', '/admin/users/0/password-reset-link');
		expect(response.status).not.toBe(HTTP_STATUS.OK);
		expect((response.body as {code: string}).code).toBe(APIErrorCodes.UNKNOWN_USER);
	});

	it('records rexctl and the host user in audit metadata', async () => {
		const target = await createTestAccount(harness);
		const before = await auditLogIds();
		const response = await socketRequest(socketPath, 'GET', `/admin/users/${target.userId}`, {
			headers: {[OPERATOR_HOST_USER_HEADER]: 'alex'},
		});
		expect(response.status).toBe(HTTP_STATUS.OK);
		const logs = await newAuditLogs(before);
		expect(logs).toHaveLength(1);
		expect(logs[0]!.adminUserId.toString()).toBe('0');
		expect(Object.fromEntries(logs[0]!.metadata)).toMatchObject({via: 'rexctl', host_user: 'alex'});
	});

	it('records the key in audit metadata for admin API keys', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
		const key = await createAdminApiKey(harness, admin, 'audit attribution', [AdminACLs.USER_LOOKUP], null);
		const before = await auditLogIds();
		await createBuilder(harness, key.token).get(`/admin/users/${admin.userId}`).expect(HTTP_STATUS.OK).execute();
		const logs = await newAuditLogs(before);
		expect(logs).toHaveLength(1);
		expect(logs[0]!.metadata.get('via')).toBe(`api_key:${key.keyId}`);
	});

	it('creates an admin whose reset link works', async () => {
		await getInstanceConfigRepository().setInstanceIntegrationsConfig({email: {delivery: 'off'}});
		const email = `operator-created-${Date.now()}@example.com`;
		const response = await socketRequest(socketPath, 'POST', '/admin/users', {
			body: {username: 'firstadmin', email, admin: true},
		});
		expect(response.status).toBe(HTTP_STATUS.OK);
		const created = response.body as CreatedUser;
		expect(created.user.acls).toEqual([AdminACLs.WILDCARD]);
		const token = created.password_reset.url.split('/reset#token=')[1]!;
		const reset = await createBuilderWithoutAuth<{user_id: string}>(harness)
			.post('/auth/reset')
			.body({token, password: NEW_PASSWORD})
			.execute();
		expect(reset.user_id).toBe(created.user.id);
		const login = await createBuilderWithoutAuth<{user_id: string}>(harness)
			.post('/auth/login')
			.body({email, password: NEW_PASSWORD})
			.execute();
		expect(login.user_id).toBe(created.user.id);
		const admins = await socketRequest(socketPath, 'GET', `/admin/users?acl=${encodeURIComponent('*')}`);
		expect((admins.body as {users: Array<{id: string}>}).users.map((user) => user.id)).toContain(created.user.id);
	});

	it('signs an admin in once with a link made on the socket', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
		const member = await createTestAccount(harness);
		await createBuilder(harness, admin.token)
			.post(`/admin/users/${admin.userId}/sign-in-link`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		const refused = await socketRequest(socketPath, 'POST', `/admin/users/${member.userId}/sign-in-link`);
		expect((refused.body as {code: string}).code).toBe(APIErrorCodes.MISSING_ACL);
		const before = await auditLogIds();
		const created = await socketRequest(socketPath, 'POST', `/admin/users/${admin.userId}/sign-in-link`, {
			headers: {[OPERATOR_HOST_USER_HEADER]: 'alex'},
		});
		expect(created.status).toBe(HTTP_STATUS.OK);
		const token = (created.body as {url: string}).url.split('/login#sign_in_link=')[1]!;
		const signedIn = await createBuilderWithoutAuth<{user_id: string; token: string}>(harness)
			.post('/auth/sign-in-link')
			.body({token})
			.execute();
		expect(signedIn.user_id).toBe(admin.userId);
		await createBuilder(harness, signedIn.token).get('/users/@me').expect(HTTP_STATUS.OK).execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/sign-in-link')
			.body({token})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		const logs = (await newAuditLogs(before)).map((log) => ({
			action: log.action,
			admin: log.adminUserId.toString(),
			target: log.targetId.toString(),
			metadata: Object.fromEntries(log.metadata),
		}));
		expect(logs).toContainEqual({
			action: 'create_sign_in_link',
			admin: '0',
			target: admin.userId,
			metadata: {via: 'rexctl', host_user: 'alex'},
		});
		expect(logs).toContainEqual({
			action: 'use_sign_in_link',
			admin: admin.userId,
			target: admin.userId,
			metadata: {via: 'rexctl'},
		});
	});

	it('creates accounts without email on a username instance', async () => {
		const config = getConfig();
		const selfHosted = config.instance.selfHosted;
		config.instance.selfHosted = true;
		try {
			await getInstanceConfigRepository().setAccountIdentityMode(AccountIdentityModes.USERNAME, 'setup');
			await expectUsernameInstanceCreation();
		} finally {
			config.instance.selfHosted = selfHosted;
		}
	});

	async function expectUsernameInstanceCreation(): Promise<void> {
		const refused = await socketRequest(socketPath, 'POST', '/admin/users', {
			body: {username: 'withemail', email: 'nope@example.com'},
		});
		expect((refused.body as {code: string}).code).toBe(APIErrorCodes.USERNAME_SIGN_IN_ONLY);
		const response = await socketRequest(socketPath, 'POST', '/admin/users', {body: {username: 'plainmember'}});
		expect(response.status).toBe(HTTP_STATUS.OK);
		const created = response.body as CreatedUser;
		expect(created.user.acls).toEqual([]);
		const admins = await socketRequest(socketPath, 'GET', '/admin/users?acl=*');
		expect((admins.body as {users: Array<{id: string}>}).users.map((user) => user.id)).not.toContain(created.user.id);
	}

	it('refuses user creation to admins without full access', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.USER_LOOKUP,
			AdminACLs.ACL_SET_USER,
		]);
		await createBuilder(harness, admin.token)
			.post('/admin/users')
			.body({username: 'notallowed', email: 'notallowed@example.com'})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});

	it('relays one request and its response through the socket', async () => {
		const result = await runRelay(
			socketPath,
			JSON.stringify({target: 'operator', method: 'GET', path: '/admin/users/@me', host_user: 'alex'}),
		);
		expect(result.code, result.stdout).toBe(0);
		const response = JSON.parse(result.stdout) as {
			status: number;
			body: {user: {id: string}};
			schema_major: number;
			relay_error?: string;
		};
		expect(response.status, result.stdout).toBe(HTTP_STATUS.OK);
		expect(response.body.user.id, result.stdout).toBe('0');
		expect(response.schema_major).toBe(1);
		expect(response.relay_error).toBeUndefined();
	});

	it('relays an error when the socket is missing', async () => {
		const result = await runRelay(
			path.join(socketDir, 'missing.sock'),
			JSON.stringify({target: 'operator', method: 'GET', path: '/admin/users/@me'}),
		);
		expect(result.code).toBe(0);
		const response = JSON.parse(result.stdout) as {status: number; relay_error: string};
		expect(response.status).toBe(0);
		expect(response.relay_error).not.toBe('');
	});

	it('gives loopback requests a client IP the api accepts', async () => {
		const seen: Array<string | undefined> = [];
		const server = createServer((req, res) => {
			seen.push(req.headers['x-forwarded-for'] as string | undefined);
			res.setHeader('Content-Type', 'application/json');
			res.end('{}');
		});
		await new Promise<void>((resolve) => server.listen(0, '127.0.0.1', resolve));
		const port = String((server.address() as AddressInfo).port);
		try {
			const result = await runRelay(
				socketPath,
				JSON.stringify({target: 'loopback', method: 'GET', path: '/.well-known/fluxer'}),
				{FLUXER_API_PORT: port, FLUXER_CLIENT_IP_HEADER_NAME: ''},
			);
			expect(result.code, result.stdout).toBe(0);
			expect((JSON.parse(result.stdout) as {status: number}).status, result.stdout).toBe(HTTP_STATUS.OK);
			expect(seen).toEqual(['127.0.0.1']);
		} finally {
			await new Promise<void>((resolve) => server.close(() => resolve()));
		}
	});

	it('keeps the relay and the api on the same socket path', async () => {
		expect(await readFile(RELAY_PATH, 'utf8')).toContain(`'${OPERATOR_SOCKET_PATH}'`);
	});

	it('gives the edge and the stack files no route to the socket', async () => {
		const roots = [path.join(REPO_ROOT, 'deploy'), path.join(REPO_ROOT, 'fluxer_rexctl/internal/stack/files')];
		const offenders: Array<string> = [];
		for (const root of roots) {
			for (const file of await filesUnder(root)) {
				if (file.includes(`${path.sep}node_modules${path.sep}`) || file.includes(`${path.sep}target${path.sep}`))
					continue;
				const text = await readFile(file, 'utf8').catch(() => '');
				if (text.includes(path.dirname(OPERATOR_SOCKET_PATH))) offenders.push(path.relative(REPO_ROOT, file));
			}
		}
		expect(offenders).toEqual([]);
	});
});
