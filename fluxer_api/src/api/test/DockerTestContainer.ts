// SPDX-License-Identifier: AGPL-3.0-or-later

import {spawnSync} from 'node:child_process';
import {createServer} from 'node:net';
import {ensurePostgresKvSchema} from '@app/api/database/PostgresKvQueryExecutor';
import type {IPostgresClient} from '@pkgs/postgres/src/Client';
import {getDefaultPostgresClient, initPostgres, shutdownPostgres} from '@pkgs/postgres/src/Client';

function tail(value: string, lines: number): string {
	return value
		.split('\n')
		.map((line) => line.trimEnd())
		.filter((line) => line.length > 0)
		.slice(-lines)
		.join('\n');
}

export function startDockerContainer(args: ReadonlyArray<string>): void {
	const result = spawnSync('docker', args, {encoding: 'utf8'});
	if (result.error) {
		throw new Error(`failed to run docker: ${result.error.message}\n  docker ${args.join(' ')}`);
	}
	if (result.status === 0) {
		return;
	}
	const stderr = tail(result.stderr ?? '', 5);
	const stdout = tail(result.stdout ?? '', 3);
	const detail = stderr || stdout || 'no output';
	throw new Error(
		`docker exited ${result.status} while starting a test container.\n  docker ${args.join(' ')}\n  ${detail}`,
	);
}

export const dockerAvailable = spawnSync('docker', ['version'], {stdio: 'ignore'}).status === 0;

async function freePort(): Promise<number> {
	const server = createServer();
	await new Promise<void>((resolve, reject) => {
		server.once('error', reject);
		server.listen(0, '127.0.0.1', resolve);
	});
	const address = server.address();
	if (!address || typeof address === 'string') throw new Error('missing test port');
	const port = address.port;
	await new Promise<void>((resolve, reject) => server.close((err) => (err ? reject(err) : resolve())));
	return port;
}

export async function startPostgresKv({
	namePrefix,
	kvTable,
	maxConnections,
}: {
	namePrefix: string;
	kvTable: string;
	maxConnections: number;
}): Promise<{client: IPostgresClient; port: number; container: string}> {
	const port = await freePort();
	const container = `${namePrefix}-${process.pid.toString(36)}-${Date.now().toString(36)}`;
	startDockerContainer([
		'run',
		'-d',
		'--name',
		container,
		'-e',
		'POSTGRES_USER=fluxer',
		'-e',
		'POSTGRES_PASSWORD=fluxer',
		'-e',
		'POSTGRES_DB=fluxer',
		'-p',
		`127.0.0.1:${port}:5432`,
		'postgres:16-alpine',
		'-c',
		'fsync=off',
	]);
	try {
		let ready = false;
		for (let attempt = 0; attempt < 180 && !ready; attempt++) {
			await new Promise((resolve) => setTimeout(resolve, 500));
			const probe = spawnSync('docker', ['exec', container, 'pg_isready', '-U', 'fluxer', '-d', 'fluxer'], {
				stdio: 'ignore',
			});
			if (probe.status !== 0) continue;
			try {
				await initPostgres({url: `postgres://fluxer:fluxer@127.0.0.1:${port}/fluxer`, maxConnections, kvTable});
				await getDefaultPostgresClient().query('SELECT 1');
				ready = true;
			} catch {
				await shutdownPostgres().catch(() => {});
			}
		}
		if (!ready) throw new Error('postgres never came up');
		const client = getDefaultPostgresClient();
		await ensurePostgresKvSchema(client);
		return {client, port, container};
	} catch (error) {
		await stopPostgresKv(container);
		throw error;
	}
}

export async function stopPostgresKv(container: string): Promise<void> {
	await shutdownPostgres().catch(() => {});
	spawnSync('docker', ['rm', '-f', container], {stdio: 'ignore'});
}
