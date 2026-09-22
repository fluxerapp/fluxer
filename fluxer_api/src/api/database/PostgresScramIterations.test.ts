// SPDX-License-Identifier: AGPL-3.0-or-later

import {spawnSync} from 'node:child_process';
import {createServer} from 'node:net';
import {startDockerContainer} from '@app/api/test/DockerTestContainer';
import {getDefaultPostgresClient, initPostgres, shutdownPostgres} from '@pkgs/postgres/src/Client';
import {afterAll, beforeAll, describe, expect, it} from 'vitest';

const CONTAINER = `fluxer-kvscram-${process.pid.toString(36)}-${Date.now().toString(36)}`;
const dockerAvailable = spawnSync('docker', ['version'], {stdio: 'ignore'}).status === 0;
const SCRAM_ITERATIONS = 200_000;

async function sleep(ms: number): Promise<void> {
	await new Promise((resolve) => setTimeout(resolve, ms));
}

async function freePort(): Promise<number> {
	return new Promise((resolve, reject) => {
		const server = createServer();
		server.on('error', reject);
		server.listen(0, '127.0.0.1', () => {
			const address = server.address();
			if (typeof address === 'string' || address === null) {
				reject(new Error('no port'));
				return;
			}
			const port = address.port;
			server.close(() => resolve(port));
		});
	});
}

describe.skipIf(!dockerAvailable)('postgres client against a server with raised SCRAM iterations', () => {
	let port: number;

	beforeAll(async () => {
		port = await freePort();
		startDockerContainer([
			'run',
			'-d',
			'--name',
			CONTAINER,
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
			'-c',
			`scram_iterations=${SCRAM_ITERATIONS}`,
		]);
		let ready = false;
		for (let attempt = 0; attempt < 180 && !ready; attempt += 1) {
			await sleep(500);
			const probe = spawnSync(
				'docker',
				['exec', CONTAINER, 'psql', '-h', '127.0.0.1', '-U', 'fluxer', '-d', 'fluxer', '-Atc', 'SELECT 1'],
				{stdio: 'ignore'},
			);
			ready = probe.status === 0;
		}
		if (!ready) throw new Error('postgres never came up');
		const rehash = spawnSync(
			'docker',
			['exec', CONTAINER, 'psql', '-U', 'fluxer', '-d', 'fluxer', '-Atc', "ALTER ROLE fluxer PASSWORD 'fluxer'"],
			{
				stdio: 'ignore',
			},
		);
		if (rehash.status !== 0) throw new Error('could not re-hash the role password');
	}, 900_000);

	afterAll(async () => {
		await shutdownPostgres().catch(() => {});
		spawnSync('docker', ['rm', '-f', CONTAINER], {stdio: 'ignore'});
	});

	it('connects when the role verifier uses more iterations than the driver default allows', async () => {
		await initPostgres({url: `postgres://fluxer:fluxer@127.0.0.1:${port}/fluxer`, maxConnections: 1});
		const verifier = await getDefaultPostgresClient().query<{rolpassword: string}>(
			"SELECT rolpassword FROM pg_authid WHERE rolname = 'fluxer'",
		);
		expect(verifier.rows[0]?.rolpassword.startsWith(`SCRAM-SHA-256$${SCRAM_ITERATIONS}:`)).toBe(true);
	});
});
