// SPDX-License-Identifier: AGPL-3.0-or-later

import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {
	createWorkerProcessErrorHandler,
	type WorkerProcessErrorSource,
} from '@app/api/worker/WorkerProcessErrorHandler';
import {describe, expect, it} from 'vitest';

function createHarness(shutdown: () => Promise<void> = async () => {}) {
	const exitCodes: Array<number> = [];
	const calls = {shutdown: 0};
	const handle = createWorkerProcessErrorHandler({
		logger: new NoopLogger(),
		shutdown: async () => {
			calls.shutdown += 1;
			await shutdown();
		},
		exit: (code) => {
			exitCodes.push(code);
		},
	});
	return {exitCodes, calls, handle};
}

function pooledClientError(fields: Record<string, unknown>): Error {
	const error = new Error(String(fields['message'] ?? 'pooled client error'));
	Object.assign(error, {client: {}}, fields);
	return error;
}

function adminShutdownError(): Error {
	return pooledClientError({
		message: 'terminating connection due to administrator command',
		code: '57P01',
		severity: 'FATAL',
		routine: 'ProcessInterrupts',
		length: 116,
		name: 'error',
	});
}

describe('Worker process error handler', () => {
	it.each([
		['57P01 admin_shutdown', '57P01'],
		['57P02 crash_shutdown', '57P02'],
		['57P03 cannot_connect_now', '57P03'],
		['08006 connection_failure', '08006'],
		['08003 connection_does_not_exist', '08003'],
		['a pooled client socket reset', 'ECONNRESET'],
	])('keeps running on %s', async (_label, code) => {
		const {exitCodes, calls, handle} = createHarness();

		await handle('uncaughtException', pooledClientError({message: 'connection lost', code}));

		expect(calls.shutdown).toBe(0);
		expect(exitCodes).toEqual([]);
	});

	it.each([['uncaughtException' as WorkerProcessErrorSource], ['unhandledRejection' as WorkerProcessErrorSource]])(
		'keeps running on a transient error arriving as %s',
		async (source) => {
			const {exitCodes, calls, handle} = createHarness();

			await handle(source, adminShutdownError());

			expect(calls.shutdown).toBe(0);
			expect(exitCodes).toEqual([]);
		},
	);

	it('keeps running on a transient error wrapped in a cause chain', async () => {
		const {exitCodes, calls, handle} = createHarness();

		await handle(
			'uncaughtException',
			new Error('Connection terminated due to connection timeout', {cause: adminShutdownError()}),
		);

		expect(calls.shutdown).toBe(0);
		expect(exitCodes).toEqual([]);
	});

	it('keeps running when the pg driver says the client is no longer queryable', async () => {
		const {exitCodes, calls, handle} = createHarness();

		await handle('uncaughtException', new Error('Client has encountered a connection error and is not queryable'));

		expect(calls.shutdown).toBe(0);
		expect(exitCodes).toEqual([]);
	});

	it.each([
		['a programming error', new TypeError('cannot read properties of undefined')],
		[
			'a Postgres error that is not connection level',
			pooledClientError({message: 'duplicate key value violates unique constraint', code: '23505'}),
		],
		['a socket error from a non-Postgres source', Object.assign(new Error('read ECONNRESET'), {code: 'ECONNRESET'})],
	])('shuts down and exits on %s', async (_label, error) => {
		const {exitCodes, calls, handle} = createHarness();

		await handle('uncaughtException', error);

		expect(calls.shutdown).toBe(1);
		expect(exitCodes).toEqual([1]);
	});

	it('exits even when shutdown itself throws', async () => {
		const {exitCodes, handle} = createHarness(async () => {
			throw new Error('shutdown failed');
		});

		await handle('uncaughtException', new Error('boom'));

		expect(exitCodes).toEqual([1]);
	});
});
