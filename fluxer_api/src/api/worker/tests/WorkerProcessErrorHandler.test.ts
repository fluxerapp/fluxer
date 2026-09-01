// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it, vi} from 'vitest';
import {createWorkerProcessErrorHandler, type WorkerProcessErrorSource} from '../WorkerProcessErrorHandler';

function createHarness(overrides: {shutdown?: () => Promise<void>; forceExitDelayMs?: number} = {}) {
	const logger = {error: vi.fn(), warn: vi.fn()};
	const exit = vi.fn();
	const shutdown = vi.fn(overrides.shutdown ?? (async () => {}));
	const handle = createWorkerProcessErrorHandler({
		logger,
		shutdown,
		exit,
		forceExitDelayMs: overrides.forceExitDelayMs ?? 5,
	});
	return {logger, exit, shutdown, handle};
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
	it('keeps the worker running when Postgres terminates a pooled connection', async () => {
		const {logger, exit, shutdown, handle} = createHarness();

		await handle('uncaughtException', adminShutdownError());

		expect(shutdown).not.toHaveBeenCalled();
		expect(exit).not.toHaveBeenCalled();
		expect(logger.error).not.toHaveBeenCalled();
		expect(logger.warn).toHaveBeenCalledTimes(1);
		const [context, message] = logger.warn.mock.calls[0]!;
		expect(message).toBe('Transient database connection error reached the worker process, keeping the worker running');
		expect(context).toMatchObject({source: 'uncaughtException'});
	});

	it.each([
		['57P01 admin_shutdown', '57P01'],
		['57P02 crash_shutdown', '57P02'],
		['57P03 cannot_connect_now', '57P03'],
		['08006 connection_failure', '08006'],
		['08003 connection_does_not_exist', '08003'],
	])('survives %s', async (_label, code) => {
		const {exit, shutdown, handle} = createHarness();

		await handle('uncaughtException', pooledClientError({message: 'connection lost', code}));

		expect(shutdown).not.toHaveBeenCalled();
		expect(exit).not.toHaveBeenCalled();
	});

	it.each([
		['uncaughtException' as WorkerProcessErrorSource],
		['unhandledRejection' as WorkerProcessErrorSource],
	])('survives a transient error arriving as %s', async (source) => {
		const {exit, shutdown, handle} = createHarness();

		await handle(source, adminShutdownError());

		expect(shutdown).not.toHaveBeenCalled();
		expect(exit).not.toHaveBeenCalled();
	});

	it('survives a socket error raised by a pooled Postgres client', async () => {
		const {exit, shutdown, handle} = createHarness();

		await handle('uncaughtException', pooledClientError({message: 'read ECONNRESET', code: 'ECONNRESET'}));

		expect(shutdown).not.toHaveBeenCalled();
		expect(exit).not.toHaveBeenCalled();
	});

	it('survives a transient error wrapped in a cause chain', async () => {
		const {exit, shutdown, handle} = createHarness();
		const wrapped = new Error('Connection terminated due to connection timeout', {cause: adminShutdownError()});

		await handle('uncaughtException', wrapped);

		expect(shutdown).not.toHaveBeenCalled();
		expect(exit).not.toHaveBeenCalled();
	});

	it('survives the pg driver telling us the client is no longer queryable', async () => {
		const {exit, shutdown, handle} = createHarness();

		await handle('uncaughtException', new Error('Client has encountered a connection error and is not queryable'));

		expect(shutdown).not.toHaveBeenCalled();
		expect(exit).not.toHaveBeenCalled();
	});

	it('still tears the worker down on a programming error', async () => {
		const {logger, exit, shutdown, handle} = createHarness();
		const bug = new TypeError('cannot read properties of undefined');

		await handle('uncaughtException', bug);

		expect(logger.warn).not.toHaveBeenCalled();
		expect(logger.error).toHaveBeenCalledWith({err: bug, source: 'uncaughtException'}, 'Uncaught Exception');
		expect(shutdown).toHaveBeenCalledTimes(1);
		expect(exit).toHaveBeenCalledWith(1);
	});

	it('still tears the worker down on a Postgres error that is not connection level', async () => {
		const {exit, shutdown, handle} = createHarness();

		await handle(
			'uncaughtException',
			pooledClientError({message: 'duplicate key value violates unique constraint', code: '23505'}),
		);

		expect(shutdown).toHaveBeenCalledTimes(1);
		expect(exit).toHaveBeenCalledWith(1);
	});

	it('does not treat a socket error from a non-Postgres source as transient', async () => {
		const {exit, shutdown, handle} = createHarness();
		const socketError = Object.assign(new Error('read ECONNRESET'), {code: 'ECONNRESET'});

		await handle('uncaughtException', socketError);

		expect(shutdown).toHaveBeenCalledTimes(1);
		expect(exit).toHaveBeenCalledWith(1);
	});

	it('labels an unhandled rejection distinctly when it is fatal', async () => {
		const {logger, exit, shutdown, handle} = createHarness();
		const bug = new Error('boom');

		await handle('unhandledRejection', bug);

		expect(logger.error).toHaveBeenCalledWith(
			{err: bug, source: 'unhandledRejection'},
			'Unhandled Rejection at Promise',
		);
		expect(shutdown).toHaveBeenCalledTimes(1);
		expect(exit).toHaveBeenCalledWith(1);
	});

	it('force exits when shutdown hangs on a fatal error', async () => {
		const {exit, handle} = createHarness({shutdown: () => new Promise<void>(() => {}), forceExitDelayMs: 5});

		void handle('uncaughtException', new Error('boom'));

		await vi.waitFor(() => expect(exit).toHaveBeenCalledWith(1));
	});

	it('exits even when shutdown itself throws', async () => {
		const {logger, exit, handle} = createHarness({
			shutdown: async () => {
				throw new Error('shutdown failed');
			},
		});

		await handle('uncaughtException', new Error('boom'));

		expect(exit).toHaveBeenCalledWith(1);
		expect(logger.error).toHaveBeenCalledTimes(2);
	});
});
