// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineTable} from '@app/api/database/CassandraTableDsl';
import {getStatementMeta} from '@app/api/database/CassandraTypes';
import {CaptureCodec} from '@app/api/database/capture/CaptureCodec';
import type {CapturedStatement, CaptureSink} from '@app/api/database/capture/CassandraWriteCapture';
import type {CdcRecord} from '@app/api/database/capture/CdcRecords';
import {CaptureClosedError, CdcWriteCapture} from '@app/api/database/capture/CdcWriteCapture';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {drainWorkerRuntime} from '@app/api/worker/WorkerShutdown';
import cassandra from 'cassandra-driver';
import {afterAll, beforeEach, describe, expect, it, vi} from 'vitest';

const loggerWarn = vi.spyOn(NoopLogger.prototype, 'warn');

afterAll(() => {
	loggerWarn.mockRestore();
});

class RecordingSink implements CaptureSink {
	readonly records: Array<CdcRecord> = [];

	async publish(_subject: string, body: string): Promise<void> {
		this.records.push(JSON.parse(body) as CdcRecord);
	}

	async close(): Promise<void> {}
}

const Users = defineTable<{user_id: bigint; username: string}, 'user_id'>({
	name: 'users',
	columns: ['user_id', 'username'],
	primaryKey: ['user_id'],
});

function userWrite(username: string): CapturedStatement {
	const query = Users.upsertAll({user_id: 1n, username});
	return {cql: query.cql, type: getStatementMeta(query.cql).type, params: query.params, meta: query.kvMeta};
}

function runtimeWithJob(job: Promise<void>, taskType: string) {
	let running = true;
	const done = () => {
		running = false;
	};
	void job.then(done, done);
	return {
		stop: () => job,
		inFlightTaskTypes: () => (running ? [taskType] : []),
	};
}

describe('worker shutdown', () => {
	beforeEach(() => {
		loggerWarn.mockClear();
	});

	it('returns once the jobs finish inside the drain budget', async () => {
		const drained = await drainWorkerRuntime(runtimeWithJob(Promise.resolve(), 'flushUserActivityBuffer'), 1000);
		expect(drained).toBe(true);
		expect(loggerWarn).not.toHaveBeenCalled();
	});

	it('closes capture with every applied write when a job outlives the drain budget', async () => {
		const sink = new RecordingSink();
		const capture = new CdcWriteCapture({
			process: 'worker',
			producer: 'worker-batch-0:boot',
			pod: 'worker-batch-0',
			codec: new CaptureCodec(cassandra.types),
			sink,
			nextTimestamp: () => Date.now() * 1000,
		});
		const sent: Array<string> = [];
		const send = (name: string) => async () => {
			sent.push(name);
		};
		const resume = Promise.withResolvers<void>();
		const job = (async () => {
			await capture.plain([userWrite('first')], false, send('first'));
			await resume.promise;
			await capture.plain([userWrite('second')], false, send('second'));
		})();
		const settled = job.catch((error: unknown) => error);

		const drained = await drainWorkerRuntime(runtimeWithJob(job, 'processPremiumStateReconciliationQueue'), 20);
		expect(drained).toBe(false);
		expect(loggerWarn).toHaveBeenCalledWith(
			{budgetMs: 20, taskTypes: ['processPremiumStateReconciliationQueue']},
			'Worker jobs still running at shutdown, leaving them for redelivery',
		);

		await capture.close(1000);
		resume.resolve();

		expect(await settled).toBeInstanceOf(CaptureClosedError);
		expect(sent).toEqual(['first']);
		expect(sink.records.at(-1)).toEqual({
			kind: 'life',
			producer: 'worker-batch-0:boot',
			state: 'closed',
			last_seq: 1,
			dropped: 0,
		});
		expect(sink.records.filter((record) => record.kind === 'k').map((record) => record.seq)).toEqual([1]);
	});
});
