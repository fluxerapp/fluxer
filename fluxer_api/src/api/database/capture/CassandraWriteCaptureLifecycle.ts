// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {hostname} from 'node:os';
import type {APIConfig} from '@app/api/config/APIConfig';
import {setCassandraWriteCapture} from '@app/api/database/CassandraQueryExecution';
import {CaptureCodec} from '@app/api/database/capture/CaptureCodec';
import type {CaptureProcess} from '@app/api/database/capture/CaptureRecords';
import {JetStreamCaptureSink} from '@app/api/database/capture/CassandraWriteCapture';
import {CdcWriteCapture} from '@app/api/database/capture/CdcWriteCapture';
import {Logger} from '@app/api/Logger';
import {getLoadedCassandraDriver, nextWriteTimestamp} from '@pkgs/cassandra/src/Client';

const CDC_DRAIN_TIMEOUT_MS = 10_000;

let started: CdcWriteCapture | null = null;

export async function startCassandraWriteCapture(
	config: APIConfig['cassandraWriteCapture'],
	process: CaptureProcess,
): Promise<void> {
	if (config.mode === 'off') {
		return;
	}
	const driver = getLoadedCassandraDriver();
	if (!driver) {
		throw new Error('Cassandra write capture needs the Cassandra driver');
	}
	const producer = `${hostname()}:${randomUUID()}`;
	const capture = new CdcWriteCapture({
		process,
		producer,
		pod: hostname(),
		codec: new CaptureCodec(driver.types),
		sink: new JetStreamCaptureSink({url: config.natsUrl, token: config.authToken, stream: config.stream}),
		nextTimestamp: nextWriteTimestamp,
	});
	capture.open();
	setCassandraWriteCapture(capture);
	started = capture;
	Logger.info({mode: config.mode, producer, stream: config.stream}, 'Cassandra write capture started');
}

export async function stopCassandraWriteCapture(): Promise<void> {
	await started?.close(CDC_DRAIN_TIMEOUT_MS);
	started = null;
}
