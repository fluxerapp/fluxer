// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {hostname} from 'node:os';
import type {APIConfig} from '@app/api/config/APIConfig';
import {setCassandraWriteCapture} from '@app/api/database/CassandraQueryExecution';
import {CaptureCodec} from '@app/api/database/capture/CaptureCodec';
import type {CaptureProcess} from '@app/api/database/capture/CaptureRecords';
import {
	announceCaptureDisabled,
	JetStreamCaptureSink,
	WriteCapture,
} from '@app/api/database/capture/CassandraWriteCapture';
import {Logger} from '@app/api/Logger';
import {getLoadedCassandraDriver, nextWriteTimestamp} from '@pkgs/cassandra/src/Client';

const CLOSE_TIMEOUT_MS = 15_000;

let started: WriteCapture | null = null;

function captureSink(config: APIConfig['cassandraWriteCapture']): JetStreamCaptureSink {
	return new JetStreamCaptureSink({url: config.natsUrl, token: config.authToken, stream: config.stream});
}

async function announceDisabled(config: APIConfig['cassandraWriteCapture'], process: CaptureProcess, producer: string) {
	const sink = captureSink(config);
	try {
		await sink.connect();
		await announceCaptureDisabled(sink, process, producer);
		Logger.warn({producer}, 'Cassandra write capture is off and announced it to the capture stream');
	} finally {
		await sink.close();
	}
}

export async function startCassandraWriteCapture(
	config: APIConfig['cassandraWriteCapture'],
	process: CaptureProcess,
): Promise<void> {
	const producer = `${hostname()}:${randomUUID()}`;
	if (config.mode === 'off') {
		if (config.natsUrl) {
			void announceDisabled(config, process, producer).catch((error: unknown) => {
				Logger.error({error}, 'Cassandra write capture could not announce that it is off');
			});
		}
		return;
	}
	const driver = getLoadedCassandraDriver();
	if (!driver) {
		throw new Error('Cassandra write capture needs the Cassandra driver');
	}
	const sink = config.mode === 'on' ? captureSink(config) : null;
	await sink?.connect();
	const capture = new WriteCapture({
		process,
		producer,
		codec: new CaptureCodec(driver.types),
		sink,
		nextTimestamp: nextWriteTimestamp,
	});
	await capture.open();
	setCassandraWriteCapture(capture);
	started = capture;
	Logger.info({mode: config.mode, producer, stream: config.stream}, 'Cassandra write capture started');
}

export async function stopCassandraWriteCapture(): Promise<void> {
	await started?.close(CLOSE_TIMEOUT_MS);
	started = null;
}
