// SPDX-License-Identifier: AGPL-3.0-or-later

import {Counter, Histogram} from '@fluxer/hono/src/middleware/Metrics';

const PREFIX = 'fluxer_clickhouse_cdc';

export const gauges = {
	lag_seconds: 0,
	pending_records: 0,
	ack_pending: 0,
	max_ack_pending: 0,
	recheck_queued: 0,
	recheck_overdue: 0,
	paused: 0,
	broken: 0,
	bootstrapped: 0,
	repair_due: 0,
	unlisted_tables: 0,
	stream_bytes: 0,
	stream_max_bytes: 0,
	stream_first_timestamp_seconds: 0,
	stream_last_timestamp_seconds: 0,
	status_refreshed_timestamp_seconds: 0,
};

const GAUGE_HELP: Record<keyof typeof gauges, string> = {
	lag_seconds: 'Age of the oldest capture record not yet applied',
	pending_records: 'Records the consumer has not acknowledged yet',
	ack_pending: 'Records delivered and not yet acknowledged',
	max_ack_pending: 'The max_ack_pending setting of the consumer',
	recheck_queued: 'Unknown-outcome keys waiting for a second read',
	recheck_overdue: 'Unknown-outcome keys due more than 5 minutes ago',
	paused: 'Whether a batch is being retried',
	broken: 'Whether the loop stopped on something it will not handle',
	bootstrapped: 'Whether the first scan of every table has finished',
	repair_due: 'Whether a table is marked for repair',
	unlisted_tables: 'Keyspace tables in neither CDC_TABLES nor CDC_SKIPPED',
	stream_bytes: 'Bytes held by the stream',
	stream_max_bytes: 'The max_bytes setting of the stream',
	stream_first_timestamp_seconds: 'Time of the oldest record in the stream',
	stream_last_timestamp_seconds: 'Time of the newest record in the stream',
	status_refreshed_timestamp_seconds: 'When the stream and consumer state was last read from NATS',
};

export const repairFinished = new Map<string, number>();
export const repairTombstones = new Map<string, number>();
export const repairScanStarted = new Map<string, number>();
export const repairRowsRead = new Map<string, number>();
export const schemaMismatch = new Set<string>();

const counters: Array<[string, string, Counter]> = [];

function counter(name: string, help: string): Counter {
	const created = new Counter();
	counters.push([`${PREFIX}_${name}`, help, created]);
	return created;
}

export const recordsTotal = counter('records_total', 'Records fetched, by kind');
export const keysReadTotal = counter('keys_read_total', 'Keys read back from Cassandra, by table');
export const rowsWrittenTotal = counter('rows_written_total', 'Rows written to ClickHouse, by table');
export const tombstonesTotal = counter('tombstones_total', 'Tombstones written, by table and source');
export const cassandraErrorsTotal = counter('cassandra_errors_total', 'Failed Cassandra reads');
export const clickhouseErrorsTotal = counter('clickhouse_errors_total', 'Failed ClickHouse calls');
export const producerGapsTotal = counter('producer_gaps_total', 'Gaps in a producer seq');
export const lossyProducersTotal = counter('lossy_producers_total', 'Producers that reported loss');
export const producersUnclosedTotal = counter(
	'producers_unclosed_total',
	'Producers that ended without a closed record',
);
export const producersDisabledTotal = counter(
	'producers_disabled_total',
	'Producers that announced they run with capture off',
);
export const streamLossTotal = counter('stream_loss_total', 'Records the stream discarded before delivery');

const batchSeconds = new Histogram([0.5, 1, 2.5, 5, 10, 15, 30, 60, 120, 300]);

export function observeBatch(seconds: number): void {
	batchSeconds.observe(seconds);
}

function byTable(name: string, help: string, values: Iterable<[string, number]>): string {
	return [
		`# HELP ${PREFIX}_${name} ${help}`,
		`# TYPE ${PREFIX}_${name} gauge`,
		...[...values].map(([table, value]) => `${PREFIX}_${name}{table="${table}"} ${value}`),
	].join('\n');
}

export function renderMetrics(): string {
	const sections = [
		...Object.entries(GAUGE_HELP).map(([name, help]) =>
			[
				`# HELP ${PREFIX}_${name} ${help}`,
				`# TYPE ${PREFIX}_${name} gauge`,
				`${PREFIX}_${name} ${gauges[name as keyof typeof gauges]}`,
			].join('\n'),
		),
		...counters.map(([name, help, metric]) => metric.render(name, help)),
		batchSeconds.render(`${PREFIX}_batch_seconds`, 'Time from the first fetch of a batch to its acknowledgement'),
		byTable('repair_last_finished_timestamp_seconds', 'When the last repair of a table finished', repairFinished),
		byTable('repair_tombstones', 'Tombstones written by the last repair of a table', repairTombstones),
		byTable(
			'repair_scan_started_timestamp_seconds',
			'When the repair scan of a table in progress started, 0 when none runs',
			repairScanStarted,
		),
		byTable('repair_rows_read', 'Rows read so far by the repair scan of a table in progress', repairRowsRead),
		byTable(
			'schema_mismatch',
			'Whether a table has a column type ClickHouse does not match',
			[...schemaMismatch].map((table): [string, number] => [table, 1]),
		),
	];
	return `${sections.join('\n\n')}\n`;
}
