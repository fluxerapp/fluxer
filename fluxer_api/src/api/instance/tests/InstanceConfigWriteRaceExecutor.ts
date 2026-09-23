// SPDX-License-Identifier: AGPL-3.0-or-later

import {getKvMeta} from '@app/api/database/CassandraMetaRegistry';
import type {CassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import type {CassandraParams, KvQueryMeta, PreparedQuery} from '@app/api/database/CassandraTypes';
import type {InstanceConfigurationRow} from '@app/api/database/types/InstanceConfigTypes';
import {InstanceConfiguration} from '@app/api/Tables';

type InstanceConfigWriteEvent = 'read' | 'write' | 'write rejected';

interface WriteGate {
	size: number;
	paused: Array<() => void>;
}

const FETCH_ROW_QUERY = InstanceConfiguration.selectCql({
	where: InstanceConfiguration.where.eq('key'),
	limit: 1,
});

export class InstanceConfigWriteRaceExecutor implements CassandraQueryExecutorForTesting {
	readonly events: Array<InstanceConfigWriteEvent> = [];
	private watchedKey: string | null = null;
	private gate: WriteGate | null = null;
	private beforeEachWrite: (() => Promise<void>) | null = null;

	constructor(private readonly base: CassandraQueryExecutorForTesting) {}

	watch(key: string): void {
		this.watchedKey = key;
		this.events.length = 0;
	}

	pauseWritesUntil(size: number): void {
		this.gate = {size, paused: []};
	}

	competeBeforeEachWrite(write: () => Promise<void>): void {
		this.beforeEachWrite = write;
	}

	async writeDirectly(key: string, value: string): Promise<void> {
		await this.base.executeQuery(InstanceConfiguration.upsertAll({key, value, updated_at: new Date()}));
	}

	async readDirectly(key: string): Promise<string | null> {
		const [row] = await this.base.executeQuery<InstanceConfigurationRow>({cql: FETCH_ROW_QUERY, params: {key}});
		return row?.value ?? null;
	}

	async executeQuery<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
		query: PreparedQuery<P>,
	): Promise<Array<T>> {
		const meta = query.kvMeta ?? getKvMeta(query.cql);
		if (this.watchedKey === null || !this.isWatched(meta, query.params)) {
			return this.base.executeQuery<T, P>(query);
		}
		if (meta?.action === 'select') {
			this.events.push('read');
			return this.base.executeQuery<T, P>(query);
		}
		await this.passGate();
		await this.beforeEachWrite?.();
		const rows = await this.base.executeQuery<T, P>(query);
		const applied = (rows[0] as {'[applied]'?: unknown} | undefined)?.['[applied]'];
		this.events.push(applied === false ? 'write rejected' : 'write');
		return rows;
	}

	executeBatch(queries: Array<{query: string; params: object; meta?: KvQueryMeta}>, atomic?: boolean): Promise<void> {
		return this.base.executeBatch(queries, atomic);
	}

	reset(): void {
		this.base.reset?.();
		this.watchedKey = null;
		this.gate = null;
		this.beforeEachWrite = null;
		this.events.length = 0;
	}

	async shutdown(): Promise<void> {
		await this.base.shutdown?.();
	}

	private isWatched(meta: KvQueryMeta | null | undefined, params: CassandraParams): boolean {
		return meta?.table.name === InstanceConfiguration.name && params.key === this.watchedKey;
	}

	private async passGate(): Promise<void> {
		const gate = this.gate;
		if (gate === null) return;
		await new Promise<void>((release) => {
			gate.paused.push(release);
			if (gate.paused.length < gate.size) return;
			this.gate = null;
			for (const release of gate.paused) release();
		});
	}
}
