// SPDX-License-Identifier: AGPL-3.0-or-later

import {timestampText} from '@app/api/capture/cdc/CdcRows';
import {CDC_DATABASE} from '@app/api/capture/cdc/CdcSchema';
import type {ClickHouseHttp} from '@app/api/capture/cdc/ClickHouseHttp';

function microsText(us: number): string {
	const ms = Math.floor(us / 1000);
	return `${timestampText(ms) ?? ''}${String(Math.floor(us) % 1000).padStart(3, '0')}`;
}

export class StateStore {
	private readonly clickhouse: ClickHouseHttp;
	private lastUs = 0;

	constructor(clickhouse: ClickHouseHttp) {
		this.clickhouse = clickhouse;
	}

	async read<T>(prefix: string): Promise<Map<string, T>> {
		const rows = await this.clickhouse.select<{name: string; value: string}>(
			`SELECT name, value FROM ${CDC_DATABASE}._state FINAL WHERE _deleted = 0 AND startsWith(name, {prefix:String})`,
			{params: {prefix}},
		);
		return new Map(rows.map((row) => [row.name, JSON.parse(row.value) as T]));
	}

	async write(entries: ReadonlyMap<string, unknown>): Promise<void> {
		if (entries.size === 0) return;
		this.lastUs = Math.max(Date.now() * 1000, this.lastUs + 1);
		const updatedAt = microsText(this.lastUs);
		const lines = [...entries].map(([name, value]) =>
			JSON.stringify({
				name,
				value: value === null ? '' : JSON.stringify(value),
				updated_at: updatedAt,
				_deleted: value === null ? 1 : 0,
			}),
		);
		await this.clickhouse.insert(`${CDC_DATABASE}._state`, lines);
	}
}
