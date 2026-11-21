// SPDX-License-Identifier: AGPL-3.0-or-later

import {isCollectionType} from '@app/api/capture/cdc/CdcRows';
import {type CdcTable, cdcTableName} from '@app/api/capture/cdc/CdcSchema';
import {canonicalStoredValue} from '@app/api/capture/cdc/CdcStoredValues';
import {type ClickHouseHttp, quoteIdentifier} from '@app/api/capture/cdc/ClickHouseHttp';
import {CDC_DROPPED_COLUMNS} from '@app/api/database/capture/CdcRecords';

const COUNT_TOLERANCE = 0.001;
const EXAMPLES = 5;

const USER_COUNTER_SQL =
	"SELECT count() AS n FROM {db}.users WHERE bot = 0 AND system = 0 AND user_id > 0 AND username != 'DeletedUser' AND deletion_reason_code <= 1";

export interface CompareColumns {
	hashed: Array<string>;
	collections: Array<string>;
}

export interface TableComparison {
	table: string;
	sync_count: number;
	cdc_count: number;
	count_ok: boolean;
	sampled: number;
	different: number;
	collection_different: number;
	examples: Array<string>;
}

export function compareColumns(
	table: string,
	syncColumns: ReadonlyArray<string>,
	cdcColumns: ReadonlyArray<string>,
	cqlTypes: ReadonlyMap<string, string>,
): CompareColumns {
	const dropped = new Set(CDC_DROPPED_COLUMNS[table] ?? []);
	const cdc = new Set(cdcColumns);
	const shared = [...new Set(syncColumns)].filter((column) => cdc.has(column) && !dropped.has(column)).sort();
	const isCollection = (column: string) => isCollectionType(cqlTypes.get(column) ?? 'text');
	return {
		hashed: shared.filter((column) => !isCollection(column)),
		collections: shared.filter(isCollection),
	};
}

export function countsAgree(a: number, b: number): boolean {
	return Math.abs(a - b) <= COUNT_TOLERANCE * Math.max(a, b);
}

function list(columns: ReadonlyArray<string>): string {
	return columns.map(quoteIdentifier).join(', ');
}

export class Comparer {
	private readonly clickhouse: ClickHouseHttp;
	private readonly syncDatabase: string;
	private readonly cdcDatabase: string;
	private readonly sinceUs: number;
	private readonly sample: number;
	private readonly seed: number;

	constructor(options: {
		clickhouse: ClickHouseHttp;
		syncDatabase: string;
		cdcDatabase: string;
		sinceUs: number;
		sample: number;
	}) {
		this.clickhouse = options.clickhouse;
		this.syncDatabase = options.syncDatabase;
		this.cdcDatabase = options.cdcDatabase;
		this.sinceUs = options.sinceUs;
		this.sample = options.sample;
		this.seed = Date.now();
	}

	async columns(database: string): Promise<Map<string, Array<string>>> {
		const rows = await this.clickhouse.select<{table: string; name: string}>(
			'SELECT table, name FROM system.columns WHERE database = {db:String} ORDER BY table, position',
			{params: {db: database}},
		);
		const columns = new Map<string, Array<string>>();
		for (const row of rows) columns.set(row.table, [...(columns.get(row.table) ?? []), row.name]);
		return columns;
	}

	async userCounter(database: string): Promise<number> {
		const [row] = await this.clickhouse.select<{n: string}>(
			USER_COUNTER_SQL.replace('{db}', quoteIdentifier(database)),
		);
		return Number(row?.n ?? 0);
	}

	async table(table: CdcTable, columns: CompareColumns): Promise<TableComparison> {
		const name = table.layout.name;
		const sync = `${quoteIdentifier(this.syncDatabase)}.${quoteIdentifier(name)}`;
		const cdc = `${quoteIdentifier(this.cdcDatabase)}.${quoteIdentifier(name)}`;
		const [syncCount, cdcCount] = await Promise.all([this.count(sync), this.count(cdc)]);
		const keys = list(table.keyColumns.map((column) => column.name));
		const sampled = [
			`SELECT ${keys} FROM ${cdcTableName(name)} FINAL`,
			`WHERE _deleted = 0 AND (_expires_at = 0 OR _expires_at > now64(3)) AND _version < ${this.sinceUs}`,
			`ORDER BY cityHash64(tuple(${keys}), ${this.seed}) LIMIT ${this.sample}`,
		].join(' ');
		const side = (source: string) =>
			`SELECT ${keys}, cityHash64(tuple(${list(columns.hashed)})) AS h FROM ${source} WHERE (${keys}) IN (${sampled})`;
		const differs = 's.h IS NULL OR s.h != c.h';
		const [hashes] = await this.clickhouse.select<{sampled: string; different: string; examples: Array<string>}>(
			[
				`SELECT count() AS sampled, countIf(${differs}) AS different,`,
				`groupArrayIf(${EXAMPLES})(toString(tuple(${keys})), ${differs}) AS examples`,
				`FROM (${side(cdc)}) AS c LEFT JOIN (${side(sync)}) AS s USING (${keys})`,
			].join(' '),
			{settings: {join_use_nulls: 1}},
		);
		const collections = await this.collections(table, columns.collections, sync, cdc, sampled);
		return {
			table: name,
			sync_count: syncCount,
			cdc_count: cdcCount,
			count_ok: countsAgree(syncCount, cdcCount),
			sampled: Number(hashes?.sampled ?? 0),
			different: Number(hashes?.different ?? 0),
			collection_different: collections.different,
			examples: [...(hashes?.examples ?? []), ...collections.examples].slice(0, EXAMPLES),
		};
	}

	private async count(source: string): Promise<number> {
		const [row] = await this.clickhouse.select<{n: string}>(`SELECT count() AS n FROM ${source}`);
		return Number(row?.n ?? 0);
	}

	private async collections(
		table: CdcTable,
		columns: ReadonlyArray<string>,
		sync: string,
		cdc: string,
		sampled: string,
	): Promise<{different: number; examples: Array<string>}> {
		if (columns.length === 0) return {different: 0, examples: []};
		const keys = table.keyColumns.map((column) => column.name);
		const read = async (source: string) => {
			const rows = await this.clickhouse.select<Record<string, unknown>>(
				`SELECT toString(tuple(${list(keys)})) AS _key, ${list(columns)} FROM ${source} WHERE (${list(keys)}) IN (${sampled})`,
			);
			return new Map(rows.map((row) => [String(row['_key']), row]));
		};
		const [syncRows, cdcRows] = await Promise.all([read(sync), read(cdc)]);
		let different = 0;
		const examples: Array<string> = [];
		for (const [key, row] of cdcRows) {
			const other = syncRows.get(key);
			for (const column of columns) {
				const same =
					other !== undefined &&
					canonicalStoredValue(String(row[column] ?? '')) === canonicalStoredValue(String(other[column] ?? ''));
				if (same) continue;
				different++;
				if (examples.length < EXAMPLES) examples.push(`${key} ${column}`);
			}
		}
		return {different, examples};
	}
}
