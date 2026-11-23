// SPDX-License-Identifier: AGPL-3.0-or-later

const REQUEST_TIMEOUT_MS = 120_000;

export interface ClickHouseOptions {
	url: string;
	user: string;
	password: string;
}

export interface QueryOptions {
	body?: string;
	settings?: Readonly<Record<string, string | number>>;
	params?: Readonly<Record<string, string>>;
	timeoutMs?: number;
}

export interface QuerySummary {
	writtenRows: number;
}

class ClickHouseError extends Error {
	override readonly name = 'ClickHouseError';
}

export function quoteIdentifier(name: string): string {
	return `\`${name.replaceAll('\\', '\\\\').replaceAll('`', '\\`')}\``;
}

function escapeParam(value: string): string {
	return value.replaceAll('\\', '\\\\').replaceAll('\t', '\\t').replaceAll('\n', '\\n');
}

export class ClickHouseHttp {
	private readonly options: ClickHouseOptions;

	constructor(options: ClickHouseOptions) {
		this.options = options;
	}

	async execute(sql: string, options: QueryOptions = {}): Promise<{text: string; summary: QuerySummary}> {
		const url = new URL(this.options.url);
		const query = options.body === undefined ? null : sql;
		if (query !== null) url.searchParams.set('query', query);
		for (const [key, value] of Object.entries(options.settings ?? {})) url.searchParams.set(key, String(value));
		for (const [key, value] of Object.entries(options.params ?? {})) {
			url.searchParams.set(`param_${key}`, escapeParam(value));
		}
		const response = await fetch(url, {
			method: 'POST',
			headers: {
				'X-ClickHouse-User': this.options.user,
				'X-ClickHouse-Key': this.options.password,
				'Content-Type': 'text/plain; charset=utf-8',
			},
			body: options.body ?? sql,
			signal: AbortSignal.timeout(options.timeoutMs ?? REQUEST_TIMEOUT_MS),
		});
		const text = await response.text();
		if (!response.ok) throw new ClickHouseError(`ClickHouse ${response.status}: ${text.trim().slice(0, 500)}`);
		const summary = JSON.parse(response.headers.get('X-ClickHouse-Summary') ?? '{}') as {written_rows?: string};
		return {text, summary: {writtenRows: Number(summary.written_rows ?? 0)}};
	}

	async command(sql: string, options: Omit<QueryOptions, 'body'> = {}): Promise<QuerySummary> {
		return (await this.execute(sql, options)).summary;
	}

	async select<T>(sql: string, options: Omit<QueryOptions, 'body'> = {}): Promise<Array<T>> {
		const {text} = await this.execute(`${sql} FORMAT JSONEachRow`, {
			...options,
			settings: {output_format_json_quote_64bit_integers: 1, ...options.settings},
		});
		return text
			.split('\n')
			.filter((line) => line !== '')
			.map((line) => JSON.parse(line) as T);
	}

	async insert(table: string, lines: ReadonlyArray<string>, deduplicationToken?: string): Promise<void> {
		if (lines.length === 0) return;
		await this.execute(`INSERT INTO ${table} FORMAT JSONEachRow`, {
			body: lines.join('\n'),
			settings: {
				async_insert: 0,
				...(deduplicationToken === undefined ? {} : {insert_deduplication_token: deduplicationToken}),
			},
		});
	}
}
