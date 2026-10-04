// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {describe, expect, test} from 'vitest';

const SCHEMA_PATH = path.resolve(
	fileURLToPath(new URL('../../../../../tools/dev/cassandra_target_schema.json', import.meta.url)),
);

interface TargetColumn {
	name: string;
	type: string;
}

interface TargetTable {
	name: string;
	columns: Array<TargetColumn>;
	primary_key: string;
}

interface TargetSchema {
	tables: Array<TargetTable>;
}

describe('guild_events Cassandra target schema', () => {
	const schema = JSON.parse(readFileSync(SCHEMA_PATH, 'utf8')) as TargetSchema;
	const table = schema.tables.find((entry) => entry.name === 'guild_events');

	test('defines the guild_events partition used by GuildEventRepository', () => {
		expect(table).toBeDefined();
		expect(table?.primary_key).toBe('((guild_id), event_id)');
		expect(table?.columns.map((column) => [column.name, column.type])).toEqual([
			['guild_id', 'bigint'],
			['event_id', 'bigint'],
			['creator_id', 'bigint'],
			['name', 'text'],
			['description', 'text'],
			['location', 'text'],
			['starts_at', 'timestamp'],
			['ends_at', 'timestamp'],
			['image_hash', 'text'],
			['created_at', 'timestamp'],
			['version', 'int'],
		]);
	});
});
