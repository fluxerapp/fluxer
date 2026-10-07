// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import type {IARMessageContextRow} from '@app/api/database/types/ReportTypes';
import {
	DSAReportEmailVerifications,
	DSAReportTickets,
	GuildReportSubmissionsByReporter,
	IARSubmissions,
	MessageReportSubmissionsByReporter,
	UserReportSubmissionsByReporter,
} from '@app/api/Tables';
import {describe, expect, it} from 'vitest';

const THIS_DIR = path.dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = path.resolve(THIS_DIR, '../../../..');

interface SchemaField {
	name: string;
	type: string;
}

const SCHEMA = JSON.parse(fs.readFileSync(path.join(REPO_ROOT, 'tools/dev/cassandra_target_schema.json'), 'utf8')) as {
	user_types: Array<{name: string; fields: Array<SchemaField>}>;
	tables: Array<{name: string; columns: Array<SchemaField>}>;
};

const IAR_MESSAGE_CONTEXT_FIELDS = {
	message_id: 'bigint',
	channel_id: 'bigint',
	author_id: 'bigint',
	webhook_id: 'bigint',
	author_username: 'text',
	author_discriminator: 'int',
	author_avatar_hash: 'text',
	content: 'text',
	timestamp: 'timestamp',
	edited_timestamp: 'timestamp',
	type: 'int',
	flags: 'int',
	mention_everyone: 'boolean',
	mention_users: 'set<bigint>',
	mention_roles: 'set<bigint>',
	mention_channels: 'set<bigint>',
	attachments: 'frozen<list<message_attachment>>',
	embeds: 'frozen<list<message_embed>>',
	sticker_items: 'frozen<list<message_sticker_item>>',
	missing_attachments: 'frozen<list<message_attachment>>',
} satisfies Record<keyof IARMessageContextRow, string>;

const REPORT_TABLES = [
	IARSubmissions,
	MessageReportSubmissionsByReporter,
	UserReportSubmissionsByReporter,
	GuildReportSubmissionsByReporter,
	DSAReportEmailVerifications,
	DSAReportTickets,
];

function sortedFields(fields: Iterable<[string, string]>): Array<[string, string]> {
	return [...fields].sort(([left], [right]) => left.localeCompare(right));
}

describe('Cassandra UDT parity', () => {
	it('stores every iar_message_context field the API writes, with the same type', () => {
		const userType = SCHEMA.user_types.find((entry) => entry.name === 'iar_message_context');
		expect(userType).toBeDefined();
		expect(sortedFields(userType!.fields.map((field) => [field.name, field.type]))).toEqual(
			sortedFields(Object.entries(IAR_MESSAGE_CONTEXT_FIELDS)),
		);
	});

	it.each(REPORT_TABLES.map((table) => [table.name, table] as const))(
		'%s has the same columns as the target schema',
		(name, table) => {
			const schemaTable = SCHEMA.tables.find((entry) => entry.name === name);
			expect(schemaTable).toBeDefined();
			expect(schemaTable!.columns.map((column) => column.name).sort()).toEqual([...table.columns].sort());
		},
	);
});
