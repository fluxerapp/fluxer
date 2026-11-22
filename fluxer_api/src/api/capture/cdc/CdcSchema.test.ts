// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TableLayout} from '@app/api/capture/CaptureTables';
import {cdcTable, schemaPlan, tableDdl, viewDdl} from '@app/api/capture/cdc/CdcSchema';
import {describe, expect, it} from 'vitest';

const users: TableLayout = {
	name: 'users',
	primaryKey: ['user_id'],
	partitionKey: ['user_id'],
	regular: ['bot', 'password_hash', 'totp_secret', 'username'],
	multiCell: new Set(),
	defaultTtl: 0,
};

const userTypes = new Map([
	['user_id', 'bigint'],
	['bot', 'boolean'],
	['password_hash', 'text'],
	['totp_secret', 'text'],
	['username', 'text'],
]);

const privateChannels: TableLayout = {
	name: 'private_channels',
	primaryKey: ['user_id', 'channel_id'],
	partitionKey: ['user_id'],
	regular: ['is_gdm'],
	multiCell: new Set(),
	defaultTtl: 0,
};

const channelTypes = new Map([
	['user_id', 'bigint'],
	['channel_id', 'bigint'],
	['is_gdm', 'boolean'],
]);

describe('cdc tables', () => {
	it('leaves the dropped columns out of the columns and the read-back', () => {
		const table = cdcTable(users, userTypes);
		expect(table.columns.map((column) => column.name)).toEqual(['bot', 'user_id', 'username']);
		expect(table.layout.regular).toEqual(['bot', 'username']);
	});

	it('orders columns alphabetically and the table by Cassandra key order', () => {
		const table = cdcTable(privateChannels, channelTypes);
		expect(table.columns.map((column) => column.name)).toEqual(['channel_id', 'is_gdm', 'user_id']);
		expect(tableDdl(table)).toContain('ORDER BY (`user_id`, `channel_id`)');
		expect(tableDdl(table)).toContain('ENGINE = ReplacingMergeTree(_version, _deleted)');
		expect(viewDdl('fluxer_live', 'cdc', table)).toBe(
			'CREATE OR REPLACE VIEW `fluxer_live`.`private_channels` DEFINER = `cdc` SQL SECURITY DEFINER AS SELECT `channel_id`, `is_gdm`, `user_id` FROM fluxer_cdc.`private_channels` FINAL WHERE _deleted = 0 AND (_expires_at = 0 OR _expires_at > now64(3))',
		);
	});

	it('refuses a layout and type list that disagree', () => {
		expect(() => cdcTable(users, new Map([['user_id', 'bigint']]))).toThrow('changed while it was read');
	});
});

describe('schema plan', () => {
	const table = cdcTable(privateChannels, channelTypes);
	const base = {
		desired: new Map([['private_channels', table]]),
		views: new Map([['private_channels', ['channel_id', 'is_gdm', 'user_id']]]),
		viewDatabase: 'fluxer_live',
		definer: 'cdc',
		cassandraTables: new Set(['private_channels']),
		today: '20261121',
	};
	const existing = (columns: Array<[string, string]>) =>
		new Map([['private_channels', new Map([...columns, ['_version', 'UInt64']] as Array<[string, string]>)]]);

	it('creates a missing table and its view', () => {
		const plan = schemaPlan({...base, existing: new Map(), views: new Map()});
		expect(plan.created).toEqual(['private_channels']);
		expect(plan.statements).toHaveLength(2);
	});

	it('does nothing when ClickHouse already matches', () => {
		const plan = schemaPlan({
			...base,
			existing: existing([
				['channel_id', 'Int64'],
				['is_gdm', 'UInt8'],
				['user_id', 'Int64'],
			]),
		});
		expect(plan).toEqual({statements: [], created: [], widened: [], mismatched: []});
	});

	it('adds and drops columns, then recreates the view', () => {
		const plan = schemaPlan({
			...base,
			existing: existing([
				['channel_id', 'Int64'],
				['old', 'String'],
				['user_id', 'Int64'],
			]),
			views: new Map([['private_channels', ['channel_id', 'old', 'user_id']]]),
		});
		expect(plan.statements).toEqual([
			'ALTER TABLE fluxer_cdc.`private_channels` ADD COLUMN `is_gdm` UInt8',
			'ALTER TABLE fluxer_cdc.`private_channels` DROP COLUMN `old`',
			viewDdl('fluxer_live', 'cdc', table),
		]);
		expect(plan.widened).toEqual(['private_channels']);
	});

	it('marks a column type change as a mismatch and changes nothing', () => {
		const plan = schemaPlan({
			...base,
			existing: existing([
				['channel_id', 'String'],
				['is_gdm', 'UInt8'],
				['user_id', 'Int64'],
			]),
		});
		expect(plan).toEqual({statements: [], created: [], widened: [], mismatched: ['private_channels']});
	});

	it('keeps a table aside when Cassandra dropped it', () => {
		const plan = schemaPlan({
			...base,
			desired: new Map(),
			existing: existing([['user_id', 'Int64']]),
			cassandraTables: new Set(),
		});
		expect(plan.statements).toEqual([
			'DROP VIEW IF EXISTS `fluxer_live`.`private_channels`',
			'RENAME TABLE fluxer_cdc.`private_channels` TO fluxer_cdc.`private_channels__dropped_20261121`',
		]);
	});
});
