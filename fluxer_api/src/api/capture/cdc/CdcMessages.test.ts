// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RowState, TableLayout} from '@app/api/capture/CaptureTables';
import {linesForKey, rowLineFor} from '@app/api/capture/cdc/CdcApply';
import {RowEncoder} from '@app/api/capture/cdc/CdcRows';
import {cdcTable, schemaPlan, tableDdl} from '@app/api/capture/cdc/CdcSchema';
import {contentSignals, normaliseContent, simhash} from '@app/api/capture/cdc/MessageSignals';
import {FLUXER_EPOCH} from '@fluxer/constants/src/Core';
import cassandra from 'cassandra-driver';
import {describe, expect, it} from 'vitest';

const encoder = new RowEncoder(cassandra.types);
const NOW_MS = Date.UTC(2026, 10, 24, 12);
const DAY_MS = 86_400_000;

const snowflake = (ms: number) => BigInt(ms - FLUXER_EPOCH) << 22n;

const messagesLayout: TableLayout = {
	name: 'messages',
	primaryKey: ['channel_id', 'bucket', 'message_id'],
	partitionKey: ['channel_id', 'bucket'],
	regular: [
		'attachments',
		'author_id',
		'call',
		'content',
		'edited_timestamp',
		'embeds',
		'flags',
		'has_reaction',
		'mention_channels',
		'mention_everyone',
		'mention_roles',
		'mention_users',
		'message_reference',
		'message_snapshots',
		'nsfw_emojis',
		'pinned_timestamp',
		'sticker_items',
		'type',
		'version',
		'webhook_avatar_hash',
		'webhook_id',
		'webhook_name',
	],
	multiCell: new Set(['mention_channels', 'mention_roles', 'mention_users', 'nsfw_emojis']),
	defaultTtl: 0,
};

const messageTypes = new Map([
	['channel_id', 'bigint'],
	['bucket', 'int'],
	['message_id', 'bigint'],
	['attachments', 'frozen<list<frozen<message_attachment>>>'],
	['author_id', 'bigint'],
	['call', 'frozen<message_call>'],
	['content', 'text'],
	['edited_timestamp', 'timestamp'],
	['embeds', 'frozen<list<frozen<message_embed>>>'],
	['flags', 'int'],
	['has_reaction', 'boolean'],
	['mention_channels', 'set<bigint>'],
	['mention_everyone', 'boolean'],
	['mention_roles', 'set<bigint>'],
	['mention_users', 'set<bigint>'],
	['message_reference', 'frozen<message_reference>'],
	['message_snapshots', 'frozen<list<frozen<message_snapshot>>>'],
	['nsfw_emojis', 'set<bigint>'],
	['pinned_timestamp', 'timestamp'],
	['sticker_items', 'frozen<list<frozen<message_sticker_item>>>'],
	['type', 'int'],
	['version', 'int'],
	['webhook_avatar_hash', 'text'],
	['webhook_id', 'bigint'],
	['webhook_name', 'text'],
]);

const reactionsLayout: TableLayout = {
	name: 'message_reactions',
	primaryKey: ['channel_id', 'bucket', 'message_id', 'emoji_id', 'emoji_name', 'user_id'],
	partitionKey: ['channel_id', 'bucket'],
	regular: ['created_at', 'emoji_animated'],
	multiCell: new Set(),
	defaultTtl: 0,
};

const reactionTypes = new Map([
	['channel_id', 'bigint'],
	['bucket', 'int'],
	['message_id', 'bigint'],
	['emoji_id', 'bigint'],
	['emoji_name', 'text'],
	['user_id', 'bigint'],
	['created_at', 'timestamp'],
	['emoji_animated', 'boolean'],
]);

const messages = cdcTable(messagesLayout, messageTypes);
const reactions = cdcTable(reactionsLayout, reactionTypes);

const SECRET_WORDS = [
	'hunter2',
	'holiday.png',
	'a beach at dusk',
	'Embed Title',
	'forwarded words',
	'Spam Bot',
	'pepe',
];

const cell = (v: unknown) => ({v, wt: 1_000, ttl: null});

function messageRow(messageId: bigint, cells: Record<string, unknown>): RowState {
	return {
		key: {channel_id: 10n, bucket: 2000, message_id: messageId},
		cells: Object.fromEntries(Object.entries(cells).map(([name, value]) => [name, cell(value)])),
	};
}

const fullRow = messageRow(snowflake(NOW_MS - DAY_MS), {
	author_id: 7n,
	content: 'hunter2 see https://Example.com/path?q=1). and fluxer.gg/abcd plus https://fluxer.app/invite/Zz-9',
	attachments: [
		{
			attachment_id: 1n,
			filename: 'holiday.png',
			description: 'a beach at dusk',
			size: 2048n,
			content_type: 'image/png',
			content_hash: 'deadbeef',
		},
	],
	embeds: [{title: 'Embed Title', description: 'hunter2'}],
	message_snapshots: [{content: 'forwarded words'}],
	sticker_items: [{sticker_id: 55n, name: 'pepe'}],
	mention_users: new Set([3n, 4n]),
	mention_roles: new Set([5n]),
	message_reference: {channel_id: 10n, message_id: 99n, guild_id: 1n, type: 0},
	webhook_name: 'Spam Bot',
	nsfw_emojis: new Set([8n]),
	flags: 4,
	type: 0,
});

function lineOf(row: RowState): Record<string, unknown> {
	const line = rowLineFor(encoder, messages, row, 5_000, {expiresAtMs: 0, rereadAtMs: 0}, NOW_MS);
	if (line === null) throw new Error('expected a line');
	return JSON.parse(line) as Record<string, unknown>;
}

describe('message metadata', () => {
	it('reads the text columns to derive signals but never emits them', () => {
		const names = messages.columns.map((column) => column.name);
		for (const name of [
			'attachments',
			'call',
			'content',
			'embeds',
			'message_snapshots',
			'nsfw_emojis',
			'sticker_items',
			'webhook_name',
		]) {
			expect(names).not.toContain(name);
		}
		expect(messages.layout.regular).toContain('content');
		expect(names).toContain('content_simhash');
		expect(names).toContain('message_created_at');
	});

	it('emits no message text, filename, alt text, embed or forwarded text', () => {
		const line = rowLineFor(encoder, messages, fullRow, 5_000, {expiresAtMs: 0, rereadAtMs: 0}, NOW_MS) ?? '';
		for (const word of SECRET_WORDS) expect(line).not.toContain(word);
		expect(line).not.toContain('message_created_at');
	});

	it('derives counts, ids, hashes and links', () => {
		const line = lineOf(fullRow);
		expect(line).toMatchObject({
			author_id: 7,
			attachment_bytes: 2048,
			attachment_content_types: ['image/png'],
			attachment_count: 1,
			attachment_hashes: ['deadbeef'],
			embed_count: 1,
			flags: 4,
			invite_codes: ['fluxer.app/Zz-9', 'fluxer.gg/abcd'],
			link_domains: ['example.com', 'fluxer.app', 'fluxer.gg'],
			link_urls: ['https://example.com/path?q=1', 'https://fluxer.app/invite/Zz-9'],
			mention_channel_count: 0,
			mention_role_count: 1,
			mention_user_ids: [3, 4],
			reference_channel_id: 10,
			reference_message_id: 99,
			snapshot_count: 1,
			sticker_ids: [55],
		});
		expect(line['content_length']).toBe(97);
		expect(typeof line['content_hash']).toBe('number');
	});

	it('writes a tombstone with the key when a message is deleted', () => {
		const key = {channel_id: 10n, bucket: 2000, message_id: snowflake(NOW_MS - DAY_MS)};
		const {lines, tombstones} = linesForKey({
			encoder,
			table: messages,
			scope: 'row',
			ts: 9_000_000,
			key,
			rows: [],
			live: [],
			readStartMs: NOW_MS,
		});
		expect(tombstones).toBe(1);
		expect(JSON.parse(lines[0])).toEqual({
			channel_id: 10,
			bucket: 2000,
			message_id: Number(key.message_id),
			_version: NOW_MS * 1000,
			_deleted: 1,
		});
	});

	it('treats a row without an author or webhook as deleted', () => {
		const orphan = messageRow(snowflake(NOW_MS - DAY_MS), {has_reaction: true});
		const {lines, tombstones} = linesForKey({
			encoder,
			table: messages,
			scope: 'row',
			ts: 1,
			key: orphan.key,
			rows: [orphan],
			live: [],
			readStartMs: NOW_MS,
		});
		expect(tombstones).toBe(1);
		expect(JSON.parse(lines[0])).toMatchObject({_deleted: 1});
	});

	it('writes nothing for a message older than the retention window', () => {
		const old = messageRow(snowflake(NOW_MS - 91 * DAY_MS), {author_id: 7n, content: 'old'});
		expect(rowLineFor(encoder, messages, old, 1, {expiresAtMs: 0, rereadAtMs: 0}, NOW_MS)).toBeNull();
		const {lines, tombstones} = linesForKey({
			encoder,
			table: messages,
			scope: 'row',
			ts: 1,
			key: old.key,
			rows: [],
			live: [],
			readStartMs: NOW_MS,
		});
		expect(lines).toEqual([]);
		expect(tombstones).toBe(0);
	});

	it('partitions by month, expires after 90 days and fills the creation time from the id', () => {
		const ddl = tableDdl(messages);
		expect(ddl).toContain(
			"`message_created_at` DateTime64(3, 'UTC') MATERIALIZED fromUnixTimestamp64Milli(toInt64(bitShiftRight(`message_id`, 22) + 1420070400000), 'UTC')",
		);
		expect(ddl).toContain(
			'PARTITION BY toYYYYMM(`message_created_at`) ORDER BY (`channel_id`, `bucket`, `message_id`)',
		);
		expect(ddl).toContain('TTL `message_created_at` + INTERVAL 90 DAY SETTINGS');
		expect(ddl).toContain('INDEX `author_id_index` `author_id` TYPE bloom_filter GRANULARITY 4');
	});

	it('adds a missing materialized column with its expression', () => {
		const existing = new Map(
			messages.columns.filter((column) => column.name !== 'message_created_at').map((c) => [c.name, c.chType]),
		);
		const plan = schemaPlan({
			desired: new Map([['messages', messages]]),
			existing: new Map([['messages', existing]]),
			views: new Map([['messages', messages.columns.map((column) => column.name)]]),
			viewDatabase: 'fluxer_live',
			definer: 'cdc',
			cassandraTables: new Set(['messages']),
			today: '20261124',
		});
		expect(plan.statements[0]).toMatch(
			/^ALTER TABLE fluxer_cdc\.`messages` ADD COLUMN `message_created_at` DateTime64\(3, 'UTC'\) MATERIALIZED /,
		);
		expect(plan.mismatched).toEqual([]);
	});
});

describe('message reactions', () => {
	const reaction = (user: bigint): RowState => ({
		key: {
			channel_id: 10n,
			bucket: 2000,
			message_id: snowflake(NOW_MS - DAY_MS),
			emoji_id: 0n,
			emoji_name: '👍',
			user_id: user,
		},
		cells: {created_at: cell(new Date(NOW_MS)), emoji_animated: cell(false)},
	});

	it('keeps every reaction column and tombstones the reactions Cassandra no longer has', () => {
		expect(reactions.columns.map((column) => column.name)).toEqual([
			'bucket',
			'channel_id',
			'created_at',
			'emoji_animated',
			'emoji_id',
			'emoji_name',
			'message_created_at',
			'message_id',
			'user_id',
		]);
		const {lines, tombstones} = linesForKey({
			encoder,
			table: reactions,
			scope: 'partition',
			ts: 0,
			key: {channel_id: 10n, bucket: 2000},
			rows: [reaction(1n)],
			live: [reaction(1n).key, reaction(2n).key],
			readStartMs: NOW_MS,
		});
		expect(tombstones).toBe(1);
		expect(JSON.parse(lines[0])).toMatchObject({emoji_name: '👍', user_id: 1, emoji_animated: 0, _deleted: 0});
		expect(JSON.parse(lines[1])).toMatchObject({user_id: 2, _deleted: 1});
	});
});

describe('content fingerprint', () => {
	it('is stable across releases', () => {
		const signals = contentSignals('Free nitro at https://scam.example/gift');
		expect(signals.hash).toBe(0x3f95025804ddbfadn);
		expect(signals.simhash).toBe(0x28672c316585ddd3n);
	});

	it('ignores case, spacing and invisible characters', () => {
		const a = contentSignals('Free  NITRO​ here');
		const b = contentSignals('free nitro here');
		expect(a.hash).toBe(b.hash);
		expect(a.simhash).toBe(b.simhash);
		expect(normaliseContent('Ｆｒｅｅ nitro ')).toBe('free nitro');
	});

	it('keeps near duplicates close and different text far apart', () => {
		const base = 'join my server for free robux and nitro giveaway today only hurry up';
		const distance = (a: bigint, b: bigint) => (a ^ b).toString(2).replaceAll('0', '').length;
		const near = distance(simhash(base), simhash(`${base} now`));
		const far = distance(simhash(base), simhash('what time is the meeting tomorrow, I forgot the agenda'));
		expect(near).toBeLessThan(12);
		expect(far).toBeGreaterThan(near * 2);
	});

	it('is empty for a message without text', () => {
		expect(contentSignals(null)).toEqual({
			length: 0,
			hash: 0n,
			simhash: 0n,
			linkDomains: [],
			linkUrls: [],
			inviteCodes: [],
		});
	});

	it('caps links and ignores unparseable ones', () => {
		const many = Array.from({length: 40}, (_, index) => `https://site${index}.example/`).join(' ');
		expect(contentSignals(many).linkUrls).toHaveLength(16);
		expect(contentSignals('http://[bad').linkUrls).toEqual([]);
		expect(contentSignals('<https://a.example/x>').linkUrls).toEqual(['https://a.example/x']);
	});
});
