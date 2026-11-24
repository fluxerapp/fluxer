// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RowState} from '@app/api/capture/CaptureTables';
import type {CdcColumn, JsonValue} from '@app/api/capture/cdc/CdcRows';
import {contentSignals} from '@app/api/capture/cdc/MessageSignals';
import {FLUXER_EPOCH} from '@fluxer/constants/src/Core';

const MESSAGE_RETENTION_DAYS = 90;
const RETENTION_MS = MESSAGE_RETENTION_DAYS * 86_400_000;
const TIMESTAMP_SHIFT = 22n;

export interface DerivedSpec {
	columns: ReadonlyArray<CdcColumn>;
	retained: (key: Readonly<Record<string, unknown>>, nowMs: number) => boolean;
	values: (row: RowState) => Record<string, JsonValue> | null;
	partitionBy: string;
	ttl: string;
	indexes: ReadonlyArray<string>;
}

const derived = (name: string, chType: string): CdcColumn => ({name, cqlType: '', chType});

const CREATED_AT: CdcColumn = {
	name: 'message_created_at',
	cqlType: '',
	chType: "DateTime64(3, 'UTC')",
	materialized: `fromUnixTimestamp64Milli(toInt64(bitShiftRight(\`message_id\`, ${TIMESTAMP_SHIFT}) + ${FLUXER_EPOCH}), 'UTC')`,
};

function createdAtMs(key: Readonly<Record<string, unknown>>): number {
	const id = key['message_id'];
	if (typeof id !== 'bigint' && typeof id !== 'number' && typeof id !== 'string') return 0;
	return Number(BigInt(id) >> TIMESTAMP_SHIFT) + FLUXER_EPOCH;
}

function retained(key: Readonly<Record<string, unknown>>, nowMs: number): boolean {
	return nowMs - createdAtMs(key) <= RETENTION_MS;
}

function integer(value: unknown): number {
	if (typeof value === 'number') return value;
	if (typeof value === 'bigint') return Number(value);
	if (value === null || value === undefined) return 0;
	const parsed = Number(String(value));
	return Number.isFinite(parsed) ? parsed : 0;
}

function id(value: unknown): bigint | null {
	if (typeof value === 'bigint') return value;
	if (typeof value === 'number' && Number.isInteger(value)) return BigInt(value);
	if (value === null || value === undefined) return null;
	try {
		return BigInt(String(value));
	} catch {
		return null;
	}
}

function entries(value: unknown): Array<unknown> {
	if (Array.isArray(value)) return value;
	if (value instanceof Set) return [...value];
	return [];
}

function field(value: unknown, name: string): unknown {
	return value !== null && typeof value === 'object' ? (value as Record<string, unknown>)[name] : undefined;
}

function ids(values: Array<unknown>): Array<bigint> {
	return values.map(id).filter((entry): entry is bigint => entry !== null);
}

function texts(values: Array<unknown>): Array<string> {
	return [...new Set(values.filter((entry): entry is string => typeof entry === 'string' && entry.length > 0))];
}

function messageValues(row: RowState): Record<string, JsonValue> | null {
	const cell = (name: string) => row.cells[name]?.v;
	if (cell('author_id') === undefined && cell('webhook_id') === undefined) return null;
	const content = cell('content');
	const signals = contentSignals(typeof content === 'string' ? content : null);
	const attachments = entries(cell('attachments'));
	const reference = cell('message_reference');
	return {
		attachment_bytes: attachments.reduce<number>((sum, attachment) => sum + integer(field(attachment, 'size')), 0),
		attachment_content_types: texts(attachments.map((attachment) => field(attachment, 'content_type'))),
		attachment_count: attachments.length,
		attachment_hashes: texts(attachments.map((attachment) => field(attachment, 'content_hash'))),
		content_hash: signals.hash,
		content_length: signals.length,
		content_simhash: signals.simhash,
		embed_count: entries(cell('embeds')).length,
		invite_codes: signals.inviteCodes,
		link_domains: signals.linkDomains,
		link_urls: signals.linkUrls,
		mention_channel_count: entries(cell('mention_channels')).length,
		mention_role_count: entries(cell('mention_roles')).length,
		mention_user_ids: ids(entries(cell('mention_users'))),
		reference_channel_id: id(field(reference, 'channel_id')) ?? 0n,
		reference_message_id: id(field(reference, 'message_id')) ?? 0n,
		snapshot_count: entries(cell('message_snapshots')).length,
		sticker_ids: ids(entries(cell('sticker_items')).map((sticker) => field(sticker, 'sticker_id'))),
	};
}

const MESSAGE_TTL = `\`message_created_at\` + INTERVAL ${MESSAGE_RETENTION_DAYS} DAY`;
const MONTHLY = 'toYYYYMM(`message_created_at`)';

export const CDC_DERIVED: Readonly<Record<string, DerivedSpec>> = {
	message_reactions: {
		columns: [CREATED_AT],
		retained,
		values: () => ({}),
		partitionBy: MONTHLY,
		ttl: MESSAGE_TTL,
		indexes: ['INDEX `user_id_index` `user_id` TYPE bloom_filter GRANULARITY 4'],
	},
	messages: {
		columns: [
			derived('attachment_bytes', 'UInt64'),
			derived('attachment_content_types', 'Array(LowCardinality(String))'),
			derived('attachment_count', 'UInt16'),
			derived('attachment_hashes', 'Array(String)'),
			derived('content_hash', 'UInt64'),
			derived('content_length', 'UInt32'),
			derived('content_simhash', 'UInt64'),
			derived('embed_count', 'UInt16'),
			derived('invite_codes', 'Array(String)'),
			derived('link_domains', 'Array(LowCardinality(String))'),
			derived('link_urls', 'Array(String)'),
			derived('mention_channel_count', 'UInt16'),
			derived('mention_role_count', 'UInt16'),
			derived('mention_user_ids', 'Array(Int64)'),
			CREATED_AT,
			derived('reference_channel_id', 'Int64'),
			derived('reference_message_id', 'Int64'),
			derived('snapshot_count', 'UInt16'),
			derived('sticker_ids', 'Array(Int64)'),
		],
		retained,
		values: messageValues,
		partitionBy: MONTHLY,
		ttl: MESSAGE_TTL,
		indexes: [
			'INDEX `author_id_index` `author_id` TYPE bloom_filter GRANULARITY 4',
			'INDEX `content_hash_index` `content_hash` TYPE bloom_filter GRANULARITY 4',
			'INDEX `link_domains_index` `link_domains` TYPE bloom_filter GRANULARITY 4',
			'INDEX `invite_codes_index` `invite_codes` TYPE bloom_filter GRANULARITY 4',
			'INDEX `attachment_hashes_index` `attachment_hashes` TYPE bloom_filter GRANULARITY 4',
		],
	},
};
