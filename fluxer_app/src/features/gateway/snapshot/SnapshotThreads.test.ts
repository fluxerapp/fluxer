// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReadyPayload} from '@app/features/gateway/events/GatewayReady';
import type {SnapshotRowOp} from '@app/features/gateway/snapshot/SnapshotEntities';
import {SnapshotReducer} from '@app/features/gateway/snapshot/SnapshotReducer';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, test} from 'vitest';

const SELF_ID = '900';

function guild(id: string, threads?: Array<Record<string, unknown>>): Record<string, unknown> {
	return {
		id,
		properties: {id, name: id},
		channels: [{id: `${id}-text`, type: ChannelTypes.GUILD_TEXT, last_message_id: null}],
		...(threads !== undefined ? {threads} : {}),
		emojis: [],
		members: [],
		roles: [],
		member_count: 1,
		joined_at: '2026-01-01T00:00:00.000Z',
	};
}

function thread(id: string, guildId: string, overrides: Record<string, unknown> = {}): Record<string, unknown> {
	return {
		id,
		guild_id: guildId,
		parent_id: `${guildId}-text`,
		type: ChannelTypes.PUBLIC_THREAD,
		last_message_id: null,
		...overrides,
	};
}

function ready(guilds: Array<Record<string, unknown>>): ReadyPayload {
	return {
		session_id: 'session',
		user: {id: SELF_ID, username: 'self', email: 'self@example.com'},
		guilds,
		private_channels: [],
		relationships: [],
		read_states: [],
		user_guild_settings: [],
	} as unknown as ReadyPayload;
}

class Rows {
	readonly channels = new Map<string, Record<string, unknown>>();
	readonly guilds = new Map<string, Record<string, unknown>>();

	emit = (op: SnapshotRowOp): void => {
		const target = op.entity === 'channel' ? this.channels : op.entity === 'guild' ? this.guilds : null;
		if (target === null) return;
		if (op.kind === 'replaceEntity') {
			target.clear();
			for (const entry of op.entries) target.set(entry.key, entry.value as unknown as Record<string, unknown>);
		} else if (op.kind === 'upsert') {
			target.set(op.key, op.value as unknown as Record<string, unknown>);
		} else if (op.kind === 'delete') {
			target.delete(op.key);
		}
	};
}

function setup(): {reducer: SnapshotReducer; rows: Rows} {
	const reducer = new SnapshotReducer();
	const rows = new Rows();
	reducer.applyReady(rows.emit, ready([guild('g1', [thread('t1', 'g1')]), guild('g2')]));
	return {reducer, rows};
}

describe('snapshot thread state', () => {
	test('READY keeps the active threads and marks only thread guilds active', () => {
		const {rows} = setup();
		expect(rows.channels.has('t1')).toBe(true);
		expect(rows.guilds.get('g1')?.threads_active).toBe(true);
		expect(rows.guilds.get('g2')?.threads_active).toBeUndefined();
	});

	test('thread events only land in guilds that have threads', () => {
		const {reducer, rows} = setup();
		reducer.applyDispatch(rows.emit, {type: 'THREAD_CREATE', data: thread('t2', 'g1') as never});
		reducer.applyDispatch(rows.emit, {type: 'THREAD_CREATE', data: thread('t3', 'g2') as never});
		expect(rows.channels.has('t2')).toBe(true);
		expect(rows.channels.has('t3')).toBe(false);
		reducer.applyDispatch(rows.emit, {type: 'THREAD_DELETE', data: {id: 't2', guild_id: 'g1'} as never});
		expect(rows.channels.has('t2')).toBe(false);
	});

	test('a message in a thread advances its last message', () => {
		const {reducer, rows} = setup();
		reducer.applyDispatch(rows.emit, {
			type: 'MESSAGE_CREATE',
			data: {id: '5000', channel_id: 't1', guild_id: 'g1', author: {id: '1'}} as never,
		});
		expect(rows.channels.get('t1')?.last_message_id).toBe('5000');
	});

	test('a list sync drops active threads the server no longer lists and keeps archived ones', () => {
		const {reducer, rows} = setup();
		reducer.applyDispatch(rows.emit, {
			type: 'THREAD_CREATE',
			data: thread('t4', 'g1', {thread_metadata: {archived: true}}) as never,
		});
		reducer.applyDispatch(rows.emit, {
			type: 'THREAD_LIST_SYNC',
			data: {guild_id: 'g1', threads: [thread('t5', 'g1')], members: []} as never,
		});
		expect(rows.channels.has('t1')).toBe(false);
		expect(rows.channels.has('t4')).toBe(true);
		expect(rows.channels.has('t5')).toBe(true);
	});

	test('deleting the parent channel removes its threads', () => {
		const {reducer, rows} = setup();
		reducer.applyDispatch(rows.emit, {type: 'CHANNEL_DELETE', data: {id: 'g1-text', type: 0, guild_id: 'g1'} as never});
		expect(rows.channels.has('t1')).toBe(false);
	});

	test('a guild that stops sending threads drops them', () => {
		const {reducer, rows} = setup();
		reducer.applyDispatch(rows.emit, {type: 'GUILD_CREATE', data: guild('g1') as never});
		expect(rows.channels.has('t1')).toBe(false);
		expect(rows.guilds.get('g1')?.threads_active).toBeUndefined();
	});
});
