// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, test} from 'vitest';
import {createChannelID, createMessageID, createUserID} from '../BrandedTypes';
import type {ChannelRow, PrivateChannelRow} from '../database/types/ChannelTypes';
import {
	channelRowFromPrivateChannelSnapshot,
	isPrivateChannelType,
	privateChannelFanOutTargets,
	privateChannelHydrationPatch,
	privateChannelLastMessageIdPatch,
	privateChannelMetadataPatch,
} from './PrivateChannelSnapshot';

const OWNER = createUserID(1n);
const MEMBER_A = createUserID(2n);
const MEMBER_B = createUserID(3n);
const CHANNEL = createChannelID(100n);
const LAST_MESSAGE = createMessageID(5000n);

function gdmRow(overrides: Partial<ChannelRow> = {}): ChannelRow {
	return {
		channel_id: CHANNEL,
		guild_id: null,
		type: ChannelTypes.GROUP_DM,
		name: 'Squad',
		topic: null,
		icon_hash: 'icon123',
		url: null,
		parent_id: null,
		position: null,
		owner_id: OWNER,
		recipient_ids: new Set([OWNER, MEMBER_A, MEMBER_B]),
		nsfw: null,
		content_warning_level: null,
		content_warning_text: null,
		rate_limit_per_user: 5,
		bitrate: null,
		user_limit: null,
		voice_connection_limit: null,
		rtc_region: null,
		last_message_id: LAST_MESSAGE,
		last_pin_timestamp: null,
		permission_overwrites: null,
		nicks: new Map([['2', 'Al']]),
		soft_deleted: false,
		indexed_at: null,
		version: 7,
		thread_metadata: null,
		message_count: null,
		total_message_sent: null,
		member_count: null,
		...overrides,
	};
}

function setValue<T>(op: unknown): T {
	const typed = op as {kind: string; value: T};
	expect(typed.kind).toBe('set');
	return typed.value;
}

describe('isPrivateChannelType', () => {
	test('matches DM, GROUP_DM, and personal notes only', () => {
		expect(isPrivateChannelType(ChannelTypes.DM)).toBe(true);
		expect(isPrivateChannelType(ChannelTypes.GROUP_DM)).toBe(true);
		expect(isPrivateChannelType(ChannelTypes.DM_PERSONAL_NOTES)).toBe(true);
		expect(isPrivateChannelType(ChannelTypes.GUILD_TEXT)).toBe(false);
	});
});

describe('privateChannelFanOutTargets', () => {
	test('returns recipients for a GDM', () => {
		expect(new Set(privateChannelFanOutTargets(gdmRow()))).toEqual(new Set([OWNER, MEMBER_A, MEMBER_B]));
	});

	test('includes the owner for personal-notes channels with no recipients', () => {
		const row = gdmRow({type: ChannelTypes.DM_PERSONAL_NOTES, recipient_ids: null, owner_id: OWNER});
		expect(privateChannelFanOutTargets(row)).toEqual([OWNER]);
	});

	test('returns nothing for guild channels', () => {
		expect(privateChannelFanOutTargets(gdmRow({type: ChannelTypes.GUILD_TEXT}))).toEqual([]);
	});
});

describe('privateChannelMetadataPatch', () => {
	test('writes stable metadata but never the volatile pointer or hydration marker', () => {
		const patch = privateChannelMetadataPatch(gdmRow());
		expect(setValue<boolean>(patch.is_gdm)).toBe(true);
		expect(setValue<number>(patch.channel_type)).toBe(ChannelTypes.GROUP_DM);
		expect(setValue<string | null>(patch.channel_name)).toBe('Squad');
		expect(setValue<number>(patch.channel_version)).toBe(7);
		expect(patch.channel_last_message_id).toBeUndefined();
		expect(patch.snapshot_at).toBeUndefined();
	});

	test('empty recipient set and nicks map collapse to null', () => {
		const patch = privateChannelMetadataPatch(gdmRow({recipient_ids: new Set(), nicks: new Map()}));
		expect(setValue<Set<unknown> | null>(patch.channel_recipient_ids)).toBeNull();
		expect(setValue<Map<unknown, unknown> | null>(patch.channel_nicks)).toBeNull();
	});

	test('missing version normalizes to zero', () => {
		const patch = privateChannelMetadataPatch(gdmRow({version: undefined as unknown as number}));
		expect(setValue<number>(patch.channel_version)).toBe(0);
	});
});

describe('privateChannelHydrationPatch', () => {
	test('adds the authoritative last message id and the snapshot marker', () => {
		const hydratedAt = new Date('2026-06-01T00:00:00.000Z');
		const patch = privateChannelHydrationPatch(gdmRow(), hydratedAt);
		expect(setValue<bigint | null>(patch.channel_last_message_id)).toBe(LAST_MESSAGE);
		expect(setValue<Date>(patch.snapshot_at)).toBe(hydratedAt);
		expect(setValue<number>(patch.channel_type)).toBe(ChannelTypes.GROUP_DM);
	});
});

describe('privateChannelLastMessageIdPatch', () => {
	test('only writes the last message id column', () => {
		const patch = privateChannelLastMessageIdPatch(LAST_MESSAGE);
		expect(Object.keys(patch)).toEqual(['channel_last_message_id']);
		expect(setValue<bigint | null>(patch.channel_last_message_id)).toBe(LAST_MESSAGE);
	});
});

describe('channelRowFromPrivateChannelSnapshot', () => {
	const baseRow: PrivateChannelRow = {
		user_id: MEMBER_A,
		channel_id: CHANNEL,
		is_gdm: true,
	};

	test('returns null for a cold (unhydrated) row', () => {
		expect(channelRowFromPrivateChannelSnapshot(baseRow)).toBeNull();
		expect(channelRowFromPrivateChannelSnapshot({...baseRow, channel_type: ChannelTypes.GROUP_DM})).toBeNull();
	});

	test('reconstructs a channel row from a hydrated snapshot', () => {
		const row = channelRowFromPrivateChannelSnapshot({
			...baseRow,
			channel_type: ChannelTypes.GROUP_DM,
			channel_name: 'Squad',
			channel_icon_hash: 'icon123',
			channel_owner_id: OWNER,
			channel_recipient_ids: new Set([OWNER, MEMBER_A, MEMBER_B]),
			channel_last_message_id: LAST_MESSAGE,
			channel_last_pin_timestamp: null,
			channel_nicks: new Map([['2', 'Al']]),
			channel_rate_limit_per_user: 5,
			channel_nsfw: null,
			channel_version: 7,
			snapshot_at: new Date('2026-06-01T00:00:00.000Z'),
		});
		expect(row).not.toBeNull();
		expect(row?.channel_id).toBe(CHANNEL);
		expect(row?.type).toBe(ChannelTypes.GROUP_DM);
		expect(row?.name).toBe('Squad');
		expect(row?.icon_hash).toBe('icon123');
		expect(row?.owner_id).toBe(OWNER);
		expect(row?.recipient_ids).toEqual(new Set([OWNER, MEMBER_A, MEMBER_B]));
		expect(row?.last_message_id).toBe(LAST_MESSAGE);
		expect(row?.rate_limit_per_user).toBe(5);
		expect(row?.version).toBe(7);
		expect(row?.soft_deleted).toBe(false);
		expect(row?.guild_id).toBeNull();
	});
});
