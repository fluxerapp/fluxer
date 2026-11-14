// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createMessageID, createUserID} from '@app/api/BrandedTypes';
import {
	channelRowFromPrivateChannelSnapshot,
	privateChannelMetadataPatch,
} from '@app/api/channel/PrivateChannelSnapshot';
import type {ChannelRow, PrivateChannelRow} from '@app/api/database/types/ChannelTypes';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, test} from 'vitest';

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
		rtc_p2p: null,
		last_message_id: LAST_MESSAGE,
		last_pin_timestamp: null,
		permission_overwrites: null,
		nicks: new Map([['2', 'Al']]),
		soft_deleted: false,
		indexed_at: null,
		version: 7,
		...overrides,
	};
}

function setValue<T>(op: unknown): T {
	const typed = op as {kind: string; value: T};
	expect(typed.kind).toBe('set');
	return typed.value;
}

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
});
