// SPDX-License-Identifier: AGPL-3.0-or-later

import {MAX_GROUP_DM_RECIPIENTS} from '@fluxer/constants/src/LimitConstants';
import {
	ChannelOverwriteResponse,
	ChannelPartialResponse,
	ChannelResponse,
} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {describe, expect, it} from 'vitest';

describe('ChannelOverwriteResponse', () => {
	it('accepts valid channel overwrite', () => {
		const result = ChannelOverwriteResponse.safeParse({
			id: '123456789012345678',
			type: 0,
			allow: '8',
			deny: '0',
		});
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data.id).toBe('123456789012345678');
			expect(result.data.type).toBe(0);
		}
	});
	it('requires all fields', () => {
		const result = ChannelOverwriteResponse.safeParse({
			id: '123456789012345678',
			type: 0,
		});
		expect(result.success).toBe(false);
	});
	it('requires type to be integer', () => {
		const result = ChannelOverwriteResponse.safeParse({
			id: '123456789012345678',
			type: 'role',
			allow: '8',
			deny: '0',
		});
		expect(result.success).toBe(false);
	});
	it('accepts member type (1)', () => {
		const result = ChannelOverwriteResponse.safeParse({
			id: '123456789012345678',
			type: 1,
			allow: '8',
			deny: '0',
		});
		expect(result.success).toBe(true);
	});
});

describe('ChannelResponse', () => {
	const validChannel = {
		id: '123456789012345678',
		type: 0,
		guild_id: '987654321098765432',
		name: 'general',
		topic: 'General discussion',
		position: 0,
		nsfw: false,
		rate_limit_per_user: 0,
	};
	it('accepts valid channel response', () => {
		const result = ChannelResponse.safeParse(validChannel);
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data.id).toBe('123456789012345678');
			expect(result.data.name).toBe('general');
		}
	});
	it('requires id and type', () => {
		const {id, ...channelWithoutId} = validChannel;
		const result = ChannelResponse.safeParse(channelWithoutId);
		expect(result.success).toBe(false);
	});
	it('accepts DM channel without guild_id', () => {
		const dmChannel = {
			id: '123456789012345678',
			type: 1,
			recipients: [
				{
					id: '111111111111111111',
					username: 'testuser',
					discriminator: '0001',
					global_name: null,
					avatar: null,
					avatar_color: null,
					flags: 0,
				},
			],
		};
		const result = ChannelResponse.safeParse(dmChannel);
		expect(result.success).toBe(true);
	});
	it('accepts channel with permission_overwrites', () => {
		const channel = {
			...validChannel,
			permission_overwrites: [
				{id: '111111111111111111', type: 0, allow: '8', deny: '0'},
				{id: '222222222222222222', type: 1, allow: '0', deny: '2048'},
			],
		};
		const result = ChannelResponse.safeParse(channel);
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data.permission_overwrites).toHaveLength(2);
		}
	});
	it('accepts more permission_overwrites than the old response ceiling', () => {
		const channel = {
			...validChannel,
			permission_overwrites: Array.from({length: 501}, (_, index) => ({
				id: String(100000000000000000n + BigInt(index)),
				type: 0,
				allow: '8',
				deny: '0',
			})),
		};
		const result = ChannelResponse.safeParse(channel);
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data.permission_overwrites).toHaveLength(501);
		}
	});
	it('accepts voice channel properties', () => {
		const voiceChannel = {
			...validChannel,
			type: 2,
			bitrate: 64000,
			user_limit: 10,
			voice_connection_limit: 5,
			rtc_region: 'us-west',
		};
		const result = ChannelResponse.safeParse(voiceChannel);
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data.bitrate).toBe(64000);
			expect(result.data.user_limit).toBe(10);
			expect(result.data.voice_connection_limit).toBe(5);
		}
	});
	it('accepts null optional fields', () => {
		const channel = {
			...validChannel,
			topic: null,
			parent_id: null,
			last_message_id: null,
			last_pin_timestamp: null,
		};
		const result = ChannelResponse.safeParse(channel);
		expect(result.success).toBe(true);
	});
	it('accepts channel with nicks', () => {
		const channel = {
			id: '123456789012345678',
			type: 3,
			nicks: {
				'111111111111111111': 'Friend 1',
				'222222222222222222': 'Friend 2',
			},
		};
		const result = ChannelResponse.safeParse(channel);
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data.nicks).toBeDefined();
		}
	});
	it('accepts valid last_pin_timestamp', () => {
		const channel = {
			...validChannel,
			last_pin_timestamp: '2024-01-15T12:30:00.000Z',
		};
		const result = ChannelResponse.safeParse(channel);
		expect(result.success).toBe(true);
	});
	it('accepts valid url', () => {
		const channel = {
			...validChannel,
			url: 'https://example.com',
		};
		const result = ChannelResponse.safeParse(channel);
		expect(result.success).toBe(true);
	});
	it('requires type to be integer', () => {
		const channel = {...validChannel, type: 'text'};
		const result = ChannelResponse.safeParse(channel);
		expect(result.success).toBe(false);
	});
	it('requires position to be integer when present', () => {
		const channel = {...validChannel, position: 1.5};
		const result = ChannelResponse.safeParse(channel);
		expect(result.success).toBe(false);
	});
});

describe('ChannelPartialResponse', () => {
	it('accepts valid partial channel', () => {
		const result = ChannelPartialResponse.safeParse({
			id: '123456789012345678',
			name: 'general',
			type: 0,
		});
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data.id).toBe('123456789012345678');
			expect(result.data.name).toBe('general');
		}
	});
	it('requires id and type', () => {
		const result = ChannelPartialResponse.safeParse({
			name: 'general',
		});
		expect(result.success).toBe(false);
	});
	it('accepts null name', () => {
		const result = ChannelPartialResponse.safeParse({
			id: '123456789012345678',
			name: null,
			type: 1,
		});
		expect(result.success).toBe(true);
	});
	it('accepts DM channel with recipients', () => {
		const result = ChannelPartialResponse.safeParse({
			id: '123456789012345678',
			type: 1,
			recipients: [{username: 'testuser'}],
		});
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data.recipients).toHaveLength(1);
		}
	});
	it('accepts channel without name or recipients', () => {
		const result = ChannelPartialResponse.safeParse({
			id: '123456789012345678',
			type: 0,
		});
		expect(result.success).toBe(true);
	});
	it('accepts a full group DM with every recipient listed', () => {
		const result = ChannelPartialResponse.safeParse({
			id: '123456789012345678',
			type: 3,
			recipients: Array.from({length: MAX_GROUP_DM_RECIPIENTS}, (_, index) => ({username: `user${index}`})),
		});
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data.recipients).toHaveLength(MAX_GROUP_DM_RECIPIENTS);
		}
	});
	it('rejects more recipients than a group DM can hold', () => {
		const result = ChannelPartialResponse.safeParse({
			id: '123456789012345678',
			type: 3,
			recipients: Array.from({length: MAX_GROUP_DM_RECIPIENTS + 1}, (_, index) => ({username: `user${index}`})),
		});
		expect(result.success).toBe(false);
	});
});
