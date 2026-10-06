// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID} from '@app/api/BrandedTypes';
import {GatewayRpcClient} from '@app/api/infrastructure/GatewayRpcClient';
import {GatewayService} from '@app/api/infrastructure/GatewayService';
import type {IGatewayRpcTransport} from '@app/api/infrastructure/IGatewayRpcTransport';
import {afterEach, describe, expect, it} from 'vitest';

const CHANNEL_ID = createChannelID(12n);

interface RecordedCall {
	method: string;
	params: Record<string, unknown>;
}

function recordingService(recorded: Array<RecordedCall>): GatewayService {
	const transport: IGatewayRpcTransport = {
		async call(method: string, params: Record<string, unknown>): Promise<unknown> {
			recorded.push({method, params});
			return null;
		},
		async destroy(): Promise<void> {},
	};
	GatewayRpcClient.createForTests(transport);
	return new GatewayService();
}

describe('call rpc caller params', () => {
	afterEach(async () => {
		await GatewayRpcClient.resetForTests();
	});

	it('sends the caller to call.create as caller_id, caller_name and caller_avatar', async () => {
		const recorded: Array<RecordedCall> = [];
		const service = recordingService(recorded);
		await service.createCall(CHANNEL_ID, '99', 'automatic', ['2'], ['1', '2'], {
			id: '1',
			name: 'Elias',
			avatar: 'a1b2c3d4',
		});
		expect(recorded).toHaveLength(1);
		expect(recorded[0].method).toBe('call.create');
		expect(recorded[0].params.caller_id).toBe('1');
		expect(recorded[0].params.caller_name).toBe('Elias');
		expect(recorded[0].params.caller_avatar).toBe('a1b2c3d4');
	});

	it('sends the caller to call.ring as caller_id, caller_name and caller_avatar', async () => {
		const recorded: Array<RecordedCall> = [];
		const service = recordingService(recorded);
		await service.ringCallRecipients(CHANNEL_ID, ['2'], {id: '1', name: 'Elias', avatar: 'a1b2c3d4'});
		expect(recorded).toHaveLength(1);
		expect(recorded[0].method).toBe('call.ring');
		expect(recorded[0].params.caller_id).toBe('1');
		expect(recorded[0].params.caller_name).toBe('Elias');
		expect(recorded[0].params.caller_avatar).toBe('a1b2c3d4');
	});

	it('sends caller_avatar as null when the caller has no avatar', async () => {
		const recorded: Array<RecordedCall> = [];
		const service = recordingService(recorded);
		await service.ringCallRecipients(CHANNEL_ID, ['2'], {id: '1', name: 'Elias', avatar: null});
		expect(recorded[0].params.caller_avatar).toBeNull();
		expect(Object.hasOwn(recorded[0].params, 'caller_avatar')).toBe(true);
	});

	it('omits every caller key from call.create when no caller was resolved', async () => {
		const recorded: Array<RecordedCall> = [];
		const service = recordingService(recorded);
		await service.createCall(CHANNEL_ID, '99', 'automatic', ['2'], ['1', '2']);
		expect(recorded[0].params).toEqual({
			channel_id: '12',
			message_id: '99',
			region: 'automatic',
			ringing: ['2'],
			recipients: ['1', '2'],
		});
	});

	it('omits every caller key from call.ring when no caller was resolved', async () => {
		const recorded: Array<RecordedCall> = [];
		const service = recordingService(recorded);
		await service.ringCallRecipients(CHANNEL_ID, ['2']);
		expect(recorded[0].params).toEqual({channel_id: '12', recipients: ['2']});
	});
});
