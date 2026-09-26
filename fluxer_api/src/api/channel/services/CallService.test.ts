// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createUserID, type UserID} from '@app/api/BrandedTypes';
import type {IChannelRepository} from '@app/api/channel/IChannelRepository';
import {CallService} from '@app/api/channel/services/CallService';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {CallCaller, CallData, IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {IVoiceRoomStore} from '@app/api/infrastructure/IVoiceRoomStore';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {User} from '@app/api/models/User';
import type {ReadStateService} from '@app/api/read_state/ReadStateService';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {beforeEach, describe, expect, it} from 'vitest';

const CALLER_ID = createUserID(1n);
const RECIPIENT_ID = createUserID(2n);
const CHANNEL_ID = createChannelID(12n);

interface CallerOverrides {
	username?: string;
	globalName?: string | null;
	avatar?: string | null;
	nickname?: string;
	userRowMissing?: boolean;
}

interface Harness {
	service: CallService;
	created: Array<CallCaller | undefined>;
	rung: Array<CallCaller | undefined>;
}

const EXISTING_CALL: CallData = {
	channel_id: CHANNEL_ID.toString(),
	message_id: '99',
	region: 'automatic',
	ringing: [],
	recipients: [CALLER_ID.toString(), RECIPIENT_ID.toString()],
	voice_states: [],
};

function harness(overrides: CallerOverrides, existingCall: CallData | null): Harness {
	const username = overrides.username ?? 'elias';
	const globalName = overrides.globalName === undefined ? 'Elias' : overrides.globalName;
	const avatar = overrides.avatar === undefined ? 'a1b2c3d4' : overrides.avatar;
	const nicknames = new Map<string, string>();
	if (overrides.nickname !== undefined) {
		nicknames.set(CALLER_ID.toString(), overrides.nickname);
	}
	const channel = {
		id: CHANNEL_ID,
		type: ChannelTypes.GROUP_DM,
		recipientIds: new Set<UserID>([CALLER_ID, RECIPIENT_ID]),
		nicknames,
	} as unknown as Channel;
	const created: Array<CallCaller | undefined> = [];
	const rung: Array<CallCaller | undefined> = [];
	const channelRepository = {
		findUnique: async () => channel,
		upsertMessage: async () => {},
		getMessage: async () => null,
	} as unknown as IChannelRepository;
	const userRepository = {
		findUnique: async () => (overrides.userRowMissing ? null : ({...callerUser(username, globalName, avatar)} as User)),
		listUsers: async () => [],
		findSettings: async () => null,
		isDmChannelOpen: async () => true,
	} as unknown as IUserRepository;
	const gatewayService = {
		getCall: async () => existingCall,
		createCall: async (
			_channelId: unknown,
			_messageId: string,
			_region: string,
			_ringing: Array<string>,
			_recipients: Array<string>,
			caller?: CallCaller,
		) => {
			created.push(caller);
			return EXISTING_CALL;
		},
		ringCallRecipients: async (_channelId: unknown, _recipients: Array<string>, caller?: CallCaller) => {
			rung.push(caller);
			return true;
		},
	} as unknown as IGatewayService;
	const userCacheService = {
		getUserPartialResponse: async (): Promise<UserPartialResponse> =>
			({
				id: CALLER_ID.toString(),
				username,
				discriminator: '0001',
				global_name: globalName,
				avatar,
				avatar_color: null,
				flags: 0,
			}) as unknown as UserPartialResponse,
	} as unknown as UserCacheService;
	const snowflakeService = {
		generateForChannel: async () => 7777n,
	} as unknown as ISnowflakeService;
	const readStateService = {
		ackMessage: async () => {},
		bulkIncrementMentionCounts: async () => {},
	} as unknown as ReadStateService;
	const service = new CallService(
		channelRepository,
		userRepository,
		{} as unknown as IGuildRepositoryAggregate,
		gatewayService,
		userCacheService,
		snowflakeService,
		readStateService,
		null,
		{} as unknown as IVoiceRoomStore,
	);
	return {service, created, rung};
}

function callerUser(username: string, globalName: string | null, avatar: string | null): Partial<User> {
	return {
		id: CALLER_ID,
		username,
		globalName,
		avatarHash: avatar,
		isBot: false,
	};
}

const requestCache = {
	userPartials: new Map(),
} as unknown as RequestCache;

describe('CallService caller identity', () => {
	let harnessState: Harness;

	const createCall = (overrides: CallerOverrides = {}) => {
		harnessState = harness(overrides, null);
		return harnessState.service.createOrGetCall({
			userId: CALLER_ID,
			channelId: CHANNEL_ID,
			ringing: [RECIPIENT_ID],
			requestCache,
		});
	};

	const ringExistingCall = (overrides: CallerOverrides = {}) => {
		harnessState = harness(overrides, EXISTING_CALL);
		return harnessState.service.ringCallRecipients({
			userId: CALLER_ID,
			channelId: CHANNEL_ID,
			requestCache,
		});
	};

	beforeEach(() => {
		requestCache.userPartials.clear();
	});

	it('sends the caller id, display name and avatar hash to createCall', async () => {
		await createCall();
		expect(harnessState.created).toEqual([{id: '1', name: 'Elias', avatar: 'a1b2c3d4'}]);
	});

	it('prefers the group dm nickname over the global name on createCall', async () => {
		await createCall({nickname: 'Eli'});
		expect(harnessState.created[0]?.name).toBe('Eli');
	});

	it('falls back to the username when the caller has no nickname and no global name', async () => {
		await createCall({globalName: null});
		expect(harnessState.created[0]?.name).toBe('elias');
	});

	it('sends a null avatar when the caller has no custom avatar', async () => {
		await createCall({avatar: null});
		expect(harnessState.created[0]).toEqual({id: '1', name: 'Elias', avatar: null});
	});

	it('sends no caller at all when the caller user row is gone', async () => {
		await createCall({userRowMissing: true});
		expect(harnessState.created).toEqual([undefined]);
	});

	it('sends the caller id, display name and avatar hash to ringCallRecipients', async () => {
		await ringExistingCall();
		expect(harnessState.rung).toEqual([{id: '1', name: 'Elias', avatar: 'a1b2c3d4'}]);
	});

	it('prefers the group dm nickname over the global name on ringCallRecipients', async () => {
		await ringExistingCall({nickname: 'Eli'});
		expect(harnessState.rung[0]?.name).toBe('Eli');
	});

	it('falls back to the username on ringCallRecipients', async () => {
		await ringExistingCall({globalName: null});
		expect(harnessState.rung[0]?.name).toBe('elias');
	});

	it('resolves the caller on the ring branch and not on the create branch', async () => {
		await ringExistingCall();
		expect(harnessState.created).toEqual([]);
		expect(harnessState.rung).toHaveLength(1);
	});
});
