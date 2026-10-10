// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RouteRateLimitConfig} from '@app/api/middleware/RateLimitMiddleware';
import {ms} from 'itty-time';

export const ChannelRateLimitConfigs = {
	CHANNEL_GET: {
		bucket: 'channel:read::channel_id',
		config: {limit: 100, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_UPDATE: {
		bucket: 'channel:update::channel_id',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_DELETE: {
		bucket: 'channel:delete::channel_id',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_READ_STATE_DELETE: {
		bucket: 'channel:read_state:delete::channel_id',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_MESSAGES_GET: {
		bucket: 'channel:messages:read::channel_id',
		config: {limit: 100, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_MESSAGES_BULK_GET: {
		bucket: 'channel:messages:bulk_read',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_MESSAGE_GET: {
		bucket: 'channel:message:read::channel_id',
		config: {limit: 100, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_MESSAGE_CREATE: {
		bucket: 'channel:message:create::channel_id',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_MESSAGE_UPDATE: {
		bucket: 'channel:message:update::channel_id',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_MESSAGE_DELETE: {
		bucket: 'channel:message:delete::channel_id',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_MESSAGE_BULK_DELETE: {
		bucket: 'channel:message:bulk_delete::channel_id',
		config: {limit: 10, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_MESSAGE_PURGE: {
		bucket: 'channel:message:purge::channel_id',
		config: {limit: 2, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	CHANNEL_MESSAGE_ACK: {
		bucket: 'channel:message:ack::channel_id',
		config: {limit: 100, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_MESSAGE_CROSSPOST: {
		bucket: 'channel:message:crosspost::channel_id',
		config: {limit: 5, windowMs: ms('5 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_MESSAGE_CROSSPOST_SOURCE: {
		bucket: 'channel:message:crosspost_source::channel_id',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_SEARCH: {
		bucket: 'channel:search::channel_id',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_ATTACHMENT_UPLOAD: {
		bucket: 'channel:attachment:upload::channel_id',
		config: {limit: 10, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	ATTACHMENT_DELETE: {
		bucket: 'attachment:delete',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	ATTACHMENT_URLS_REFRESH: {
		bucket: 'attachment:refresh_urls',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_TYPING: {
		bucket: 'channel:typing::channel_id',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_PINS: {
		bucket: 'channel:pins::channel_id',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_REACTIONS: {
		bucket: 'channel:reactions::channel_id',
		config: {limit: 30, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_CALL_GET: {
		bucket: 'channel:call:get::channel_id',
		config: {limit: 60, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_CALL_UPDATE: {
		bucket: 'channel:call:update::channel_id',
		config: {limit: 10, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_CALL_RING: {
		bucket: 'channel:call:ring::channel_id',
		config: {limit: 5, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_CALL_STOP_RINGING: {
		bucket: 'channel:call:stop_ringing::channel_id',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_STREAM_UPDATE: {
		bucket: 'channel:stream:update::stream_key',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_STREAM_PREVIEW_GET: {
		bucket: 'channel:stream:preview:get::stream_key',
		config: {limit: 60, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_STREAM_PREVIEW_POST: {
		bucket: 'channel:stream:preview:post::stream_key',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_STREAM_PREVIEW_UPLOAD_URL: {
		bucket: 'channel:stream:preview:upload_url::stream_key',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_STREAM_PREVIEW_DELETE: {
		bucket: 'channel:stream:preview:delete::stream_key',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_FOLLOW: {
		bucket: 'channel:follow::channel_id',
		config: {limit: 5, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_FOLLOWER_STATS: {
		bucket: 'channel:follower_stats::channel_id',
		config: {limit: 10, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_THREAD_CREATE: {
		bucket: 'channel:thread:create::channel_id',
		config: {limit: 10, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_THREAD_MEMBER_PUT: {
		bucket: 'channel:thread:member:put::channel_id',
		config: {limit: 10, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_THREAD_MEMBER_DELETE: {
		bucket: 'channel:thread:member:delete::channel_id',
		config: {limit: 10, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_THREAD_MEMBER_SETTINGS: {
		bucket: 'channel:thread:member:settings::channel_id',
		config: {limit: 10, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_THREAD_MEMBERS_LIST: {
		bucket: 'channel:thread:members:list::channel_id',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_THREAD_MEMBER_GET: {
		bucket: 'channel:thread:member:get::channel_id',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_THREADS_ARCHIVED_LIST: {
		bucket: 'channel:threads:archived:list::channel_id',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_THREADS_SEARCH: {
		bucket: 'channel:threads:search::channel_id',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_POST_DATA: {
		bucket: 'channel:post_data::channel_id',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	CHANNEL_FORUM_TAGS: {
		bucket: 'channel:forum_tags::channel_id',
		config: {limit: 10, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	VOICE_ENTRANCE_SOUND_PLAY: {
		bucket: 'voice:entrance_sound:play::user_id::channel_id',
		config: {limit: 3, windowMs: ms('30 seconds')},
	} as RouteRateLimitConfig,
	VOICE_P2P_CONNECTION_REPORTS: {
		bucket: 'voice:p2p:connection_reports',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
} as const;
