// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {
	THREAD_ARCHIVED_LIST_DEFAULT_LIMIT,
	THREAD_ARCHIVED_LIST_MAX_LIMIT,
	THREAD_ARCHIVED_LIST_MIN_LIMIT,
	THREAD_NAME_MAX_LENGTH,
	THREAD_NAME_MIN_LENGTH,
	THREAD_RATE_LIMIT_PER_USER_MAX,
} from '@fluxer/constants/src/ThreadConstants';
import {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {ThreadMemberResponse} from '@fluxer/schema/src/domains/channel/ThreadSchemas';
import {IsoTimestampStringType} from '@fluxer/schema/src/primitives/DateValidators';
import {createQueryIntegerType, QueryBooleanType} from '@fluxer/schema/src/primitives/QueryValidators';
import {
	createInt32EnumType,
	createStringType,
	Int32Type,
	SnowflakeType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const THREAD_MEMBERS_LIST_MAX_LIMIT = 100;
const LOCATION_MAX_LENGTH = 100;

export const ThreadAutoArchiveDurationSchema = createInt32EnumType(
	[
		[60, 'ONE_HOUR', 'One hour'],
		[1440, 'ONE_DAY', 'One day'],
		[4320, 'THREE_DAYS', 'Three days'],
		[10080, 'ONE_WEEK', 'One week'],
	],
	'Minutes of inactivity before the thread stops showing in the channel list',
	'ThreadAutoArchiveDuration',
);

const ThreadNameType = createStringType(THREAD_NAME_MIN_LENGTH, THREAD_NAME_MAX_LENGTH).describe(
	'The name of the thread (1-100 characters)',
);

const ThreadRateLimitPerUserType = Int32Type.max(THREAD_RATE_LIMIT_PER_USER_MAX).describe(
	'Seconds a user has to wait before sending another message (0-21600)',
);

const LocationType = z.string().max(LOCATION_MAX_LENGTH).optional().describe('Accepted and ignored');

export const StartThreadFromMessageRequest = z.object({
	name: ThreadNameType,
	auto_archive_duration: ThreadAutoArchiveDurationSchema.optional(),
	rate_limit_per_user: ThreadRateLimitPerUserType.optional(),
	location: LocationType,
});

export type StartThreadFromMessageRequest = z.infer<typeof StartThreadFromMessageRequest>;

export const StartThreadRequest = z.object({
	name: ThreadNameType,
	type: createInt32EnumType(
		[
			[ChannelTypes.ANNOUNCEMENT_THREAD, 'ANNOUNCEMENT_THREAD', 'A thread in an announcement channel'],
			[ChannelTypes.PUBLIC_THREAD, 'PUBLIC_THREAD', 'A public thread'],
			[ChannelTypes.PRIVATE_THREAD, 'PRIVATE_THREAD', 'A private thread'],
		],
		'The type of thread to create',
		'ThreadChannelType',
	),
	auto_archive_duration: ThreadAutoArchiveDurationSchema.optional(),
	rate_limit_per_user: ThreadRateLimitPerUserType.optional(),
	invitable: z.boolean().optional().describe('Whether non-moderators can add other non-moderators (private threads)'),
	location: LocationType,
});

export type StartThreadRequest = z.infer<typeof StartThreadRequest>;

export const ThreadLocationQuery = z.object({
	location: LocationType,
});

export const ThreadMembersListQuery = z.object({
	with_member: QueryBooleanType.describe('Whether to include a guild member object for each thread member'),
	after: SnowflakeType.optional().describe('Get thread members after this user ID'),
	limit: createQueryIntegerType({
		defaultValue: THREAD_MEMBERS_LIST_MAX_LIMIT,
		minValue: 1,
		maxValue: THREAD_MEMBERS_LIST_MAX_LIMIT,
	}).describe('Max number of thread members to return (1-100, default 100)'),
});

export type ThreadMembersListQuery = z.infer<typeof ThreadMembersListQuery>;

export const ThreadMemberGetQuery = z.object({
	with_member: QueryBooleanType.describe('Whether to include a guild member object for the thread member'),
});

const archivedListLimit = createQueryIntegerType({
	defaultValue: THREAD_ARCHIVED_LIST_DEFAULT_LIMIT,
	minValue: THREAD_ARCHIVED_LIST_MIN_LIMIT,
	maxValue: THREAD_ARCHIVED_LIST_MAX_LIMIT,
}).describe('Max number of threads to return (2-100, default 50)');

export const ArchivedThreadsQuery = z.object({
	before: IsoTimestampStringType.optional().describe('Get threads archived before this timestamp'),
	limit: archivedListLimit,
});

export type ArchivedThreadsQuery = z.infer<typeof ArchivedThreadsQuery>;

export const JoinedArchivedThreadsQuery = z.object({
	before: SnowflakeType.optional().describe('Get threads before this thread ID'),
	limit: archivedListLimit,
});

export type JoinedArchivedThreadsQuery = z.infer<typeof JoinedArchivedThreadsQuery>;

export {ThreadChannelResponse};

export const ActiveThreadsResponse = z.object({
	threads: z.array(ThreadChannelResponse).describe('The active threads'),
	members: z.array(ThreadMemberResponse).describe('A thread member object for each returned thread the user joined'),
});

export type ActiveThreadsResponse = z.infer<typeof ActiveThreadsResponse>;

export const ArchivedThreadsResponse = ActiveThreadsResponse.extend({
	threads: z.array(ThreadChannelResponse).describe('The archived threads'),
	has_more: z.boolean().describe('Whether there are potentially more threads to fetch'),
});

export type ArchivedThreadsResponse = z.infer<typeof ArchivedThreadsResponse>;

export const ThreadMemberListResponse = z.array(ThreadMemberResponse);

export type ThreadMemberListResponse = z.infer<typeof ThreadMemberListResponse>;
