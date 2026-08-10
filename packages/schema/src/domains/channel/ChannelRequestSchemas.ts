// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {CONTENT_WARNING_TEXT_MAX_LENGTH} from '@fluxer/constants/src/GuildConstants';
import {
	AVATAR_MAX_SIZE,
	CHANNEL_RATE_LIMIT_PER_USER_MAX,
	CHANNEL_RATE_LIMIT_PER_USER_MIN,
	CHANNEL_TOPIC_MAX_LENGTH,
	CHANNEL_TOPIC_MIN_LENGTH,
	RTC_REGION_ID_MAX_LENGTH,
	RTC_REGION_ID_MIN_LENGTH,
	VOICE_CHANNEL_BITRATE_MAX,
	VOICE_CHANNEL_BITRATE_MIN,
	VOICE_CHANNEL_CONNECTION_LIMIT_MAX,
	VOICE_CHANNEL_CONNECTION_LIMIT_MIN,
	VOICE_CHANNEL_USER_LIMIT_MAX,
	VOICE_CHANNEL_USER_LIMIT_MIN,
} from '@fluxer/constants/src/LimitConstants';
import {ChannelNicknameOverrides} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {ReadStateResponse} from '@fluxer/schema/src/domains/gateway/GatewaySchemas';
import {ChannelOverwriteTypeSchema, GeneralChannelNameType} from '@fluxer/schema/src/primitives/ChannelValidators';
import {createBase64StringType} from '@fluxer/schema/src/primitives/FileValidators';
import {ContentWarningLevelSchema} from '@fluxer/schema/src/primitives/GuildValidators';
import {QueryBooleanType} from '@fluxer/schema/src/primitives/QueryValidators';
import {
	coerceNumberFromString,
	createNamedLiteral,
	createNamedLiteralUnion,
	createStringType,
	Int32Type,
	SnowflakeType,
	UnsignedInt64Type,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {URLType} from '@fluxer/schema/src/primitives/UrlValidators';
import {z} from 'zod';

const ChannelOverwriteRequest = z.object({
	id: SnowflakeType.describe('The ID of the role or user to overwrite permissions for'),
	type: createNamedLiteralUnion(
		[
			[0, 'ROLE'],
			[1, 'MEMBER'],
		],
		'The type of overwrite (0 = role, 1 = member)',
	),
	allow: UnsignedInt64Type.optional().describe('fluxer:UnsignedInt64Type Bitwise value of allowed permissions'),
	deny: UnsignedInt64Type.optional().describe('fluxer:UnsignedInt64Type Bitwise value of denied permissions'),
});

const ChannelCommonBase = z.object({
	topic: createStringType(CHANNEL_TOPIC_MIN_LENGTH, CHANNEL_TOPIC_MAX_LENGTH)
		.nullish()
		.describe(`The channel topic (${CHANNEL_TOPIC_MIN_LENGTH}-${CHANNEL_TOPIC_MAX_LENGTH} characters)`),
	url: URLType.nullish().describe('External URL for link channels'),
	parent_id: SnowflakeType.nullish().describe('ID of the parent category for this channel'),
	bitrate: z
		.number()
		.int()
		.min(VOICE_CHANNEL_BITRATE_MIN)
		.max(VOICE_CHANNEL_BITRATE_MAX)
		.nullish()
		.describe(`Voice channel bitrate in bits per second (${VOICE_CHANNEL_BITRATE_MIN}-${VOICE_CHANNEL_BITRATE_MAX})`),
	user_limit: z
		.number()
		.int()
		.min(VOICE_CHANNEL_USER_LIMIT_MIN)
		.max(VOICE_CHANNEL_USER_LIMIT_MAX)
		.nullish()
		.describe(
			`Maximum users allowed in voice channel (${VOICE_CHANNEL_USER_LIMIT_MIN}-${VOICE_CHANNEL_USER_LIMIT_MAX}, ${VOICE_CHANNEL_USER_LIMIT_MIN} means unlimited)`,
		),
	voice_connection_limit: z
		.number()
		.int()
		.min(VOICE_CHANNEL_CONNECTION_LIMIT_MIN)
		.max(VOICE_CHANNEL_CONNECTION_LIMIT_MAX)
		.nullish()
		.describe(
			`Maximum active voice connections allowed per user in a voice channel (${VOICE_CHANNEL_CONNECTION_LIMIT_MIN}-${VOICE_CHANNEL_CONNECTION_LIMIT_MAX})`,
		),
	permission_overwrites: z
		.array(ChannelOverwriteRequest)
		.optional()
		.describe('Permission overwrites for roles and members'),
	rate_limit_per_user: z
		.number()
		.int()
		.min(CHANNEL_RATE_LIMIT_PER_USER_MIN)
		.max(CHANNEL_RATE_LIMIT_PER_USER_MAX)
		.nullish()
		.describe(`Slowmode delay in seconds (${CHANNEL_RATE_LIMIT_PER_USER_MIN}-${CHANNEL_RATE_LIMIT_PER_USER_MAX})`),
});
const ChannelContentWarningFields = {
	nsfw_override: z
		.boolean()
		.nullish()
		.describe(
			'Per-channel adult-content override (true=on, false=off, null=inherit from category then guild). Takes precedence over the legacy `nsfw` field if both are present.',
		),
	content_warning_level: ContentWarningLevelSchema.optional().describe(
		'Channel-level content warning override (0=inherit, 1=force-warn)',
	),
	content_warning_text: z
		.string()
		.max(CONTENT_WARNING_TEXT_MAX_LENGTH)
		.nullish()
		.describe('Custom channel content warning text (max 200 characters); null inherits from parent or guild'),
} as const;
const ChannelCreateCommon = ChannelCommonBase.extend({
	nsfw: z.boolean().default(false).describe('Whether the channel is marked as NSFW'),
	...ChannelContentWarningFields,
});
const ChannelUpdateCommon = ChannelCommonBase.extend({
	nsfw: z
		.boolean()
		.nullish()
		.describe(
			'Legacy: setting true maps to nsfw_override=true; setting false maps to nsfw_override=null (inherit). Prefer nsfw_override.',
		),
	...ChannelContentWarningFields,
	icon: createBase64StringType(1, Math.ceil(AVATAR_MAX_SIZE * (4 / 3)))
		.nullish()
		.describe('Base64-encoded icon image for group DM channels'),
	owner_id: SnowflakeType.nullish().describe('ID of the new owner for group DM channels'),
	nicks: ChannelNicknameOverrides.optional().describe('Custom nicknames for users in this channel'),
	rtc_region: createStringType(RTC_REGION_ID_MIN_LENGTH, RTC_REGION_ID_MAX_LENGTH)
		.nullish()
		.describe(
			`Voice region ID for the voice channel (${RTC_REGION_ID_MIN_LENGTH}-${RTC_REGION_ID_MAX_LENGTH} characters)`,
		),
});
const ChannelCreateTextRequest = ChannelCreateCommon.extend({
	type: createNamedLiteral(ChannelTypes.GUILD_TEXT, 'GUILD_TEXT', 'Channel type (text channel)'),
	name: GeneralChannelNameType.describe('The name of the channel'),
});

const ChannelCreateVoiceRequest = ChannelCreateCommon.extend({
	type: createNamedLiteral(ChannelTypes.GUILD_VOICE, 'GUILD_VOICE', 'Channel type (voice channel)'),
	name: GeneralChannelNameType.describe('The name of the channel'),
});

const ChannelCreateCategoryRequest = ChannelCreateCommon.extend({
	type: createNamedLiteral(ChannelTypes.GUILD_CATEGORY, 'GUILD_CATEGORY', 'Channel type (category)'),
	name: GeneralChannelNameType.describe('The name of the category'),
});

const ChannelCreateLinkRequest = ChannelCreateCommon.extend({
	type: createNamedLiteral(ChannelTypes.GUILD_LINK, 'GUILD_LINK', 'Channel type (link channel)'),
	name: GeneralChannelNameType.describe('The name of the channel'),
});

export const ChannelCreateRequest = z.discriminatedUnion('type', [
	ChannelCreateTextRequest,
	ChannelCreateVoiceRequest,
	ChannelCreateCategoryRequest,
	ChannelCreateLinkRequest,
]);

export type ChannelCreateRequest = z.infer<typeof ChannelCreateRequest>;

const ChannelUpdateTextRequest = ChannelUpdateCommon.extend({
	type: createNamedLiteral(ChannelTypes.GUILD_TEXT, 'GUILD_TEXT', 'Channel type (text channel)'),
	name: GeneralChannelNameType.nullish().describe('The name of the channel'),
});

const ChannelUpdateVoiceRequest = ChannelUpdateCommon.extend({
	type: createNamedLiteral(ChannelTypes.GUILD_VOICE, 'GUILD_VOICE', 'Channel type (voice channel)'),
	name: GeneralChannelNameType.nullish().describe('The name of the channel'),
});

const ChannelUpdateCategoryRequest = ChannelUpdateCommon.extend({
	type: createNamedLiteral(ChannelTypes.GUILD_CATEGORY, 'GUILD_CATEGORY', 'Channel type (category)'),
	name: GeneralChannelNameType.nullish().describe('The name of the category'),
});

const ChannelUpdateLinkRequest = ChannelUpdateCommon.extend({
	type: createNamedLiteral(ChannelTypes.GUILD_LINK, 'GUILD_LINK', 'Channel type (link channel)'),
	name: GeneralChannelNameType.nullish().describe('The name of the channel'),
});

const ChannelUpdateGroupDmRequest = z.object({
	type: createNamedLiteral(ChannelTypes.GROUP_DM, 'GROUP_DM', 'Channel type (group DM)'),
	name: GeneralChannelNameType.nullish().describe('The name of the group DM'),
	icon: createBase64StringType(1, Math.ceil(AVATAR_MAX_SIZE * (4 / 3)))
		.nullish()
		.describe('Base64-encoded icon image for the group DM'),
	owner_id: SnowflakeType.nullish().describe('ID of the new owner of the group DM'),
	nicks: ChannelNicknameOverrides.nullish().describe('Custom nicknames for users in this group DM'),
});

export const ChannelUpdateRequest = z.discriminatedUnion('type', [
	ChannelUpdateTextRequest,
	ChannelUpdateVoiceRequest,
	ChannelUpdateCategoryRequest,
	ChannelUpdateLinkRequest,
	ChannelUpdateGroupDmRequest,
]);

export type ChannelUpdateRequest = z.infer<typeof ChannelUpdateRequest>;

export const PermissionOverwriteCreateRequest = z.object({
	type: ChannelOverwriteTypeSchema.describe('The type of overwrite (0 = role, 1 = member)'),
	allow: UnsignedInt64Type.nullish().describe('fluxer:UnsignedInt64Type Bitwise value of allowed permissions'),
	deny: UnsignedInt64Type.nullish().describe('fluxer:UnsignedInt64Type Bitwise value of denied permissions'),
});

export type PermissionOverwriteCreateRequest = z.infer<typeof PermissionOverwriteCreateRequest>;

export const DeleteChannelQuery = z.object({
	silent: QueryBooleanType.describe('Whether to suppress the system message when leaving a group DM'),
	delete_messages: QueryBooleanType.optional().describe(
		'When leaving a group DM, also delete all messages the caller has sent in the channel',
	),
});

export type DeleteChannelQuery = z.infer<typeof DeleteChannelQuery>;

export const ReadStateAckBulkRequest = z.object({
	read_states: z
		.array(
			z.object({
				channel_id: SnowflakeType.describe('The ID of the channel'),
				message_id: SnowflakeType.describe('The ID of the last read message'),
			}),
		)
		.min(1)
		.max(100)
		.describe('Array of channel/message pairs to acknowledge'),
});

export type ReadStateAckBulkRequest = z.infer<typeof ReadStateAckBulkRequest>;

export const ReadStateAckRequest = z.object({
	read_states: z
		.array(
			z.object({
				channel_id: SnowflakeType.describe('The ID of the channel'),
				message_id: SnowflakeType.describe('The ID of the message to acknowledge'),
				mention_count: Int32Type.optional().describe('Number of unread mentions after this acknowledgement'),
				manual: z.boolean().optional().describe('Whether this acknowledgement is an explicit manual read marker'),
			}),
		)
		.min(1)
		.max(100)
		.describe('Read-state acknowledgements to apply. Supports normal and manual acknowledgements.'),
});

export type ReadStateAckRequest = z.infer<typeof ReadStateAckRequest>;

export const ReadStateAckResponse = z.object({
	read_states: z.array(ReadStateResponse).describe('Authoritative read states after applying the acknowledgement'),
	read_state_proto: z
		.string()
		.describe('Authoritative read states after applying the acknowledgement, encoded as a base64 protobuf bundle'),
});

export type ReadStateAckResponse = z.infer<typeof ReadStateAckResponse>;

export const ChannelPositionUpdateRequest = z.array(
	z.object({
		id: SnowflakeType.describe('The ID of the channel to reposition'),
		position: z.number().int().nonnegative().optional().describe('New position for the channel'),
		parent_id: SnowflakeType.nullish().describe('New parent category ID'),
		preceding_sibling_id: SnowflakeType.nullish().describe(
			'ID of the sibling channel that should directly precede this channel after reordering',
		),
		lock_permissions: z.boolean().optional().describe('Whether to sync permissions with the new parent'),
	}),
);

export type ChannelPositionUpdateRequest = z.infer<typeof ChannelPositionUpdateRequest>;

const GeoCoordinateString = createStringType(1, 32);
export const CallUpdateBodySchema = z.object({
	region: createStringType(RTC_REGION_ID_MIN_LENGTH, RTC_REGION_ID_MAX_LENGTH)
		.nullish()
		.describe(
			`The preferred voice region for the call (${RTC_REGION_ID_MIN_LENGTH}-${RTC_REGION_ID_MAX_LENGTH} characters). Omit or set to null for automatic region selection.`,
		),
	latitude: GeoCoordinateString.optional().describe('Client latitude used for automatic region selection'),
	longitude: GeoCoordinateString.optional().describe('Client longitude used for automatic region selection'),
});

export type CallUpdateBodySchema = z.infer<typeof CallUpdateBodySchema>;

export const CallRingBodySchema = z.object({
	recipients: z.array(SnowflakeType).optional().describe('User IDs to ring for the call'),
	latitude: GeoCoordinateString.optional().describe('Client latitude used for automatic region selection'),
	longitude: GeoCoordinateString.optional().describe('Client longitude used for automatic region selection'),
});

export type CallRingBodySchema = z.infer<typeof CallRingBodySchema>;

export const VoiceDebugLoggingToggleBodySchema = z.object({
	enabled: z.boolean().describe('Whether voice debug logging should be active for this channel'),
	duration_ms: coerceNumberFromString(z.number().int().min(60000).max(14400000))
		.optional()
		.describe('Optional activation duration in milliseconds. Defaults to one hour and is capped at four hours.'),
});

export type VoiceDebugLoggingToggleBodySchema = z.infer<typeof VoiceDebugLoggingToggleBodySchema>;

const VoiceDebugLoggingTimestampNs = z
	.string()
	.regex(/^[0-9]{1,32}$/)
	.describe('Nanosecond timestamp encoded as an unsigned decimal string');

export const VoiceDebugLoggingEventSchema = z
	.object({
		type: createStringType(1, 128).describe('Client-side diagnostic event type'),
		timestamp_ns: VoiceDebugLoggingTimestampNs.describe('Client wall-clock Unix timestamp in nanoseconds'),
		monotonic_ns: VoiceDebugLoggingTimestampNs.optional().describe('Client monotonic timestamp in nanoseconds'),
		data: z.record(z.string(), z.unknown()).optional().describe('Event-specific diagnostic payload'),
	})
	.passthrough();

export type VoiceDebugLoggingEventSchema = z.infer<typeof VoiceDebugLoggingEventSchema>;

export const VoiceDebugLoggingEventsBodySchema = z.object({
	session_id: createStringType(1, 128).describe('Active voice debug logging session id'),
	connection_id: createStringType(1, 128).optional().describe('Client voice connection id'),
	participant_identity: createStringType(1, 256).optional().describe('LiveKit participant identity'),
	events: z.array(VoiceDebugLoggingEventSchema).min(1).max(200).describe('NDJSON batch events to store'),
});

export type VoiceDebugLoggingEventsBodySchema = z.infer<typeof VoiceDebugLoggingEventsBodySchema>;

export const VoicePresenceHeartbeatBodySchema = z.object({
	connection_id: createStringType(1, 128).describe('Client voice connection id'),
});

export type VoicePresenceHeartbeatBodySchema = z.infer<typeof VoicePresenceHeartbeatBodySchema>;

export const StreamUpdateBodySchema = z.object({
	region: createStringType(RTC_REGION_ID_MIN_LENGTH, RTC_REGION_ID_MAX_LENGTH)
		.optional()
		.describe(
			`The preferred voice region for the stream (${RTC_REGION_ID_MIN_LENGTH}-${RTC_REGION_ID_MAX_LENGTH} characters)`,
		),
});

export type StreamUpdateBodySchema = z.infer<typeof StreamUpdateBodySchema>;

export const StreamPreviewUploadBodySchema = z.object({
	channel_id: SnowflakeType.describe('The ID of the channel where the stream is active'),
	thumbnail: createStringType(1, 2000000).describe('Base64-encoded thumbnail image data'),
	content_type: createStringType(1, 64).optional().describe('MIME type of the thumbnail image'),
});

export type StreamPreviewUploadBodySchema = z.infer<typeof StreamPreviewUploadBodySchema>;

export const StreamPreviewUploadUrlBodySchema = z.object({
	channel_id: SnowflakeType.describe('The ID of the channel where the stream is active'),
	content_type: createStringType(1, 64).optional().describe('MIME type of the thumbnail image'),
});

export type StreamPreviewUploadUrlBodySchema = z.infer<typeof StreamPreviewUploadUrlBodySchema>;

export const StreamPreviewUploadUrlResponseSchema = z.object({
	upload_url: URLType.describe('URL used to upload the stream preview with a PUT request'),
	method: z.literal('PUT').describe('HTTP method to use for the upload URL'),
	content_type: createStringType(1, 64).describe('MIME type that must be sent with the upload request'),
	expires_at: z.string().datetime().describe('ISO timestamp when the upload URL expires'),
	expires_in: Int32Type.describe('Number of seconds the upload URL remains valid'),
	max_bytes: Int32Type.describe('Maximum supported preview image size in bytes'),
});

export type StreamPreviewUploadUrlResponseSchema = z.infer<typeof StreamPreviewUploadUrlResponseSchema>;
