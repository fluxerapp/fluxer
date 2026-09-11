// SPDX-License-Identifier: AGPL-3.0-or-later

import {ArchiveSubjectTypeSchema} from '@fluxer/schema/src/domains/admin/AdminArchiveSchemas';
import {createStringType, SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const GuildIdParam = z.object({
	guild_id: SnowflakeType.describe('The ID of the guild'),
});

export type GuildIdParam = z.infer<typeof GuildIdParam>;

export const ChannelIdParam = z.object({
	channel_id: SnowflakeType.describe('The ID of the channel'),
});

export type ChannelIdParam = z.infer<typeof ChannelIdParam>;

export const UserIdParam = z.object({
	user_id: SnowflakeType.describe('The ID of the user'),
});

export type UserIdParam = z.infer<typeof UserIdParam>;

export const MessageIdParam = z.object({
	message_id: SnowflakeType.describe('The ID of the message'),
});

export type MessageIdParam = z.infer<typeof MessageIdParam>;

export const WebhookIdParam = z.object({
	webhook_id: SnowflakeType.describe('The ID of the webhook'),
});

export type WebhookIdParam = z.infer<typeof WebhookIdParam>;

export const InviteCodeParam = z.object({
	invite_code: createStringType().describe('The unique invite code'),
});

export type InviteCodeParam = z.infer<typeof InviteCodeParam>;

export const ApplicationIdParam = z.object({
	id: SnowflakeType.describe('The ID of the application'),
});

export type ApplicationIdParam = z.infer<typeof ApplicationIdParam>;

export const GuildIdUserIdParam = GuildIdParam.extend(UserIdParam.shape);

export type GuildIdUserIdParam = z.infer<typeof GuildIdUserIdParam>;

export const GuildIdRoleIdParam = GuildIdParam.extend({
	role_id: SnowflakeType.describe('The ID of the role'),
});

export type GuildIdRoleIdParam = z.infer<typeof GuildIdRoleIdParam>;

export const GuildIdUserIdRoleIdParam = GuildIdRoleIdParam.extend(UserIdParam.shape);

export type GuildIdUserIdRoleIdParam = z.infer<typeof GuildIdUserIdRoleIdParam>;

export const ChannelIdMessageIdParam = ChannelIdParam.extend(MessageIdParam.shape);

export type ChannelIdMessageIdParam = z.infer<typeof ChannelIdMessageIdParam>;

export const ChannelIdUserIdParam = ChannelIdParam.extend(UserIdParam.shape);

export type ChannelIdUserIdParam = z.infer<typeof ChannelIdUserIdParam>;

export const ChannelIdOverwriteIdParam = ChannelIdParam.extend({
	overwrite_id: SnowflakeType.describe('The ID of the permission overwrite'),
});

export type ChannelIdOverwriteIdParam = z.infer<typeof ChannelIdOverwriteIdParam>;

export const ChannelIdMessageIdAttachmentIdParam = ChannelIdMessageIdParam.extend({
	attachment_id: SnowflakeType.describe('The ID of the attachment'),
});

export type ChannelIdMessageIdAttachmentIdParam = z.infer<typeof ChannelIdMessageIdAttachmentIdParam>;

export const WebhookIdTokenParam = WebhookIdParam.extend({
	token: createStringType().describe('The webhook token'),
});

export type WebhookIdTokenParam = z.infer<typeof WebhookIdTokenParam>;

export const WebhookIdTokenMessageIdParam = WebhookIdTokenParam.extend(MessageIdParam.shape);

export type WebhookIdTokenMessageIdParam = z.infer<typeof WebhookIdTokenMessageIdParam>;

export const TargetIdParam = z.object({
	target_id: SnowflakeType.describe('The ID of the target user'),
});

export type TargetIdParam = z.infer<typeof TargetIdParam>;

export const ApplicationAuthorizationIdParam = z.object({
	applicationId: SnowflakeType.describe('The ID of the application'),
});

export type ApplicationAuthorizationIdParam = z.infer<typeof ApplicationAuthorizationIdParam>;

export const SuccessResponse = z.object({
	success: z.literal(true).describe('Whether the operation succeeded'),
});

export type SuccessResponse = z.infer<typeof SuccessResponse>;

export const EmojiIdParam = z.object({
	emoji_id: SnowflakeType.describe('The ID of the emoji'),
});

export type EmojiIdParam = z.infer<typeof EmojiIdParam>;

export const StickerIdParam = z.object({
	sticker_id: SnowflakeType.describe('The ID of the sticker'),
});

export type StickerIdParam = z.infer<typeof StickerIdParam>;

export const GuildIdEmojiIdParam = GuildIdParam.extend(EmojiIdParam.shape);

export type GuildIdEmojiIdParam = z.infer<typeof GuildIdEmojiIdParam>;

export const GuildIdStickerIdParam = GuildIdParam.extend(StickerIdParam.shape);

export type GuildIdStickerIdParam = z.infer<typeof GuildIdStickerIdParam>;

export const GiftCodeParam = z.object({
	code: createStringType(1, 32).describe('The gift code'),
});

export type GiftCodeParam = z.infer<typeof GiftCodeParam>;

export const StreamKeyParam = z.object({
	stream_key: createStringType(1, 256).describe('The stream key'),
});

export type StreamKeyParam = z.infer<typeof StreamKeyParam>;

export const ChannelIdMessageIdEmojiParam = ChannelIdMessageIdParam.extend({
	emoji: createStringType(1, 64).describe('The emoji identifier'),
});

export type ChannelIdMessageIdEmojiParam = z.infer<typeof ChannelIdMessageIdEmojiParam>;

export const ChannelIdMessageIdEmojiTargetIdParam = ChannelIdMessageIdEmojiParam.extend(TargetIdParam.shape);

export type ChannelIdMessageIdEmojiTargetIdParam = z.infer<typeof ChannelIdMessageIdEmojiTargetIdParam>;

export const ReportIdParam = z.object({
	report_id: SnowflakeType.describe('The ID of the report'),
});

export type ReportIdParam = z.infer<typeof ReportIdParam>;

export const KeyIdParam = z.object({
	key_id: SnowflakeType.describe('The ID of the key'),
});

export type KeyIdParam = z.infer<typeof KeyIdParam>;

export const MemeIdParam = z.object({
	meme_id: SnowflakeType.describe('The ID of the favorite meme'),
});

export type MemeIdParam = z.infer<typeof MemeIdParam>;

export const SessionIdQuerySchema = z.object({
	session_id: createStringType(1, 64).optional().describe('The session ID for synchronization'),
});

export type SessionIdQuerySchema = z.infer<typeof SessionIdQuerySchema>;

export const CredentialIdParam = z.object({
	credential_id: createStringType(1, 2048).describe('The ID of the WebAuthn credential'),
});

export type CredentialIdParam = z.infer<typeof CredentialIdParam>;

export const ArchivePathParam = z.object({
	subject_type: ArchiveSubjectTypeSchema.describe('The type of subject (user or guild)'),
	subject_id: SnowflakeType.describe('The ID of the subject'),
	archive_id: SnowflakeType.describe('The ID of the archive'),
});

export type ArchivePathParam = z.infer<typeof ArchivePathParam>;

export const JobIdParam = z.object({
	job_id: SnowflakeType.describe('The ID of the job'),
});

export type JobIdParam = z.infer<typeof JobIdParam>;

export const HarvestIdParam = z.object({
	harvestId: SnowflakeType.describe('The ID of the harvest request'),
});

export type HarvestIdParam = z.infer<typeof HarvestIdParam>;
