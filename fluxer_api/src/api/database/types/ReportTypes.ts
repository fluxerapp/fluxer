// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageAttachment, MessageEmbed, MessageStickerItem} from '@app/api/database/types/MessageTypes';

type MentionCollection = ReadonlyArray<bigint> | Set<bigint> | null | undefined;

export interface IARMessageContextRow {
	message_id: bigint;
	channel_id: bigint | null;
	author_id: bigint;
	author_username: string;
	author_discriminator: number;
	author_avatar_hash: string | null;
	content: string | null;
	timestamp: Date;
	edited_timestamp: Date | null;
	type: number;
	flags: number;
	mention_everyone: boolean;
	mention_users: MentionCollection;
	mention_roles: MentionCollection;
	mention_channels: MentionCollection;
	attachments: Array<MessageAttachment> | null;
	embeds: Array<MessageEmbed> | null;
	sticker_items: Array<MessageStickerItem> | null;
}

export interface IARSubmissionRow {
	report_id: bigint;
	reporter_id: bigint | null;
	reporter_email: string | null;
	reporter_full_legal_name: string | null;
	reporter_country_of_residence: string | null;
	reported_at: Date;
	status: number;
	report_type: number;
	category: string;
	additional_info: string | null;
	reported_user_id: bigint | null;
	reported_user_avatar_hash: string | null;
	reported_guild_id: bigint | null;
	reported_guild_name: string | null;
	reported_guild_icon_hash: string | null;
	reported_message_id: bigint | null;
	reported_channel_id: bigint | null;
	reported_channel_name: string | null;
	message_context: Array<IARMessageContextRow> | null;
	guild_context_id: bigint | null;
	resolved_at: Date | null;
	resolved_by_admin_id: bigint | null;
	public_comment: string | null;
	audit_log_reason: string | null;
	reported_guild_invite_code: string | null;
	reported_guild_nsfw?: boolean | null;
	reported_guild_content_warning_level?: number | null;
	reported_guild_content_warning_text?: string | null;
	reported_channel_nsfw_override?: boolean | null;
	reported_channel_content_warning_level?: number | null;
	reported_channel_content_warning_text?: string | null;
	reported_channel_effective_nsfw?: boolean | null;
	reported_channel_effective_content_warning_level?: number | null;
	reported_channel_effective_content_warning_text?: string | null;
}

export interface DSAReportEmailVerificationRow {
	email_lower: string;
	code_hash: string;
	expires_at: Date;
	last_sent_at: Date;
}

export interface DSAReportTicketRow {
	ticket: string;
	email_lower: string;
	expires_at: Date;
	created_at: Date;
}

export interface MessageReportSubmissionByReporterRow {
	reporter_id: bigint;
	channel_id: bigint;
	message_id: bigint;
	report_id: bigint;
	reported_at: Date;
}

export const IAR_SUBMISSION_COLUMNS = [
	'report_id',
	'reporter_id',
	'reporter_email',
	'reporter_full_legal_name',
	'reporter_country_of_residence',
	'reported_at',
	'status',
	'report_type',
	'category',
	'additional_info',
	'reported_user_id',
	'reported_user_avatar_hash',
	'reported_guild_id',
	'reported_guild_name',
	'reported_guild_icon_hash',
	'reported_message_id',
	'reported_channel_id',
	'reported_channel_name',
	'message_context',
	'guild_context_id',
	'resolved_at',
	'resolved_by_admin_id',
	'public_comment',
	'audit_log_reason',
	'reported_guild_invite_code',
	'reported_guild_nsfw',
	'reported_guild_content_warning_level',
	'reported_guild_content_warning_text',
	'reported_channel_nsfw_override',
	'reported_channel_content_warning_level',
	'reported_channel_content_warning_text',
	'reported_channel_effective_nsfw',
	'reported_channel_effective_content_warning_level',
	'reported_channel_effective_content_warning_text',
] as const satisfies ReadonlyArray<keyof IARSubmissionRow>;
export const DSA_REPORT_EMAIL_VERIFICATION_COLUMNS = [
	'email_lower',
	'code_hash',
	'expires_at',
	'last_sent_at',
] as const satisfies ReadonlyArray<keyof DSAReportEmailVerificationRow>;
export const DSA_REPORT_TICKET_COLUMNS = [
	'ticket',
	'email_lower',
	'expires_at',
	'created_at',
] as const satisfies ReadonlyArray<keyof DSAReportTicketRow>;
export const MESSAGE_REPORT_SUBMISSION_BY_REPORTER_COLUMNS = [
	'reporter_id',
	'channel_id',
	'message_id',
	'report_id',
	'reported_at',
] as const satisfies ReadonlyArray<keyof MessageReportSubmissionByReporterRow>;
