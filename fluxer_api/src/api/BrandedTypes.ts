// SPDX-License-Identifier: AGPL-3.0-or-later

declare const __brand: unique symbol;

type Brand<T, TBrand extends string> = T & {
	readonly [__brand]: TBrand;
};

interface BrandedValue {
	readonly [__brand]: unknown;
}

const brand = <TValue extends string | bigint, TBrand extends string>(value: TValue): Brand<TValue, TBrand> =>
	value as Brand<TValue, TBrand>;
const rebrand = <TValue extends string | bigint, TBrand extends string>(
	value: Brand<TValue, string>,
): Brand<TValue, TBrand> => value as Brand<TValue, TBrand>;

export type UserID = Brand<bigint, 'UserID'>;
export type GuildID = Brand<bigint, 'GuildID'>;
export type ChannelID = Brand<bigint, 'ChannelID'>;
export type MessageID = Brand<bigint, 'MessageID'>;
export type RoleID = Brand<bigint, 'RoleID'>;
export type EmojiID = Brand<bigint, 'EmojiID'>;
export type WebhookID = Brand<bigint, 'WebhookID'>;
export type AttachmentID = Brand<bigint, 'AttachmentID'>;
export type StickerID = Brand<bigint, 'StickerID'>;
export type ReportID = Brand<bigint, 'ReportID'>;
export type MemeID = Brand<bigint, 'MemeID'>;
export type ApplicationID = Brand<bigint, 'ApplicationID'>;
export type EntranceSoundID = Brand<bigint, 'EntranceSoundID'>;
export type ScheduledEventID = Brand<bigint, 'ScheduledEventID'>;
export type InviteCode = Brand<string, 'InviteCode'>;
export type VanityURLCode = Brand<string, 'VanityURLCode'>;
export type EmailVerificationToken = Brand<string, 'EmailVerificationToken'>;
export type PasswordResetToken = Brand<string, 'PasswordResetToken'>;
export type EmailRevertToken = Brand<string, 'EmailRevertToken'>;
export type IpAuthorizationToken = Brand<string, 'IpAuthorizationToken'>;
type IpAuthorizationTicket = Brand<string, 'IpAuthorizationTicket'>;
type MfaTicket = Brand<string, 'MfaTicket'>;
export type WebhookToken = Brand<string, 'WebhookToken'>;
export type MfaBackupCode = Brand<string, 'MfaBackupCode'>;
export type PhoneVerificationToken = Brand<string, 'PhoneVerificationToken'>;

export function createUserID<T extends bigint>(id: T extends BrandedValue ? never : T): UserID {
	return brand<T, 'UserID'>(id);
}

export function createGuildID<T extends bigint>(id: T extends BrandedValue ? never : T): GuildID {
	return brand<T, 'GuildID'>(id);
}

export function createChannelID<T extends bigint>(id: T extends BrandedValue ? never : T): ChannelID {
	return brand<T, 'ChannelID'>(id);
}

export function createMessageID<T extends bigint>(id: T extends BrandedValue ? never : T): MessageID {
	return brand<T, 'MessageID'>(id);
}

export function createRoleID<T extends bigint>(id: T extends BrandedValue ? never : T): RoleID {
	return brand<T, 'RoleID'>(id);
}

export function createEmojiID<T extends bigint>(id: T extends BrandedValue ? never : T): EmojiID {
	return brand<T, 'EmojiID'>(id);
}

export function createWebhookID<T extends bigint>(id: T extends BrandedValue ? never : T): WebhookID {
	return brand<T, 'WebhookID'>(id);
}

export function createAttachmentID<T extends bigint>(id: T extends BrandedValue ? never : T): AttachmentID {
	return brand<T, 'AttachmentID'>(id);
}

export function createStickerID<T extends bigint>(id: T extends BrandedValue ? never : T): StickerID {
	return brand<T, 'StickerID'>(id);
}

export function createReportID<T extends bigint>(id: T extends BrandedValue ? never : T): ReportID {
	return brand<T, 'ReportID'>(id);
}

export function createMemeID<T extends bigint>(id: T extends BrandedValue ? never : T): MemeID {
	return brand<T, 'MemeID'>(id);
}

export function createApplicationID<T extends bigint>(id: T extends BrandedValue ? never : T): ApplicationID {
	return brand<T, 'ApplicationID'>(id);
}

export function createEntranceSoundID<T extends bigint>(id: T extends BrandedValue ? never : T): EntranceSoundID {
	return brand<T, 'EntranceSoundID'>(id);
}

export function createScheduledEventID<T extends bigint>(id: T extends BrandedValue ? never : T): ScheduledEventID {
	return brand<T, 'ScheduledEventID'>(id);
}

export function createInviteCode<T extends string>(code: T extends BrandedValue ? never : T): InviteCode {
	return brand<T, 'InviteCode'>(code);
}

export function createVanityURLCode<T extends string>(code: T extends BrandedValue ? never : T): VanityURLCode {
	return brand<T, 'VanityURLCode'>(code);
}

export function createEmailVerificationToken<T extends string>(
	token: T extends BrandedValue ? never : T,
): EmailVerificationToken {
	return brand<T, 'EmailVerificationToken'>(token);
}

export function createPasswordResetToken<T extends string>(
	token: T extends BrandedValue ? never : T,
): PasswordResetToken {
	return brand<T, 'PasswordResetToken'>(token);
}

export function createEmailRevertToken<T extends string>(token: T extends BrandedValue ? never : T): EmailRevertToken {
	return brand<T, 'EmailRevertToken'>(token);
}

export function createIpAuthorizationToken<T extends string>(
	token: T extends BrandedValue ? never : T,
): IpAuthorizationToken {
	return brand<T, 'IpAuthorizationToken'>(token);
}

export function createIpAuthorizationTicket<T extends string>(
	ticket: T extends BrandedValue ? never : T,
): IpAuthorizationTicket {
	return brand<T, 'IpAuthorizationTicket'>(ticket);
}

export function createMfaTicket<T extends string>(ticket: T extends BrandedValue ? never : T): MfaTicket {
	return brand<T, 'MfaTicket'>(ticket);
}

export function createWebhookToken<T extends string>(token: T extends BrandedValue ? never : T): WebhookToken {
	return brand<T, 'WebhookToken'>(token);
}

export function createMfaBackupCode<T extends string>(code: T extends BrandedValue ? never : T): MfaBackupCode {
	return brand<T, 'MfaBackupCode'>(code);
}

export function createUserIDSet(ids: Set<bigint>): Set<UserID> {
	return ids as Set<UserID>;
}

export function createGuildIDSet(ids: Set<bigint>): Set<GuildID> {
	return ids as Set<GuildID>;
}

export function createRoleIDSet(ids: Set<bigint>): Set<RoleID> {
	return ids as Set<RoleID>;
}

export function guildIdToRoleId(guildId: GuildID): RoleID {
	return rebrand<bigint, 'RoleID'>(guildId);
}

export function channelIdToUserId(channelId: ChannelID): UserID {
	return rebrand<bigint, 'UserID'>(channelId);
}

export function userIdToChannelId(userId: UserID): ChannelID {
	return rebrand<bigint, 'ChannelID'>(userId);
}

export function vanityCodeToInviteCode(vanityCode: VanityURLCode): InviteCode {
	return rebrand<string, 'InviteCode'>(vanityCode);
}

export function channelIdToMessageId(channelId: ChannelID): MessageID {
	return rebrand<bigint, 'MessageID'>(channelId);
}

export function applicationIdToUserId(applicationId: ApplicationID): UserID {
	return rebrand<bigint, 'UserID'>(applicationId);
}
