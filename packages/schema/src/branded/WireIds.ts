// SPDX-License-Identifier: AGPL-3.0-or-later

declare const GuildIdBrand: unique symbol;
declare const ChannelIdBrand: unique symbol;
declare const UserIdBrand: unique symbol;
declare const RoleIdBrand: unique symbol;
declare const MessageIdBrand: unique symbol;

export type GuildId = string & {
	readonly __brand: typeof GuildIdBrand;
};
export type ChannelId = string & {
	readonly __brand: typeof ChannelIdBrand;
};
export type UserId = string & {
	readonly __brand: typeof UserIdBrand;
};
export type RoleId = string & {
	readonly __brand: typeof RoleIdBrand;
};
export type MessageId = string & {
	readonly __brand: typeof MessageIdBrand;
};
