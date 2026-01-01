/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {GuildSplashCardAlignmentValue} from '~/Constants';
import {
	GuildFeatures,
	GuildSplashCardAlignment,
	LARGE_GUILD_THRESHOLD,
	MAX_GUILD_EMOJIS_ANIMATED,
	MAX_GUILD_EMOJIS_ANIMATED_MORE_EMOJI,
	MAX_GUILD_EMOJIS_STATIC,
	MAX_GUILD_EMOJIS_STATIC_MORE_EMOJI,
	MAX_GUILD_STICKERS,
	MAX_GUILD_STICKERS_MORE_STICKERS,
	MessageNotifications,
} from '~/Constants';
import type {Channel} from '~/records/ChannelRecord';
import type {GuildEmoji} from '~/records/GuildEmojiRecord';
import type {GuildMember} from '~/records/GuildMemberRecord';
import type {GuildRole} from '~/records/GuildRoleRecord';
import {GuildRoleRecord} from '~/records/GuildRoleRecord';
import type {GuildSticker} from '~/records/GuildStickerRecord';
import type {Presence} from '~/stores/PresenceStore';
import type {VoiceState} from '~/stores/voice/MediaEngineFacade';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';

export type Guild = Readonly<{
	id: string;
	name: string;
	icon: string | null;
	banner?: string | null;
	banner_width?: number | null;
	banner_height?: number | null;
	splash?: string | null;
	splash_width?: number | null;
	splash_height?: number | null;
	splash_card_alignment?: GuildSplashCardAlignmentValue;
	embed_splash?: string | null;
	embed_splash_width?: number | null;
	embed_splash_height?: number | null;
	vanity_url_code: string | null;
	owner_id: string;
	system_channel_id: string | null;
	system_channel_flags?: number;
	rules_channel_id?: string | null;
	afk_channel_id?: string | null;
	afk_timeout?: number;
	features: ReadonlyArray<string>;
	verification_level?: number;
	mfa_level?: number;
	nsfw_level?: number;
	explicit_content_filter?: number;
	default_message_notifications?: number;
	disabled_operations?: number;
	joined_at?: string;
	unavailable?: boolean;
	member_count?: number;
}>;

export type GuildReadyData = Readonly<{
	id: string;
	properties: Omit<Guild, 'roles'>;
	channels: ReadonlyArray<Channel>;
	emojis: ReadonlyArray<GuildEmoji>;
	stickers?: ReadonlyArray<GuildSticker>;
	members: ReadonlyArray<GuildMember>;
	member_count: number;
	presences?: ReadonlyArray<Presence>;
	voice_states?: ReadonlyArray<VoiceState>;
	roles: ReadonlyArray<GuildRole>;
	joined_at: string;
	unavailable?: boolean;
}>;

type GuildInput = Guild | GuildRecord;

export class GuildRecord {
	readonly id: string;
	readonly name: string;
	readonly icon: string | null;
	readonly banner: string | null;
	readonly bannerWidth: number | null;
	readonly bannerHeight: number | null;
	readonly splash: string | null;
	readonly splashWidth: number | null;
	readonly splashHeight: number | null;
	readonly splashCardAlignment: GuildSplashCardAlignmentValue;
	readonly embedSplash: string | null;
	readonly embedSplashWidth: number | null;
	readonly embedSplashHeight: number | null;
	readonly features: ReadonlySet<string>;
	readonly vanityURLCode: string | null;
	readonly ownerId: string;
	readonly systemChannelId: string | null;
	readonly systemChannelFlags: number;
	readonly rulesChannelId: string | null;
	readonly afkChannelId: string | null;
	readonly afkTimeout: number;
	readonly roles: Readonly<Record<string, GuildRoleRecord>>;
	readonly verificationLevel: number;
	readonly mfaLevel: number;
	readonly nsfwLevel: number;
	readonly explicitContentFilter: number;
	readonly defaultMessageNotifications: number;
	private readonly _disabledOperations: number;
	readonly joinedAt: string | null;
	readonly unavailable: boolean;
	readonly memberCount: number;

	constructor(guild: GuildInput) {
		this.id = guild.id;
		this.name = guild.name;
		this.icon = guild.icon;
		this.banner = this.normalizeBanner(guild);
		this.bannerWidth = this.normalizeBannerWidth(guild);
		this.bannerHeight = this.normalizeBannerHeight(guild);
		this.splash = this.normalizeSplash(guild);
		this.splashWidth = this.normalizeSplashWidth(guild);
		this.splashHeight = this.normalizeSplashHeight(guild);
		this.splashCardAlignment = this.normalizeSplashCardAlignment(guild);
		this.embedSplash = this.normalizeEmbedSplash(guild);
		this.embedSplashWidth = this.normalizeEmbedSplashWidth(guild);
		this.embedSplashHeight = this.normalizeEmbedSplashHeight(guild);
		this.features = new Set(guild.features);
		this.vanityURLCode = this.normalizeVanityUrlCode(guild);
		this.ownerId = this.normalizeOwnerId(guild);
		this.systemChannelId = this.normalizeSystemChannelId(guild);
		this.systemChannelFlags = this.normalizeSystemChannelFlags(guild);
		this.rulesChannelId = this.normalizeRulesChannelId(guild);
		this.afkChannelId = this.normalizeAfkChannelId(guild);
		this.afkTimeout = this.normalizeAfkTimeout(guild);
		this.roles = this.normalizeRoles(guild);
		this.verificationLevel = this.normalizeVerificationLevel(guild);
		this.mfaLevel = this.normalizeMfaLevel(guild);
		this.nsfwLevel = this.normalizeNsfwLevel(guild);
		this.explicitContentFilter = this.normalizeExplicitContentFilter(guild);
		this.defaultMessageNotifications = this.normalizeDefaultMessageNotifications(guild);
		this._disabledOperations = this.normalizeDisabledOperations(guild);
		this.joinedAt = this.normalizeJoinedAt(guild);
		this.unavailable = guild.unavailable ?? false;
		this.memberCount = this.normalizeMemberCount(guild);
	}

	private normalizeField<T>(guild: GuildInput, snakeCase: keyof Guild, camelCase: keyof GuildRecord): T {
		const value = this.isGuildInput(guild) ? guild[snakeCase] : guild[camelCase];
		return (value === undefined ? null : value) as T;
	}

	private normalizeFieldWithDefault<T>(
		guild: GuildInput,
		snakeCase: keyof Guild,
		camelCase: keyof GuildRecord,
		defaultValue: T,
	): T {
		return this.isGuildInput(guild) ? ((guild[snakeCase] ?? defaultValue) as T) : (guild[camelCase] as T);
	}

	private normalizeBanner(guild: GuildInput): string | null {
		return this.normalizeField(guild, 'banner', 'banner');
	}

	private normalizeBannerWidth(guild: GuildInput): number | null {
		return this.normalizeField(guild, 'banner_width', 'bannerWidth');
	}

	private normalizeBannerHeight(guild: GuildInput): number | null {
		return this.normalizeField(guild, 'banner_height', 'bannerHeight');
	}

	private normalizeSplash(guild: GuildInput): string | null {
		return this.normalizeField(guild, 'splash', 'splash');
	}

	private normalizeSplashWidth(guild: GuildInput): number | null {
		return this.normalizeField(guild, 'splash_width', 'splashWidth');
	}

	private normalizeSplashHeight(guild: GuildInput): number | null {
		return this.normalizeField(guild, 'splash_height', 'splashHeight');
	}

	private normalizeSplashCardAlignment(guild: GuildInput): GuildSplashCardAlignmentValue {
		if (this.isGuildInput(guild)) {
			return guild.splash_card_alignment ?? GuildSplashCardAlignment.CENTER;
		}
		return guild.splashCardAlignment ?? GuildSplashCardAlignment.CENTER;
	}

	private normalizeEmbedSplash(guild: GuildInput): string | null {
		return this.normalizeField(guild, 'embed_splash', 'embedSplash');
	}

	private normalizeEmbedSplashWidth(guild: GuildInput): number | null {
		return this.normalizeField(guild, 'embed_splash_width', 'embedSplashWidth');
	}

	private normalizeEmbedSplashHeight(guild: GuildInput): number | null {
		return this.normalizeField(guild, 'embed_splash_height', 'embedSplashHeight');
	}

	private normalizeVanityUrlCode(guild: GuildInput): string | null {
		return this.normalizeField(guild, 'vanity_url_code', 'vanityURLCode');
	}

	private normalizeOwnerId(guild: GuildInput): string {
		return this.normalizeField(guild, 'owner_id', 'ownerId');
	}

	private normalizeSystemChannelId(guild: GuildInput): string | null {
		return this.normalizeField(guild, 'system_channel_id', 'systemChannelId');
	}

	private normalizeSystemChannelFlags(guild: GuildInput): number {
		return this.normalizeFieldWithDefault(guild, 'system_channel_flags', 'systemChannelFlags', 0);
	}

	private normalizeRulesChannelId(guild: GuildInput): string | null {
		return this.normalizeField(guild, 'rules_channel_id', 'rulesChannelId');
	}

	private normalizeAfkChannelId(guild: GuildInput): string | null {
		return this.normalizeField(guild, 'afk_channel_id', 'afkChannelId');
	}

	private normalizeAfkTimeout(guild: GuildInput): number {
		return this.normalizeFieldWithDefault(guild, 'afk_timeout', 'afkTimeout', 0);
	}

	private normalizeRoles(guild: GuildInput): Readonly<Record<string, GuildRoleRecord>> {
		return Object.freeze('roles' in guild ? {...guild.roles} : {});
	}

	private normalizeVerificationLevel(guild: GuildInput): number {
		return this.normalizeFieldWithDefault(guild, 'verification_level', 'verificationLevel', 0);
	}

	private normalizeMfaLevel(guild: GuildInput): number {
		return this.normalizeFieldWithDefault(guild, 'mfa_level', 'mfaLevel', 0);
	}

	private normalizeNsfwLevel(guild: GuildInput): number {
		return this.normalizeFieldWithDefault(guild, 'nsfw_level', 'nsfwLevel', 0);
	}

	private normalizeExplicitContentFilter(guild: GuildInput): number {
		return this.normalizeFieldWithDefault(guild, 'explicit_content_filter', 'explicitContentFilter', 0);
	}

	private normalizeDefaultMessageNotifications(guild: GuildInput): number {
		return this.normalizeFieldWithDefault(guild, 'default_message_notifications', 'defaultMessageNotifications', 0);
	}

	private normalizeDisabledOperations(guild: GuildInput): number {
		return this.normalizeFieldWithDefault(guild, 'disabled_operations', 'disabledOperations', 0);
	}

	private normalizeJoinedAt(guild: GuildInput): string | null {
		return this.normalizeField(guild, 'joined_at', 'joinedAt');
	}

	private normalizeMemberCount(guild: GuildInput): number {
		if (this.isGuildInput(guild)) {
			const value = (guild as Guild).member_count;
			return typeof value === 'number' ? value : 0;
		}
		return (guild as GuildRecord).memberCount ?? 0;
	}

	private isGuildInput(guild: GuildInput): guild is Guild {
		return 'vanity_url_code' in guild;
	}

	get disabledOperations(): number {
		return this._disabledOperations;
	}

	static fromGuildReadyData(guildData: GuildReadyData): GuildRecord {
		const roles = Object.freeze(
			guildData.roles.reduce<Record<string, GuildRoleRecord>>(
				(acc, role) => ({
					// biome-ignore lint/performance/noAccumulatingSpread: acceptable for guild roles - manageable dataset size and immutability required
					...acc,
					[role.id]: new GuildRoleRecord(guildData.properties.id, role),
				}),
				{},
			),
		);

		return new GuildRecord({
			...guildData.properties,
			roles,
			joined_at: guildData.joined_at,
			unavailable: guildData.unavailable,
		});
	}

	toJSON(): Guild & {
		roles: Readonly<Record<string, GuildRoleRecord>>;
	} {
		return {
			id: this.id,
			name: this.name,
			icon: this.icon,
			banner: this.banner,
			banner_width: this.bannerWidth,
			banner_height: this.bannerHeight,
			splash: this.splash,
			splash_width: this.splashWidth,
			splash_height: this.splashHeight,
			splash_card_alignment: this.splashCardAlignment,
			embed_splash: this.embedSplash,
			embed_splash_width: this.embedSplashWidth,
			embed_splash_height: this.embedSplashHeight,
			features: [...this.features],
			vanity_url_code: this.vanityURLCode,
			owner_id: this.ownerId,
			system_channel_id: this.systemChannelId,
			system_channel_flags: this.systemChannelFlags,
			rules_channel_id: this.rulesChannelId,
			afk_channel_id: this.afkChannelId,
			afk_timeout: this.afkTimeout,
			verification_level: this.verificationLevel,
			mfa_level: this.mfaLevel,
			nsfw_level: this.nsfwLevel,
			explicit_content_filter: this.explicitContentFilter,
			default_message_notifications: this.defaultMessageNotifications,
			disabled_operations: this._disabledOperations,
			joined_at: this.joinedAt ?? undefined,
			unavailable: this.unavailable,
			member_count: this.memberCount,
			roles: this.roles,
		};
	}

	withUpdates(guild: Partial<Guild>): GuildRecord {
		return new GuildRecord({
			...this,
			name: guild.name ?? this.name,
			icon: guild.icon ?? this.icon,
			banner: guild.banner ?? this.banner,
			bannerWidth: guild.banner_width ?? this.bannerWidth,
			bannerHeight: guild.banner_height ?? this.bannerHeight,
			splash: guild.splash ?? this.splash,
			splashWidth: guild.splash_width ?? this.splashWidth,
			splashHeight: guild.splash_height ?? this.splashHeight,
			splashCardAlignment: guild.splash_card_alignment ?? this.splashCardAlignment,
			embedSplash: guild.embed_splash ?? this.embedSplash,
			embedSplashWidth: guild.embed_splash_width ?? this.embedSplashWidth,
			embedSplashHeight: guild.embed_splash_height ?? this.embedSplashHeight,
			features: guild.features ? new Set(guild.features) : this.features,
			vanityURLCode: guild.vanity_url_code ?? this.vanityURLCode,
			ownerId: guild.owner_id ?? this.ownerId,
			systemChannelId: guild.system_channel_id ?? this.systemChannelId,
			systemChannelFlags: guild.system_channel_flags ?? this.systemChannelFlags,
			rulesChannelId: guild.rules_channel_id ?? this.rulesChannelId,
			afkChannelId: guild.afk_channel_id ?? this.afkChannelId,
			afkTimeout: guild.afk_timeout ?? this.afkTimeout,
			verificationLevel: guild.verification_level ?? this.verificationLevel,
			mfaLevel: guild.mfa_level ?? this.mfaLevel,
			nsfwLevel: guild.nsfw_level ?? this.nsfwLevel,
			explicitContentFilter: guild.explicit_content_filter ?? this.explicitContentFilter,
			defaultMessageNotifications: guild.default_message_notifications ?? this.defaultMessageNotifications,
			disabledOperations: guild.disabled_operations ?? this.disabledOperations,
			unavailable: guild.unavailable ?? this.unavailable,
			memberCount: guild.member_count ?? this.memberCount,
		});
	}

	withRoles(roles: Record<string, GuildRoleRecord>): GuildRecord {
		return new GuildRecord({
			...this,
			roles: Object.freeze({...roles}),
		});
	}

	addRole(role: GuildRoleRecord): GuildRecord {
		return this.withRoles({
			...this.roles,
			[role.id]: role,
		});
	}

	removeRole(roleId: string): GuildRecord {
		const {[roleId]: _, ...remainingRoles} = this.roles;
		return this.withRoles(remainingRoles);
	}

	updateRole(role: GuildRoleRecord): GuildRecord {
		if (!this.roles[role.id]) {
			return this;
		}
		return this.addRole(role);
	}

	getRole(roleId: string): GuildRoleRecord | undefined {
		return this.roles[roleId];
	}

	get createdAt(): Date {
		return new Date(SnowflakeUtils.extractTimestamp(this.id));
	}

	isOwner(userId?: string | null): boolean {
		return userId != null && this.ownerId === userId;
	}

	get maxStaticEmojis(): number {
		if (this.features.has(GuildFeatures.MORE_EMOJI)) {
			return MAX_GUILD_EMOJIS_STATIC_MORE_EMOJI;
		}
		if (this.features.has(GuildFeatures.UNLIMITED_EMOJI)) {
			return Number.POSITIVE_INFINITY;
		}
		return MAX_GUILD_EMOJIS_STATIC;
	}

	get maxAnimatedEmojis(): number {
		if (this.features.has(GuildFeatures.MORE_EMOJI)) {
			return MAX_GUILD_EMOJIS_ANIMATED_MORE_EMOJI;
		}
		if (this.features.has(GuildFeatures.UNLIMITED_EMOJI)) {
			return Number.POSITIVE_INFINITY;
		}
		return MAX_GUILD_EMOJIS_ANIMATED;
	}

	get maxStickers(): number {
		if (this.features.has(GuildFeatures.MORE_STICKERS)) {
			return MAX_GUILD_STICKERS_MORE_STICKERS;
		}
		if (this.features.has(GuildFeatures.UNLIMITED_STICKERS)) {
			return Number.POSITIVE_INFINITY;
		}
		return MAX_GUILD_STICKERS;
	}

	get isLargeGuild(): boolean {
		return this.features.has(GuildFeatures.LARGE_GUILD_OVERRIDE) || this.memberCount > LARGE_GUILD_THRESHOLD;
	}

	get effectiveMessageNotifications(): number {
		if (this.memberCount === undefined || this.memberCount === null || this.memberCount < 0) {
			return this.defaultMessageNotifications;
		}
		if (this.isLargeGuild) {
			return MessageNotifications.ONLY_MENTIONS;
		}
		return this.defaultMessageNotifications;
	}

	get isNotificationOverrideActive(): boolean {
		return this.isLargeGuild && this.defaultMessageNotifications === MessageNotifications.ALL_MESSAGES;
	}
}
