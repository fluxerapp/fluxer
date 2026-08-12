// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import {GuildRole} from '@app/features/guild/models/GuildRole';
import {noteText} from '@app/features/theme/fonts/ScriptFontLoader';
import {LARGE_GUILD_THRESHOLD} from '@fluxer/constants/src/GatewayConstants';
import type {GuildSplashCardAlignmentValue} from '@fluxer/constants/src/GuildConstants';
import {GuildFeatures, GuildNSFWLevel, GuildSplashCardAlignment} from '@fluxer/constants/src/GuildConstants';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {MAX_GUILD_EMOJIS, MAX_GUILD_STICKERS} from '@fluxer/constants/src/LimitConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {resolveLimit} from '@fluxer/limits/src/LimitResolver';
import type {Guild as WireGuild} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {GuildRole as WireGuildRole} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';

interface GuildRecordOptions {
	instanceId?: string;
}

type GuildInput = WireGuild | Guild;
type GuildInputWithRoleRecord = Omit<WireGuild, 'roles'> & {
	roles: Readonly<Record<string, GuildRole>>;
};
type GuildConstructorInput = GuildInput | GuildInputWithRoleRecord;

export class Guild {
	readonly instanceId: string;
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
	readonly roles: Readonly<Record<string, GuildRole>>;
	readonly verificationLevel: number;
	readonly mfaLevel: number;
	readonly nsfw: boolean;
	readonly contentWarningLevel: number;
	readonly contentWarningText: string | null;
	readonly explicitContentFilter: number;
	readonly defaultMessageNotifications: number;
	private readonly _disabledOperations: number;
	readonly joinedAt: string | null;
	readonly unavailable: boolean;
	readonly messageHistoryCutoff: string | null;
	readonly memberCount: number;

	constructor(guild: GuildConstructorInput, options?: GuildRecordOptions) {
		this.instanceId =
			options?.instanceId ?? (guild instanceof Guild ? guild.instanceId : RuntimeConfig.localInstanceDomain);
		this.id = guild.id;
		this.name = guild.name;
		noteText(this.name);
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
		this.nsfw = this.normalizeNsfw(guild);
		this.contentWarningLevel = this.normalizeContentWarningLevel(guild);
		this.contentWarningText = this.normalizeContentWarningText(guild);
		this.explicitContentFilter = this.normalizeExplicitContentFilter(guild);
		this.defaultMessageNotifications = this.normalizeDefaultMessageNotifications(guild);
		this._disabledOperations = this.normalizeDisabledOperations(guild);
		this.joinedAt = this.normalizeJoinedAt(guild);
		this.messageHistoryCutoff = this.normalizeMessageHistoryCutoff(guild);
		this.unavailable = guild.unavailable ?? false;
		this.memberCount = this.normalizeMemberCount(guild);
	}

	private normalizeField<T>(guild: GuildConstructorInput, snakeCase: keyof WireGuild, camelCase: keyof Guild): T {
		const value = this.isGuildInput(guild) ? guild[snakeCase] : (guild as Guild)[camelCase];
		return (value === undefined ? null : value) as T;
	}

	private normalizeFieldWithDefault<T>(
		guild: GuildConstructorInput,
		snakeCase: keyof WireGuild,
		camelCase: keyof Guild,
		defaultValue: T,
	): T {
		return this.isGuildInput(guild) ? ((guild[snakeCase] ?? defaultValue) as T) : ((guild as Guild)[camelCase] as T);
	}

	private normalizeBanner(guild: GuildConstructorInput): string | null {
		return this.normalizeField(guild, 'banner', 'banner');
	}

	private normalizeBannerWidth(guild: GuildConstructorInput): number | null {
		return this.normalizeField(guild, 'banner_width', 'bannerWidth');
	}

	private normalizeBannerHeight(guild: GuildConstructorInput): number | null {
		return this.normalizeField(guild, 'banner_height', 'bannerHeight');
	}

	private normalizeSplash(guild: GuildConstructorInput): string | null {
		return this.normalizeField(guild, 'splash', 'splash');
	}

	private normalizeSplashWidth(guild: GuildConstructorInput): number | null {
		return this.normalizeField(guild, 'splash_width', 'splashWidth');
	}

	private normalizeSplashHeight(guild: GuildConstructorInput): number | null {
		return this.normalizeField(guild, 'splash_height', 'splashHeight');
	}

	private normalizeSplashCardAlignment(guild: GuildConstructorInput): GuildSplashCardAlignmentValue {
		if (this.isGuildInput(guild)) {
			return guild.splash_card_alignment ?? GuildSplashCardAlignment.CENTER;
		}
		return (guild as Guild).splashCardAlignment ?? GuildSplashCardAlignment.CENTER;
	}

	private normalizeEmbedSplash(guild: GuildConstructorInput): string | null {
		return this.normalizeField(guild, 'embed_splash', 'embedSplash');
	}

	private normalizeEmbedSplashWidth(guild: GuildConstructorInput): number | null {
		return this.normalizeField(guild, 'embed_splash_width', 'embedSplashWidth');
	}

	private normalizeEmbedSplashHeight(guild: GuildConstructorInput): number | null {
		return this.normalizeField(guild, 'embed_splash_height', 'embedSplashHeight');
	}

	private normalizeVanityUrlCode(guild: GuildConstructorInput): string | null {
		return this.normalizeField(guild, 'vanity_url_code', 'vanityURLCode');
	}

	private normalizeOwnerId(guild: GuildConstructorInput): string {
		return this.normalizeField(guild, 'owner_id', 'ownerId');
	}

	private normalizeSystemChannelId(guild: GuildConstructorInput): string | null {
		return this.normalizeField(guild, 'system_channel_id', 'systemChannelId');
	}

	private normalizeSystemChannelFlags(guild: GuildConstructorInput): number {
		return this.normalizeFieldWithDefault(guild, 'system_channel_flags', 'systemChannelFlags', 0);
	}

	private normalizeRulesChannelId(guild: GuildConstructorInput): string | null {
		return this.normalizeField(guild, 'rules_channel_id', 'rulesChannelId');
	}

	private normalizeAfkChannelId(guild: GuildConstructorInput): string | null {
		return this.normalizeField(guild, 'afk_channel_id', 'afkChannelId');
	}

	private normalizeAfkTimeout(guild: GuildConstructorInput): number {
		return this.normalizeFieldWithDefault(guild, 'afk_timeout', 'afkTimeout', 0);
	}

	private normalizeRoles(guild: GuildConstructorInput): Readonly<Record<string, GuildRole>> {
		if (guild instanceof Guild) {
			return Object.freeze({...guild.roles});
		}
		const roles = guild.roles;
		if (!roles) {
			return {};
		}
		if (Array.isArray(roles)) {
			const wireRoles = roles as ReadonlyArray<WireGuildRole>;
			return Object.freeze(
				wireRoles.reduce<Record<string, GuildRole>>((acc, role) => {
					acc[role.id] = new GuildRole(guild.id, role);
					return acc;
				}, {}),
			);
		}
		const roleRecords = roles as Readonly<Record<string, GuildRole>>;
		return Object.freeze({...roleRecords});
	}

	private normalizeVerificationLevel(guild: GuildConstructorInput): number {
		return this.normalizeFieldWithDefault(guild, 'verification_level', 'verificationLevel', 0);
	}

	private normalizeMfaLevel(guild: GuildConstructorInput): number {
		return this.normalizeFieldWithDefault(guild, 'mfa_level', 'mfaLevel', 0);
	}

	private normalizeNsfw(guild: GuildConstructorInput): boolean {
		return this.normalizeFieldWithDefault(guild, 'nsfw', 'nsfw', false);
	}

	private normalizeContentWarningLevel(guild: GuildConstructorInput): number {
		return this.normalizeFieldWithDefault(guild, 'content_warning_level', 'contentWarningLevel', 0);
	}

	private normalizeContentWarningText(guild: GuildConstructorInput): string | null {
		const value = this.isGuildInput(guild) ? guild.content_warning_text : (guild as Guild).contentWarningText;
		return value ?? null;
	}

	private normalizeExplicitContentFilter(guild: GuildConstructorInput): number {
		return this.normalizeFieldWithDefault(guild, 'explicit_content_filter', 'explicitContentFilter', 0);
	}

	private normalizeDefaultMessageNotifications(guild: GuildConstructorInput): number {
		return this.normalizeFieldWithDefault(guild, 'default_message_notifications', 'defaultMessageNotifications', 0);
	}

	private normalizeDisabledOperations(guild: GuildConstructorInput): number {
		return this.normalizeFieldWithDefault(guild, 'disabled_operations', 'disabledOperations', 0);
	}

	private normalizeJoinedAt(guild: GuildConstructorInput): string | null {
		return this.normalizeField(guild, 'joined_at', 'joinedAt');
	}

	private normalizeMessageHistoryCutoff(guild: GuildConstructorInput): string | null {
		return this.normalizeField(guild, 'message_history_cutoff', 'messageHistoryCutoff');
	}

	private normalizeMemberCount(guild: GuildConstructorInput): number {
		if (this.isGuildInput(guild)) {
			const value = guild.member_count;
			return typeof value === 'number' ? value : 0;
		}
		return (guild as Guild).memberCount ?? 0;
	}

	private isGuildInput(guild: GuildConstructorInput): guild is WireGuild {
		return 'vanity_url_code' in guild;
	}

	get disabledOperations(): number {
		return this._disabledOperations;
	}

	get nsfwLevel(): number {
		return this.nsfw ? GuildNSFWLevel.AGE_RESTRICTED : GuildNSFWLevel.SAFE;
	}

	static fromGuildReadyData(guildData: GuildReadyData, instanceId?: string): Guild {
		const roles = Object.freeze(
			guildData.roles.reduce<Record<string, GuildRole>>((acc, role) => {
				acc[role.id] = new GuildRole(guildData.properties.id, role);
				return acc;
			}, {}),
		);
		return new Guild(
			{
				...guildData.properties,
				roles,
				joined_at: guildData.joined_at,
				unavailable: guildData.unavailable,
			},
			{instanceId},
		);
	}

	toJSON(): WireGuild {
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
			nsfw: this.nsfw,
			content_warning_level: this.contentWarningLevel,
			content_warning_text: this.contentWarningText,
			explicit_content_filter: this.explicitContentFilter,
			default_message_notifications: this.defaultMessageNotifications,
			disabled_operations: this._disabledOperations,
			message_history_cutoff: this.messageHistoryCutoff,
			joined_at: this.joinedAt ?? undefined,
			unavailable: this.unavailable,
			member_count: this.memberCount,
			roles: Object.values(this.roles).map((role) => role.toJSON()),
		};
	}

	withUpdates(guild: Partial<WireGuild>): Guild {
		return new Guild(
			{
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
				nsfw: guild.nsfw ?? this.nsfw,
				contentWarningLevel: guild.content_warning_level ?? this.contentWarningLevel,
				contentWarningText:
					guild.content_warning_text !== undefined ? (guild.content_warning_text ?? null) : this.contentWarningText,
				explicitContentFilter: guild.explicit_content_filter ?? this.explicitContentFilter,
				defaultMessageNotifications: guild.default_message_notifications ?? this.defaultMessageNotifications,
				disabledOperations: guild.disabled_operations ?? this.disabledOperations,
				messageHistoryCutoff:
					guild.message_history_cutoff !== undefined
						? (guild.message_history_cutoff ?? null)
						: this.messageHistoryCutoff,
				unavailable: guild.unavailable ?? this.unavailable,
				memberCount: guild.member_count ?? this.memberCount,
			},
			{instanceId: this.instanceId},
		);
	}

	withRoles(roles: Record<string, GuildRole>): Guild {
		return new Guild(
			{
				...this,
				roles: Object.freeze({...roles}),
			},
			{instanceId: this.instanceId},
		);
	}

	addRole(role: GuildRole): Guild {
		return this.withRoles({
			...this.roles,
			[role.id]: role,
		});
	}

	removeRole(roleId: string): Guild {
		const {[roleId]: _, ...remainingRoles} = this.roles;
		return this.withRoles(remainingRoles);
	}

	updateRole(role: GuildRole): Guild {
		if (!this.roles[role.id]) {
			return this;
		}
		return this.addRole(role);
	}

	getRole(roleId: string): GuildRole | undefined {
		return this.roles[roleId];
	}

	get createdAt(): Date {
		return new Date(SnowflakeUtils.extractTimestamp(this.id));
	}

	isOwner(userId?: string | null): boolean {
		return userId != null && this.ownerId === userId;
	}

	get maxStaticEmojis(): number {
		return this.maxEmojis;
	}

	get maxAnimatedEmojis(): number {
		return this.maxEmojis;
	}

	get maxEmojis(): number {
		if (this.features.has(GuildFeatures.UNLIMITED_EMOJI)) {
			return Number.POSITIVE_INFINITY;
		}
		return this.resolveGuildLimit('max_guild_emojis', MAX_GUILD_EMOJIS);
	}

	get maxStickers(): number {
		if (this.features.has(GuildFeatures.UNLIMITED_STICKERS)) {
			return Number.POSITIVE_INFINITY;
		}
		return this.resolveGuildLimit('max_guild_stickers', MAX_GUILD_STICKERS);
	}

	private resolveGuildLimit(key: LimitKey, fallback: number): number {
		const resolved = resolveLimit(
			RuntimeConfig.limits,
			{
				traits: new Set(),
				guildFeatures: new Set(this.features),
			},
			key,
			{evaluationContext: 'guild'},
		);
		if (!Number.isFinite(resolved) || resolved < 0) {
			return fallback;
		}
		return Math.floor(resolved);
	}

	get cloneEmojiAllowed(): boolean {
		return !this.features.has(GuildFeatures.CLONE_EMOJI_DISABLED);
	}

	get cloneStickerAllowed(): boolean {
		return !this.features.has(GuildFeatures.CLONE_STICKER_DISABLED);
	}

	get isLargeGuild(): boolean {
		return (
			this.features.has(GuildFeatures.LARGE_GUILD_OVERRIDE) ||
			this.features.has(GuildFeatures.VERY_LARGE_GUILD) ||
			this.memberCount > LARGE_GUILD_THRESHOLD
		);
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

	equals(other: Guild): boolean {
		if (this === other) return true;
		if (this.instanceId !== other.instanceId) return false;
		if (this.id !== other.id) return false;
		if (this.name !== other.name) return false;
		if (this.icon !== other.icon) return false;
		if (this.banner !== other.banner) return false;
		if (this.bannerWidth !== other.bannerWidth) return false;
		if (this.bannerHeight !== other.bannerHeight) return false;
		if (this.splash !== other.splash) return false;
		if (this.splashWidth !== other.splashWidth) return false;
		if (this.splashHeight !== other.splashHeight) return false;
		if (this.splashCardAlignment !== other.splashCardAlignment) return false;
		if (this.embedSplash !== other.embedSplash) return false;
		if (this.embedSplashWidth !== other.embedSplashWidth) return false;
		if (this.embedSplashHeight !== other.embedSplashHeight) return false;
		if (this.vanityURLCode !== other.vanityURLCode) return false;
		if (this.ownerId !== other.ownerId) return false;
		if (this.systemChannelId !== other.systemChannelId) return false;
		if (this.systemChannelFlags !== other.systemChannelFlags) return false;
		if (this.rulesChannelId !== other.rulesChannelId) return false;
		if (this.afkChannelId !== other.afkChannelId) return false;
		if (this.afkTimeout !== other.afkTimeout) return false;
		if (this.verificationLevel !== other.verificationLevel) return false;
		if (this.mfaLevel !== other.mfaLevel) return false;
		if (this.nsfw !== other.nsfw) return false;
		if (this.contentWarningLevel !== other.contentWarningLevel) return false;
		if (this.contentWarningText !== other.contentWarningText) return false;
		if (this.explicitContentFilter !== other.explicitContentFilter) return false;
		if (this.defaultMessageNotifications !== other.defaultMessageNotifications) return false;
		if (this._disabledOperations !== other._disabledOperations) return false;
		if (this.joinedAt !== other.joinedAt) return false;
		if (this.messageHistoryCutoff !== other.messageHistoryCutoff) return false;
		if (this.unavailable !== other.unavailable) return false;
		if (this.memberCount !== other.memberCount) return false;
		if (this.features.size !== other.features.size) return false;
		for (const feature of this.features) {
			if (!other.features.has(feature)) return false;
		}
		const thisRoleIds = Object.keys(this.roles);
		const otherRoleIds = Object.keys(other.roles);
		if (thisRoleIds.length !== otherRoleIds.length) return false;
		for (const roleId of thisRoleIds) {
			const otherRole = other.roles[roleId];
			if (!otherRole || !this.roles[roleId].equals(otherRole)) return false;
		}
		return true;
	}
}
