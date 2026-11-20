import {createPersonaID, type PersonaID, type UserID} from '@app/api/BrandedTypes';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {stripAvatarForUser, stripBannerForUser} from '@app/api/infrastructure/AssetEntitlementUtils';
import {contentModerationService} from '@app/api/infrastructure/ContentModerationService';
import type {EntityAssetService, PreparedAssetUpload} from '@app/api/infrastructure/EntityAssetService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {resolveLimitSafe} from '@app/api/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import {profileSubstringBlocklistCache} from '@app/api/middleware/ProfileSubstringBlocklistCache';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Persona} from '@app/api/models/Persona';
import {PersonaTrigger} from '@app/api/models/PersonaTrigger';
import type {User} from '@app/api/models/User';
import type {UserAccountRepository} from '@app/api/user/repositories/account/UserAccountRepository';
import {isProfileSubstringExempt} from '@app/api/user/UserHelpers';
import {deriveDominantAvatarColor} from '@app/api/utils/AvatarColorUtils';
import * as EmojiUtils from '@app/api/utils/EmojiUtils';
import {MAX_BIO_LENGTH} from '@fluxer/constants/src/LimitConstants.js';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes.js';
import {ContentBlockedError} from '@fluxer/errors/src/domains/content/ContentBlockedError.js';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError.js';
import type {
	OwnPersonaResponse,
	PersonaCreateRequest,
	PersonaResponse,
} from '@fluxer/schema/src/domains/persona/PersonaSchemas.js';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService.js';
import {ms} from 'itty-time/ms';
import type {PersonaRepository} from '../repositories/PersonaRepository';

interface PersonaFieldUpdates {
	avatar_color?: Persona['avatarColor'];
	avatar_hash?: Persona['avatarHash'];
	banner_color?: Persona['bannerColor'];
	banner_hash?: Persona['bannerHash'];
}

export class PersonaService {
	constructor(
		private readonly personaRepository: PersonaRepository,
		private readonly snowflakeService: ISnowflakeService,
		private readonly entityAssetService: EntityAssetService,
		private readonly userCacheService: UserCacheService,
		private readonly requestCache: RequestCache,
		private readonly storageService: IStorageService,
		private readonly rateLimitService: IRateLimitService,
		private readonly limitConfigService: LimitConfigService,
		private readonly userAccountRepository: UserAccountRepository,
		private readonly guildRepository: GuildRepository,
	) {}
	async getUserPersonas(user: User): Promise<Array<OwnPersonaResponse>> {
		const results = await this.personaRepository.listUserPersonas(user.id);
		const personas = new Set<OwnPersonaResponse>();
		for (const v of results) {
			const triggers = await this.personaRepository.getPersonaTriggers(user.id, v.id);
			personas.add({
				id: v.id.toString(),
				//user: user,
				avatar: stripAvatarForUser(user, v.avatarHash),
				avatar_color: v.avatarColor,
				banner: stripBannerForUser(user, v.bannerHash),
				banner_color: v.bannerColor,
				display_name: v.displayName || v.internalName,
				internal_name: v.internalName,
				bio: v.bio,
				tags: v.tags || [],
				pronouns: v.pronouns,
				accent_color: v.accentColor,
				triggers: triggers || [],
			});
		}
		return Array.from(personas);
	}

	async checkPersonaExists(userId: UserID, personaId: PersonaID): Promise<boolean> {
		const v = await this.personaRepository.getPersona(userId, personaId);
		return !!v;
	}

	async getPersona(targetUser: User, personaId: PersonaID): Promise<PersonaResponse | null> {
		const v = await this.personaRepository.getPersona(targetUser.id, personaId);
		if (v === null) return null;
		const userPartial = await this.userCacheService.getUserPartialResponse(targetUser.id, this.requestCache);
		return {
			id: v.id.toString(),
			user: userPartial,
			avatar: stripAvatarForUser(targetUser, v.avatarHash),
			avatar_color: v.avatarColor,
			banner: stripBannerForUser(targetUser, v.bannerHash),
			banner_color: v.bannerColor,
			//internal_name: v.internalName,
			display_name: v.displayName || v.internalName,
			bio: v.bio,
			pronouns: v.pronouns,
			//tags: v.tags || [],
			accent_color: v.accentColor,
			//triggers: triggers || [],
		};
	}

	async getOwnPersona(user: User, personaId: PersonaID): Promise<OwnPersonaResponse | null> {
		const v = await this.personaRepository.getPersona(user.id, personaId);
		if (v === null) return null;
		const triggers = await this.personaRepository.getPersonaTriggers(user.id, v.id);
		return {
			id: v.id.toString(),
			avatar: stripAvatarForUser(user, v.avatarHash),
			avatar_color: v.avatarColor,
			banner: stripBannerForUser(user, v.bannerHash),
			banner_color: v.bannerColor,
			internal_name: v.internalName,
			display_name: v.displayName || v.internalName,
			bio: v.bio,
			pronouns: v.pronouns,
			tags: v.tags || [],
			accent_color: v.accentColor,
			triggers: triggers || [],
		};
	}

	async createPersona(user: User, data: PersonaCreateRequest): Promise<PersonaID> {
		// TODO: it looks like this is the right place to do data validation, screening, and snowflake generation
		const personaId = createPersonaID(await this.snowflakeService.generate());
		contentModerationService.scanText(data.display_name, {
			userId: user.id,
			guildId: null,
			channelId: null,
			messageId: null,
			surface: 'profile_field',
		});
		contentModerationService.scanText(data.internal_name, {
			userId: user.id,
			guildId: null,
			channelId: null,
			messageId: null,
			surface: 'profile_field',
		});
		let preparedAvatar: PreparedAssetUpload | null = null;
		if (data.avatar) {
			// TODO: how to make this decay?
			preparedAvatar = await this.entityAssetService.prepareAssetUpload({
				assetType: 'avatar',
				entityType: 'persona',
				entityId: personaId,
				previousHash: null,
				base64Image: data.avatar,
				errorPath: 'avatar',
			});
		}
		let preparedBanner: PreparedAssetUpload | null = null;
		if (data.banner) {
			// TODO: how to make this decay?
			preparedBanner = await this.entityAssetService.prepareAssetUpload({
				assetType: 'banner',
				entityType: 'persona',
				entityId: personaId,
				previousHash: null,
				base64Image: data.banner,
				errorPath: 'banner',
			});
		}
		const newBio = await this.processBioUpdate({user, personaId, initialBio: null, bio: data.bio || null});
		const newPronouns = await this.processPronounsUpdate({
			user,
			personaId,
			initialPronouns: null,
			pronouns: data.pronouns || null,
		});
		const avatarHash = preparedAvatar?.newHash ?? null;
		const bannerHash = preparedBanner?.newHash ?? null;
		// Write to database via repository
		await this.personaRepository.createPersona({
			persona_id: personaId,
			owner_id: user.id,
			accent_color: data.accent_color || null,
			internal_name: data.internal_name,
			display_name: data.display_name || data.internal_name,
			bio: newBio || null,
			group: null,
			tags: null,
			pronouns: newPronouns || null,
			avatar_hash: avatarHash,
			banner_hash: bannerHash,
			avatar_color: preparedAvatar?.imageBuffer ? await deriveDominantAvatarColor(preparedAvatar?.imageBuffer) : null,
			banner_color: preparedBanner?.imageBuffer ? await deriveDominantAvatarColor(preparedBanner?.imageBuffer) : null,
			last_used_at: null,
		});
		await this.personaRepository.setPersonaTriggers(
			user.id,
			personaId,
			data.triggers.map((v) => new PersonaTrigger(v.prefix || null, v.suffix || null)),
		);
		return personaId;
	}

	async updatePersona(user: User, personaId: PersonaID, data: Partial<PersonaCreateRequest>): Promise<PersonaResponse> {
		const oldPersona = await this.personaRepository.getPersona(user.id, personaId);
		if (!oldPersona) {
			throw new Error('Persona not found');
		}
		// TODO: it looks like this is the right place to do data validation, screening, and snowflake generation
		if (data.internal_name)
			contentModerationService.scanText(data.internal_name, {
				userId: user.id,
				guildId: null,
				channelId: null,
				messageId: null,
				surface: 'profile_field',
			});
		if (data.display_name)
			contentModerationService.scanText(data.display_name, {
				userId: user.id,
				guildId: null,
				channelId: null,
				messageId: null,
				surface: 'profile_field',
			});
		const updates: PersonaFieldUpdates = {};
		const newBio = await this.processBioUpdate({user, personaId, initialBio: oldPersona.bio, bio: data.bio || null});
		const newPronouns = await this.processPronounsUpdate({
			user,
			personaId,
			initialPronouns: oldPersona.pronouns,
			pronouns: data.pronouns || null,
		});
		const newAvatar =
			data.avatar !== undefined
				? await this.processAvatarUpdate({
						user,
						personaId,
						initialAvatarHash: oldPersona.avatarHash,
						avatar: data.avatar,
						updates,
					})
				: undefined;
		const newBanner =
			data.banner !== undefined
				? await this.processBannerUpdate({
						user,
						personaId,
						initialBannerHash: oldPersona.bannerHash,
						banner: data.banner,
						updates,
					})
				: undefined;
		// Write to database via repository
		await this.personaRepository.updatePersona(user.id, personaId, {
			accent_color: data.accent_color,
			internal_name: data.internal_name,
			display_name: data.display_name,
			bio: newBio === false ? undefined : newBio,
			pronouns: newPronouns === false ? undefined : newPronouns,
			tags: data.tags,
			avatar_hash: newAvatar !== undefined ? updates.avatar_hash : undefined,
			banner_hash: newBanner !== undefined ? updates.banner_hash : undefined,
			avatar_color: newAvatar !== undefined ? updates.avatar_color : undefined,
			banner_color: newBanner !== undefined ? updates.banner_color : undefined,
		});
		if (data.triggers)
			await this.personaRepository.setPersonaTriggers(
				user.id,
				personaId,
				data.triggers.map((v) => new PersonaTrigger(v.prefix || null, v.suffix || null)),
			);
		if (data.triggers === null) await this.personaRepository.setPersonaTriggers(user.id, personaId, []);
		return (await this.getPersona(user, personaId))!;
	}

	async deletePersona(userId: UserID, personaId: PersonaID): Promise<void> {
		const oldPersona = await this.personaRepository.getPersona(userId, personaId);
		if (!oldPersona) throw new Error('Persona not found');
		await Promise.allSettled([
			oldPersona?.avatarHash
				? this.storageService.deleteAvatar({
						prefix: `avatars/${personaId}`,
						key: oldPersona?.avatarHash?.replace(/^a_/, '') || '',
					})
				: Promise.resolve(),
			//oldPersona?.bannerHash ? this.entityAssetService.queueAssetDeletion("banner", "persona", personaId, oldPersona?.bannerHash, undefined, "Persona deletion") : Promise.resolve(),
			oldPersona?.avatarHash
				? this.storageService.deleteAvatar({
						prefix: `banners/${personaId}`,
						key: oldPersona?.bannerHash?.replace(/^a_/, '') || '',
					})
				: Promise.resolve(),
		]);
		await this.personaRepository.deletePersona(userId, personaId);
	}

	private async processBioUpdate(params: {
		user: User;
		personaId: PersonaID;
		initialBio: string | null;
		bio: string | null;
	}): Promise<string | null | false> {
		const {user, personaId, initialBio, bio} = params;
		if (bio !== initialBio) {
			const bioRateLimit = await this.rateLimitService.checkLimit({
				identifier: `bio_change:${personaId}`,
				maxAttempts: 25,
				windowMs: ms('30 minutes'),
			});
			if (!bioRateLimit.allowed) {
				const minutes = Math.ceil((bioRateLimit.retryAfter || 0) / 60);
				throw InputValidationError.fromCode('bio', ValidationErrorCodes.BIO_CHANGED_TOO_MANY_TIMES, {minutes});
			}
			const ctx = createLimitMatchContext({user});
			const maxBioLength = resolveLimitSafe(
				this.limitConfigService.getConfigSnapshot(),
				ctx,
				'max_bio_length',
				MAX_BIO_LENGTH,
			);
			if (bio && bio.length > maxBioLength) {
				throw InputValidationError.fromCode('bio', ValidationErrorCodes.CONTENT_EXCEEDS_MAX_LENGTH, {
					maxLength: maxBioLength,
				});
			}
			let sanitizedBio = bio;
			if (bio) {
				sanitizedBio = await EmojiUtils.sanitizeCustomEmojis({
					content: bio,
					userId: user.id,
					webhookId: null,
					guildId: null,
					userRepository: this.userAccountRepository,
					guildRepository: this.guildRepository,
					limitConfigService: this.limitConfigService,
					allowCodeBlocks: false,
				});
			}
			if (
				sanitizedBio &&
				!isProfileSubstringExempt(user) &&
				profileSubstringBlocklistCache.containsBannedSubstring('bio', sanitizedBio)
			) {
				throw new ContentBlockedError();
			}
			return sanitizedBio;
		}
		return false;
	}
	private async processPronounsUpdate(params: {
		user: User;
		personaId: PersonaID;
		initialPronouns: string | null;
		pronouns: string | null;
	}): Promise<string | null | false> {
		const {user, pronouns, initialPronouns, personaId} = params;
		if (pronouns !== initialPronouns) {
			const pronounsRateLimit = await this.rateLimitService.checkLimit({
				identifier: `pronouns_change:${personaId}`,
				maxAttempts: 25,
				windowMs: ms('30 minutes'),
			});
			if (!pronounsRateLimit.allowed) {
				const minutes = Math.ceil((pronounsRateLimit.retryAfter || 0) / 60);
				throw InputValidationError.fromCode('pronouns', ValidationErrorCodes.PRONOUNS_CHANGED_TOO_MANY_TIMES, {
					minutes,
				});
			}
			if (
				pronouns &&
				!isProfileSubstringExempt(user) &&
				profileSubstringBlocklistCache.containsBannedSubstring('pronouns', pronouns)
			) {
				throw new ContentBlockedError();
			}
			return pronouns;
		}
		return false;
	}
	private async processAvatarUpdate(params: {
		user: User;
		personaId: PersonaID;
		initialAvatarHash: string | null;
		avatar: string | null;
		updates: PersonaFieldUpdates;
	}): Promise<PreparedAssetUpload | null> {
		const {user, personaId, initialAvatarHash, avatar, updates} = params;
		if (avatar === null) {
			updates.avatar_hash = null;
			updates.avatar_color = null;
			if (initialAvatarHash) {
				return await this.entityAssetService.prepareAssetUpload({
					assetType: 'avatar',
					entityType: 'persona',
					entityId: personaId,
					previousHash: initialAvatarHash,
					base64Image: null,
					errorPath: 'avatar',
				});
			}
			return null;
		}
		const avatarRateLimit = await this.rateLimitService.checkLimit({
			identifier: `avatar_change:${personaId}`,
			maxAttempts: 25,
			windowMs: ms('30 minutes'),
		});
		if (!avatarRateLimit.allowed) {
			const minutes = Math.ceil((avatarRateLimit.retryAfter || 0) / 60);
			throw InputValidationError.fromCode('avatar', ValidationErrorCodes.AVATAR_CHANGED_TOO_MANY_TIMES, {minutes});
		}
		const prepared = await this.entityAssetService.prepareAssetUpload({
			assetType: 'avatar',
			entityType: 'persona',
			entityId: personaId,
			previousHash: initialAvatarHash,
			base64Image: avatar,
			errorPath: 'avatar',
			uploader: user,
		});
		const ctx = createLimitMatchContext({user});
		const hasAnimatedAvatar = resolveLimitSafe(
			this.limitConfigService.getConfigSnapshot(),
			ctx,
			'feature_animated_avatar',
			0,
		);
		if (prepared.isAnimated && hasAnimatedAvatar === 0) {
			await this.entityAssetService.rollbackAssetUpload(prepared);
			throw InputValidationError.fromCode('avatar', ValidationErrorCodes.ANIMATED_AVATARS_REQUIRE_PREMIUM);
		}
		if (prepared.imageBuffer) {
			const derivedColor = await deriveDominantAvatarColor(prepared.imageBuffer);
			updates.avatar_color = derivedColor;
		}
		if (prepared.newHash !== initialAvatarHash) {
			updates.avatar_hash = prepared.newHash;
			return prepared;
		}
		return null;
	}
	private async processBannerUpdate(params: {
		user: User;
		personaId: PersonaID;
		initialBannerHash: string | null;
		banner: string | null;
		updates: PersonaFieldUpdates;
	}): Promise<PreparedAssetUpload | null> {
		const {user, personaId, initialBannerHash, banner, updates} = params;
		if (banner === null) {
			updates.banner_hash = null;
			updates.banner_color = null;
			if (initialBannerHash) {
				return await this.entityAssetService.prepareAssetUpload({
					assetType: 'banner',
					entityType: 'persona',
					entityId: personaId,
					previousHash: initialBannerHash,
					base64Image: null,
					errorPath: 'banner',
				});
			}
			return null;
		}
		const bannerRateLimit = await this.rateLimitService.checkLimit({
			identifier: `banner_change:${personaId}`,
			maxAttempts: 25,
			windowMs: ms('30 minutes'),
		});
		if (!bannerRateLimit.allowed) {
			const minutes = Math.ceil((bannerRateLimit.retryAfter || 0) / 60);
			throw InputValidationError.fromCode('banner', ValidationErrorCodes.BANNER_CHANGED_TOO_MANY_TIMES, {minutes});
		}
		const prepared = await this.entityAssetService.prepareAssetUpload({
			assetType: 'banner',
			entityType: 'persona',
			entityId: personaId,
			previousHash: initialBannerHash,
			base64Image: banner,
			errorPath: 'banner',
			uploader: user,
		});
		const ctx = createLimitMatchContext({user});
		const canUseBanner = resolveLimitSafe(
			this.limitConfigService.getConfigSnapshot(),
			ctx,
			'feature_animated_banner',
			0,
		);
		console.warn('HELLO');
		if (banner !== null && canUseBanner === 0) {
			await this.entityAssetService.rollbackAssetUpload(prepared);
			throw InputValidationError.fromCode('banner', ValidationErrorCodes.BANNERS_REQUIRE_PREMIUM);
		}
		if (prepared.imageBuffer) {
			const derivedColor = await deriveDominantAvatarColor(prepared.imageBuffer);
			updates.banner_color = derivedColor;
		}
		if (prepared.newHash !== initialBannerHash) {
			updates.banner_hash = prepared.newHash;
			return prepared;
		}
		return null;
	}
}
