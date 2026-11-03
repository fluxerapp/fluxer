// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserRow} from '@app/api/database/types/UserTypes';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {sanitizeExpiredPremiumMemberPerks} from '@app/api/guild/services/member/PremiumMemberSanitization';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import type {DiscriminatorService} from '@app/api/infrastructure/DiscriminatorService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {getCachedTagStyle} from '@app/api/instance/AccountIdentityModeCache';
import {Logger} from '@app/api/Logger';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {User} from '@app/api/models/User';
import {RpcTimingRecorder, type RpcTimingSteps, startRpcTiming, timeRpcStep} from '@app/api/rpc/RpcTimings';
import type {UserData} from '@app/api/rpc/RpcTypes';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {createPremiumClearPatch, shouldStripExpiredPremium} from '@app/api/user/UserHelpers';
import {dispatchUserUpdate} from '@app/api/user/UserMappers';
import {TagStyles} from '@fluxer/constants/src/AccountIdentityConstants';
import {PremiumFlags, UserFlags} from '@fluxer/constants/src/UserConstants';
import type {RpcSessionTimings} from '@fluxer/schema/src/domains/rpc/RpcSchemas';

interface SessionStartUserRepository extends Pick<UserRepository, 'patchUpsert' | 'updateFlags'> {}

interface SessionStartGuildRepository extends Pick<GuildRepository, 'getMember' | 'upsertMember'> {}

interface SessionStartUserCacheService
	extends Pick<UserCacheService, 'getUserPartialResponse' | 'setUserPartialResponseFromUserInBackground'> {}

interface SessionStartGatewayService extends Pick<IGatewayService, 'dispatchGuild' | 'dispatchPresence'> {}

interface SessionStartDiscriminatorService extends Pick<DiscriminatorService, 'generateDiscriminator'> {}

interface SessionStartDeps {
	userRepository: SessionStartUserRepository;
	guildRepository: SessionStartGuildRepository;
	userCacheService: SessionStartUserCacheService;
	gatewayService: SessionStartGatewayService;
	discriminatorService: SessionStartDiscriminatorService;
}

interface ProcessSessionStartParams {
	userData: UserData;
	requestCache: RequestCache;
	geoipCountryIso: string | null;
	clientIp: string | null;
}

interface ProcessSessionStartResult {
	user: User;
	timings: RpcSessionTimings;
}

export class RpcSessionStartService {
	constructor(private readonly deps: SessionStartDeps) {}

	async processSessionStart(params: ProcessSessionStartParams): Promise<ProcessSessionStartResult> {
		const timings = new RpcTimingRecorder();
		const {userData, requestCache, geoipCountryIso, clientIp} = params;
		let user = userData.user;
		const startedUser = user;
		void emitActivity(
			'session_started',
			startedUser.id.toString(),
			{
				user_id: startedUser.id.toString(),
				is_bot: startedUser.isBot,
				flags: startedUser.flags.toString(),
				has_ever_paid: startedUser.hasEverPurchased,
			},
			{
				ip: clientIp,
				country: geoipCountryIso,
				ua: null,
				locale: startedUser.locale,
				channel: 'other',
				request_id: null,
			},
		);
		timings.timeSync('clear_expired_custom_status', () => {
			const userSettings = userData.settings;
			if (userSettings?.customStatus?.isExpired()) {
				const clearedSettings = Object.assign(Object.create(Object.getPrototypeOf(userSettings)), userSettings, {
					customStatus: null,
				});
				userData.settings = clearedSettings;
			}
		});
		let needsSessionStartedFlag = false;
		let premiumFlagsToUpdate: number | null = null;
		let hadPremium = false;
		let isPremium = false;
		let needsPremiumStrip = false;
		let hasBeenSanitized = false;
		timings.timeSync('compute_session_and_premium_flags', () => {
			if (!(user.flags & UserFlags.HAS_SESSION_STARTED)) {
				needsSessionStartedFlag = true;
			}
			hadPremium = user.premiumType != null && user.premiumType > 0;
			isPremium = user.isPremium();
			needsPremiumStrip = shouldStripExpiredPremium(user);
			hasBeenSanitized = (user.premiumFlags & PremiumFlags.PERKS_SANITIZED) !== 0;
		});
		if (needsPremiumStrip) {
			await timings.time('strip_expired_premium', async () => {
				try {
					const strippedUser = await this.deps.userRepository.patchUpsert(
						user.id,
						createPremiumClearPatch(),
						user.toRow(),
					);
					if (strippedUser) {
						user = strippedUser;
						userData.user = strippedUser;
						this.deps.userCacheService.setUserPartialResponseFromUserInBackground(strippedUser, requestCache);
					}
				} catch (error) {
					Logger.warn({userId: user.id.toString(), error}, 'Failed to strip expired premium on RPC session start');
				}
			});
		}
		if (
			!isPremium &&
			(user.premiumFlags & PremiumFlags.DISCRIMINATOR) !== 0 &&
			getCachedTagStyle() === TagStyles.RANDOM
		) {
			const resetDiscriminatorSteps: RpcTimingSteps = {};
			const resetDiscriminatorStartedAtNs = startRpcTiming();
			try {
				const discriminatorResult = await timeRpcStep(resetDiscriminatorSteps, 'generate_discriminator', async () =>
					this.deps.discriminatorService.generateDiscriminator({
						username: user.username,
						user,
					}),
				);
				if (discriminatorResult.available && discriminatorResult.discriminator !== -1) {
					const updatedUser = await timeRpcStep(resetDiscriminatorSteps, 'persist_discriminator', async () =>
						this.deps.userRepository.patchUpsert(
							user.id,
							{
								discriminator: discriminatorResult.discriminator,
							},
							user.toRow(),
						),
					);
					if (updatedUser) {
						Object.assign(user, updatedUser);
						userData.user = user;
						this.deps.userCacheService.setUserPartialResponseFromUserInBackground(user, requestCache);
						premiumFlagsToUpdate = (premiumFlagsToUpdate ?? user.premiumFlags) & ~PremiumFlags.DISCRIMINATOR;
					}
				}
			} catch (error) {
				Logger.error({userId: user.id.toString(), error}, 'Failed to reset discriminator after premium expired');
			} finally {
				timings.record('reset_expired_premium_discriminator', resetDiscriminatorStartedAtNs, resetDiscriminatorSteps);
			}
		}
		if (hadPremium && !isPremium && !hasBeenSanitized) {
			const sanitizePremiumSteps: RpcTimingSteps = {};
			const sanitizePremiumStartedAtNs = startRpcTiming();
			const guildIdsToProcess = userData.guildIds;
			try {
				await timeRpcStep(sanitizePremiumSteps, 'sanitize_expired_premium_member_perks', () =>
					sanitizeExpiredPremiumMemberPerks({
						userId: user.id,
						guildIds: guildIdsToProcess,
						guildRepository: this.deps.guildRepository,
						gatewayService: this.deps.gatewayService,
						userCacheService: this.deps.userCacheService,
						requestCache,
					}),
				);
			} catch (error) {
				Logger.error(
					{userId: user.id.toString(), guildIds: guildIdsToProcess.map(String), error},
					'Failed to sanitize guild member premium perks for multiple guilds',
				);
			}
			premiumFlagsToUpdate = (premiumFlagsToUpdate ?? user.premiumFlags) | PremiumFlags.PERKS_SANITIZED;
			await timeRpcStep(sanitizePremiumSteps, 'dispatch_user_update_after_premium_sanitization', async () => {
				try {
					await dispatchUserUpdate(this.deps.gatewayService, user);
				} catch (error) {
					Logger.warn(
						{userId: user.id.toString(), error},
						'Failed to dispatch user update after premium perks sanitization',
					);
				}
			});
			timings.record('sanitize_expired_premium_perks', sanitizePremiumStartedAtNs, sanitizePremiumSteps);
		}
		if (needsSessionStartedFlag) {
			await timings.time('persist_session_started_flag', async () => {
				try {
					const updatedUser = await this.deps.userRepository.updateFlags(
						user.id,
						(flags) => flags | UserFlags.HAS_SESSION_STARTED,
					);
					if (updatedUser) {
						user = updatedUser;
						userData.user = updatedUser;
					}
				} catch (error) {
					Logger.warn({userId: user.id, error}, 'Failed to persist the session started flag');
				}
			});
		}
		const flagPatch: Partial<UserRow> = {};
		timings.timeSync('build_session_flag_patch', () => {
			if (premiumFlagsToUpdate !== null && premiumFlagsToUpdate !== user.premiumFlags) {
				flagPatch.premium_flags = premiumFlagsToUpdate;
			}
		});
		if (Object.keys(flagPatch).length > 0) {
			await timings.time('persist_session_flags', async () => {
				try {
					const updatedUser = await this.deps.userRepository.patchUpsert(user.id, flagPatch, user.toRow());
					if (updatedUser) {
						user = updatedUser;
						userData.user = updatedUser;
					}
				} catch (error) {
					Logger.warn({userId: user.id, error}, 'Failed to persist flags during session start');
				}
			});
		}
		return {user, timings: timings.finalize()};
	}
}
