// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DEFERRED_PHONE_ON_COMMUNITY_JOIN,
	imposePhoneRequirements,
	PremiumFlags,
	SuspiciousActivityFlags,
	UserFlags,
} from '@fluxer/constants/src/UserConstants';
import type {RpcSessionTimings} from '@fluxer/schema/src/domains/rpc/RpcSchemas';
import {Config} from '../Config';
import type {UserRow} from '../database/types/UserTypes';
import {mapGuildMemberToResponse} from '../guild/GuildModel';
import type {IGuildRepositoryAggregate} from '../guild/repositories/IGuildRepositoryAggregate';
import type {IDiscriminatorService} from '../infrastructure/DiscriminatorService';
import type {IGatewayService} from '../infrastructure/IGatewayService';
import type {UserCacheService} from '../infrastructure/UserCacheService';
import {Logger} from '../Logger';
import type {RequestCache} from '../middleware/RequestCacheMiddleware';
import type {User} from '../models/User';
import {countryRequiresInboundPhoneVerification} from '../risk/AbusePolicy';
import type {IUserRepository} from '../user/IUserRepository';
import type {PaymentRepository} from '../user/repositories/PaymentRepository';
import {createPremiumClearPatch, shouldStripExpiredPremium} from '../user/UserHelpers';
import {mapUserToPrivateResponse} from '../user/UserMappers';
import {
	createRpcTimingNode,
	RpcTimingRecorder,
	type RpcTimingSteps,
	startRpcTiming,
	timeRpcStep,
	timeRpcStepSync,
} from './RpcTimings';
import type {UserData} from './RpcTypes';

interface SessionStartUserRepository
	extends Pick<
		IUserRepository,
		'recordCountrySighting' | 'patchUpsert' | 'hasCountrySightingOutsideSet' | 'listUserIdsByLastActiveIp' | 'listUsers'
	> {}

interface SessionStartGuildRepository extends Pick<IGuildRepositoryAggregate, 'getMember' | 'upsertMember'> {}

interface SessionStartUserCacheService
	extends Pick<UserCacheService, 'getUserPartialResponse' | 'setUserPartialResponseFromUserInBackground'> {}

interface SessionStartGatewayService extends Pick<IGatewayService, 'dispatchGuild' | 'dispatchPresence'> {}

interface SessionStartDiscriminatorService extends Pick<IDiscriminatorService, 'generateDiscriminator'> {}

interface SessionStartPaymentRepository extends Pick<PaymentRepository, 'hasEverPaidSuccessfully'> {}

interface SessionStartDeps {
	userRepository: SessionStartUserRepository;
	guildRepository: SessionStartGuildRepository;
	userCacheService: SessionStartUserCacheService;
	gatewayService: SessionStartGatewayService;
	discriminatorService: SessionStartDiscriminatorService;
	paymentRepository: SessionStartPaymentRepository;
}

interface ProcessSessionStartParams {
	userData: UserData;
	requestCache: RequestCache;
	geoipCountryIso: string | null;
	clientIp: string | null;
}

interface ProcessSessionStartResult {
	user: User;
	flagsToUpdate: bigint | null;
	timings: RpcSessionTimings;
}

export class RpcSessionStartService {
	constructor(private readonly deps: SessionStartDeps) {}

	async processSessionStart(params: ProcessSessionStartParams): Promise<ProcessSessionStartResult> {
		const timings = new RpcTimingRecorder();
		const {userData, requestCache, geoipCountryIso, clientIp} = params;
		let user = userData.user;
		if (!user.isBot && geoipCountryIso) {
			timings.timeSync('queue_country_sighting_record', () => {
				this.recordCountrySightingInBackground(user, geoipCountryIso);
			});
		}
		const inboundPhonePolicySteps: RpcTimingSteps = {};
		const inboundPhonePolicyStartedAtNs = startRpcTiming();
		const inboundPhoneApplied = await this.applyConfiguredCountryInboundPhoneRequirement({
			user,
			geoipCountryIso,
			clientIp,
			timingSteps: inboundPhonePolicySteps,
		});
		timings.record('apply_country_inbound_phone_requirement', inboundPhonePolicyStartedAtNs, inboundPhonePolicySteps);
		if (inboundPhoneApplied) {
			user = inboundPhoneApplied;
			userData.user = inboundPhoneApplied;
			this.deps.userCacheService.setUserPartialResponseFromUserInBackground(inboundPhoneApplied, requestCache);
		}
		timings.timeSync('clear_expired_custom_status', () => {
			const userSettings = userData.settings;
			if (userSettings?.customStatus?.isExpired()) {
				const clearedSettings = Object.assign(Object.create(Object.getPrototypeOf(userSettings)), userSettings, {
					customStatus: null,
				});
				userData.settings = clearedSettings;
			}
		});
		let flagsToUpdate: bigint | null = null;
		let premiumFlagsToUpdate: number | null = null;
		let hadPremium = false;
		let isPremium = false;
		let needsPremiumStrip = false;
		let hasBeenSanitized = false;
		timings.timeSync('compute_session_and_premium_flags', () => {
			if (!(user.flags & UserFlags.HAS_SESSION_STARTED)) {
				flagsToUpdate = (flagsToUpdate ?? user.flags) | UserFlags.HAS_SESSION_STARTED;
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
		if (!isPremium && (user.premiumFlags & PremiumFlags.DISCRIMINATOR) !== 0) {
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
				const members = await timeRpcStep(sanitizePremiumSteps, 'fetch_guild_members', async () =>
					Promise.all(
						guildIdsToProcess.map(async (guildId) => {
							try {
								const member = await this.deps.guildRepository.getMember(guildId, user.id);
								return {guildId, member, error: null};
							} catch (error) {
								Logger.error(
									{userId: user.id.toString(), guildId: guildId.toString(), error},
									'Failed to fetch guild member for premium sanitization',
								);
								return {guildId, member: null, error};
							}
						}),
					),
				);
				const membersToSanitize = timeRpcStepSync(sanitizePremiumSteps, 'select_members_to_sanitize', () =>
					members.filter(
						({member, error}) =>
							!error &&
							member &&
							!member.isPremiumSanitized &&
							(member.avatarHash || member.bannerHash || member.bio || member.accentColor !== null),
					),
				);
				if (membersToSanitize.length > 0) {
					const updatePromises = membersToSanitize.map(({guildId, member}) =>
						this.deps.guildRepository
							.upsertMember({
								...member!.toRow(),
								is_premium_sanitized: true,
							})
							.then((updatedMember) => ({guildId, updatedMember, error: null})),
					);
					const updatedResults = await timeRpcStep(sanitizePremiumSteps, 'upsert_sanitized_guild_members', async () =>
						Promise.all(updatePromises),
					);
					const dispatchPromises = updatedResults.map(async ({guildId, updatedMember, error}) => {
						if (error) return;
						try {
							await this.deps.gatewayService.dispatchGuild({
								guildId,
								event: 'GUILD_MEMBER_UPDATE',
								data: await mapGuildMemberToResponse(updatedMember!, this.deps.userCacheService, requestCache),
							});
						} catch (error) {
							Logger.error(
								{userId: user.id.toString(), guildId: guildId.toString(), error},
								'Failed to dispatch guild member update after premium sanitization',
							);
						}
					});
					await timeRpcStep(sanitizePremiumSteps, 'dispatch_sanitized_member_updates', async () =>
						Promise.all(dispatchPromises),
					);
				}
			} catch (error) {
				Logger.error(
					{userId: user.id.toString(), guildIds: guildIdsToProcess.map(String), error},
					'Failed to sanitize guild member premium perks for multiple guilds',
				);
			}
			premiumFlagsToUpdate = (premiumFlagsToUpdate ?? user.premiumFlags) | PremiumFlags.PERKS_SANITIZED;
			await timeRpcStep(sanitizePremiumSteps, 'dispatch_user_update_after_premium_sanitization', async () => {
				try {
					await this.deps.gatewayService.dispatchPresence({
						userId: user.id,
						event: 'USER_UPDATE',
						data: mapUserToPrivateResponse(user),
					});
				} catch (error) {
					Logger.warn(
						{userId: user.id.toString(), error},
						'Failed to dispatch user update after premium perks sanitization',
					);
				}
			});
			timings.record('sanitize_expired_premium_perks', sanitizePremiumStartedAtNs, sanitizePremiumSteps);
		}
		const flagPatch: Partial<UserRow> = {};
		timings.timeSync('build_session_flag_patch', () => {
			if (flagsToUpdate !== null && flagsToUpdate !== user.flags) {
				flagPatch.flags = flagsToUpdate;
			}
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
		return {user, flagsToUpdate, timings: timings.finalize()};
	}

	private recordCountrySightingInBackground(user: User, countryIso: string): void {
		void this.deps.userRepository.recordCountrySighting(user.id, countryIso).catch((error) => {
			Logger.warn(
				{userId: user.id.toString(), countryIso, error},
				'Failed to record country sighting at RPC session start',
			);
		});
	}

	private async applyConfiguredCountryInboundPhoneRequirement({
		user,
		geoipCountryIso,
		clientIp,
		timingSteps,
	}: {
		user: User;
		geoipCountryIso: string | null;
		clientIp: string | null;
		timingSteps: RpcTimingSteps;
	}): Promise<User | null> {
		if (timeRpcStepSync(timingSteps, 'check_bot_user', () => user.isBot)) return null;
		if (timeRpcStepSync(timingSteps, 'check_verified_phone', () => user.hasVerifiedPhone)) return null;
		if (
			!timeRpcStepSync(timingSteps, 'check_configured_country_policy', () =>
				countryRequiresInboundPhoneVerification(geoipCountryIso),
			)
		) {
			return null;
		}
		if (
			timeRpcStepSync(timingSteps, 'check_not_suspicious_flag', () => (user.flags & UserFlags.NOT_SUSPICIOUS) !== 0n)
		) {
			Logger.info(
				{userId: user.id.toString(), countryIso: geoipCountryIso},
				'Skipping configured-country inbound phone requirement: user has NOT_SUSPICIOUS flag',
			);
			return null;
		}
		if (
			await timeRpcStep(timingSteps, 'check_prior_successful_payment', async () =>
				this.deps.paymentRepository.hasEverPaidSuccessfully(user.id),
			)
		) {
			Logger.info(
				{userId: user.id.toString(), countryIso: geoipCountryIso},
				'Skipping configured-country inbound phone requirement: user has prior successful payment',
			);
			return null;
		}
		const hasOutOfPolicyHistory = await timeRpcStep(timingSteps, 'check_out_of_policy_country_history', async () =>
			this.deps.userRepository.hasCountrySightingOutsideSet(user.id, Config.abusePolicy.inboundPhoneCountryCodes),
		);
		if (!hasOutOfPolicyHistory) {
			Logger.info(
				{userId: user.id.toString(), countryIso: geoipCountryIso},
				'Skipping configured-country inbound phone requirement: no out-of-policy country session history',
			);
			return null;
		}
		const sameIpHistorySteps: RpcTimingSteps = {};
		const sameIpHistoryStartedAtNs = startRpcTiming();
		const hasSameIpVerifiedPhoneHistory = await this.hasVerifiedPhoneHistoryOnSameIp(
			user.id,
			clientIp,
			sameIpHistorySteps,
		);
		timingSteps.check_same_ip_verified_phone_history = createRpcTimingNode(
			sameIpHistoryStartedAtNs,
			sameIpHistorySteps,
		);
		if (hasSameIpVerifiedPhoneHistory) {
			Logger.info(
				{userId: user.id.toString(), countryIso: geoipCountryIso},
				'Skipping configured-country inbound phone requirement: same-IP phone verification history exists',
			);
			return null;
		}
		const requiredFlags = timeRpcStepSync(
			timingSteps,
			'compute_required_inbound_phone_flags',
			() => SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE | SuspiciousActivityFlags.REQUIRE_INBOUND_PHONE_VERIFICATION,
		);
		if (
			timeRpcStepSync(
				timingSteps,
				'check_required_inbound_phone_flags_already_set',
				() =>
					(user.suspiciousActivityFlags & requiredFlags) === requiredFlags &&
					(user.suspiciousActivityFlags & DEFERRED_PHONE_ON_COMMUNITY_JOIN) === 0,
			)
		) {
			return null;
		}
		const newFlags = imposePhoneRequirements(user.suspiciousActivityFlags, requiredFlags);
		try {
			const updatedUser = await timeRpcStep(timingSteps, 'persist_inbound_phone_requirement', async () =>
				this.deps.userRepository.patchUpsert(user.id, {suspicious_activity_flags: newFlags}, user.toRow()),
			);
			if (updatedUser) {
				Logger.info(
					{userId: user.id.toString(), countryIso: geoipCountryIso, newFlags},
					'Applied inbound phone verification requirement on session start (configured country policy)',
				);
				return updatedUser;
			}
		} catch (error) {
			Logger.warn(
				{userId: user.id.toString(), countryIso: geoipCountryIso, error},
				'Failed to apply inbound phone verification requirement on session start',
			);
		}
		return null;
	}

	private async hasVerifiedPhoneHistoryOnSameIp(
		userId: User['id'],
		clientIp: string | null,
		timingSteps: RpcTimingSteps,
	): Promise<boolean> {
		if (!timeRpcStepSync(timingSteps, 'check_client_ip_present', () => clientIp !== null)) return false;
		const resolvedClientIp = clientIp!;
		try {
			const sameIpUsers = await timeRpcStep(timingSteps, 'list_user_ids_by_last_active_ip', async () =>
				this.deps.userRepository.listUserIdsByLastActiveIp(resolvedClientIp, Number.MAX_SAFE_INTEGER, 0),
			);
			const hasSameIpUserIds = timeRpcStepSync(
				timingSteps,
				'check_same_ip_user_ids_present',
				() => sameIpUsers.userIds.length > 0,
			);
			if (!hasSameIpUserIds) {
				return false;
			}
			const users = await timeRpcStep(timingSteps, 'list_same_ip_users', async () =>
				this.deps.userRepository.listUsers(sameIpUsers.userIds),
			);
			return timeRpcStepSync(timingSteps, 'scan_same_ip_users_for_verified_phone', () =>
				users.some((candidate) => candidate.hasVerifiedPhone),
			);
		} catch (error) {
			Logger.warn(
				{userId: userId.toString(), error},
				'Failed to evaluate same-IP phone verification history at RPC session start',
			);
			return false;
		}
	}
}
