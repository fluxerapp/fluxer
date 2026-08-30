// SPDX-License-Identifier: AGPL-3.0-or-later

import {PremiumFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {Config} from '../../Config';
import {mapGuildMemberToResponse} from '../../guild/GuildModel';
import type {IGuildRepositoryAggregate} from '../../guild/repositories/IGuildRepositoryAggregate';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {PremiumStateReconciliationQueueService} from '../../infrastructure/PremiumStateReconciliationQueueService';
import type {UserCacheService} from '../../infrastructure/UserCacheService';
import {Logger} from '../../Logger';
import {createRequestCache} from '../../middleware/RequestCacheMiddleware';
import type {User} from '../../models/User';
import type {IUserRepository} from '../../user/IUserRepository';
import {checkIsPremium, createPremiumClearPatch, shouldStripExpiredPremium} from '../../user/UserHelpers';
import {mapUserToPrivateResponse} from '../../user/UserMappers';
import {getWorkerDependencies} from '../WorkerContext';

const BATCH_SIZE = 100;

interface SweepResult {
	processed: number;
	stripped: number;
	sanitized: number;
	reconcileEnqueued: number;
	skipped: number;
	failed: number;
}

interface SweepDeps {
	userRepository: IUserRepository;
	guildRepository: IGuildRepositoryAggregate;
	userCacheService: UserCacheService;
	gatewayService: IGatewayService;
	premiumStateReconciliationQueueService: PremiumStateReconciliationQueueService;
}

async function sanitizeGuildMemberPerks(user: User, deps: SweepDeps): Promise<boolean> {
	const guildIds = await deps.userRepository.getUserGuildIds(user.id);
	if (guildIds.length === 0) {
		return false;
	}
	const requestCache = createRequestCache();
	const members = await Promise.all(
		guildIds.map(async (guildId) => {
			try {
				const member = await deps.guildRepository.getMember(guildId, user.id);
				return {guildId, member, error: null};
			} catch (error) {
				Logger.error(
					{userId: user.id.toString(), guildId: guildId.toString(), error},
					'Failed to fetch guild member for expired premium sweep sanitization',
				);
				return {guildId, member: null, error};
			}
		}),
	);
	const membersToSanitize = members.filter(
		({member, error}) =>
			!error &&
			member &&
			!member.isPremiumSanitized &&
			(member.avatarHash || member.bannerHash || member.bio || member.accentColor !== null),
	);
	if (membersToSanitize.length === 0) {
		return false;
	}
	const updatePromises = membersToSanitize.map(({guildId, member}) =>
		deps.guildRepository
			.upsertMember({
				...member!.toRow(),
				is_premium_sanitized: true,
			})
			.then((updatedMember) => ({guildId, updatedMember, error: null}))
			.catch((error) => {
				Logger.error(
					{userId: user.id.toString(), guildId: guildId.toString(), error},
					'Failed to upsert guild member during expired premium sweep sanitization',
				);
				return {guildId, updatedMember: null, error};
			}),
	);
	const updatedResults = await Promise.all(updatePromises);
	const dispatchPromises = updatedResults.map(async ({guildId, updatedMember, error}) => {
		if (error || !updatedMember) return;
		try {
			await deps.gatewayService.dispatchGuild({
				guildId,
				event: 'GUILD_MEMBER_UPDATE',
				data: await mapGuildMemberToResponse(updatedMember, deps.userCacheService, requestCache),
			});
		} catch (error) {
			Logger.error(
				{userId: user.id.toString(), guildId: guildId.toString(), error},
				'Failed to dispatch guild member update during expired premium sweep sanitization',
			);
		}
	});
	await Promise.all(dispatchPromises);
	return true;
}

async function processUser(user: User, deps: SweepDeps, result: SweepResult): Promise<void> {
	if (user.isBot) {
		result.skipped += 1;
		return;
	}
	if ((user.premiumType ?? 0) <= 0) {
		result.skipped += 1;
		return;
	}
	if (user.premiumType === UserPremiumTypes.LIFETIME) {
		result.skipped += 1;
		return;
	}
	if ((user.premiumFlags & PremiumFlags.ENABLED_OVERRIDE) !== 0) {
		result.skipped += 1;
		return;
	}
	if (!shouldStripExpiredPremium(user)) {
		if (user.premiumType === UserPremiumTypes.SUBSCRIPTION && (user.stripeSubscriptionId || user.stripeCustomerId)) {
			try {
				await deps.premiumStateReconciliationQueueService.enqueueUser(user.id);
				result.reconcileEnqueued += 1;
			} catch (error) {
				Logger.warn(
					{userId: user.id.toString(), error},
					'Failed to enqueue active subscription user for reconciliation during sweep',
				);
			}
		}
		result.skipped += 1;
		return;
	}
	if (checkIsPremium(user)) {
		result.skipped += 1;
		return;
	}
	try {
		let currentUser = await deps.userRepository.patchUpsert(user.id, createPremiumClearPatch(), user.toRow());
		deps.userCacheService.setUserPartialResponseFromUserInBackground(currentUser);
		await deps.gatewayService.dispatchPresence({
			userId: currentUser.id,
			event: 'USER_UPDATE',
			data: mapUserToPrivateResponse(currentUser),
		});
		result.stripped += 1;
		const hasBeenSanitized = !!(user.premiumFlags & PremiumFlags.PERKS_SANITIZED);
		if (!hasBeenSanitized) {
			try {
				const didSanitize = await sanitizeGuildMemberPerks(user, deps);
				if (didSanitize) {
					result.sanitized += 1;
				}
			} catch (error) {
				Logger.error(
					{userId: user.id.toString(), error},
					'Failed to sanitize guild member perks during expired premium sweep',
				);
			}
			const flaggedUser = await deps.userRepository.patchUpsert(
				currentUser.id,
				{premium_flags: currentUser.premiumFlags | PremiumFlags.PERKS_SANITIZED},
				currentUser.toRow(),
			);
			currentUser = flaggedUser;
			deps.userCacheService.setUserPartialResponseFromUserInBackground(currentUser);
			await deps.gatewayService.dispatchPresence({
				userId: currentUser.id,
				event: 'USER_UPDATE',
				data: mapUserToPrivateResponse(currentUser),
			});
		}
	} catch (error) {
		Logger.error({userId: user.id.toString(), error}, 'Failed to strip expired premium during sweep');
		result.failed += 1;
	}
}

async function processExpiredPremiumSweepCore(deps: SweepDeps): Promise<SweepResult> {
	const result: SweepResult = {
		processed: 0,
		stripped: 0,
		sanitized: 0,
		reconcileEnqueued: 0,
		skipped: 0,
		failed: 0,
	};
	if (Config.instance.selfHosted) {
		Logger.debug('Skipping expired premium sweep on a self-hosted instance');
		return result;
	}
	Logger.debug('Starting expired premium sweep');
	let pageState: string | null = null;
	while (true) {
		const page = await deps.userRepository.scanAllUsersPage(BATCH_SIZE, pageState);
		const users = page.users;
		if (users.length === 0) {
			break;
		}
		for (const user of users) {
			try {
				await processUser(user, deps, result);
			} catch (error) {
				Logger.error({error, userId: user.id}, 'Unhandled error processing user in expired premium sweep');
				result.failed += 1;
			}
		}
		result.processed += users.length;
		pageState = page.pageState;
		if (result.processed % 1000 === 0) {
			Logger.debug(
				{processed: result.processed, stripped: result.stripped, sanitized: result.sanitized},
				'Expired premium sweep progress',
			);
		}
		if (!pageState) {
			break;
		}
	}
	Logger.info(
		{
			processed: result.processed,
			stripped: result.stripped,
			sanitized: result.sanitized,
			reconcileEnqueued: result.reconcileEnqueued,
			skipped: result.skipped,
			failed: result.failed,
		},
		'Completed expired premium sweep',
	);
	return result;
}

const processExpiredPremiumSweep: WorkerTaskHandler = async (_payload, helpers) => {
	helpers.logger.debug('Processing expired premium sweep task');
	const {userRepository, guildRepository, userCacheService, gatewayService, premiumStateReconciliationQueueService} =
		getWorkerDependencies();
	await processExpiredPremiumSweepCore({
		userRepository,
		guildRepository,
		userCacheService,
		gatewayService,
		premiumStateReconciliationQueueService,
	});
};

export default processExpiredPremiumSweep;
