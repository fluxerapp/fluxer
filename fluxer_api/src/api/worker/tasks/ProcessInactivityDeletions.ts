// SPDX-License-Identifier: AGPL-3.0-or-later

import {revokeAllAuthSessions} from '@app/api/auth/AuthSessionRevocation';
import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import {EnforcementDecisionRepository} from '@app/api/enforcement/EnforcementDecisionRepository';
import {clearExpiredSuspension} from '@app/api/enforcement/SuspensionState';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import type {KVActivityTracker} from '@app/api/infrastructure/KVActivityTracker';
import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {
	type PartialUserChangePropagationDeps,
	propagateUserUpdate,
} from '@app/api/user/services/PartialUserChangePropagation';
import {reschedulePendingDeletion} from '@app/api/user/services/PendingDeletionCoordinator';
import type {UserDeletionEligibilityService} from '@app/api/user/services/UserDeletionEligibilityService';
import {mapWithConcurrency} from '@app/api/utils/ConcurrencyUtils';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import {TestEmailService} from '@pkgs/email/src/TestEmailService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';

const BATCH_SIZE = 100;
const USER_PROCESSING_CONCURRENCY = 8;

interface InactivityCheckResult {
	warningsSent: number;
	deletionsScheduled: number;
	suspensionsCleared: number;
	errors: number;
}

async function scheduleDeletion(
	userRepository: UserRepository,
	deletionQueueService: KVAccountDeletionQueueService,
	gatewayService: IGatewayService,
	user: User,
	userId: UserID,
): Promise<void> {
	const gracePeriodMs = Config.deletionGracePeriodHours * ms('1 hour');
	const pendingDeletionAt = new Date(Date.now() + gracePeriodMs);
	await userRepository.updateDeletionSchedule(user, {
		flags: user.flags | UserFlags.SELF_DELETED,
		pending_deletion_at: pendingDeletionAt,
		deletion_reason_code: DeletionReasons.INACTIVITY,
		deletion_scheduled_by: SYSTEM_USER_ID,
		deletion_scheduled_at: new Date(),
	});
	await reschedulePendingDeletion({
		userId,
		currentPendingDeletionAt: user.pendingDeletionAt,
		nextPendingDeletionAt: pendingDeletionAt,
		deletionReasonCode: DeletionReasons.INACTIVITY,
		deletionQueue: deletionQueueService,
	});
	await revokeAllAuthSessions({users: userRepository, gateway: gatewayService}, userId);
	Logger.debug({userId, pendingDeletionAt, reason: 'INACTIVITY'}, 'Scheduled inactive user for deletion');
}

interface ProcessUserDeps {
	userRepository: UserRepository;
	deletionQueueService: KVAccountDeletionQueueService;
	gatewayService: IGatewayService;
	emailService: IEmailService;
	isEmailEnabled: () => Promise<boolean>;
	activityTracker: KVActivityTracker;
	deletionEligibilityService: UserDeletionEligibilityService;
	decisionRepository: Pick<EnforcementDecisionRepository, 'get' | 'endIfActive'>;
	userUpdateDeps: PartialUserChangePropagationDeps;
}

async function clearSuspensionIfExpired(
	user: User,
	deps: ProcessUserDeps,
	result: InactivityCheckResult,
): Promise<User> {
	const expiry = await clearExpiredSuspension({users: deps.userRepository, repository: deps.decisionRepository}, user);
	if (!expiry) return user;
	await propagateUserUpdate(deps.userUpdateDeps, expiry.previous, expiry.cleared);
	result.suspensionsCleared++;
	Logger.debug({userId: user.id}, 'Cleared an expired suspension');
	return expiry.cleared;
}

async function loadPageActivities(
	activityTracker: KVActivityTracker,
	users: ReadonlyArray<User>,
): Promise<ReadonlyMap<UserID, Date | null> | null> {
	try {
		return await activityTracker.getActivities(users.map((user) => user.id));
	} catch (error) {
		Logger.warn({error}, 'Batched activity lookup failed, falling back to per-user activity lookups');
		return null;
	}
}

async function processUser(
	listedUser: User,
	deps: ProcessUserDeps,
	pageActivities: ReadonlyMap<UserID, Date | null> | null,
	result: InactivityCheckResult,
): Promise<void> {
	const {
		userRepository,
		deletionQueueService,
		gatewayService,
		emailService,
		isEmailEnabled,
		activityTracker,
		deletionEligibilityService,
	} = deps;
	const user = await clearSuspensionIfExpired(listedUser, deps, result);
	const userId = user.id;
	if (user.pendingDeletionAt) {
		Logger.debug({userId}, 'User already pending deletion, skipping');
		return;
	}
	if (user.isBot) {
		Logger.debug({userId}, 'User is a bot, skipping');
		return;
	}
	if (user.flags & UserFlags.APP_STORE_REVIEWER) {
		Logger.debug({userId}, 'User is an app store reviewer, skipping');
		return;
	}
	const prefetchedActivity = pageActivities?.get(userId);
	const lastActivity =
		prefetchedActivity !== undefined ? prefetchedActivity : await activityTracker.getActivity(userId);
	const now = new Date();
	const userInactiveMs = lastActivity ? now.getTime() - lastActivity.getTime() : Infinity;
	if (userInactiveMs < ms('2 years')) {
		return;
	}
	const isEligible = await deletionEligibilityService.isEligibleForInactivityDeletion(user);
	if (!isEligible) {
		Logger.debug({userId}, 'User not eligible for inactivity deletion');
		return;
	}
	const hasWarningSent = await deletionEligibilityService.hasWarningSent(userId);
	if (hasWarningSent) {
		const hasGracePeriodExpired = await deletionEligibilityService.hasWarningGracePeriodExpired(userId);
		if (hasGracePeriodExpired) {
			Logger.debug({userId}, 'Warning grace period expired, scheduling deletion');
			await scheduleDeletion(userRepository, deletionQueueService, gatewayService, user, userId);
			result.deletionsScheduled++;
		} else {
			Logger.debug({userId}, 'Warning grace period still active, skipping (idempotency check)');
		}
		return;
	}
	const isTestRun = Config.dev.testModeEnabled;
	const usingTestEmailService = emailService instanceof TestEmailService;
	const canSendEmail = !!user.email && (usingTestEmailService || isTestRun || (await isEmailEnabled()));
	if (!canSendEmail) {
		return;
	}
	try {
		const deletionDate = new Date(now.getTime() + ms('30 days'));
		const sent = await emailService.sendInactivityWarningEmail(
			user.email,
			user.username,
			deletionDate,
			lastActivity ?? snowflakeToDate(BigInt(userId)),
			user.locale,
		);
		if (sent) {
			await deletionEligibilityService.markWarningSent(userId);
			result.warningsSent++;
			Logger.debug({userId, email: user.email}, 'Sent inactivity warning email');
		}
	} catch (emailError) {
		Logger.error({error: emailError, userId, email: user.email}, 'Failed to send inactivity warning email');
		result.errors++;
	}
}

interface ProcessInactivityDeletionsDeps {
	kvClient: IKVProvider;
	userRepository: UserRepository;
	deletionQueueService: KVAccountDeletionQueueService;
	gatewayService: IGatewayService;
	emailService: IEmailService;
	isEmailEnabled: () => Promise<boolean>;
	activityTracker: KVActivityTracker;
	deletionEligibilityService: UserDeletionEligibilityService;
	userUpdateDeps: PartialUserChangePropagationDeps;
}

export async function processInactivityDeletionsCore(
	deps: ProcessInactivityDeletionsDeps,
): Promise<InactivityCheckResult> {
	const {
		userRepository,
		deletionQueueService,
		gatewayService,
		emailService,
		isEmailEnabled,
		activityTracker,
		deletionEligibilityService,
		userUpdateDeps,
	} = deps;
	const result: InactivityCheckResult = {
		warningsSent: 0,
		deletionsScheduled: 0,
		suspensionsCleared: 0,
		errors: 0,
	};
	Logger.debug('Starting inactivity deletion check');
	const needsRebuild = await activityTracker.needsRebuild();
	if (needsRebuild) {
		Logger.info('Activity tracker needs rebuild, rebuilding from Cassandra');
		await activityTracker.rebuildActivities();
	}
	const userDeps: ProcessUserDeps = {
		userRepository,
		deletionQueueService,
		gatewayService,
		emailService,
		isEmailEnabled,
		activityTracker,
		deletionEligibilityService,
		decisionRepository: new EnforcementDecisionRepository(),
		userUpdateDeps,
	};
	let pageState: string | null = null;
	let processedUsers = 0;
	do {
		const page = await userRepository.scanAllUsersPage(BATCH_SIZE, pageState);
		pageState = page.pageState;
		const users = page.users;
		if (users.length === 0) continue;
		const pageActivities = await loadPageActivities(activityTracker, users);
		await mapWithConcurrency(users, USER_PROCESSING_CONCURRENCY, async (user) => {
			try {
				await processUser(user, userDeps, pageActivities, result);
			} catch (userError) {
				Logger.error({error: userError, userId: user.id}, 'Failed to process inactive user');
				result.errors++;
			}
		});
		processedUsers += users.length;
		if (processedUsers % 1000 === 0) {
			Logger.debug(
				{processedUsers, warningsSent: result.warningsSent, deletionsScheduled: result.deletionsScheduled},
				'Inactivity deletion progress',
			);
		}
	} while (pageState);
	Logger.info(
		{
			processedUsers,
			warningsSent: result.warningsSent,
			deletionsScheduled: result.deletionsScheduled,
			suspensionsCleared: result.suspensionsCleared,
			errors: result.errors,
		},
		'Completed inactivity deletion processing',
	);
	return result;
}

const processInactivityDeletions: WorkerTaskHandler = async (_payload, helpers) => {
	helpers.logger.debug('Processing processInactivityDeletions task');
	const {
		kvClient,
		userRepository,
		deletionQueueService,
		gatewayService,
		emailService,
		instanceConfigRepository,
		activityTracker,
		deletionEligibilityService,
		userCacheService,
		guildRepository,
	} = getWorkerDependencies();
	await processInactivityDeletionsCore({
		kvClient,
		userRepository,
		deletionQueueService,
		gatewayService,
		emailService,
		isEmailEnabled: () => instanceConfigRepository.isEmailEnabled(),
		activityTracker,
		deletionEligibilityService,
		userUpdateDeps: {userCacheService, gatewayService, userRepository, guildRepository},
	});
};

export default processInactivityDeletions;
