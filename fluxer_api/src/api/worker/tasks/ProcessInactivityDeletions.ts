// SPDX-License-Identifier: AGPL-3.0-or-later

import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import {TestEmailService} from '@pkgs/email/src/TestEmailService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';
import type {UserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import type {KVAccountDeletionQueueService} from '../../infrastructure/KVAccountDeletionQueueService';
import type {KVActivityTracker} from '../../infrastructure/KVActivityTracker';
import {Logger} from '../../Logger';
import type {User} from '../../models/User';
import type {UserRepository} from '../../user/repositories/UserRepository';
import {reschedulePendingDeletion} from '../../user/services/PendingDeletionCoordinator';
import type {UserDeletionEligibilityService} from '../../user/services/UserDeletionEligibilityService';
import {mapWithConcurrency} from '../../utils/ConcurrencyUtils';
import {getWorkerDependencies} from '../WorkerContext';

const BATCH_SIZE = 100;
const USER_PROCESSING_CONCURRENCY = 8;

interface InactivityCheckResult {
	warningsSent: number;
	deletionsScheduled: number;
	errors: number;
}

async function scheduleDeletion(
	userRepository: UserRepository,
	deletionQueueService: KVAccountDeletionQueueService,
	user: User,
	userId: UserID,
): Promise<void> {
	const gracePeriodMs = Config.deletionGracePeriodHours * ms('1 hour');
	const pendingDeletionAt = new Date(Date.now() + gracePeriodMs);
	await userRepository.patchUpsert(
		userId,
		{
			flags: user.flags | UserFlags.SELF_DELETED,
			pending_deletion_at: pendingDeletionAt,
			deletion_reason_code: DeletionReasons.INACTIVITY,
		},
		user.toRow(),
	);
	await reschedulePendingDeletion({
		userId,
		currentPendingDeletionAt: user.pendingDeletionAt,
		nextPendingDeletionAt: pendingDeletionAt,
		deletionReasonCode: DeletionReasons.INACTIVITY,
		userRepository,
		deletionQueue: deletionQueueService,
	});
	Logger.debug({userId, pendingDeletionAt, reason: 'INACTIVITY'}, 'Scheduled inactive user for deletion');
}

interface ProcessUserDeps {
	userRepository: UserRepository;
	deletionQueueService: KVAccountDeletionQueueService;
	emailService: IEmailService;
	isEmailEnabled: () => Promise<boolean>;
	activityTracker: KVActivityTracker;
	deletionEligibilityService: UserDeletionEligibilityService;
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
	user: User,
	deps: ProcessUserDeps,
	pageActivities: ReadonlyMap<UserID, Date | null> | null,
	result: InactivityCheckResult,
): Promise<void> {
	const {
		userRepository,
		deletionQueueService,
		emailService,
		isEmailEnabled,
		activityTracker,
		deletionEligibilityService,
	} = deps;
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
			await scheduleDeletion(userRepository, deletionQueueService, user, userId);
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
			lastActivity || new Date(0),
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
	emailService: IEmailService;
	isEmailEnabled: () => Promise<boolean>;
	activityTracker: KVActivityTracker;
	deletionEligibilityService: UserDeletionEligibilityService;
}

export async function processInactivityDeletionsCore(
	deps: ProcessInactivityDeletionsDeps,
): Promise<InactivityCheckResult> {
	const {
		userRepository,
		deletionQueueService,
		emailService,
		isEmailEnabled,
		activityTracker,
		deletionEligibilityService,
	} = deps;
	const result: InactivityCheckResult = {
		warningsSent: 0,
		deletionsScheduled: 0,
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
		emailService,
		isEmailEnabled,
		activityTracker,
		deletionEligibilityService,
	};
	let pageState: string | null = null;
	let processedUsers = 0;
	while (true) {
		const page = await userRepository.scanAllUsersPage(BATCH_SIZE, pageState);
		const users = page.users;
		if (users.length === 0) {
			break;
		}
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
		pageState = page.pageState;
		if (processedUsers % 1000 === 0) {
			Logger.debug(
				{processedUsers, warningsSent: result.warningsSent, deletionsScheduled: result.deletionsScheduled},
				'Inactivity deletion progress',
			);
		}
		if (!pageState) {
			break;
		}
	}
	Logger.info(
		{
			processedUsers,
			warningsSent: result.warningsSent,
			deletionsScheduled: result.deletionsScheduled,
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
		emailService,
		instanceConfigRepository,
		activityTracker,
		deletionEligibilityService,
	} = getWorkerDependencies();
	await processInactivityDeletionsCore({
		kvClient,
		userRepository,
		deletionQueueService,
		emailService,
		isEmailEnabled: () => instanceConfigRepository.isEmailEnabled(),
		activityTracker,
		deletionEligibilityService,
	});
};

export default processInactivityDeletions;
