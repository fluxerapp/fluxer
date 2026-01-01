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

import type {Task} from 'graphile-worker';
import type {Redis} from 'ioredis';
import type {UserID} from '~/BrandedTypes';
import {Config} from '~/Config';
import {DeletionReasons, UserFlags} from '~/Constants';
import type {IEmailService} from '~/infrastructure/IEmailService';
import type {RedisActivityTracker} from '~/infrastructure/RedisActivityTracker';
import {TestEmailService} from '~/infrastructure/TestEmailService';
import {Logger} from '~/Logger';
import type {User} from '~/Models';
import type {UserDeletionEligibilityService} from '~/user/services/UserDeletionEligibilityService';
import type {UserRepository} from '~/user/UserRepository';
import {getWorkerDependencies} from '../WorkerContext';

const INACTIVITY_THRESHOLD_MS = 2 * 365 * 24 * 60 * 60 * 1000;
const WARNING_EXPIRATION_MS = 30 * 24 * 60 * 60 * 1000;
const BATCH_SIZE = 100;

export interface InactivityCheckResult {
	warningsSent: number;
	deletionsScheduled: number;
	errors: number;
}

async function scheduleDeletion(userRepository: UserRepository, user: User, userId: UserID): Promise<void> {
	const gracePeriodMs = Config.deletionGracePeriodHours * 60 * 60 * 1000;
	const pendingDeletionAt = new Date(Date.now() + gracePeriodMs);

	await userRepository.patchUpsert(userId, {
		flags: user.flags | UserFlags.SELF_DELETED,
		pending_deletion_at: pendingDeletionAt,
	});

	await userRepository.addPendingDeletion(userId, pendingDeletionAt, DeletionReasons.INACTIVITY);

	Logger.debug({userId, pendingDeletionAt, reason: 'INACTIVITY'}, 'Scheduled inactive user for deletion');
}

interface ProcessInactivityDeletionsDeps {
	redis: Redis;
	userRepository: UserRepository;
	emailService: IEmailService;
	activityTracker: RedisActivityTracker;
	deletionEligibilityService: UserDeletionEligibilityService;
}

export async function processInactivityDeletionsCore(
	deps: ProcessInactivityDeletionsDeps,
): Promise<InactivityCheckResult> {
	const {userRepository, emailService, activityTracker, deletionEligibilityService} = deps;

	const result: InactivityCheckResult = {
		warningsSent: 0,
		deletionsScheduled: 0,
		errors: 0,
	};

	try {
		Logger.debug('Starting inactivity deletion check');

		const needsRebuild = await activityTracker.needsRebuild();
		if (needsRebuild) {
			Logger.info('Activity tracker needs rebuild, rebuilding from Cassandra');
			await activityTracker.rebuildActivities();
		}

		let lastUserId: UserID | undefined;
		let processedUsers = 0;

		while (true) {
			try {
				const users = await userRepository.listAllUsersPaginated(BATCH_SIZE, lastUserId);

				if (users.length === 0) {
					break;
				}

				for (const user of users) {
					try {
						const userId = user.id;

						if (user.pendingDeletionAt) {
							Logger.debug({userId}, 'User already pending deletion, skipping');
							continue;
						}

						if (user.isBot) {
							Logger.debug({userId}, 'User is a bot, skipping');
							continue;
						}

						if (user.flags & UserFlags.APP_STORE_REVIEWER) {
							Logger.debug({userId}, 'User is an app store reviewer, skipping');
							continue;
						}

						const lastActivity = await activityTracker.getActivity(userId);
						const now = new Date();
						const userInactiveMs = lastActivity ? now.getTime() - lastActivity.getTime() : Infinity;

						if (userInactiveMs < INACTIVITY_THRESHOLD_MS) {
							continue;
						}

						const isEligible = await deletionEligibilityService.isEligibleForInactivityDeletion(user);

						if (!isEligible) {
							Logger.debug({userId}, 'User not eligible for inactivity deletion');
							continue;
						}

						const hasWarningSent = await deletionEligibilityService.hasWarningSent(userId);

						if (hasWarningSent) {
							const hasGracePeriodExpired = await deletionEligibilityService.hasWarningGracePeriodExpired(userId);

							if (hasGracePeriodExpired) {
								Logger.debug({userId}, 'Warning grace period expired, scheduling deletion');
								await scheduleDeletion(userRepository, user, userId);
								result.deletionsScheduled++;
							} else {
								Logger.debug({userId}, 'Warning grace period still active, skipping (idempotency check)');
							}
							continue;
						}

						const isTestRun = Config.dev.testModeEnabled;
						const usingTestEmailService = emailService instanceof TestEmailService;
						const canSendEmail = !!user.email && (Config.email.enabled || usingTestEmailService || isTestRun);

						if (canSendEmail) {
							try {
								const deletionDate = new Date(now.getTime() + WARNING_EXPIRATION_MS);
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
					} catch (userError) {
						Logger.error({error: userError, userId: user.id}, 'Failed to process inactive user');
						result.errors++;
					}
				}

				processedUsers += users.length;
				lastUserId = users[users.length - 1].id;

				if (processedUsers % 1000 === 0) {
					Logger.debug(
						{processedUsers, warningsSent: result.warningsSent, deletionsScheduled: result.deletionsScheduled},
						'Inactivity deletion progress',
					);
				}
			} catch (batchError) {
				Logger.error({error: batchError}, 'Failed to process batch of users');
				result.errors++;
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
	} catch (error) {
		Logger.error({error}, 'Fatal error in processInactivityDeletions task');
		throw error;
	}
}

const processInactivityDeletions: Task = async (_payload, helpers) => {
	helpers.logger.debug('Processing processInactivityDeletions task');

	const {redis, userRepository, emailService, activityTracker, deletionEligibilityService} = getWorkerDependencies();

	await processInactivityDeletionsCore({
		redis,
		userRepository,
		emailService,
		activityTracker,
		deletionEligibilityService,
	});
};

export default processInactivityDeletions;
