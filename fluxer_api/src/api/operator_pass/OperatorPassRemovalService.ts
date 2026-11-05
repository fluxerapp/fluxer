// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {
	OperatorPassRemovalEndReason,
	OperatorPassRemovalHistoryRow,
	OperatorPassRemovalRow,
} from '@app/api/database/types/OperatorPassTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {Logger} from '@app/api/Logger';
import type {OperatorPassServices} from '@app/api/middleware/OperatorPassServiceFactory';
import type {User} from '@app/api/models/User';
import {
	dispatchOperatorPassUserUpdate,
	isOperatorPassEntitled,
	loadOperatorPassEntitlementState,
} from '@app/api/operator_pass/OperatorPassEntitlementService';
import {
	keepsOperatorsThroughVisionary,
	type OperatorPassLabsRoleService,
} from '@app/api/operator_pass/OperatorPassLabsRole';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {operatorPassNow} from '@app/api/operator_pass/OperatorPassSchedule';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {OperatorPassRemovalLimitedFirstError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassRemovalLimitedFirstError';
import {OperatorPassRemovalNoticeRequiredError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassRemovalNoticeRequiredError';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import {ms} from 'itty-time';

const OPERATOR_PASS_REMOVAL_NOTICE_PERIOD_MS = ms('7 days');

const EXPIRY_PAGE_SIZE = 100;

interface OperatorPassRemovalParams {
	user: User;
	adminUserId: UserID;
	rule: string;
	facts: string;
	automated: boolean;
	serious: boolean;
	durationDays: number | null;
	notifyUser: boolean;
	dispute?: OperatorPassRemovalDispute | null;
}

interface OperatorPassRemovalDispute {
	id: string;
	deadline: Date;
}

function hasServedLimitedRemoval(history: ReadonlyArray<OperatorPassRemovalHistoryRow>, before: Date): boolean {
	return history.some(
		(entry) =>
			entry.removed_until != null && entry.end_reason !== 'restored' && entry.removed_at.getTime() < before.getTime(),
	);
}

interface OperatorPassRemovalResult {
	removal: OperatorPassRemovalRow;
	notified: boolean;
}

export class OperatorPassRemovalService {
	constructor(
		private readonly repository: OperatorPassRepository,
		private readonly userRepository: UserRepository,
		private readonly roleService: OperatorPassLabsRoleService,
		private readonly emailService: IEmailService,
		private readonly gatewayService: IGatewayService,
		private readonly services: Pick<OperatorPassServices, 'wallService' | 'holdingService'>,
	) {}

	async sendNotice(params: {
		user: User;
		adminUserId: UserID;
		rule: string;
		facts: string;
	}): Promise<OperatorPassRemovalResult> {
		const now = operatorPassNow();
		const existing = await this.repository.findRemoval(params.user.id);
		const removal: OperatorPassRemovalRow = {
			user_id: params.user.id,
			removal_state: existing?.removal_state === 'removed' ? 'removed' : 'notice',
			rule: params.rule,
			facts: params.facts,
			automated: existing?.automated ?? false,
			serious: existing?.serious ?? null,
			removed_until: existing?.removed_until ?? null,
			notice_sent_at: now,
			removed_at: existing?.removed_at ?? null,
			admin_user_id: params.adminUserId,
			created_at: existing?.created_at ?? now,
			updated_at: now,
			version: (existing?.version ?? 0) + 1,
		};
		await this.repository.saveRemoval(removal);
		const notified = params.user.email
			? await this.emailService.send(
					'operator_pass_removal_notice',
					params.user.email,
					{
						rule: params.rule,
						facts: params.facts,
						replyBy: new Date(now.getTime() + OPERATOR_PASS_REMOVAL_NOTICE_PERIOD_MS),
						visionary: keepsOperatorsThroughVisionary(params.user),
					},
					params.user.locale,
				)
			: false;
		return {removal, notified};
	}

	async remove(params: OperatorPassRemovalParams): Promise<OperatorPassRemovalResult> {
		const now = operatorPassNow();
		const existing = await this.repository.findRemoval(params.user.id);
		const permanent = params.durationDays === null;
		if (permanent && !params.serious) {
			const noticeSentAt = existing?.notice_sent_at ?? null;
			const history = await this.repository.listRemovalHistory(params.user.id);
			if (!hasServedLimitedRemoval(history, noticeSentAt ?? now)) {
				throw new OperatorPassRemovalLimitedFirstError();
			}
			if (noticeSentAt === null || now.getTime() - noticeSentAt.getTime() < OPERATOR_PASS_REMOVAL_NOTICE_PERIOD_MS) {
				throw new OperatorPassRemovalNoticeRequiredError();
			}
		}
		const removal: OperatorPassRemovalRow = {
			user_id: params.user.id,
			removal_state: 'removed',
			rule: params.rule,
			facts: params.facts,
			automated: params.automated,
			serious: params.serious,
			removed_until: permanent ? null : new Date(now.getTime() + (params.durationDays ?? 0) * ms('1 day')),
			notice_sent_at: existing?.notice_sent_at ?? null,
			removed_at: now,
			admin_user_id: params.adminUserId,
			created_at: existing?.created_at ?? now,
			updated_at: now,
			version: (existing?.version ?? 0) + 1,
		};
		if (existing?.removal_state === 'removed' && existing.removed_at) {
			await this.repository.endRemovalHistory(params.user.id, existing.removed_at, now, 'replaced');
		}
		await this.repository.saveRemovalHistory({
			user_id: params.user.id,
			removed_at: now,
			removed_until: removal.removed_until,
			rule: params.rule,
			facts: params.facts,
			automated: params.automated,
			serious: params.serious,
			notice_sent_at: removal.notice_sent_at,
			admin_user_id: params.adminUserId,
			ended_at: null,
			end_reason: null,
		});
		return this.applyRemoval(params.user, removal, existing, params.notifyUser, params.dispute ?? null);
	}

	async resumeRemoval(
		user: User,
		entry: OperatorPassRemovalHistoryRow,
		notifyUser: boolean,
		dispute: OperatorPassRemovalDispute | null = null,
		existingEndReason: OperatorPassRemovalEndReason | null = 'replaced',
	): Promise<OperatorPassRemovalResult> {
		const existing = await this.repository.findRemoval(user.id);
		if (
			existingEndReason !== null &&
			existing?.removal_state === 'removed' &&
			existing.removed_at &&
			existing.removed_at.getTime() !== entry.removed_at.getTime()
		) {
			await this.repository.endRemovalHistory(user.id, existing.removed_at, operatorPassNow(), existingEndReason);
		}
		const removal: OperatorPassRemovalRow = {
			user_id: user.id,
			removal_state: 'removed',
			rule: entry.rule,
			facts: entry.facts,
			automated: entry.automated,
			serious: entry.serious,
			removed_until: entry.removed_until ?? null,
			notice_sent_at: entry.notice_sent_at ?? null,
			removed_at: entry.removed_at,
			admin_user_id: entry.admin_user_id,
			created_at: existing?.created_at ?? entry.removed_at,
			updated_at: operatorPassNow(),
			version: (existing?.version ?? 0) + 1,
		};
		return this.applyRemoval(user, removal, existing, notifyUser, dispute);
	}

	private async applyRemoval(
		user: User,
		removal: OperatorPassRemovalRow,
		existing: OperatorPassRemovalRow | null,
		notifyUser: boolean,
		dispute: OperatorPassRemovalDispute | null,
	): Promise<OperatorPassRemovalResult> {
		const alreadySaved =
			existing?.removal_state === 'removed' && existing.removed_at?.getTime() === removal.removed_at?.getTime();
		if (!alreadySaved) {
			await this.repository.saveRemoval(removal);
		}
		if (existing?.removal_state !== 'removed') {
			await this.services.wallService.recordHidden(user.id, 'removal');
		}
		await this.services.wallService.endBusinessListing(user.id);
		await this.syncHolder(user.id);
		const notified =
			notifyUser && user.email
				? await this.emailService.send(
						'operator_pass_perks_removed',
						user.email,
						{
							until: removal.removed_until ?? null,
							rule: removal.rule,
							facts: removal.facts,
							automated: removal.automated,
							visionary: keepsOperatorsThroughVisionary(user),
							dispute,
						},
						user.locale,
					)
				: false;
		Logger.info(
			{userId: user.id, permanent: removal.removed_until == null, removedUntil: removal.removed_until},
			'Removed Operator Pass perks under section 8',
		);
		return {removal: alreadySaved && existing ? existing : removal, notified};
	}

	async restore(userId: UserID): Promise<boolean> {
		return this.endRemoval(userId, 'restored');
	}

	private async endRemoval(userId: UserID, reason: OperatorPassRemovalEndReason): Promise<boolean> {
		const existing = await this.repository.findRemoval(userId);
		if (!existing) {
			return false;
		}
		if (existing.removal_state === 'removed' && existing.removed_at) {
			const endedAt = operatorPassNow();
			await this.repository.endRemovalHistory(userId, existing.removed_at, endedAt, reason);
			const next = await this.findOpenRemoval(userId, endedAt);
			const user = next ? await this.userRepository.findUnique(userId) : null;
			if (next && user) {
				await this.resumeRemoval(user, next, true, null, null);
				Logger.info({userId, reason}, 'Ended an Operator Pass removal, and another open removal took its place');
				return true;
			}
		}
		await this.repository.deleteRemoval(userId);
		if (existing.removal_state === 'removed') {
			await this.services.wallService.recordShownAgain(userId);
		}
		await this.syncHolder(userId);
		Logger.info({userId, state: existing.removal_state, reason}, 'Restored Operator Pass perks');
		return true;
	}

	private async findOpenRemoval(userId: UserID, now: Date): Promise<OperatorPassRemovalHistoryRow | null> {
		const history = await this.repository.listRemovalHistory(userId);
		return (
			history.findLast(
				(entry) =>
					entry.ended_at == null && (entry.removed_until == null || entry.removed_until.getTime() > now.getTime()),
			) ?? null
		);
	}

	async restoreExpired(): Promise<number> {
		const now = operatorPassNow();
		let restored = 0;
		let pageState: string | null = null;
		do {
			const page = await this.repository.scanRemovals(EXPIRY_PAGE_SIZE, pageState);
			for (const removal of page.rows) {
				if (
					removal.removal_state === 'removed' &&
					removal.removed_until != null &&
					removal.removed_until.getTime() <= now.getTime()
				) {
					if (await this.endRemoval(removal.user_id, 'expired')) {
						restored += 1;
					}
				}
			}
			pageState = page.pageState;
		} while (pageState);
		return restored;
	}

	private async syncHolder(userId: UserID): Promise<void> {
		const user = await this.userRepository.findUnique(userId);
		const entitled =
			user !== null && isOperatorPassEntitled(await loadOperatorPassEntitlementState(this.repository, userId));
		await this.roleService.syncLabsRoles(userId, user, entitled);
		await dispatchOperatorPassUserUpdate(
			{repository: this.repository, userRepository: this.userRepository, gatewayService: this.gatewayService},
			userId,
		);
		await this.services.holdingService.dispatchPassesUpdate([userId]);
	}
}
