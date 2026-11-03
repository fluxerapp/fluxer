// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createRoleID, type GuildID, type RoleID, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import type {OperatorPassLabsLeftReason} from '@app/api/database/types/OperatorPassTypes';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {GuildService} from '@app/api/guild/services/GuildService';
import {Logger} from '@app/api/Logger';
import {createRequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {getWorkerService} from '@app/api/middleware/ServiceRegistry';
import type {User} from '@app/api/models/User';
import {
	isOperatorPassEntitled,
	loadOperatorPassEntitlementState,
} from '@app/api/operator_pass/OperatorPassEntitlementService';
import {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {isAccountLimited} from '@app/api/user/AccountLimit';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {JoinSourceTypes} from '@fluxer/constants/src/GuildConstants';
import {PremiumFlags, UserFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {BannedFromGuildError} from '@fluxer/errors/src/domains/guild/BannedFromGuildError';
import {IpBannedFromGuildError} from '@fluxer/errors/src/domains/guild/IpBannedFromGuildError';
import {ms} from 'itty-time';

export const OPERATOR_PASS_LABS_INVITE_URL = 'https://fluxer.gg/fluxer-labs';

export const OPERATOR_PASS_LABS_VISIONARY_ROLE_SYNC_TASK = 'operatorPassLabsVisionaryRoleSync';

export const OPERATOR_PASS_LABS_DELIVERY_TASK = 'operatorPassLabsDelivery';

export interface LabsDeliveryJob {
	userId: UserID;
	greeting: boolean | null;
	origin: string;
	retry: number;
}

export interface LabsReconcileSummary {
	checked: number;
	queued: number;
}

type LabsDelivery = 'added' | 'done' | 'retry';

type LabsAdd = 'added' | 'left' | 'banned' | 'blocked' | 'retry';

function labsJoinStatusForAdd(added: LabsAdd): LabsJoinStatus {
	switch (added) {
		case 'added':
			return 'joined';
		case 'left':
		case 'banned':
			return added;
		default:
			return 'unavailable';
	}
}

const LABS_RETRY_DELAYS_MS = [
	ms('1 minute'),
	ms('5 minutes'),
	ms('15 minutes'),
	ms('1 hour'),
	ms('3 hours'),
	ms('6 hours'),
	ms('12 hours'),
];

export interface LabsVisionaryRoleSyncSummary {
	granted: number;
	removed: number;
	failed: number;
}

interface LabsOperatorRole {
	guildId: GuildID;
	roleId: RoleID;
}

const labsRepository = new OperatorPassRepository();

const LABS_BLOCKING_FLAGS = UserFlags.DELETED | UserFlags.SELF_DELETED | UserFlags.DISABLED | UserFlags.SPAMMER;

type LabsAccount = Pick<
	User,
	| 'id'
	| 'premiumType'
	| 'isBot'
	| 'isSystem'
	| 'flags'
	| 'pendingDeletionAt'
	| 'deletionStartedAt'
	| 'contentHiddenSince'
	| 'tempBannedUntil'
	| 'premiumFlags'
	| 'hiddenProfileBadges'
>;

type VisionaryAccount = Pick<User, 'id' | 'premiumType' | 'premiumFlags' | 'hiddenProfileBadges'>;

function showsVisionaryInLabs(user: Omit<VisionaryAccount, 'id'>): boolean {
	return (
		user.premiumType === UserPremiumTypes.LIFETIME &&
		!user.hiddenProfileBadges.has('visionary') &&
		(user.premiumFlags & PremiumFlags.BADGE_MASKED) === 0
	);
}

export function isBlockedFromLabsAutoJoin(user: Omit<LabsAccount, 'id' | 'premiumType'>, now: Date): boolean {
	return (
		user.isBot ||
		user.isSystem ||
		(user.flags & LABS_BLOCKING_FLAGS) !== 0n ||
		isAccountLimited(user) ||
		user.pendingDeletionAt != null ||
		user.deletionStartedAt != null ||
		user.contentHiddenSince != null ||
		(user.tempBannedUntil != null && user.tempBannedUntil.getTime() > now.getTime())
	);
}

function isGoneForGood(user: Omit<LabsAccount, 'id' | 'premiumType'>): boolean {
	return (
		user.isBot ||
		user.isSystem ||
		(user.flags & (UserFlags.DELETED | UserFlags.SELF_DELETED)) !== 0n ||
		user.deletionStartedAt != null
	);
}

export function getLabsOperatorsChannelId(): bigint | null {
	return getLabsOperatorRole() ? Config.operatorPass.labsOperatorsChannelId : null;
}

export function getLabsGuildId(): GuildID | null {
	return getLabsOperatorRole()?.guildId ?? null;
}

export type LabsJoinStatus = 'joined' | 'left' | 'banned' | 'unavailable' | 'not_held';

export async function recordOperatorPassLabsDeparture(
	guildId: GuildID,
	userId: UserID,
	reason: Exclude<OperatorPassLabsLeftReason, 'history'>,
): Promise<void> {
	const labs = getLabsOperatorRole();
	if (!labs || labs.guildId !== guildId) {
		return;
	}
	try {
		await labsRepository.recordLabsLeft(userId, new Date(), reason);
	} catch (error) {
		Logger.error({userId, guildId, reason, error}, 'Failed to record that someone left Fluxer Labs');
	}
}

export function isLabsAutoJoinConfigured(): boolean {
	return getLabsOperatorRole() !== null;
}

async function queueLabsDelivery(job: LabsDeliveryJob, runAt: Date): Promise<void> {
	try {
		await getWorkerService().addJob(
			OPERATOR_PASS_LABS_DELIVERY_TASK,
			{user_id: job.userId.toString(), greeting: job.greeting, origin: job.origin, retry: job.retry},
			{runAt, jobKey: `${job.userId}:${job.origin}:${job.retry}`},
		);
	} catch (error) {
		Logger.error(
			{userId: job.userId, origin: job.origin, retry: job.retry, error},
			'Failed to queue another try at adding an Operator to Fluxer Labs, the daily Labs check tries again',
		);
	}
}

async function scheduleLabsRetry(job: LabsDeliveryJob): Promise<void> {
	const delay = LABS_RETRY_DELAYS_MS[job.retry];
	if (delay === undefined) {
		Logger.error(
			{userId: job.userId, origin: job.origin, retry: job.retry},
			'Still could not add an Operator to Fluxer Labs, the daily Labs check tries again',
		);
		return;
	}
	await queueLabsDelivery({...job, retry: job.retry + 1}, new Date(Date.now() + delay));
}

function getLabsOperatorRole(): LabsOperatorRole | null {
	const {labsGuildId, labsOperatorRoleId} = Config.operatorPass;
	if (Config.instance.selfHosted || labsGuildId == null || labsOperatorRoleId == null) {
		return null;
	}
	return {guildId: createGuildID(labsGuildId), roleId: createRoleID(labsOperatorRoleId)};
}

function getLabsVisionaryRoleId(): RoleID | null {
	const {labsVisionaryRoleId} = Config.operatorPass;
	return getLabsOperatorRole() && labsVisionaryRoleId != null ? createRoleID(labsVisionaryRoleId) : null;
}

function getLabsAccessRoleId(): RoleID | null {
	const {labsAccessRoleId} = Config.operatorPass;
	return getLabsOperatorRole() && labsAccessRoleId != null ? createRoleID(labsAccessRoleId) : null;
}

interface LabsRoleWants {
	operator: boolean;
	visionary: boolean;
	access: boolean;
}

export function labsRolesFor(user: Omit<VisionaryAccount, 'id'> | null, entitled: boolean): LabsRoleWants {
	const lifetime = user?.premiumType === UserPremiumTypes.LIFETIME;
	const split = getLabsAccessRoleId() !== null;
	return {
		operator: entitled && (!split || !user?.hiddenProfileBadges.has('operator_pass')),
		visionary:
			getLabsVisionaryRoleId() !== null && lifetime && (!split || (user !== null && showsVisionaryInLabs(user))),
		access: split && (entitled || lifetime),
	};
}

export function keepsOperatorsThroughVisionary(user: Pick<User, 'premiumType'> | null): boolean {
	return getLabsVisionaryRoleId() !== null && user?.premiumType === UserPremiumTypes.LIFETIME;
}

export async function resolveOperatorPassJoinRoleIds(
	guildId: GuildID,
	user: User,
	guildRepository: GuildRepository,
): Promise<Set<RoleID> | null> {
	const labs = getLabsOperatorRole();
	if (!labs || labs.guildId !== guildId) {
		return null;
	}
	const wants = labsRolesFor(
		user,
		isOperatorPassEntitled(await loadOperatorPassEntitlementState(labsRepository, user.id)),
	);
	const candidates = labsRoleTargets(labs.roleId)
		.filter(({key}) => wants[key])
		.map(({roleId}) => roleId);
	const roleIds = new Set<RoleID>();
	for (const roleId of candidates) {
		if (await guildRepository.getRole(roleId, guildId)) {
			roleIds.add(roleId);
		}
	}
	return roleIds.size > 0 ? roleIds : null;
}

function labsRoleTargets(operatorRoleId: RoleID): Array<{key: keyof LabsRoleWants; roleId: RoleID}> {
	const targets: Array<{key: keyof LabsRoleWants; roleId: RoleID}> = [{key: 'operator', roleId: operatorRoleId}];
	const visionaryRoleId = getLabsVisionaryRoleId();
	if (visionaryRoleId) targets.push({key: 'visionary', roleId: visionaryRoleId});
	const accessRoleId = getLabsAccessRoleId();
	if (accessRoleId) targets.push({key: 'access', roleId: accessRoleId});
	return targets;
}

export class OperatorPassLabsRoleService {
	constructor(
		private readonly guildRepository: GuildRepository,
		private readonly guildService: GuildService,
	) {}

	async deliver(user: LabsAccount, greeting: boolean | null): Promise<void> {
		const labs = getLabsOperatorRole();
		if (!labs) {
			Logger.warn({userId: user.id}, 'Fluxer Labs guild or Operator role is not configured, so nobody is added to it');
			return;
		}
		let outcome: LabsDelivery = 'retry';
		try {
			if (greeting !== null) {
				await labsRepository.setLabsGreeting(user.id, greeting);
			}
			outcome = await this.deliverTo(labs, user);
		} catch (error) {
			Logger.error(
				{userId: user.id, guildId: labs.guildId, error},
				'Failed to add an Operator to Fluxer Labs, trying again later',
			);
		}
		if (outcome === 'retry') {
			await scheduleLabsRetry({userId: user.id, greeting, origin: `delivered-${Date.now()}`, retry: 0});
		}
	}

	async retryDelivery(job: LabsDeliveryJob, userRepository: Pick<UserRepository, 'findUnique'>): Promise<void> {
		const labs = getLabsOperatorRole();
		if (!labs) {
			return;
		}
		let outcome: LabsDelivery = 'retry';
		try {
			const user = await userRepository.findUnique(job.userId);
			if (!user) {
				return;
			}
			if (job.greeting !== null && (await labsRepository.findLabsMember(user.id))?.greeting == null) {
				await labsRepository.setLabsGreeting(user.id, job.greeting);
			}
			outcome = await this.deliverTo(labs, user);
		} catch (error) {
			Logger.error(
				{userId: job.userId, guildId: labs.guildId, retry: job.retry, error},
				'Failed again to add an Operator to Fluxer Labs',
			);
		}
		if (outcome === 'retry') {
			await scheduleLabsRetry(job);
		}
	}

	async queueMissingLabsMembers(
		userRepository: Pick<UserRepository, 'listUsers'>,
		pageSize: number,
		now: Date = new Date(),
	): Promise<LabsReconcileSummary | null> {
		const labs = getLabsOperatorRole();
		if (!labs) {
			return null;
		}
		const summary: LabsReconcileSummary = {checked: 0, queued: 0};
		const origin = `daily-${now.toISOString().slice(0, 10)}`;
		let pageState: string | null = null;
		do {
			const page = await labsRepository.scanOperatorPasses(pageSize, pageState);
			pageState = page.pageState;
			const passes = page.rows.filter((pass) => pass.source !== 'admin' && pass.paused_at == null);
			if (passes.length === 0) {
				continue;
			}
			const users = await userRepository.listUsers(passes.map((pass) => pass.user_id));
			for (const user of users) {
				if (isBlockedFromLabsAutoJoin(user, now)) {
					continue;
				}
				summary.checked++;
				if ((await labsRepository.findLabsMember(user.id))?.left_at != null) {
					continue;
				}
				const member = await this.guildRepository.getMember(labs.guildId, user.id);
				const accessRoleId = getLabsAccessRoleId();
				if (member && (member.roleIds.has(labs.roleId) || (accessRoleId && member.roleIds.has(accessRoleId)))) {
					continue;
				}
				if (!isOperatorPassEntitled(await loadOperatorPassEntitlementState(labsRepository, user.id))) {
					continue;
				}
				await queueLabsDelivery({userId: user.id, greeting: null, origin, retry: 0}, now);
				summary.queued++;
			}
		} while (pageState);
		return summary;
	}

	async queueGreetingIfDue(user: LabsAccount, now: Date = new Date()): Promise<boolean> {
		const labs = getLabsOperatorRole();
		if (!labs || getLabsOperatorsChannelId() === null || isBlockedFromLabsAutoJoin(user, now)) {
			return false;
		}
		const record = await labsRepository.findLabsMember(user.id);
		if (record?.greeting === false || record?.greeted_at != null) {
			return false;
		}
		if (
			!(await this.guildRepository.getMember(labs.guildId, user.id)) ||
			!isOperatorPassEntitled(await loadOperatorPassEntitlementState(labsRepository, user.id))
		) {
			return false;
		}
		await labsRepository.queueLabsGreeting(user.id, now);
		return true;
	}

	private async deliverTo(labs: LabsOperatorRole, user: LabsAccount): Promise<LabsDelivery> {
		if (!isOperatorPassEntitled(await loadOperatorPassEntitlementState(labsRepository, user.id))) {
			return 'done';
		}
		const now = new Date();
		if ((await this.guildRepository.getMember(labs.guildId, user.id)) === null) {
			const added = await this.addToLabs(user, labs, now);
			if (added === 'retry' || (added === 'blocked' && !isGoneForGood(user))) {
				return 'retry';
			}
			if (added !== 'added') {
				return 'done';
			}
		}
		await this.finishLabsMembership(user, now);
		return 'done';
	}

	private async finishLabsMembership(user: LabsAccount, now: Date): Promise<void> {
		await this.resyncLabsRoles(user);
		await this.queueGreetingIfDue(user, now);
	}

	async joinFromCheckout(user: LabsAccount): Promise<LabsJoinStatus> {
		const labs = getLabsOperatorRole();
		if (!labs) {
			return 'unavailable';
		}
		if (!isOperatorPassEntitled(await loadOperatorPassEntitlementState(labsRepository, user.id))) {
			return 'not_held';
		}
		const now = new Date();
		let status: LabsJoinStatus = 'joined';
		if ((await this.guildRepository.getMember(labs.guildId, user.id)) === null) {
			const added = await this.addToLabs(user, labs, now);
			status = labsJoinStatusForAdd(added);
		}
		if (status === 'joined') {
			await this.finishLabsMembership(user, now);
		}
		Logger.info({userId: user.id, result: status}, 'Operator asked to join Fluxer Labs after checkout');
		return status;
	}

	private async addToLabs(user: LabsAccount, labs: LabsOperatorRole, now: Date): Promise<LabsAdd> {
		if (isBlockedFromLabsAutoJoin(user, now)) {
			return 'blocked';
		}
		const record = await labsRepository.findLabsMember(user.id);
		if (record?.left_at != null) {
			return record.left_reason === 'banned' ? 'banned' : 'left';
		}
		const history = await this.guildRepository.getMembershipMetadata(labs.guildId, user.id);
		if (history?.last_left_at != null) {
			await labsRepository.recordLabsLeft(user.id, history.last_left_at, 'history');
			return 'left';
		}
		try {
			await this.guildService.members.addUserToGuild({
				userId: user.id,
				guildId: labs.guildId,
				sendJoinMessage: false,
				skipGuildLimitCheck: true,
				joinSourceType: JoinSourceTypes.ADMIN_FORCE_ADD,
				initiatorId: SYSTEM_USER_ID,
				mentionsOnlyNotifications: true,
				requestCache: createRequestCache(),
			});
		} catch (error) {
			if (error instanceof BannedFromGuildError) {
				await labsRepository.recordLabsLeft(user.id, now, 'banned');
				return 'banned';
			}
			if (error instanceof IpBannedFromGuildError) {
				return 'banned';
			}
			throw error;
		}
		await labsRepository.recordLabsAdded(user.id, now);
		return 'added';
	}

	async isLabsMember(userId: UserID): Promise<boolean> {
		const labs = getLabsOperatorRole();
		return labs !== null && (await this.guildRepository.getMember(labs.guildId, userId)) !== null;
	}

	async resyncLabsRoles(user: VisionaryAccount): Promise<void> {
		const entitled = isOperatorPassEntitled(await loadOperatorPassEntitlementState(labsRepository, user.id));
		await this.syncLabsRoles(user.id, user, entitled);
	}

	async syncLabsRoles(userId: UserID, user: VisionaryAccount | null, entitled: boolean): Promise<void> {
		const labs = getLabsOperatorRole();
		if (!labs) {
			return;
		}
		try {
			const member = await this.guildRepository.getMember(labs.guildId, userId);
			if (member) {
				await this.applyLabsRoles(labs, member, userId, labsRolesFor(user, entitled));
			}
		} catch (error) {
			Logger.error({userId, guildId: labs.guildId, error}, 'Failed to update the Fluxer Labs roles');
		}
	}

	async syncLabsRolesForMembers(
		userRepository: Pick<UserRepository, 'listUsers'>,
		pageSize: number,
	): Promise<LabsVisionaryRoleSyncSummary | null> {
		const labs = getLabsOperatorRole();
		if (!labs) {
			return null;
		}
		const summary: LabsVisionaryRoleSyncSummary = {granted: 0, removed: 0, failed: 0};
		let afterUserId: UserID | undefined;
		for (;;) {
			const members = await this.guildRepository.listMembersPaginated(labs.guildId, pageSize, afterUserId);
			if (members.length === 0) {
				break;
			}
			afterUserId = members[members.length - 1].userId;
			const users = new Map(
				(await userRepository.listUsers(members.map((member) => member.userId))).map((user) => [user.id, user]),
			);
			for (const member of members) {
				try {
					const user = users.get(member.userId) ?? null;
					const entitled =
						user !== null &&
						isOperatorPassEntitled(await loadOperatorPassEntitlementState(labsRepository, member.userId));
					const changes = await this.applyLabsRoles(labs, member, member.userId, labsRolesFor(user, entitled));
					summary.granted += changes.granted;
					summary.removed += changes.removed;
				} catch (error) {
					summary.failed++;
					Logger.error({userId: member.userId, guildId: labs.guildId, error}, 'Failed to update the Fluxer Labs roles');
				}
			}
			if (members.length < pageSize) {
				break;
			}
		}
		return summary;
	}

	private async applyLabsRoles(
		labs: LabsOperatorRole,
		member: {roleIds: ReadonlySet<RoleID>},
		userId: UserID,
		wants: LabsRoleWants,
	): Promise<{granted: number; removed: number}> {
		const changes = {granted: 0, removed: 0};
		for (const {key, roleId} of labsRoleTargets(labs.roleId)) {
			if (wants[key] === member.roleIds.has(roleId)) {
				continue;
			}
			const change = {
				targetId: userId,
				guildId: labs.guildId,
				roleId,
				initiatorId: SYSTEM_USER_ID,
				requestCache: createRequestCache(),
			};
			if (wants[key]) {
				await this.guildService.members.systemAddMemberRole(change);
				changes.granted++;
			} else {
				await this.guildService.members.systemRemoveMemberRole(change);
				changes.removed++;
			}
		}
		return changes;
	}
}
