// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type ChannelID,
	createChannelID,
	createGuildID,
	createRoleID,
	type GuildID,
	type RoleID,
	type UserID,
} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {ChannelOperationsService} from '@app/api/channel/services/channel_data/ChannelOperationsService';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import type {OperatorPassLabsLeftReason} from '@app/api/database/types/OperatorPassTypes';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {GuildService} from '@app/api/guild/services/GuildService';
import {Logger} from '@app/api/Logger';
import {createRequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {getWorkerService} from '@app/api/middleware/ServiceRegistry';
import type {Channel} from '@app/api/models/Channel';
import type {User} from '@app/api/models/User';
import {
	isOperatorPassEntitled,
	loadOperatorPassEntitlementState,
} from '@app/api/operator_pass/OperatorPassEntitlementService';
import {operatorPassLabsRolesLockKey, withOperatorPassLock} from '@app/api/operator_pass/OperatorPassLocks';
import {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {isAccountLimited} from '@app/api/user/AccountLimit';
import {isProfileHidden} from '@app/api/user/ProfileVisibility';
import {applyPremiumBadgeMasking} from '@app/api/user/profile_badges/PremiumBadgeMasking';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {JoinSourceTypes} from '@fluxer/constants/src/GuildConstants';
import {
	PremiumFlags,
	type ProfilePrivacyLevel,
	ProfilePrivacyLevels,
	UserFlags,
	UserPremiumTypes,
} from '@fluxer/constants/src/UserConstants';
import {BannedFromGuildError} from '@fluxer/errors/src/domains/guild/BannedFromGuildError';
import {IpBannedFromGuildError} from '@fluxer/errors/src/domains/guild/IpBannedFromGuildError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {ms} from 'itty-time';

export const OPERATOR_PASS_LABS_INVITE_URL = 'https://fluxer.gg/fluxer-labs';

export const OPERATOR_PASS_LABS_VISIONARY_ROLE_SYNC_TASK = 'operatorPassLabsVisionaryRoleSync';

export const OPERATOR_PASS_LABS_DELIVERY_TASK = 'operatorPassLabsDelivery';

export const OPERATOR_PASS_LABS_GREETING_TASK = 'operatorPassLabsGreeting';

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

interface LabsRoleChanges {
	granted: number;
	removed: number;
}

class LabsRolesBusyError extends Error {
	constructor(userId: UserID) {
		super(`Another update of the Fluxer Labs roles of ${userId} is running`);
	}
}

const labsRepository = new OperatorPassRepository();

const LABS_BLOCKING_FLAGS = UserFlags.DELETED | UserFlags.SELF_DELETED | UserFlags.DISABLED | UserFlags.SPAMMER;

type LabsRoleSubject = Pick<
	User,
	| 'premiumType'
	| 'premiumFlags'
	| 'premiumSince'
	| 'premiumLifetimeSequence'
	| 'hiddenProfileBadges'
	| 'flags'
	| 'isSystem'
	| 'tempBannedUntil'
	| 'pendingDeletionAt'
	| 'deletionReasonCode'
>;

type LabsAccount = LabsRoleSubject & Pick<User, 'id' | 'isBot' | 'deletionStartedAt' | 'contentHiddenSince'>;

type LabsUserRepository = Pick<UserRepository, 'findUnique' | 'findSettings' | 'listUsers'>;

export interface OperatorPassLabsRoleDeps {
	guildRepository: GuildRepository;
	guildService: GuildService;
	userRepository: LabsUserRepository;
	cacheService: ICacheService;
	channelRepository: Pick<ChannelRepository, 'channelData'>;
	channelOverwrites: Pick<ChannelOperationsService, 'systemSetMemberOverwrite'>;
}

export interface LabsRoleState {
	user: LabsRoleSubject;
	profilePrivacy: ProfilePrivacyLevel;
	entitled: boolean;
}

export async function labsProfilePrivacy(
	userRepository: Pick<UserRepository, 'findSettings'>,
	userId: UserID,
): Promise<ProfilePrivacyLevel> {
	return (await userRepository.findSettings(userId))?.profilePrivacy ?? ProfilePrivacyLevels.ALL_GUILDS;
}

async function isEntitled(userId: UserID): Promise<boolean> {
	return isOperatorPassEntitled(await loadOperatorPassEntitlementState(labsRepository, userId));
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
	await queueLabsRoleSync(userId, 0, new Date());
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

async function queueLabsRoleSync(userId: UserID, retry: number, runAt: Date): Promise<void> {
	try {
		await getWorkerService().addJob(
			OPERATOR_PASS_LABS_VISIONARY_ROLE_SYNC_TASK,
			{user_id: userId.toString(), retry},
			{runAt, jobKey: `labs-roles:${userId}:${retry}:${Date.now()}`},
		);
	} catch (error) {
		Logger.error({userId, retry, error}, 'Failed to queue a Fluxer Labs role update, the daily Labs role sync does it');
	}
}

async function scheduleLabsRoleSyncRetry(userId: UserID, retry: number): Promise<void> {
	const delay = LABS_RETRY_DELAYS_MS[retry];
	if (delay === undefined) {
		Logger.error({userId, retry}, 'Still could not update the Fluxer Labs roles, the daily Labs role sync tries again');
		return;
	}
	await queueLabsRoleSync(userId, retry, new Date(Date.now() + delay));
}

async function queueLabsGreetingRetraction(userId: UserID, messageId: bigint): Promise<void> {
	try {
		await getWorkerService().addJob(
			OPERATOR_PASS_LABS_GREETING_TASK,
			{retract_user_id: userId.toString()},
			{jobKey: `labs-greeting-retract:${userId}:${messageId}`},
		);
	} catch (error) {
		Logger.error({userId, error}, 'Failed to queue taking a name out of an #operators welcome');
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

function getOperatorsChannelId(): ChannelID | null {
	const channelId = getLabsOperatorsChannelId();
	return channelId === null ? null : createChannelID(channelId);
}

interface LabsRoleWants {
	operator: boolean;
	visionary: boolean;
	memberAccess: boolean;
}

export function hidesOperatorsChannel(user: Pick<User, 'premiumFlags'> | null): boolean {
	return user !== null && (user.premiumFlags & PremiumFlags.OPERATORS_CHANNEL_HIDDEN) !== 0;
}

const NO_LABS_ROLES: LabsRoleWants = {operator: false, visionary: false, memberAccess: false};

function isShownToEveryLabsMember(user: LabsRoleSubject, profilePrivacy: ProfilePrivacyLevel, now: number): boolean {
	return profilePrivacy === ProfilePrivacyLevels.ALL_GUILDS && !isProfileHidden(user, now);
}

function showsVisionaryBadge(user: LabsRoleSubject): boolean {
	const shown = applyPremiumBadgeMasking(user, {
		premiumFlags: user.premiumFlags,
		hidden: user.hiddenProfileBadges,
		selfHosted: false,
		restricted: false,
	});
	return shown.premiumType === UserPremiumTypes.LIFETIME;
}

export function showsOperatorBadgeInLabs(
	user: LabsRoleSubject,
	profilePrivacy: ProfilePrivacyLevel,
	now = Date.now(),
): boolean {
	return isShownToEveryLabsMember(user, profilePrivacy, now) && !user.hiddenProfileBadges.has('operator_pass');
}

export function keepsOperatorsThroughVisionary(user: Pick<User, 'premiumType'> | null): boolean {
	return getLabsVisionaryRoleId() !== null && user?.premiumType === UserPremiumTypes.LIFETIME;
}

function hasLabsAccess(user: LabsRoleSubject, entitled: boolean): boolean {
	return entitled || keepsOperatorsThroughVisionary(user);
}

export function labsRolesFor({user, profilePrivacy, entitled}: LabsRoleState, now = Date.now()): LabsRoleWants {
	if (hidesOperatorsChannel(user) || !hasLabsAccess(user, entitled)) {
		return NO_LABS_ROLES;
	}
	const operator = entitled && showsOperatorBadgeInLabs(user, profilePrivacy, now);
	const visionary =
		getLabsVisionaryRoleId() !== null &&
		isShownToEveryLabsMember(user, profilePrivacy, now) &&
		showsVisionaryBadge(user);
	return {operator, visionary, memberAccess: !operator && !visionary};
}

export interface LabsJoin {
	roleIds: Set<RoleID> | null;
	memberAccess: boolean;
}

export async function resolveOperatorPassJoin(
	guildId: GuildID,
	user: User,
	guildRepository: GuildRepository,
	userRepository: Pick<UserRepository, 'findSettings'>,
): Promise<LabsJoin | null> {
	const labs = getLabsOperatorRole();
	if (!labs || labs.guildId !== guildId) {
		return null;
	}
	const wants = labsRolesFor({
		user,
		entitled: await isEntitled(user.id),
		profilePrivacy: await labsProfilePrivacy(userRepository, user.id),
	});
	const roleIds = new Set<RoleID>();
	for (const {key, roleId} of labsRoleTargets(labs)) {
		if (wants[key] && (await guildRepository.getRole(roleId, guildId))) {
			roleIds.add(roleId);
		}
	}
	return {roleIds: roleIds.size > 0 ? roleIds : null, memberAccess: wants.memberAccess};
}

export async function finishOperatorPassJoin(userId: UserID, join: LabsJoin | null): Promise<void> {
	if (join?.memberAccess) {
		await queueLabsRoleSync(userId, 0, new Date());
	}
}

function labsRoleTargets(labs: LabsOperatorRole): Array<{key: 'operator' | 'visionary'; roleId: RoleID}> {
	const targets: Array<{key: 'operator' | 'visionary'; roleId: RoleID}> = [{key: 'operator', roleId: labs.roleId}];
	const visionaryRoleId = getLabsVisionaryRoleId();
	if (visionaryRoleId) targets.push({key: 'visionary', roleId: visionaryRoleId});
	return targets;
}

function memberAccessAllow(labs: LabsOperatorRole, channel: Channel): bigint {
	let allow = 0n;
	for (const {roleId} of labsRoleTargets(labs)) {
		allow |= channel.permissionOverwrites.get(roleId)?.allow ?? 0n;
	}
	if (allow === 0n) {
		throw new Error('#operators has no Operator or Visionary role overwrite to give hidden members the same access');
	}
	return allow;
}

type LabsOverwriteChange = {kind: 'keep'} | {kind: 'set'; allow: bigint} | {kind: 'remove'};

interface LabsRolePlan {
	roleIds: Set<RoleID>;
	roleGrants: number;
	roleRemovals: number;
	overwrite: LabsOverwriteChange;
}

function planLabsRoles(
	labs: LabsOperatorRole,
	roleIds: ReadonlySet<RoleID> | null,
	channel: Channel | null,
	userId: UserID,
	wants: LabsRoleWants,
): LabsRolePlan {
	const plan: LabsRolePlan = {roleIds: new Set(), roleGrants: 0, roleRemovals: 0, overwrite: {kind: 'keep'}};
	if (roleIds) {
		for (const {key, roleId} of labsRoleTargets(labs)) {
			if (wants[key]) plan.roleIds.add(roleId);
			if (wants[key] && !roleIds.has(roleId)) plan.roleGrants++;
			if (!wants[key] && roleIds.has(roleId)) plan.roleRemovals++;
		}
	}
	if (!channel) {
		return plan;
	}
	const current = channel.permissionOverwrites.get(userId);
	if (roleIds && wants.memberAccess) {
		const allow = memberAccessAllow(labs, channel);
		if (current?.type !== 1 || current.allow !== allow || current.deny !== 0n) {
			plan.overwrite = {kind: 'set', allow};
		}
	} else if (current) {
		plan.overwrite = {kind: 'remove'};
	}
	return plan;
}

function countChanges(plan: LabsRolePlan): LabsRoleChanges {
	return {
		granted: plan.roleGrants + (plan.overwrite.kind === 'set' ? 1 : 0),
		removed: plan.roleRemovals + (plan.overwrite.kind === 'remove' ? 1 : 0),
	};
}

function changesNothing(plan: LabsRolePlan): boolean {
	const {granted, removed} = countChanges(plan);
	return granted === 0 && removed === 0;
}

interface LabsMemberState {
	wants: LabsRoleWants;
	hidesOperator: boolean;
}

export class OperatorPassLabsRoleService {
	constructor(private readonly deps: OperatorPassLabsRoleDeps) {}

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
				const member = await this.deps.guildRepository.getMember(labs.guildId, user.id);
				if (member && (hidesOperatorsChannel(user) || (await this.reachesOperators(labs, member.roleIds, user.id)))) {
					continue;
				}
				if (!(await isEntitled(user.id))) {
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
		if (
			!labs ||
			getLabsOperatorsChannelId() === null ||
			hidesOperatorsChannel(user) ||
			isBlockedFromLabsAutoJoin(user, now)
		) {
			return false;
		}
		const record = await labsRepository.findLabsMember(user.id);
		if (record?.greeting === false || record?.greeted_at != null) {
			return false;
		}
		if (!(await this.deps.guildRepository.getMember(labs.guildId, user.id))) {
			return false;
		}
		const state: LabsRoleState = {
			user,
			entitled: await isEntitled(user.id),
			profilePrivacy: await labsProfilePrivacy(this.deps.userRepository, user.id),
		};
		if (!labsRolesFor(state, now.getTime()).operator) {
			return false;
		}
		await labsRepository.queueLabsGreeting(user.id, now);
		return true;
	}

	private async deliverTo(labs: LabsOperatorRole, user: LabsAccount): Promise<LabsDelivery> {
		if (!(await isEntitled(user.id))) {
			return 'done';
		}
		const now = new Date();
		if ((await this.deps.guildRepository.getMember(labs.guildId, user.id)) === null) {
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
		await this.resyncLabsRoles(user.id);
		await this.queueGreetingIfDue(user, now);
	}

	async joinFromCheckout(user: LabsAccount): Promise<LabsJoinStatus> {
		const labs = getLabsOperatorRole();
		if (!labs) {
			return 'unavailable';
		}
		if (!(await isEntitled(user.id))) {
			return 'not_held';
		}
		const now = new Date();
		let status: LabsJoinStatus = 'joined';
		if ((await this.deps.guildRepository.getMember(labs.guildId, user.id)) === null) {
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
		const history = await this.deps.guildRepository.getMembershipMetadata(labs.guildId, user.id);
		if (history?.last_left_at != null) {
			await labsRepository.recordLabsLeft(user.id, history.last_left_at, 'history');
			return 'left';
		}
		try {
			await this.deps.guildService.members.addUserToGuild({
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
		return labs !== null && (await this.deps.guildRepository.getMember(labs.guildId, userId)) !== null;
	}

	async resyncLabsRoles(userId: UserID): Promise<void> {
		const labs = getLabsOperatorRole();
		if (!labs) {
			return;
		}
		try {
			await this.applyCurrentLabsRoles(labs, userId);
		} catch (error) {
			Logger.warn(
				{userId, guildId: labs.guildId, error},
				'Could not update the Fluxer Labs roles now, trying again later',
			);
			await scheduleLabsRoleSyncRetry(userId, 0);
		}
	}

	async retryLabsRoleSync(userId: UserID, nextRetry: number): Promise<void> {
		const labs = getLabsOperatorRole();
		if (!labs) {
			return;
		}
		try {
			await this.applyCurrentLabsRoles(labs, userId);
		} catch (error) {
			Logger.error(
				{userId, guildId: labs.guildId, retry: nextRetry, error},
				'Failed again to update the Fluxer Labs roles',
			);
			await scheduleLabsRoleSyncRetry(userId, nextRetry);
		}
	}

	async syncLabsRolesForMembers(pageSize: number): Promise<LabsVisionaryRoleSyncSummary | null> {
		const labs = getLabsOperatorRole();
		if (!labs) {
			return null;
		}
		const summary: LabsVisionaryRoleSyncSummary = {granted: 0, removed: 0, failed: 0};
		const sync = async (userId: UserID) => {
			try {
				const changes = await this.applyCurrentLabsRoles(labs, userId);
				summary.granted += changes.granted;
				summary.removed += changes.removed;
			} catch (error) {
				summary.failed++;
				Logger.error({userId, guildId: labs.guildId, error}, 'Failed to update the Fluxer Labs roles');
			}
		};
		let afterUserId: UserID | undefined;
		for (;;) {
			const members = await this.deps.guildRepository.listMembersPaginated(labs.guildId, pageSize, afterUserId);
			if (members.length === 0) {
				break;
			}
			afterUserId = members[members.length - 1].userId;
			const users = new Map(
				(await this.deps.userRepository.listUsers(members.map((member) => member.userId))).map((user) => [
					user.id,
					user,
				]),
			);
			const channel = await this.findOperatorsChannel();
			for (const member of members) {
				try {
					const state = await this.labsStateOf(users.get(member.userId));
					const plan = planLabsRoles(labs, member.roleIds, channel, member.userId, state.wants);
					if (changesNothing(plan) && !(state.hidesOperator && (await this.hasSentGreeting(member.userId)))) {
						continue;
					}
				} catch (error) {
					summary.failed++;
					Logger.error({userId: member.userId, guildId: labs.guildId, error}, 'Failed to check the Fluxer Labs roles');
					continue;
				}
				await sync(member.userId);
			}
			if (members.length < pageSize) {
				break;
			}
		}
		const channel = await this.findOperatorsChannel();
		for (const [targetId, overwrite] of channel?.permissionOverwrites ?? []) {
			if (overwrite.type === 1 && !(await this.deps.guildRepository.getMember(labs.guildId, targetId as UserID))) {
				await sync(targetId as UserID);
			}
		}
		return summary;
	}

	private async findOperatorsChannel(): Promise<Channel | null> {
		const channelId = getOperatorsChannelId();
		return channelId === null ? null : await this.deps.channelRepository.channelData.findUnique(channelId);
	}

	private async reachesOperators(
		labs: LabsOperatorRole,
		roleIds: ReadonlySet<RoleID>,
		userId: UserID,
	): Promise<boolean> {
		if (labsRoleTargets(labs).some(({roleId}) => roleIds.has(roleId))) {
			return true;
		}
		return (await this.findOperatorsChannel())?.permissionOverwrites.has(userId) ?? false;
	}

	private async hasSentGreeting(userId: UserID): Promise<boolean> {
		return (await labsRepository.findLabsMember(userId))?.greeting_message_id != null;
	}

	private async labsStateOf(user: (LabsRoleSubject & Pick<User, 'id'>) | undefined | null): Promise<LabsMemberState> {
		if (!user) {
			return {wants: NO_LABS_ROLES, hidesOperator: false};
		}
		const entitled = await isEntitled(user.id);
		if (!hasLabsAccess(user, entitled)) {
			return {wants: NO_LABS_ROLES, hidesOperator: false};
		}
		const profilePrivacy = await labsProfilePrivacy(this.deps.userRepository, user.id);
		return {
			wants: labsRolesFor({user, entitled, profilePrivacy}),
			hidesOperator: !showsOperatorBadgeInLabs(user, profilePrivacy),
		};
	}

	private async applyCurrentLabsRoles(labs: LabsOperatorRole, userId: UserID): Promise<LabsRoleChanges> {
		return withOperatorPassLock(
			this.deps.cacheService,
			operatorPassLabsRolesLockKey(userId),
			async () => {
				const member = await this.deps.guildRepository.getMember(labs.guildId, userId);
				const state = member
					? await this.labsStateOf(await this.deps.userRepository.findUnique(userId))
					: {wants: NO_LABS_ROLES, hidesOperator: false};
				const plan = planLabsRoles(
					labs,
					member?.roleIds ?? null,
					await this.findOperatorsChannel(),
					userId,
					state.wants,
				);
				await this.applyLabsRoles(labs, userId, plan);
				if (state.hidesOperator) {
					const messageId = (await labsRepository.findLabsMember(userId))?.greeting_message_id;
					if (messageId != null) {
						await queueLabsGreetingRetraction(userId, messageId);
					}
				}
				return countChanges(plan);
			},
			() => new LabsRolesBusyError(userId),
		);
	}

	private async applyLabsRoles(labs: LabsOperatorRole, userId: UserID, plan: LabsRolePlan): Promise<void> {
		const channelId = getOperatorsChannelId();
		const setOverwrite = async (allow: bigint | null) => {
			if (channelId === null) return;
			await this.deps.channelOverwrites.systemSetMemberOverwrite({
				channelId,
				userId,
				allow,
				requestCache: createRequestCache(),
			});
		};
		if (plan.overwrite.kind === 'set') {
			await setOverwrite(plan.overwrite.allow);
		}
		if (plan.roleGrants > 0 || plan.roleRemovals > 0) {
			await this.deps.guildService.members.systemSyncManagedRoles({
				targetId: userId,
				guildId: labs.guildId,
				managedRoleIds: new Set(labsRoleTargets(labs).map(({roleId}) => roleId)),
				grantedRoleIds: plan.roleIds,
				recordAudit: false,
				requestCache: createRequestCache(),
			});
		}
		if (plan.overwrite.kind === 'remove') {
			await setOverwrite(null);
		}
	}
}
