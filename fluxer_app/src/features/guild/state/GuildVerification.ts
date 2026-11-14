// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Guild} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import GuildMembers from '@app/features/member/state/GuildMembers';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import {type AccountGateResult, canPerform} from '@app/features/user/utils/AccountGates';
import {TierGatedActions} from '@fluxer/constants/src/AccountTierConstants';
import type {TierGateDenial} from '@fluxer/constants/src/AccountTierRules';
import {
	GuildFeatures,
	GuildOperations,
	GuildVerificationLevel,
	getEffectiveGuildVerificationLevel,
} from '@fluxer/constants/src/GuildConstants';
import {getGuildVerificationWait, isExemptFromGuildVerification} from '@fluxer/constants/src/GuildVerificationRules';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import isEqual from 'lodash/isEqual';
import {makeAutoObservable} from 'mobx';

export const VerificationFailureReason = {
	ACCOUNT_GATE: 'ACCOUNT_GATE',
	ACCOUNT_TOO_NEW: 'ACCOUNT_TOO_NEW',
	NOT_MEMBER_LONG_ENOUGH: 'NOT_MEMBER_LONG_ENOUGH',
	SEND_MESSAGE_DISABLED: 'SEND_MESSAGE_DISABLED',
	TIMED_OUT: 'TIMED_OUT',
} as const;

export type VerificationFailureReason = ValueOf<typeof VerificationFailureReason>;

export interface VerificationStatus {
	canAccess: boolean;
	reason?: VerificationFailureReason;
	verificationEndsAt?: number;
	accountGate?: TierGateDenial;
}

function accountGateStatus(gate: AccountGateResult): VerificationStatus | null {
	if (gate.allowed) return null;
	return {canAccess: false, reason: VerificationFailureReason.ACCOUNT_GATE, accountGate: gate};
}

function computeVerificationLevelStatus(
	guild: Guild,
	user: User,
	member: GuildMember | null,
	now: number,
): VerificationStatus {
	const verificationLevel = getEffectiveGuildVerificationLevel(
		guild.verificationLevel ?? GuildVerificationLevel.NONE,
		guild.features.has(GuildFeatures.DISCOVERABLE),
	);
	const exempt = isExemptFromGuildVerification({
		verificationLevel,
		isOwner: user.id === guild.ownerId,
		isBot: user.bot,
		hasRoles: (member?.roles.size ?? 0) > 0,
	});
	if (exempt) return {canAccess: true};
	const verificationGate = accountGateStatus(canPerform(user, TierGatedActions.GUILD_VERIFICATION));
	if (verificationGate) return verificationGate;
	const wait = getGuildVerificationWait({
		verificationLevel,
		accountCreatedAt: user.createdAt.getTime(),
		memberJoinedAt: member?.joinedAt?.getTime() ?? null,
		now,
	});
	return wait ? {canAccess: false, reason: wait.reason, verificationEndsAt: wait.endsAt} : {canAccess: true};
}

function nextRecheckDelay(statuses: ReadonlyArray<VerificationStatus>, now: number): number | null {
	let next: number | null = null;
	for (const status of statuses) {
		if (status.canAccess || status.verificationEndsAt === undefined) continue;
		const delay = status.verificationEndsAt - now;
		if (delay > 0 && (next === null || delay < next)) next = delay;
	}
	return next;
}

function computeMessageStatus(
	guild: Guild,
	user: User,
	member: GuildMember | null,
	now: number,
	levelStatus: VerificationStatus,
): VerificationStatus {
	if (member?.communicationDisabledUntil) {
		const timeoutUntil = member.communicationDisabledUntil;
		if (timeoutUntil.getTime() > now) {
			return {
				canAccess: false,
				reason: VerificationFailureReason.TIMED_OUT,
				verificationEndsAt: timeoutUntil.getTime(),
			};
		}
	}
	if ((guild.disabledOperations & GuildOperations.SEND_MESSAGE) !== 0) {
		return {canAccess: false, reason: VerificationFailureReason.SEND_MESSAGE_DISABLED};
	}
	if (user.reportsAccountTier) {
		const messageGate = accountGateStatus(canPerform(user, TierGatedActions.MESSAGE));
		if (messageGate) return messageGate;
	}
	return levelStatus;
}

interface GuildVerificationStatuses {
	readonly message: VerificationStatus;
	readonly level: VerificationStatus;
}

function computeStatuses(guild: Guild): GuildVerificationStatuses {
	const user = Users.getCurrentUser();
	if (!user) {
		return {
			message: accountGateStatus(canPerform(null, TierGatedActions.MESSAGE)) ?? {canAccess: false},
			level: {canAccess: true},
		};
	}
	const member = GuildMembers.getMember(guild.id, user.id);
	const now = Date.now();
	const level = computeVerificationLevelStatus(guild, user, member, now);
	return {message: computeMessageStatus(guild, user, member, now, level), level};
}

class GuildVerification {
	statuses: Record<string, GuildVerificationStatuses> = {};
	private timers: Record<string, NodeJS.Timeout> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	handleGatewayReady(): void {
		this.recomputeAll();
	}

	handleGuildCreate(guild: {id: string}): void {
		this.recomputeGuild(guild.id);
	}

	handleGuildUpdate(guild: {id: string}): void {
		this.recomputeGuild(guild.id);
	}

	handleGuildDelete(guildId: string): void {
		const newStatuses = {...this.statuses};
		delete newStatuses[guildId];
		if (this.timers[guildId]) {
			clearTimeout(this.timers[guildId]);
			delete this.timers[guildId];
		}
		if (!isEqual(this.statuses, newStatuses)) this.statuses = Object.freeze(newStatuses);
	}

	handleGuildMemberUpdate(guildId: string): void {
		this.recomputeGuild(guildId);
	}

	handleUserUpdate(): void {
		this.recomputeAll();
	}

	private recomputeAll(): void {
		const guilds = Guilds.getGuilds();
		const newStatuses: Record<string, GuildVerificationStatuses> = {};
		for (const timerId of Object.values(this.timers)) {
			clearTimeout(timerId);
		}
		const newTimers: Record<string, NodeJS.Timeout> = {};
		for (const guild of guilds) {
			const statuses = computeStatuses(guild);
			newStatuses[guild.id] = statuses;
			const delay = nextRecheckDelay([statuses.message, statuses.level], Date.now());
			if (delay !== null) {
				newTimers[guild.id] = setTimeout(() => {
					this.recomputeGuild(guild.id);
				}, delay);
			}
		}
		if (!isEqual(this.statuses, newStatuses)) this.statuses = Object.freeze(newStatuses);
		this.timers = newTimers;
	}

	private recomputeGuild(guildId: string): void {
		const guild = Guilds.getGuild(guildId);
		if (!guild) {
			return;
		}
		const statuses = computeStatuses(guild);
		const newStatuses = {...this.statuses, [guildId]: statuses};
		if (this.timers[guildId]) {
			clearTimeout(this.timers[guildId]);
		}
		const newTimers = {...this.timers};
		delete newTimers[guildId];
		const delay = nextRecheckDelay([statuses.message, statuses.level], Date.now());
		if (delay !== null) {
			newTimers[guildId] = setTimeout(() => {
				this.recomputeGuild(guildId);
			}, delay);
		}
		if (!isEqual(this.statuses, newStatuses)) this.statuses = Object.freeze(newStatuses);
		this.timers = newTimers;
	}

	canAccessGuild(guildId: string): boolean {
		return this.statuses[guildId]?.message.canAccess ?? true;
	}

	getVerificationStatus(guildId: string): VerificationStatus | null {
		return this.statuses[guildId]?.message ?? null;
	}

	getVerificationLevelStatus(guildId: string): VerificationStatus {
		return this.statuses[guildId]?.level ?? {canAccess: true};
	}

	getFailureReason(guildId: string): VerificationFailureReason | null {
		return this.statuses[guildId]?.message.reason ?? null;
	}
}

export default new GuildVerification();
