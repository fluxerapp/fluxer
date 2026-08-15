// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import Channels from '@app/features/channel/state/Channels';
import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import type {Presence as WirePresence} from '@app/features/gateway/types/GatewayPresenceTypes';
import Guilds from '@app/features/guild/state/Guilds';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MemberSidebar from '@app/features/member/state/MemberSidebar';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {deferUntilModulesLoaded} from '@app/features/platform/utils/DeferUntilModulesLoaded';
import LocalPresence from '@app/features/presence/state/LocalPresence';
import TransientPresence from '@app/features/presence/state/TransientPresence';
import Relationships from '@app/features/relationship/state/Relationships';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {type CustomStatus, fromGatewayCustomStatus} from '@app/features/user/state/CustomStatus';
import {CustomStatusEmitter} from '@app/features/user/state/CustomStatusEmitter';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {StatusType} from '@fluxer/constants/src/StatusConstants';
import {normalizeStatus, StatusTypes} from '@fluxer/constants/src/StatusConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import type {UserPrivate} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {makeAutoObservable, observable, reaction} from 'mobx';

interface FlattenedPresence {
	status: StatusType;
	timestamp: number;
	afk?: boolean;
	mobile?: boolean;
	guildIds: Set<string>;
	customStatus: CustomStatus | null;
}

type StatusListener = (userId: string, status: StatusType, isMobile: boolean) => void;

class Presence {
	private logger = new Logger('Presence');
	private presences = new Map<string, FlattenedPresence>();
	private remotePresenceCountsByGuild = new Map<string, number>();
	private remotePresenceCountVersionByGuild = observable.map<string, number>();
	private customStatuses = new Map<string, CustomStatus | null>();
	statuses = new Map<string, StatusType>();
	private mobilePresenceUserIds = new Map<string, true>();
	presenceVersion = 0;
	private statusListeners: Map<string, Set<StatusListener>> = new Map();

	constructor() {
		makeAutoObservable<
			this,
			| 'statusListeners'
			| 'presences'
			| 'remotePresenceCountsByGuild'
			| 'remotePresenceCountVersionByGuild'
			| 'mobilePresenceUserIds'
			| 'suppressVersionBump'
		>(
			this,
			{
				statusListeners: false,
				presences: false,
				remotePresenceCountsByGuild: false,
				remotePresenceCountVersionByGuild: false,
				mobilePresenceUserIds: observable.shallow,
				suppressVersionBump: false,
			},
			{autoBind: true},
		);
		deferUntilModulesLoaded(() => {
			reaction(
				() => ({status: LocalPresence.status, customStatus: LocalPresence.customStatus}),
				() => this.syncLocalPresence(),
			);
		});
	}

	private suppressVersionBump = false;

	private bumpPresenceVersion(): void {
		if (this.suppressVersionBump) return;
		this.presenceVersion++;
	}

	private isCountedStatus(status: StatusType): boolean {
		return status !== StatusTypes.OFFLINE && status !== StatusTypes.INVISIBLE;
	}

	private isCountedPresence(presence: FlattenedPresence): boolean {
		return this.isCountedStatus(presence.status);
	}

	private adjustRemotePresenceCount(guildId: string, delta: 1 | -1): void {
		const previous = this.remotePresenceCountsByGuild.get(guildId) ?? 0;
		const next = previous + delta;
		if (next <= 0) {
			if (previous > 0) {
				this.bumpRemotePresenceCountVersion(guildId);
			}
			this.remotePresenceCountsByGuild.delete(guildId);
			return;
		}
		if (next === previous) return;
		this.remotePresenceCountsByGuild.set(guildId, next);
		this.bumpRemotePresenceCountVersion(guildId);
	}

	private bumpRemotePresenceCountVersion(guildId: string): void {
		this.remotePresenceCountVersionByGuild.set(guildId, (this.remotePresenceCountVersionByGuild.get(guildId) ?? 0) + 1);
	}

	private addPresenceCounts(presence: FlattenedPresence): void {
		if (!this.isCountedPresence(presence)) {
			return;
		}
		for (const guildId of presence.guildIds) {
			this.adjustRemotePresenceCount(guildId, 1);
		}
	}

	private removePresenceCounts(presence: FlattenedPresence): void {
		if (!this.isCountedPresence(presence)) {
			return;
		}
		for (const guildId of presence.guildIds) {
			this.adjustRemotePresenceCount(guildId, -1);
		}
	}

	private addGuildPresenceCount(presence: FlattenedPresence, guildId: string): void {
		if (this.isCountedPresence(presence)) {
			this.adjustRemotePresenceCount(guildId, 1);
		}
	}

	private removeGuildPresenceCount(presence: FlattenedPresence, guildId: string): void {
		if (this.isCountedPresence(presence)) {
			this.adjustRemotePresenceCount(guildId, -1);
		}
	}

	private syncPresenceCountsForStatusChange(presence: FlattenedPresence, oldStatus: StatusType): void {
		const wasCounted = this.isCountedStatus(oldStatus);
		const isCounted = this.isCountedPresence(presence);
		if (wasCounted === isCounted) {
			return;
		}
		const delta = isCounted ? 1 : -1;
		for (const guildId of presence.guildIds) {
			this.adjustRemotePresenceCount(guildId, delta);
		}
	}

	getStatus(userId: string): StatusType {
		return this.statuses.get(userId) ?? StatusTypes.OFFLINE;
	}

	isMobile(userId: string): boolean {
		if (userId === Authentication.currentUserId) {
			return MobileLayout.isMobileLayout();
		}
		return this.mobilePresenceUserIds.has(userId);
	}

	getCustomStatus(userId: string): CustomStatus | null {
		return this.customStatuses.get(userId) ?? null;
	}

	getPresenceCount(guildId: string): number {
		this.remotePresenceCountVersionByGuild.get(guildId);
		const currentUserId = Authentication.currentUserId;
		const localStatus = LocalPresence.getStatus();
		const localPresence =
			currentUserId &&
			GuildMembers.getMember(guildId, currentUserId) != null &&
			localStatus !== StatusTypes.OFFLINE &&
			localStatus !== StatusTypes.INVISIBLE
				? 1
				: 0;
		const remotePresences = this.remotePresenceCountsByGuild.get(guildId) ?? 0;
		return localPresence + remotePresences;
	}

	subscribeToUserStatus(userId: string, listener: StatusListener): () => void {
		let listeners = this.statusListeners.get(userId);
		if (!listeners) {
			listeners = new Set();
			this.statusListeners.set(userId, listeners);
		}
		listeners.add(listener);
		listener(userId, this.getStatus(userId), this.isMobile(userId));
		return () => {
			const currentListeners = this.statusListeners.get(userId);
			if (!currentListeners) {
				return;
			}
			currentListeners.delete(listener);
			if (currentListeners.size === 0) {
				this.statusListeners.delete(userId);
			}
		};
	}

	handleGuildMemberAdd(guildId: string, userId: string): void {
		if (userId === Authentication.currentUserId) {
			return;
		}
		const presence = this.presences.get(userId);
		if (!presence) {
			return;
		}
		if (!presence.guildIds.has(guildId)) {
			presence.guildIds.add(guildId);
			this.addGuildPresenceCount(presence, guildId);
		}
		this.bumpPresenceVersion();
	}

	handleGuildMemberRemove(guildId: string, userId: string): void {
		if (userId === Authentication.currentUserId) {
			return;
		}
		const presence = this.presences.get(userId);
		if (!presence) {
			return;
		}
		if (!presence.guildIds.has(guildId)) {
			this.bumpPresenceVersion();
			return;
		}
		if (presence.guildIds.size === 1) {
			this.evictPresence(userId);
			return;
		}
		presence.guildIds.delete(guildId);
		this.removeGuildPresenceCount(presence, guildId);
		this.bumpPresenceVersion();
	}

	handleGuildMemberUpdate(guildId: string, userId: string): void {
		if (userId === Authentication.currentUserId) {
			return;
		}
		const guild = Guilds.getGuild(guildId);
		if (!guild) {
			return;
		}
		const presence = this.presences.get(userId);
		if (!presence) {
			return;
		}
		if (!presence.guildIds.has(guildId)) {
			presence.guildIds.add(guildId);
			this.addGuildPresenceCount(presence, guildId);
		}
		presence.timestamp = Date.now();
		this.bumpPresenceVersion();
	}

	handleConnectionOpen(
		user: UserPrivate,
		guilds: Array<GuildReadyData>,
		presences?: ReadonlyArray<WirePresence>,
	): void {
		TransientPresence.clear();
		const localStatus = LocalPresence.getStatus();
		const localCustomStatus = LocalPresence.customStatus;
		this.presences.clear();
		this.remotePresenceCountsByGuild.clear();
		this.remotePresenceCountVersionByGuild.clear();
		this.statuses.clear();
		this.customStatuses.clear();
		this.mobilePresenceUserIds.clear();
		this.bumpPresenceVersion();
		this.statuses.set(user.id, localStatus);
		this.customStatuses.set(user.id, localCustomStatus);
		const userGuildIds = new Map<string, Set<string>>();
		const meContextUserIds = this.buildMeContextUserIds(user.id);
		for (const guild of guilds) {
			if (guild.unavailable) {
				continue;
			}
			this.indexGuildMembers(guild, user.id, userGuildIds);
		}
		if (presences?.length) {
			this.suppressVersionBump = true;
			try {
				for (const presence of presences) {
					const presenceUserId = presence.user.id;
					this.handleReadyPresence(presence, userGuildIds.get(presenceUserId), meContextUserIds.has(presenceUserId));
				}
			} finally {
				this.suppressVersionBump = false;
			}
			this.bumpPresenceVersion();
		}
		this.resyncExternalStatusListeners();
	}

	handleSessionInvalidated(): void {
		TransientPresence.clear();
		const previousUserIds = new Set<string>([
			...Array.from(this.presences.keys()),
			...Array.from(this.statuses.keys()),
			...Array.from(this.customStatuses.keys()),
		]);
		this.presences.clear();
		this.remotePresenceCountsByGuild.clear();
		this.remotePresenceCountVersionByGuild.clear();
		this.statuses.clear();
		this.customStatuses.clear();
		this.mobilePresenceUserIds.clear();
		this.bumpPresenceVersion();
		for (const userId of previousUserIds) {
			this.notifyStatusListeners(userId, StatusTypes.OFFLINE, false);
			queueMicrotask(() => CustomStatusEmitter.emitPresenceChange(userId));
		}
		this.resyncExternalStatusListeners();
	}

	handleGuildCreate(guild: GuildReadyData): void {
		if (guild.unavailable) {
			return;
		}
		const currentUserId = Authentication.currentUserId;
		if (!currentUserId) {
			return;
		}
		const members = guild.members;
		if (!members?.length) {
			return;
		}
		let updated = false;
		for (const member of members) {
			const userId = member.user.id;
			if (!userId || userId === currentUserId) {
				continue;
			}
			const presence = this.presences.get(userId);
			if (presence) {
				if (!presence.guildIds.has(guild.id)) {
					presence.guildIds.add(guild.id);
					this.addGuildPresenceCount(presence, guild.id);
				}
				updated = true;
			}
		}
		if (updated) {
			this.bumpPresenceVersion();
		}
	}

	handleGuildDelete(guildId: string): void {
		const usersToEvict: Array<string> = [];
		let changed = false;
		for (const [userId, presence] of this.presences) {
			if (!presence.guildIds.has(guildId)) {
				continue;
			}
			this.removeGuildPresenceCount(presence, guildId);
			presence.guildIds.delete(guildId);
			changed = true;
			if (presence.guildIds.size === 0) {
				usersToEvict.push(userId);
			}
		}
		for (const userId of usersToEvict) {
			this.evictPresence(userId);
		}
		if (changed && usersToEvict.length === 0) {
			this.bumpPresenceVersion();
		}
	}

	handlePresenceUpdate(presence: WirePresence): void {
		const {guild_id: guildIdRaw, user, status, afk, mobile, custom_status: customStatusPayload} = presence;
		const normalizedStatus = normalizeStatus(status);
		const userId = user.id;
		const customStatus = fromGatewayCustomStatus(customStatusPayload);
		if (userId === Authentication.currentUserId) {
			return;
		}
		const guildId = guildIdRaw ?? ME;
		const existing = this.presences.get(userId);
		const now = Date.now();
		if (normalizedStatus === StatusTypes.OFFLINE) {
			TransientPresence.clearPresence(userId);
		}
		if (!existing) {
			const guildIds = new Set<string>();
			guildIds.add(guildId);
			const flattened: FlattenedPresence = {
				status: normalizedStatus,
				timestamp: now,
				afk,
				mobile,
				guildIds,
				customStatus,
			};
			this.presences.set(userId, flattened);
			this.addPresenceCounts(flattened);
			this.customStatuses.set(userId, customStatus);
			this.updateStatusFromPresence(userId, flattened);
			this.bumpPresenceVersion();
			queueMicrotask(() => CustomStatusEmitter.emitPresenceChange(userId));
			return;
		}
		const oldStatus = existing.status;
		if (!existing.guildIds.has(guildId)) {
			existing.guildIds.add(guildId);
			if (this.isCountedStatus(oldStatus)) {
				this.adjustRemotePresenceCount(guildId, 1);
			}
		}
		existing.status = normalizedStatus;
		this.syncPresenceCountsForStatusChange(existing, oldStatus);
		existing.timestamp = now;
		if (afk !== undefined) {
			existing.afk = afk;
		}
		if (mobile !== undefined) {
			existing.mobile = mobile;
		}
		existing.customStatus = customStatus;
		this.customStatuses.set(userId, customStatus);
		if (normalizedStatus === StatusTypes.OFFLINE && guildIdRaw == null) {
			existing.guildIds.delete(ME);
			if (existing.guildIds.size === 0) {
				this.evictPresence(userId);
				return;
			}
		}
		this.updateStatusFromPresence(userId, existing);
		this.bumpPresenceVersion();
		queueMicrotask(() => CustomStatusEmitter.emitPresenceChange(userId));
	}

	private handleReadyPresence(presence: WirePresence, initialGuildIds?: Set<string>, hasMeContext = false): void {
		const {user, status, afk, mobile, custom_status: customStatusPayload} = presence;
		const normalizedStatus = normalizeStatus(status);
		const customStatus = fromGatewayCustomStatus(customStatusPayload);
		const userId = user.id;
		if (userId === Authentication.currentUserId) {
			return;
		}
		const now = Date.now();
		const guildIds = initialGuildIds && initialGuildIds.size > 0 ? new Set<string>(initialGuildIds) : new Set<string>();
		if (hasMeContext || guildIds.size === 0) {
			guildIds.add(ME);
		}
		const flattened: FlattenedPresence = {
			status: normalizedStatus,
			timestamp: now,
			afk,
			mobile,
			guildIds,
			customStatus,
		};
		this.presences.set(userId, flattened);
		this.addPresenceCounts(flattened);
		this.customStatuses.set(userId, customStatus);
		this.updateStatusFromPresence(userId, flattened);
		this.bumpPresenceVersion();
		queueMicrotask(() => CustomStatusEmitter.emitPresenceChange(userId));
	}

	private indexGuildMembers(
		guild: GuildReadyData,
		currentUserId: string,
		userGuildIds: Map<string, Set<string>>,
	): void {
		const members = guild.members;
		if (!members?.length) {
			return;
		}
		for (const member of members) {
			const userId = member.user.id;
			if (!userId || userId === currentUserId) {
				continue;
			}
			let guildIds = userGuildIds.get(userId);
			if (!guildIds) {
				guildIds = new Set<string>();
				userGuildIds.set(userId, guildIds);
			}
			guildIds.add(guild.id);
		}
	}

	private syncLocalPresence(): void {
		if (!Authentication) return;
		const userId = Authentication.currentUserId;
		if (!userId) {
			return;
		}
		const localStatus = LocalPresence.getStatus();
		const localCustomStatus = LocalPresence.customStatus;
		MemberSidebar.handleLocalPresenceUpdate(userId, localStatus, localCustomStatus);
		const oldStatus = this.statuses.get(userId);
		let changed = false;
		if (oldStatus !== localStatus) {
			this.statuses.set(userId, localStatus);
			this.notifyStatusListeners(userId, localStatus, this.isMobile(userId));
			changed = true;
		}
		this.customStatuses.set(userId, localCustomStatus);
		changed = true;
		if (changed) {
			this.bumpPresenceVersion();
		}
		queueMicrotask(() => CustomStatusEmitter.emitPresenceChange(userId));
	}

	private buildMeContextUserIds(currentUserId: string): Set<string> {
		const userIds = new Set<string>();
		for (const relationship of Relationships.getRelationships()) {
			if (relationship.type === RelationshipTypes.FRIEND || relationship.type === RelationshipTypes.INCOMING_REQUEST) {
				userIds.add(relationship.id);
			}
		}
		for (const channel of Channels.getPrivateChannels()) {
			if (channel.type !== ChannelTypes.GROUP_DM) {
				continue;
			}
			for (const userId of channel.recipientIds) {
				if (userId !== currentUserId) {
					userIds.add(userId);
				}
			}
		}
		return userIds;
	}

	private resyncExternalStatusListeners(): void {
		for (const userId of Array.from(this.statusListeners.keys())) {
			this.notifyStatusListeners(userId, this.getStatus(userId), this.isMobile(userId));
		}
	}

	private notifyStatusListeners(userId: string, status: StatusType, isMobile: boolean): void {
		const listeners = this.statusListeners.get(userId);
		if (!listeners || listeners.size === 0) {
			return;
		}
		for (const listener of listeners) {
			try {
				listener(userId, status, isMobile);
			} catch (error) {
				this.logger.error(`Error in status listener for user ${userId}:`, error);
			}
		}
	}

	private updateStatusFromPresence(userId: string, presence: FlattenedPresence): void {
		const oldStatus = this.statuses.get(userId) ?? StatusTypes.OFFLINE;
		const newStatus = presence.status ?? StatusTypes.OFFLINE;
		const statusChanged = oldStatus !== newStatus;
		if (statusChanged) {
			this.statuses.set(userId, newStatus);
		}
		if (presence.mobile) {
			this.mobilePresenceUserIds.set(userId, true);
		} else {
			this.mobilePresenceUserIds.delete(userId);
		}
		const newMobile = this.mobilePresenceUserIds.has(userId);
		this.notifyStatusListeners(userId, newStatus, newMobile);
	}

	private evictPresence(userId: string): void {
		TransientPresence.clearPresence(userId);
		const presence = this.presences.get(userId);
		if (presence) {
			this.removePresenceCounts(presence);
		}
		this.presences.delete(userId);
		this.mobilePresenceUserIds.delete(userId);
		this.customStatuses.delete(userId);
		this.bumpPresenceVersion();
		const oldStatus = this.statuses.get(userId);
		if (oldStatus === undefined) {
			return;
		}
		this.statuses.delete(userId);
		if (oldStatus !== StatusTypes.OFFLINE) {
			this.notifyStatusListeners(userId, StatusTypes.OFFLINE, false);
		}
	}
}

export default new Presence();
