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

import {makeAutoObservable, reaction} from 'mobx';
import type {StatusType} from '~/Constants';
import {ChannelTypes, ME, normalizeStatus, RelationshipTypes, StatusTypes} from '~/Constants';
import {type CustomStatus, fromGatewayCustomStatus, type GatewayCustomStatusPayload} from '~/lib/customStatus';
import type {GuildReadyData} from '~/records/GuildRecord';
import type {UserPartial, UserPrivate} from '~/records/UserRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import ChannelStore from '~/stores/ChannelStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import LocalPresenceStore from '~/stores/LocalPresenceStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import RelationshipStore from '~/stores/RelationshipStore';

export interface Presence {
	readonly guild_id?: string | null;
	readonly user: UserPartial;
	readonly status?: string | null;
	readonly afk?: boolean;
	readonly mobile?: boolean;
	readonly custom_status?: GatewayCustomStatusPayload | null;
}

interface FlattenedPresence {
	status: StatusType;
	timestamp: number;
	afk?: boolean;
	mobile?: boolean;
	guildIds: Set<string>;
	customStatus: CustomStatus | null;
}

type StatusListener = (userId: string, status: StatusType, isMobile: boolean) => void;

class PresenceStore {
	private presences = new Map<string, FlattenedPresence>();
	private customStatuses = new Map<string, CustomStatus | null>();

	statuses = new Map<string, StatusType>();

	presenceVersion = 0;

	private statusListeners: Map<string, Set<StatusListener>> = new Map();

	constructor() {
		makeAutoObservable<this, 'statusListeners' | 'presences'>(
			this,
			{
				statusListeners: false,
				presences: false,
			},
			{autoBind: true},
		);

		reaction(
			() => ({status: LocalPresenceStore.status, customStatus: LocalPresenceStore.customStatus}),
			() => this.syncLocalPresence(),
		);
	}

	private bumpPresenceVersion(): void {
		this.presenceVersion++;
	}

	getStatus(userId: string): StatusType {
		return this.statuses.get(userId) ?? StatusTypes.OFFLINE;
	}

	isMobile(userId: string): boolean {
		if (userId === AuthenticationStore.currentUserId) {
			return MobileLayoutStore.isMobileLayout();
		}
		return this.presences.get(userId)?.mobile ?? false;
	}

	getCustomStatus(userId: string): CustomStatus | null {
		return this.customStatuses.get(userId) ?? null;
	}

	getPresenceCount(guildId: string): number {
		void this.presenceVersion;

		const currentUserId = AuthenticationStore.currentUserId;
		const localStatus = LocalPresenceStore.getStatus();
		const localPresence =
			currentUserId &&
			GuildMemberStore.getMember(guildId, currentUserId) != null &&
			localStatus !== StatusTypes.OFFLINE &&
			localStatus !== StatusTypes.INVISIBLE
				? 1
				: 0;

		let remotePresences = 0;

		for (const presence of this.presences.values()) {
			if (
				presence.guildIds.has(guildId) &&
				presence.status !== StatusTypes.OFFLINE &&
				presence.status !== StatusTypes.INVISIBLE
			) {
				remotePresences++;
			}
		}

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
		if (userId === AuthenticationStore.currentUserId) {
			return;
		}

		const presence = this.presences.get(userId);
		if (!presence) {
			return;
		}

		presence.guildIds.add(guildId);
		this.bumpPresenceVersion();
	}

	handleGuildMemberRemove(guildId: string, userId: string): void {
		if (userId === AuthenticationStore.currentUserId) {
			return;
		}

		const presence = this.presences.get(userId);
		if (!presence) {
			return;
		}

		if (presence.guildIds.delete(guildId) && presence.guildIds.size === 0) {
			this.evictPresence(userId);
			return;
		}

		this.bumpPresenceVersion();
	}

	handleGuildMemberUpdate(guildId: string, userId: string): void {
		if (userId === AuthenticationStore.currentUserId) {
			return;
		}

		const guild = GuildStore.getGuild(guildId);
		if (!guild) {
			return;
		}

		const presence = this.presences.get(userId);
		if (!presence) {
			return;
		}

		presence.guildIds.add(guildId);
		presence.timestamp = Date.now();
		this.bumpPresenceVersion();
	}

	handleConnectionOpen(user: UserPrivate, guilds: Array<GuildReadyData>, presences?: ReadonlyArray<Presence>): void {
		const localStatus = LocalPresenceStore.getStatus();
		const localCustomStatus = LocalPresenceStore.customStatus;

		this.presences.clear();
		this.statuses.clear();
		this.customStatuses.clear();
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
			for (const presence of presences) {
				const presenceUserId = presence.user.id;
				this.handleReadyPresence(presence, userGuildIds.get(presenceUserId), meContextUserIds.has(presenceUserId));
			}
		}

		this.resyncExternalStatusListeners();
	}

	handleGuildCreate(guild: GuildReadyData): void {
		if (guild.unavailable) {
			return;
		}

		const currentUserId = AuthenticationStore.currentUserId;
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
				presence.guildIds.add(guild.id);
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

	handlePresenceUpdate(presence: Presence): void {
		const {guild_id: guildIdRaw, user, status, afk, mobile, custom_status: customStatusPayload} = presence;
		const normalizedStatus = normalizeStatus(status);
		const userId = user.id;
		const customStatus = fromGatewayCustomStatus(customStatusPayload);

		if (userId === AuthenticationStore.currentUserId) {
			return;
		}

		const guildId = guildIdRaw ?? ME;

		const existing = this.presences.get(userId);
		const now = Date.now();

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
			this.customStatuses.set(userId, customStatus);
			this.updateStatusFromPresence(userId, flattened);
			this.bumpPresenceVersion();
			return;
		}

		existing.guildIds.add(guildId);
		existing.status = normalizedStatus;
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
	}

	private handleReadyPresence(presence: Presence, initialGuildIds?: Set<string>, hasMeContext = false): void {
		const {user, status, afk, mobile, custom_status: customStatusPayload} = presence;
		const normalizedStatus = normalizeStatus(status);
		const customStatus = fromGatewayCustomStatus(customStatusPayload);
		const userId = user.id;

		if (userId === AuthenticationStore.currentUserId) {
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
		this.customStatuses.set(userId, customStatus);
		this.updateStatusFromPresence(userId, flattened);
		this.bumpPresenceVersion();
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
		if (!AuthenticationStore) return;
		const userId = AuthenticationStore.currentUserId;
		if (!userId) {
			return;
		}

		const localStatus = LocalPresenceStore.getStatus();
		const localCustomStatus = LocalPresenceStore.customStatus;
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
	}

	private buildMeContextUserIds(currentUserId: string): Set<string> {
		const userIds = new Set<string>();

		for (const relationship of RelationshipStore.getRelationships()) {
			if (relationship.type === RelationshipTypes.FRIEND || relationship.type === RelationshipTypes.INCOMING_REQUEST) {
				userIds.add(relationship.id);
			}
		}

		for (const channel of ChannelStore.getPrivateChannels()) {
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
				console.error(`Error in status listener for user ${userId}:`, error);
			}
		}
	}

	private updateStatusFromPresence(userId: string, presence: FlattenedPresence): void {
		const oldStatus = this.statuses.get(userId) ?? StatusTypes.OFFLINE;
		const newStatus = presence.status ?? StatusTypes.OFFLINE;
		const newMobile = presence.mobile ?? false;

		const statusChanged = oldStatus !== newStatus;

		if (statusChanged) {
			this.statuses.set(userId, newStatus);
		}

		this.notifyStatusListeners(userId, newStatus, newMobile);
	}

	private evictPresence(userId: string): void {
		this.presences.delete(userId);
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

export default new PresenceStore();
