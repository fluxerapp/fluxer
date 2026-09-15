// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import Favorites from '@app/features/messaging/state/Favorites';
import Navigation from '@app/features/navigation/state/Navigation';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {FAVORITES_GUILD_ID, ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {Channel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {action, computed, makeAutoObservable, reaction} from 'mobx';

interface ChannelVisit {
	channelId: string;
	guildId: string;
	timestamp: number;
}

interface ChannelHistoryEntry {
	channelId: string;
	guildId: string;
}

const MAX_RECENTLY_VISITED_CHANNELS = 20;
const RECENT_CHANNEL_HISTORY_LIMIT = 12;
const VIEWED_CHANNEL_HISTORY_LIMIT = 50;

class SelectedChannel {
	selectedChannelIds = new Map<string, string>();
	recentlyVisitedChannels: Array<ChannelVisit> = [];
	private navigationDisposer: (() => void) | null = null;
	private viewedChannelHistory: Array<ChannelHistoryEntry> = [];
	private viewedChannelHistoryIndex = -1;
	private isApplyingViewedChannelHistory = false;

	constructor() {
		makeAutoObservable(
			this,
			{
				sortedRecentVisits: computed,
				recentChannelVisits: computed,
			},
			{autoBind: true},
		);
		void this.initPersistence();
	}

	@action
	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'SelectedChannel', ['selectedChannelIds', 'recentlyVisitedChannels']);
		this.migrateRecentVisits();
		this.setupNavigationReaction();
	}

	private setupNavigationReaction(): void {
		this.navigationDisposer?.();
		this.navigationDisposer = reaction(
			() => [Navigation.guildId, Navigation.channelId],
			([guildId, channelId]) => {
				if (!guildId || !channelId) {
					return;
				}
				this.selectChannel(guildId, channelId);
			},
			{
				fireImmediately: true,
			},
		);
	}

	private normalizeGuildId(guildId: string | null | undefined): string | null {
		if (!guildId) return null;
		if (guildId === '@favorites') return FAVORITES_GUILD_ID;
		return guildId;
	}

	private getCurrentGuildId(): string | null {
		return this.normalizeGuildId(Navigation.guildId);
	}

	private isStoredChannelNavigable(channelId: string): boolean {
		const channel = Channels.getChannel(channelId);
		if (!channel || channel.isPrivate()) return true;
		return channel.type !== ChannelTypes.GUILD_CATEGORY && channel.type !== ChannelTypes.GUILD_LINK;
	}

	get currentChannelId(): string | null {
		const guildId = this.getCurrentGuildId();
		if (guildId == null) return null;
		return this.selectedChannelIds.get(guildId) ?? null;
	}

	@action
	private migrateRecentVisits(): void {
		let needsMigration = false;
		for (let i = 0; i < this.recentlyVisitedChannels.length; i++) {
			if (!this.recentlyVisitedChannels[i].guildId) {
				needsMigration = true;
				break;
			}
		}
		if (needsMigration) {
			this.recentlyVisitedChannels.length = 0;
		}
	}

	get sortedRecentVisits(): ReadonlyArray<ChannelVisit> {
		const sorted = this.recentlyVisitedChannels.slice();
		sorted.sort((a, b) => b.timestamp - a.timestamp);
		return sorted;
	}

	get recentChannelVisits(): ReadonlyArray<{
		channelId: string;
		guildId: string;
	}> {
		const sorted = this.sortedRecentVisits;
		const limit = Math.min(sorted.length, RECENT_CHANNEL_HISTORY_LIMIT);
		const result: Array<{
			channelId: string;
			guildId: string;
		}> = new Array(limit);
		for (let i = 0; i < limit; i++) {
			const visit = sorted[i];
			result[i] = {channelId: visit.channelId, guildId: visit.guildId};
		}
		return result;
	}

	@action
	selectChannel(guildId?: string, channelId?: string | null): void {
		const normalizedGuildId = this.normalizeGuildId(guildId ?? null);
		if (!normalizedGuildId) return;
		if (channelId == null) {
			this.removeGuildSelection(normalizedGuildId);
			return;
		}
		if (!this.isStoredChannelNavigable(channelId)) {
			this.removeGuildSelection(normalizedGuildId);
			return;
		}
		const existingSelection = this.selectedChannelIds.get(normalizedGuildId);
		if (existingSelection !== channelId) {
			this.selectedChannelIds.set(normalizedGuildId, channelId);
		}
		this.updateRecentVisit(normalizedGuildId, channelId);
		this.recordViewedChannel(normalizedGuildId, channelId);
	}

	private updateRecentVisit(guildId: string, channelId: string): void {
		const now = Date.now();
		const list = this.recentlyVisitedChannels;
		for (let i = 0; i < list.length; i++) {
			const visit = list[i];
			if (visit.channelId === channelId && visit.guildId === guildId) {
				visit.timestamp = now;
				return;
			}
		}
		list.push({channelId, guildId, timestamp: now});
		this.pruneRecentVisits();
	}

	private pruneRecentVisits(): void {
		if (this.recentlyVisitedChannels.length <= MAX_RECENTLY_VISITED_CHANNELS) {
			return;
		}
		this.recentlyVisitedChannels.sort((a, b) => b.timestamp - a.timestamp);
		this.recentlyVisitedChannels.splice(MAX_RECENTLY_VISITED_CHANNELS);
	}

	private recordViewedChannel(guildId: string, channelId: string): void {
		if (this.isApplyingViewedChannelHistory) {
			return;
		}
		const current = this.viewedChannelHistory[this.viewedChannelHistoryIndex];
		if (current?.guildId === guildId && current.channelId === channelId) {
			return;
		}
		if (this.viewedChannelHistoryIndex < this.viewedChannelHistory.length - 1) {
			this.viewedChannelHistory.splice(this.viewedChannelHistoryIndex + 1);
		}
		this.viewedChannelHistory.push({guildId, channelId});
		if (this.viewedChannelHistory.length > VIEWED_CHANNEL_HISTORY_LIMIT) {
			this.viewedChannelHistory.splice(0, this.viewedChannelHistory.length - VIEWED_CHANNEL_HISTORY_LIMIT);
		}
		this.viewedChannelHistoryIndex = this.viewedChannelHistory.length - 1;
	}

	@action
	navigateViewedChannelHistory(direction: -1 | 1): boolean {
		let nextIndex = this.viewedChannelHistoryIndex + direction;
		let target = this.viewedChannelHistory[nextIndex];
		while (target && !this.isStoredChannelNavigable(target.channelId)) {
			nextIndex += direction;
			target = this.viewedChannelHistory[nextIndex];
		}
		if (!target) {
			return false;
		}
		this.viewedChannelHistoryIndex = nextIndex;
		this.isApplyingViewedChannelHistory = true;
		try {
			if (target.guildId === ME) {
				Navigation.navigateToDM(target.channelId);
			} else if (target.guildId === FAVORITES_GUILD_ID) {
				Navigation.navigateToFavorites(target.channelId);
			} else {
				Navigation.navigateToGuild(target.guildId, target.channelId);
			}
		} finally {
			this.isApplyingViewedChannelHistory = false;
		}
		return true;
	}

	@action
	deselectChannel(): void {
		const guildId = this.getCurrentGuildId();
		if (guildId != null) {
			this.removeGuildSelection(guildId);
		}
	}

	@action
	clearGuildSelection(guildId: string): void {
		const normalizedGuildId = this.normalizeGuildId(guildId);
		if (!normalizedGuildId) return;
		this.removeGuildSelection(normalizedGuildId);
	}

	@action
	getNavigableSelectedChannelId(guildId: string): string | null {
		const normalizedGuildId = this.normalizeGuildId(guildId);
		if (!normalizedGuildId) return null;
		const channelId = this.selectedChannelIds.get(normalizedGuildId);
		if (!channelId) return null;
		if (this.isStoredChannelNavigable(channelId)) {
			return channelId;
		}
		this.removeGuildSelection(normalizedGuildId);
		return null;
	}

	@action
	handleChannelDelete(channel: Channel): void {
		const guildId = channel.guild_id ?? ME;
		const normalizedGuildId = this.normalizeGuildId(guildId) ?? guildId;
		const selectedChannelId = this.selectedChannelIds.get(normalizedGuildId);
		if (selectedChannelId === channel.id) {
			this.removeGuildSelection(normalizedGuildId);
		}
	}

	@action
	private removeGuildSelection(guildId: string): void {
		this.selectedChannelIds.delete(guildId);
	}

	@action
	getValidatedFavoritesChannel(): string | null {
		const selectedChannelId = this.getNavigableSelectedChannelId(FAVORITES_GUILD_ID);
		if (selectedChannelId && Favorites.isChannelAccessible(selectedChannelId)) {
			return selectedChannelId;
		}
		for (const favoriteChannel of Favorites.sortedChannels) {
			if (!Favorites.isChannelAccessible(favoriteChannel.channelId)) continue;
			if (!this.isStoredChannelNavigable(favoriteChannel.channelId)) continue;
			this.selectChannel(FAVORITES_GUILD_ID, favoriteChannel.channelId);
			return favoriteChannel.channelId;
		}
		this.removeGuildSelection(FAVORITES_GUILD_ID);
		return null;
	}
}

export default new SelectedChannel();
