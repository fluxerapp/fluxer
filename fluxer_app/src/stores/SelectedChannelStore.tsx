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

import {action, makeAutoObservable, reaction} from 'mobx';
import {FAVORITES_GUILD_ID, ME} from '~/Constants';
import {makePersistent} from '~/lib/MobXPersistence';
import type {Channel} from '~/records/ChannelRecord';
import NavigationStore from '~/stores/NavigationStore';
import FavoritesStore from './FavoritesStore';

interface ChannelVisit {
	channelId: string;
	guildId: string;
	timestamp: number;
}

const MAX_RECENTLY_VISITED_CHANNELS = 20;
const RECENT_CHANNEL_HISTORY_LIMIT = 12;

class SelectedChannelStore {
	selectedChannelIds = new Map<string, string>();
	recentlyVisitedChannels: Array<ChannelVisit> = [];
	private navigationDisposer: (() => void) | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initPersistence();
	}

	@action
	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'SelectedChannelStore', ['selectedChannelIds', 'recentlyVisitedChannels']);
		this.migrateRecentVisits();
		this.setupNavigationReaction();
	}

	private setupNavigationReaction(): void {
		this.navigationDisposer?.();
		this.navigationDisposer = reaction(
			() => [NavigationStore.guildId, NavigationStore.channelId],
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
		return this.normalizeGuildId(NavigationStore.guildId);
	}

	get currentChannelId(): string | null {
		const guildId = this.getCurrentGuildId();
		if (guildId == null) return null;
		return this.selectedChannelIds.get(guildId) ?? null;
	}

	@action
	private migrateRecentVisits(): void {
		const needsMigration = this.recentlyVisitedChannels.some((visit) => !visit.guildId);
		if (needsMigration) {
			this.recentlyVisitedChannels = [];
		}
	}

	private getSortedRecentVisits(): Array<ChannelVisit> {
		return [...this.recentlyVisitedChannels].sort((a, b) => b.timestamp - a.timestamp);
	}

	get recentChannels(): ReadonlyArray<string> {
		return this.getSortedRecentVisits()
			.slice(0, RECENT_CHANNEL_HISTORY_LIMIT)
			.map((visit) => visit.channelId);
	}

	get recentChannelVisits(): ReadonlyArray<{channelId: string; guildId: string}> {
		return this.getSortedRecentVisits()
			.slice(0, RECENT_CHANNEL_HISTORY_LIMIT)
			.map((visit) => ({channelId: visit.channelId, guildId: visit.guildId}));
	}

	@action
	selectChannel(guildId?: string, channelId?: string | null): void {
		const normalizedGuildId = this.normalizeGuildId(guildId ?? null);
		if (!normalizedGuildId) return;

		if (channelId == null) {
			this.removeGuildSelection(normalizedGuildId);
			return;
		}

		this.updateRecentVisit(normalizedGuildId, channelId);
		this.selectedChannelIds.set(normalizedGuildId, channelId);
	}

	private updateRecentVisit(guildId: string, channelId: string): void {
		const now = Date.now();
		const existingIndex = this.recentlyVisitedChannels.findIndex(
			(visit) => visit.channelId === channelId && visit.guildId === guildId,
		);

		if (existingIndex >= 0) {
			const updated = [...this.recentlyVisitedChannels];
			updated[existingIndex] = {...updated[existingIndex], timestamp: now};
			this.recentlyVisitedChannels = updated;
		} else {
			this.recentlyVisitedChannels = [...this.recentlyVisitedChannels, {channelId, guildId, timestamp: now}];
		}

		this.pruneRecentVisits();
	}

	private pruneRecentVisits(): void {
		if (this.recentlyVisitedChannels.length <= MAX_RECENTLY_VISITED_CHANNELS) {
			return;
		}

		this.recentlyVisitedChannels = [...this.recentlyVisitedChannels]
			.sort((a, b) => b.timestamp - a.timestamp)
			.slice(0, MAX_RECENTLY_VISITED_CHANNELS);
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
		const selectedChannelId = this.selectedChannelIds.get(FAVORITES_GUILD_ID);

		if (selectedChannelId && FavoritesStore.isChannelAccessible(selectedChannelId)) {
			return selectedChannelId;
		}

		const firstAccessible = FavoritesStore.getFirstAccessibleChannel();
		if (firstAccessible) {
			this.selectChannel(FAVORITES_GUILD_ID, firstAccessible.channelId);
			return firstAccessible.channelId;
		}

		this.removeGuildSelection(FAVORITES_GUILD_ID);
		return null;
	}
}

export default new SelectedChannelStore();
