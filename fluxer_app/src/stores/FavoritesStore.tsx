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

import {makeAutoObservable} from 'mobx';
import {makePersistent} from '~/lib/MobXPersistence';
import ChannelStore from './ChannelStore';
import UserGuildSettingsStore from './UserGuildSettingsStore';

export interface FavoriteChannel {
	channelId: string;
	guildId: string;
	parentId: string | null;
	position: number;
	nickname: string | null;
}

export interface FavoriteCategory {
	id: string;
	name: string;
	position: number;
}

class FavoritesStore {
	channels: Array<FavoriteChannel> = [];
	categories: Array<FavoriteCategory> = [];
	collapsedCategories = new Set<string>();
	hideMutedChannels: boolean = false;
	isMuted: boolean = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'FavoritesStore', [
			'channels',
			'categories',
			'collapsedCategories',
			'hideMutedChannels',
			'isMuted',
		]);
	}

	get hasAnyFavorites(): boolean {
		return this.channels.length > 0;
	}

	get sortedCategories(): ReadonlyArray<FavoriteCategory> {
		return [...this.categories].sort((a, b) => a.position - b.position);
	}

	get sortedChannels(): ReadonlyArray<FavoriteChannel> {
		return [...this.channels].sort((a, b) => a.position - b.position);
	}

	getChannel(channelId: string): FavoriteChannel | undefined {
		return this.channels.find((ch) => ch.channelId === channelId);
	}

	getCategory(categoryId: string): FavoriteCategory | undefined {
		return this.categories.find((cat) => cat.id === categoryId);
	}

	getChannelsInCategory(categoryId: string | null): ReadonlyArray<FavoriteChannel> {
		return this.sortedChannels.filter((ch) => ch.parentId === categoryId);
	}

	isCategoryCollapsed(categoryId: string): boolean {
		return this.collapsedCategories.has(categoryId);
	}

	getFirstAccessibleChannel(): FavoriteChannel | undefined {
		for (const fav of this.sortedChannels) {
			const channel = ChannelStore.getChannel(fav.channelId);
			if (!channel) continue;

			if (this.hideMutedChannels && channel.guildId) {
				if (UserGuildSettingsStore.isGuildOrChannelMuted(channel.guildId, channel.id)) {
					continue;
				}
			}

			return fav;
		}

		return undefined;
	}

	isChannelAccessible(channelId: string): boolean {
		const fav = this.getChannel(channelId);
		if (!fav) return false;

		const channel = ChannelStore.getChannel(fav.channelId);
		if (!channel) return false;

		if (this.hideMutedChannels && channel.guildId) {
			if (UserGuildSettingsStore.isGuildOrChannelMuted(channel.guildId, channel.id)) {
				return false;
			}
		}

		return true;
	}

	addChannel(channelId: string, guildId: string, parentId: string | null = null): void {
		const existing = this.channels.find((ch) => ch.channelId === channelId);
		if (existing) return;

		const position = this.channels.length;
		this.channels.push({
			channelId,
			guildId,
			parentId,
			position,
			nickname: null,
		});
	}

	addChannels(channelIds: Array<string>, guildId: string, parentId: string | null = null): void {
		for (const channelId of channelIds) {
			this.addChannel(channelId, guildId, parentId);
		}
	}

	removeChannel(channelId: string): void {
		const index = this.channels.findIndex((ch) => ch.channelId === channelId);
		if (index === -1) return;

		this.channels.splice(index, 1);
		this.reorderChannels();
	}

	setChannelNickname(channelId: string, nickname: string | null): void {
		const channel = this.channels.find((ch) => ch.channelId === channelId);
		if (!channel) return;

		channel.nickname = nickname;
	}

	moveChannel(channelId: string, newParentId: string | null, newIndexInCategory: number): void {
		const channel = this.channels.find((ch) => ch.channelId === channelId);
		if (!channel) return;

		const sorted = [...this.sortedChannels];
		const currentIndex = sorted.findIndex((ch) => ch.channelId === channelId);
		if (currentIndex === -1) return;

		sorted.splice(currentIndex, 1);

		channel.parentId = newParentId;

		const categoryChannels = sorted.filter((ch) => ch.parentId === newParentId);

		if (newIndexInCategory >= categoryChannels.length) {
			const lastInCat = categoryChannels[categoryChannels.length - 1];
			if (lastInCat) {
				const lastIndex = sorted.indexOf(lastInCat);
				sorted.splice(lastIndex + 1, 0, channel);
			} else {
				sorted.push(channel);
			}
		} else {
			const targetChannel = categoryChannels[newIndexInCategory];
			const targetIndex = sorted.indexOf(targetChannel);
			sorted.splice(targetIndex, 0, channel);
		}

		this.channels = sorted;
		this.reorderChannels();
	}

	createCategory(name: string): string {
		const id = `favorite-category-${Date.now()}`;
		const position = this.categories.length;
		this.categories.push({id, name, position});
		return id;
	}

	renameCategory(categoryId: string, name: string): void {
		const category = this.categories.find((cat) => cat.id === categoryId);
		if (!category) return;

		category.name = name;
	}

	removeCategory(categoryId: string): void {
		const index = this.categories.findIndex((cat) => cat.id === categoryId);
		if (index === -1) return;

		this.categories.splice(index, 1);

		for (const channel of this.channels) {
			if (channel.parentId === categoryId) {
				channel.parentId = null;
			}
		}

		this.collapsedCategories.delete(categoryId);
		this.reorderCategories();
	}

	toggleCategoryCollapsed(categoryId: string): void {
		if (this.collapsedCategories.has(categoryId)) {
			this.collapsedCategories.delete(categoryId);
		} else {
			this.collapsedCategories.add(categoryId);
		}
	}

	setHideMutedChannels(value: boolean): void {
		this.hideMutedChannels = value;
	}

	toggleMuted(): void {
		this.isMuted = !this.isMuted;
	}

	private reorderChannels(): void {
		this.channels = this.channels.map((ch, index) => ({
			...ch,
			position: index,
		}));
	}

	private reorderCategories(): void {
		this.categories = this.categories.map((cat, index) => ({
			...cat,
			position: index,
		}));
	}
}

export default new FavoritesStore();
