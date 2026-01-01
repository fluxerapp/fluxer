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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {action, makeAutoObservable, reaction} from 'mobx';
import {ChannelTypes} from '~/Constants';
import type {UserRecord} from '~/records/UserRecord';
import UserStore from '~/stores/UserStore';

interface ChannelSnapshot {
	readonly id: string;
	readonly name?: string;
	readonly recipientIds: ReadonlyArray<string>;
	readonly type: number;
	readonly nicks: Readonly<Record<string, string>>;
}

class ChannelDisplayNameStore {
	private readonly channelSnapshots = new Map<string, ChannelSnapshot>();
	private readonly displayNames = new Map<string, string>();
	private i18n: I18n | null = null;

	constructor() {
		makeAutoObservable<this, 'recomputeChannel' | 'recomputeAll'>(
			this,
			{
				syncChannel: action,
				removeChannel: action,
				recomputeChannel: action,
				recomputeAll: action,
				clear: action,
			},
			{autoBind: true},
		);

		reaction(
			() => {
				if (!UserStore) return [];
				return UserStore.usersList.map((user) => ({
					id: user.id,
					username: user.username,
					globalName: user.globalName,
				}));
			},
			() => this.recomputeAll(),
		);
	}

	setI18n(i18n: I18n): void {
		this.i18n = i18n;
	}

	getDisplayName(channelId: string): string | undefined {
		return this.displayNames.get(channelId);
	}

	clear(): void {
		this.channelSnapshots.clear();
		this.displayNames.clear();
	}

	syncChannel(channel: ChannelSnapshot): void {
		if (!this.shouldTrackChannel(channel)) {
			this.channelSnapshots.delete(channel.id);
			this.displayNames.delete(channel.id);
			return;
		}

		this.channelSnapshots.set(channel.id, channel);
		this.recomputeChannel(channel);
	}

	removeChannel(channelId: string): void {
		this.channelSnapshots.delete(channelId);
		this.displayNames.delete(channelId);
	}

	private recomputeAll(): void {
		for (const snapshot of this.channelSnapshots.values()) {
			this.recomputeChannel(snapshot);
		}
	}

	private recomputeChannel(snapshot: ChannelSnapshot): void {
		const displayName = this.computeGroupDMDisplayName(snapshot);
		this.displayNames.set(snapshot.id, displayName);
	}

	private shouldTrackChannel(snapshot: ChannelSnapshot): boolean {
		if (snapshot.type !== ChannelTypes.GROUP_DM) {
			return false;
		}

		return !(snapshot.name && snapshot.name.trim().length > 0);
	}

	private computeGroupDMDisplayName(snapshot: ChannelSnapshot): string {
		if (!this.i18n) {
			throw new Error('ChannelDisplayNameStore: i18n not initialized');
		}
		const currentUser = UserStore.getCurrentUser();
		const currentUserId = currentUser?.id ?? null;
		const otherIds = snapshot.recipientIds.filter((id) => id !== currentUserId);

		if (otherIds.length === 0) {
			if (currentUser) {
				const resolvedName = this.getBaseName(currentUser, snapshot);

				if (resolvedName && resolvedName.length > 0) {
					const translatedGroupName = this.i18n._(msg`${resolvedName}'s Group`);
					if (translatedGroupName.includes(resolvedName)) {
						return translatedGroupName;
					}
					return `${resolvedName}'s Group`;
				}
			}

			return this.i18n._(msg`Unnamed Group`);
		}

		if (otherIds.length === 1) {
			const displayName = this.getUserDisplayName(snapshot, otherIds[0]);
			if (displayName) {
				return displayName;
			}

			return this.i18n._(msg`Unnamed Group`);
		}

		if (otherIds.length <= 4) {
			const names = [...otherIds]
				.sort((a, b) => b.localeCompare(a))
				.map((userId) => this.getUserDisplayName(snapshot, userId))
				.filter((name): name is string => Boolean(name));

			return names.length > 0 ? names.join(', ') : this.i18n._(msg`Unnamed Group`);
		}

		return this.i18n._(msg`Unnamed Group`);
	}

	private getBaseName(user: UserRecord, snapshot: ChannelSnapshot): string {
		const overrideNick = snapshot.nicks?.[user.id];
		return overrideNick ?? user.displayName;
	}

	private getUserDisplayName(snapshot: ChannelSnapshot, userId: string): string | null {
		const user = UserStore.getUser(userId);
		if (!user) {
			return null;
		}

		const overrideNick = snapshot.nicks?.[user.id];
		const baseName = overrideNick ?? user.displayName;
		return baseName || null;
	}
}

export default new ChannelDisplayNameStore();
