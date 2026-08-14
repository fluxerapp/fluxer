// SPDX-License-Identifier: AGPL-3.0-or-later

import {onLocaleChange} from '@app/features/i18n/utils/LocaleChangeListener';
import {deferUntilModulesLoaded} from '@app/features/platform/utils/DeferUntilModulesLoaded';
import {noteText} from '@app/features/theme/fonts/ScriptFontLoader';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {action, makeAutoObservable, reaction} from 'mobx';

const S_GROUP_DESCRIPTOR = msg({
	message: "{resolvedName}'s group",
	comment:
		'Short label in the channel display name state. Keep it concise. Preserve {resolvedName}; it is inserted by code.',
});
const UNNAMED_GROUP_DESCRIPTOR = msg({
	message: 'Unnamed group',
	comment: 'Short label in the channel display name state. Keep it concise.',
});

interface ChannelSnapshot {
	readonly id: string;
	readonly name?: string;
	readonly recipientIds: ReadonlyArray<string>;
	readonly type: number;
	readonly nicks: Readonly<Record<string, string>>;
}

class ChannelDisplayName {
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
		deferUntilModulesLoaded(() => {
			reaction(
				() => {
					if (!Users) return [];
					return Users.usersList.map((user) => ({
						id: user.id,
						username: user.username,
						globalName: user.globalName,
					}));
				},
				() => this.recomputeAll(),
			);
		});
	}

	setI18n(i18n: I18n): void {
		this.i18n = i18n;
		onLocaleChange(() => this.recomputeAll());
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
		noteText(displayName);
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
			throw new Error('ChannelDisplayName: i18n not initialized');
		}
		const currentUser = Users.getCurrentUser();
		const currentUserId = currentUser?.id ?? null;
		const otherIds = snapshot.recipientIds.filter((id) => id !== currentUserId);
		if (otherIds.length === 0) {
			if (currentUser) {
				const resolvedName = this.getBaseName(currentUser, snapshot);
				if (resolvedName && resolvedName.length > 0) {
					const translatedGroupName = this.i18n._(S_GROUP_DESCRIPTOR, {resolvedName});
					if (translatedGroupName.includes(resolvedName)) {
						return translatedGroupName;
					}
					return `${resolvedName}'s Group`;
				}
			}
			return this.i18n._(UNNAMED_GROUP_DESCRIPTOR);
		}
		if (otherIds.length === 1) {
			const displayName = this.getUserDisplayName(snapshot, otherIds[0]);
			if (displayName) {
				return displayName;
			}
			return this.i18n._(UNNAMED_GROUP_DESCRIPTOR);
		}
		if (otherIds.length <= 4) {
			const names = [...otherIds]
				.sort((a, b) => b.localeCompare(a))
				.map((userId) => this.getUserDisplayName(snapshot, userId))
				.filter((name): name is string => Boolean(name));
			return names.length > 0 ? names.join(', ') : this.i18n._(UNNAMED_GROUP_DESCRIPTOR);
		}
		return this.i18n._(UNNAMED_GROUP_DESCRIPTOR);
	}

	private getBaseName(user: User, snapshot: ChannelSnapshot): string {
		const overrideNick = snapshot.nicks?.[user.id];
		return overrideNick ?? user.displayName;
	}

	private getUserDisplayName(snapshot: ChannelSnapshot, userId: string): string | null {
		const user = Users.getUser(userId);
		if (!user) {
			return null;
		}
		const overrideNick = snapshot.nicks?.[user.id];
		const baseName = overrideNick ?? user.displayName;
		return baseName || null;
	}
}

export default new ChannelDisplayName();
