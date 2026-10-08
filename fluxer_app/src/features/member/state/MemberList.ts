// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage from '@app/features/platform/state/PersistentStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {MemberListStateSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable, reaction} from 'mobx';

const logger = new Logger('MemberList');
const DEFAULT_HIDDEN_CHANNEL_MEMBER_LIST_STORAGE_KEY = 'member_list_default_hidden_channel_overrides';
const getInitialWidth = (): number => window.innerWidth;

interface MemberListVisibilityOptions {
	channelId?: string | null;
	defaultHiddenForChannel?: boolean;
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value != null && !Array.isArray(value);
}

function parseBooleanMap(value: unknown): Record<string, boolean> {
	if (!isRecord(value)) {
		return {};
	}
	const result: Record<string, boolean> = {};
	for (const [key, rawValue] of Object.entries(value)) {
		if (typeof rawValue !== 'boolean' || !rawValue) {
			continue;
		}
		result[key] = true;
	}
	return result;
}

function getInitialDefaultHiddenChannelState(): Record<string, boolean> {
	return parseBooleanMap(AppStorage.getJSON<unknown>(DEFAULT_HIDDEN_CHANNEL_MEMBER_LIST_STORAGE_KEY));
}

class MemberList {
	isMembersOpen = getInitialWidth() >= 1024;
	defaultHiddenChannelMembersOpenByChannelId: Record<string, boolean> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.defaultHiddenChannelMembersOpenByChannelId = getInitialDefaultHiddenChannelState();
		initializeStore(this, () => this.initPersistence());
	}

	private async initPersistence(): Promise<void> {
		const initialDefault = this.isMembersOpen;
		await makeSyncedField(this, {
			field: 'memberList',
			schema: MemberListStateSchema,
			persist: ['isMembersOpen'],
			toMessage: (s) => ({
				membersOpen: s.isMembersOpen === initialDefault ? undefined : s.isMembersOpen,
			}),
			applyMessage: (s, m) => {
				if (m.membersOpen !== undefined) {
					s.isMembersOpen = m.membersOpen;
				}
			},
		});
	}

	toggleMembers(): void {
		this.isMembersOpen = !this.isMembersOpen;
		logger.debug(`Toggled members list: ${this.isMembersOpen}`);
	}

	isMembersVisible(options: MemberListVisibilityOptions = {}): boolean {
		if (!this.isMembersOpen) {
			return false;
		}
		if (!options.defaultHiddenForChannel) {
			return true;
		}
		return this.isDefaultHiddenChannelMembersOpen(options.channelId);
	}

	isDefaultHiddenChannelMembersOpen(channelId?: string | null): boolean {
		if (!channelId) {
			return false;
		}
		return this.defaultHiddenChannelMembersOpenByChannelId[channelId] === true;
	}

	toggleDefaultHiddenChannelMembers(channelId?: string | null): void {
		if (!channelId) {
			return;
		}
		this.setDefaultHiddenChannelMembersOpen(channelId, !this.isDefaultHiddenChannelMembersOpen(channelId));
	}

	private setDefaultHiddenChannelMembersOpen(channelId: string, isOpen: boolean): void {
		const next = {...this.defaultHiddenChannelMembersOpenByChannelId};
		if (isOpen) {
			next[channelId] = true;
		} else {
			delete next[channelId];
		}
		this.defaultHiddenChannelMembersOpenByChannelId = next;
		AppStorage.setJSON(DEFAULT_HIDDEN_CHANNEL_MEMBER_LIST_STORAGE_KEY, next);
		logger.debug(`Toggled default-hidden channel members list: channel=${channelId}, open=${isOpen}`);
	}

	subscribe(callback: () => void): () => void {
		return reaction(
			() => [this.isMembersOpen, this.defaultHiddenChannelMembersOpenByChannelId],
			() => callback(),
			{fireImmediately: true},
		);
	}
}

export default new MemberList();
