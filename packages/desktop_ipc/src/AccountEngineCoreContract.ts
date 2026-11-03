// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	AccountEngineAccountActivity,
	AccountEngineMention,
	AccountEngineNotification,
	AccountEngineSnapshotCapture,
	AccountEngineSnapshotCursor,
	AccountEngineUser,
} from './AccountEngineContract';

export type AccountEngineRowOp =
	| {readonly kind: 'upsert'; readonly entity: string; readonly key: string; readonly value: unknown}
	| {readonly kind: 'delete'; readonly entity: string; readonly key: string}
	| {readonly kind: 'deleteByPrefix'; readonly entity: string; readonly keyPrefix: string}
	| {
			readonly kind: 'replaceEntity';
			readonly entity: string;
			readonly entries: ReadonlyArray<{readonly key: string; readonly value: unknown}>;
	  };

export interface AccountEngineRowStore {
	has(storageKey: string): boolean;
	get(storageKey: string, entity: string, key: string): string | null;
	entriesByPrefix(
		storageKey: string,
		entity: string,
		prefix: string,
	): Iterable<{readonly key: string; readonly value: string}>;
	apply(storageKey: string, op: AccountEngineRowOp): void;
	writeCursor(storageKey: string, cursor: AccountEngineSnapshotCursor): void;
	readCursor(storageKey: string): AccountEngineSnapshotCursor | null;
	capture(storageKey: string): AccountEngineSnapshotCapture | null;
	replaceAccount(storageKey: string, capture: AccountEngineSnapshotCapture): void;
	evict(storageKey: string): void;
}

export interface AccountEngineCoreCounts {
	readonly mentionCount: number;
	readonly unreadChannelCount: number;
	readonly lastMessageAt: number | null;
	readonly degraded: boolean;
}

export interface AccountEngineCoreOptions {
	readonly accountKey: string;
	readonly userId: string;
	readonly store: AccountEngineRowStore;
	readonly onCountsChanged: (counts: AccountEngineCoreCounts) => void;
	readonly onNotification: (notification: Omit<AccountEngineNotification, 'accountKey'>) => void;
	readonly onMention: (mention: AccountEngineMention) => void;
	readonly onChannelRead: (channelId: string) => void;
	readonly onSelfUser: (user: AccountEngineUser) => void;
	readonly onTokenRotated: (token: string) => void;
	readonly onInvalid: (error: Error) => void;
}

export interface AccountEngineCore {
	seedFromStore(mentionCounts: Readonly<Record<string, number>> | null): boolean;
	applyReady(data: unknown): {readonly sessionId: string | null};
	applyDispatch(type: string, data: unknown): void;
	replaceFromCapture(
		capture: AccountEngineSnapshotCapture,
		mentionCounts: Readonly<Record<string, number>> | null,
	): void;
	mentionCounts(): Record<string, number>;
	refreshUnread(): void;
	setUnreadBadgeCustomization(enabled: boolean): void;
	activity(channelLimit: number): Omit<AccountEngineAccountActivity, 'mentions' | 'accountKey'>;
	dispose(): void;
}

export type CreateAccountEngineCore = (options: AccountEngineCoreOptions) => AccountEngineCore;

export const ACCOUNT_ENGINE_CORE_SCHEMA_EPOCH_OFFSET = 1000;
