// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ExpressionMetadataCommands from '@app/features/expressions/commands/ExpressionMetadataCommands';
import {action, makeAutoObservable, runInAction} from 'mobx';

export interface ExpressionMetadata {
	id: string;
	guildId: string;
	name: string;
	animated: boolean;
	allowCloning: boolean;
}

export interface ExpressionMetadataRequestState {
	loading: boolean;
	error: Error | null;
	data: ExpressionMetadata | null;
}

const IDLE_STATE: ExpressionMetadataRequestState = {loading: false, error: null, data: null};

class ExpressionMetadataState {
	emojiMetadata: Map<string, ExpressionMetadataRequestState> = new Map();
	stickerMetadata: Map<string, ExpressionMetadataRequestState> = new Map();
	private pendingEmojiRequests: Map<string, Promise<ExpressionMetadata>> = new Map();
	private pendingStickerRequests: Map<string, Promise<ExpressionMetadata>> = new Map();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	getEmojiMetadata(emojiId: string): ExpressionMetadataRequestState {
		return this.emojiMetadata.get(emojiId) ?? IDLE_STATE;
	}

	getStickerMetadata(stickerId: string): ExpressionMetadataRequestState {
		return this.stickerMetadata.get(stickerId) ?? IDLE_STATE;
	}

	fetchEmojiMetadata = action(async (emojiId: string): Promise<ExpressionMetadata> => {
		const existing = this.pendingEmojiRequests.get(emojiId);
		if (existing) return existing;
		const cached = this.emojiMetadata.get(emojiId);
		if (cached?.data) return cached.data;
		runInAction(() => {
			this.emojiMetadata = new Map(this.emojiMetadata).set(emojiId, {loading: true, error: null, data: null});
		});
		const promise = ExpressionMetadataCommands.fetchEmojiMetadata(emojiId);
		runInAction(() => {
			this.pendingEmojiRequests = new Map(this.pendingEmojiRequests).set(emojiId, promise);
		});
		try {
			const metadata = await promise;
			runInAction(() => {
				const next = new Map(this.pendingEmojiRequests);
				next.delete(emojiId);
				this.pendingEmojiRequests = next;
				this.emojiMetadata = new Map(this.emojiMetadata).set(emojiId, {
					loading: false,
					error: null,
					data: metadata,
				});
			});
			return metadata;
		} catch (error) {
			runInAction(() => {
				const next = new Map(this.pendingEmojiRequests);
				next.delete(emojiId);
				this.pendingEmojiRequests = next;
				this.emojiMetadata = new Map(this.emojiMetadata).set(emojiId, {
					loading: false,
					error: error as Error,
					data: null,
				});
			});
			throw error;
		}
	});
	fetchStickerMetadata = action(async (stickerId: string): Promise<ExpressionMetadata> => {
		const existing = this.pendingStickerRequests.get(stickerId);
		if (existing) return existing;
		const cached = this.stickerMetadata.get(stickerId);
		if (cached?.data) return cached.data;
		runInAction(() => {
			this.stickerMetadata = new Map(this.stickerMetadata).set(stickerId, {loading: true, error: null, data: null});
		});
		const promise = ExpressionMetadataCommands.fetchStickerMetadata(stickerId);
		runInAction(() => {
			this.pendingStickerRequests = new Map(this.pendingStickerRequests).set(stickerId, promise);
		});
		try {
			const metadata = await promise;
			runInAction(() => {
				const next = new Map(this.pendingStickerRequests);
				next.delete(stickerId);
				this.pendingStickerRequests = next;
				this.stickerMetadata = new Map(this.stickerMetadata).set(stickerId, {
					loading: false,
					error: null,
					data: metadata,
				});
			});
			return metadata;
		} catch (error) {
			runInAction(() => {
				const next = new Map(this.pendingStickerRequests);
				next.delete(stickerId);
				this.pendingStickerRequests = next;
				this.stickerMetadata = new Map(this.stickerMetadata).set(stickerId, {
					loading: false,
					error: error as Error,
					data: null,
				});
			});
			throw error;
		}
	});
}

export default new ExpressionMetadataState();
