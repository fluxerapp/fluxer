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
import type {Channel} from '~/records/ChannelRecord';
import {type Message, MessageRecord} from '~/records/MessageRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import type {ReactionEmoji} from '~/utils/ReactionUtils';

interface ChannelPinEntry {
	message: MessageRecord;
	pinnedAt: string;
}

interface ChannelPinState {
	fetched: boolean;
	hasMore: boolean;
	isLoading: boolean;
	lastPinTimestamp?: string | null;
	error?: string | null;
}

class ChannelPinsStore {
	private channelPins: Record<string, Array<ChannelPinEntry>> = {};
	private channelState: Record<string, ChannelPinState> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	isFetched(channelId: string): boolean {
		return this.channelState[channelId]?.fetched ?? false;
	}

	getPins(channelId: string): ReadonlyArray<ChannelPinEntry> {
		return this.channelPins[channelId] ?? [];
	}

	getHasMore(channelId: string): boolean {
		return this.channelState[channelId]?.hasMore ?? true;
	}

	getIsLoading(channelId: string): boolean {
		return this.channelState[channelId]?.isLoading ?? false;
	}

	getOldestPinnedAt(channelId: string): string | undefined {
		const pins = this.channelPins[channelId];
		if (!pins || pins.length === 0) {
			return undefined;
		}
		return pins[pins.length - 1]?.pinnedAt;
	}

	getLastPinnedMessageId(channelId: string): string | undefined {
		const pins = this.channelPins[channelId];
		if (!pins || pins.length === 0) {
			return undefined;
		}
		return pins[pins.length - 1]?.message.id;
	}

	handleFetchPending(channelId: string): void {
		const state = this.channelState[channelId] ?? this.createDefaultState();
		this.channelState = {
			...this.channelState,
			[channelId]: {...state, isLoading: true, error: null},
		};
	}

	handleChannelPinsFetchSuccess(
		channelId: string,
		pins: ReadonlyArray<{message: Message; pinned_at: string}>,
		hasMore: boolean,
	): void {
		const existingPins = this.channelPins[channelId] ?? [];
		const newPins = pins.map(({message, pinned_at}) => ({
			message: new MessageRecord(message),
			pinnedAt: pinned_at ?? message.timestamp ?? new Date().toISOString(),
		}));
		const isLoadMore = this.channelState[channelId]?.fetched && this.channelState[channelId]?.isLoading;

		this.channelPins = {
			...this.channelPins,
			[channelId]: isLoadMore ? [...existingPins, ...newPins] : newPins,
		};

		this.channelState = {
			...this.channelState,
			[channelId]: {
				fetched: true,
				hasMore,
				isLoading: false,
				lastPinTimestamp: pins.at(0)?.pinned_at ?? this.channelState[channelId]?.lastPinTimestamp ?? null,
				error: null,
			},
		};
	}

	handleChannelPinsFetchError(channelId: string): void {
		const state = this.channelState[channelId] ?? this.createDefaultState();
		this.channelState = {
			...this.channelState,
			[channelId]: {...state, isLoading: false, hasMore: false, fetched: true, error: 'fetch_error'},
		};
	}

	handleChannelDelete(channel: Channel): void {
		const {[channel.id]: _, ...remainingChannels} = this.channelPins;
		const {[channel.id]: __, ...remainingState} = this.channelState;
		this.channelPins = remainingChannels;
		this.channelState = remainingState;
	}

	handleChannelPinsUpdate(channelId: string, lastPinTimestamp: string | null): void {
		this.channelState = {
			...this.channelState,
			[channelId]: {
				fetched: false,
				hasMore: true,
				isLoading: false,
				lastPinTimestamp,
			},
		};
		this.channelPins = {
			...this.channelPins,
			[channelId]: [],
		};
	}

	handleMessageUpdate(message: Message): void {
		const channelId = message.channel_id;
		const existingPins = this.channelPins[channelId] ?? [];
		const existingIndex = existingPins.findIndex((pin) => pin.message.id === message.id);

		if (existingIndex === -1 && !('flags' in message && message.pinned)) {
			return;
		}

		let updatedPins: Array<ChannelPinEntry>;
		if (existingIndex !== -1) {
			if ('flags' in message && !message.pinned) {
				updatedPins = [...existingPins.slice(0, existingIndex), ...existingPins.slice(existingIndex + 1)];
			} else {
				updatedPins = [
					...existingPins.slice(0, existingIndex),
					{
						...existingPins[existingIndex],
						message: existingPins[existingIndex].message.withUpdates(message),
					},
					...existingPins.slice(existingIndex + 1),
				];
			}
		} else {
			updatedPins = [
				{
					message: new MessageRecord(message as Message),
					pinnedAt: this.channelState[channelId]?.lastPinTimestamp ?? new Date().toISOString(),
				},
				...existingPins,
			];
		}

		this.channelPins = {
			...this.channelPins,
			[channelId]: updatedPins,
		};
	}

	handleMessageDelete(channelId: string, messageId: string): void {
		const existingPins = this.channelPins[channelId] ?? [];
		const existingIndex = existingPins.findIndex((pin) => pin.message.id === messageId);

		if (existingIndex === -1) {
			return;
		}

		this.channelPins = {
			...this.channelPins,
			[channelId]: [...existingPins.slice(0, existingIndex), ...existingPins.slice(existingIndex + 1)],
		};
	}

	private updateMessageInChannel(
		channelId: string,
		messageId: string,
		updater: (message: MessageRecord) => MessageRecord,
	): void {
		const existingPins = this.channelPins[channelId] ?? [];
		const existingIndex = existingPins.findIndex((pin) => pin.message.id === messageId);

		if (existingIndex === -1) {
			return;
		}

		this.channelPins = {
			...this.channelPins,
			[channelId]: [
				...existingPins.slice(0, existingIndex),
				{...existingPins[existingIndex], message: updater(existingPins[existingIndex].message)},
				...existingPins.slice(existingIndex + 1),
			],
		};
	}

	handleMessageReactionAdd(channelId: string, messageId: string, userId: string, emoji: ReactionEmoji): void {
		this.updateMessageInChannel(channelId, messageId, (message) =>
			message.withReaction(emoji, true, userId === AuthenticationStore.currentUserId),
		);
	}

	handleMessageReactionRemove(channelId: string, messageId: string, userId: string, emoji: ReactionEmoji): void {
		this.updateMessageInChannel(channelId, messageId, (message) =>
			message.withReaction(emoji, false, userId === AuthenticationStore.currentUserId),
		);
	}

	handleMessageReactionRemoveAll(channelId: string, messageId: string): void {
		this.updateMessageInChannel(channelId, messageId, (message) => message.withUpdates({reactions: []}));
	}

	handleMessageReactionRemoveEmoji(channelId: string, messageId: string, emoji: ReactionEmoji): void {
		this.updateMessageInChannel(channelId, messageId, (message) => message.withoutReactionEmoji(emoji));
	}

	private createDefaultState(): ChannelPinState {
		return {
			fetched: false,
			hasMore: true,
			isLoading: false,
			lastPinTimestamp: undefined,
			error: null,
		};
	}
}

export default new ChannelPinsStore();
