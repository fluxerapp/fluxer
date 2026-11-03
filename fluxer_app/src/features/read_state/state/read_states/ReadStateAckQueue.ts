// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReadStateEntry} from '@app/features/read_state/state/read_states/ReadStateEntry';
import {
	ACK_BATCH_DELAY_MS,
	chunkEntries,
	compareMessageIds,
	type ReadStateAckRequestEntry,
} from '@app/features/read_state/state/read_states/shared';
import type {ReadStateAckResponse} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';

const ACK_BATCH_SIZE = 100;
const ACK_RETRY_BASE_DELAY_MS = 5000;
const ACK_RETRY_MAX_DELAY_MS = 60000;

interface PendingAck {
	channelId: string;
	messageId: string;
	deadline: number;
	attempt: number;
	userId: string | null;
}

interface AckQueueCallbacks {
	isPreviewChannel: (channelId: string) => boolean;
	currentUserId: () => string | null;
	getState: (channelId: string) => Pick<ReadStateEntry, 'ackMessageId' | 'inFlightAckMessageId'> | undefined;
	postAckEntries: (entries: Array<ReadStateAckRequestEntry>) => Promise<ReadStateAckResponse | null>;
	applyResponse: (response: ReadStateAckResponse | null) => void;
	reportError: (error: unknown) => void;
}

export class ReadStateAckQueue {
	private readonly pendingAcks = new Map<string, PendingAck>();
	private ackFlushTimer: ReturnType<typeof setTimeout> | null = null;
	private isFlushingAcks = false;

	constructor(private readonly callbacks: AckQueueCallbacks) {}

	clear(): void {
		this.pendingAcks.clear();
		this.clearAckFlushTimer();
	}

	capture(): Array<PendingAck> {
		return Array.from(this.pendingAcks.values(), (pending) => ({...pending}));
	}

	restore(preserved: ReadonlyArray<PendingAck>): void {
		const currentUserId = this.callbacks.currentUserId();
		for (const pending of preserved) {
			if (pending.userId !== currentUserId) {
				continue;
			}
			const state = this.callbacks.getState(pending.channelId);
			if (state == null) {
				continue;
			}
			if (compareMessageIds(pending.messageId, state.ackMessageId) <= 0) {
				continue;
			}
			this.pendingAcks.set(pending.channelId, {...pending, deadline: Date.now()});
		}
		if (this.pendingAcks.size === 0) {
			return;
		}
		this.scheduleAckFlush();
		void this.flush('all');
	}

	enqueue(
		channelId: string,
		messageId: string,
		options: {
			immediate?: boolean;
			hadMentions?: boolean;
		},
	): void {
		if (this.callbacks.isPreviewChannel(channelId)) {
			return;
		}
		const now = Date.now();
		const delay = options.immediate || options.hadMentions ? 0 : ACK_BATCH_DELAY_MS;
		const deadline = now + delay;
		const existing = this.pendingAcks.get(channelId);
		const existingIsNewer = existing != null && compareMessageIds(existing.messageId, messageId) > 0;
		const pending: PendingAck = {
			channelId,
			messageId: existingIsNewer ? existing.messageId : messageId,
			deadline: existing == null ? deadline : Math.min(existing.deadline, deadline),
			attempt: existing?.attempt ?? 0,
			userId: this.callbacks.currentUserId(),
		};
		this.pendingAcks.set(channelId, pending);
		const state = this.callbacks.getState(channelId);
		if (state != null) {
			state.inFlightAckMessageId = pending.messageId;
		}
		this.scheduleAckFlush();
	}

	private clearAckFlushTimer(): void {
		if (this.ackFlushTimer != null) {
			clearTimeout(this.ackFlushTimer);
			this.ackFlushTimer = null;
		}
	}

	private scheduleAckFlush(): void {
		this.clearAckFlushTimer();
		if (this.pendingAcks.size === 0 || this.isFlushingAcks) {
			return;
		}
		let nextDeadline = Number.POSITIVE_INFINITY;
		for (const pending of this.pendingAcks.values()) {
			nextDeadline = Math.min(nextDeadline, pending.deadline);
		}
		const delay = Math.max(0, nextDeadline - Date.now());
		this.ackFlushTimer = setTimeout(() => {
			this.ackFlushTimer = null;
			void this.flush('due');
		}, delay);
	}

	cancel(channelId: string): void {
		const deleted = this.pendingAcks.delete(channelId);
		const state = this.callbacks.getState(channelId);
		if (state != null) {
			state.inFlightAckMessageId = null;
		}
		if (deleted) {
			this.scheduleAckFlush();
		}
	}

	cancelIfCovered(channelId: string, ackedMessageId: string): void {
		const pending = this.pendingAcks.get(channelId);
		if (pending == null) {
			return;
		}
		if (compareMessageIds(ackedMessageId, pending.messageId) < 0) {
			return;
		}
		this.cancel(channelId);
	}

	async flush(mode: 'due' | 'all'): Promise<void> {
		if (this.isFlushingAcks) {
			return;
		}
		this.clearAckFlushTimer();
		const now = Date.now();
		const due = Array.from(this.pendingAcks.values()).filter((pending) => mode === 'all' || pending.deadline <= now);
		if (due.length === 0) {
			this.scheduleAckFlush();
			return;
		}
		this.isFlushingAcks = true;
		try {
			for (const chunk of chunkEntries(due, ACK_BATCH_SIZE)) {
				await this.sendAckChunk(chunk);
			}
		} finally {
			this.isFlushingAcks = false;
			this.scheduleAckFlush();
		}
	}

	private async sendAckChunk(entries: Array<PendingAck>): Promise<void> {
		try {
			const response = await this.callbacks.postAckEntries(
				entries.map((entry) => ({
					channel_id: entry.channelId,
					message_id: entry.messageId,
				})),
			);
			this.callbacks.applyResponse(response);
			for (const entry of entries) {
				const current = this.pendingAcks.get(entry.channelId);
				if (current?.messageId === entry.messageId) {
					this.pendingAcks.delete(entry.channelId);
					const state = this.callbacks.getState(entry.channelId);
					if (state?.inFlightAckMessageId === entry.messageId) {
						state.inFlightAckMessageId = null;
					}
				}
			}
		} catch (error) {
			this.callbacks.reportError(error);
			const now = Date.now();
			for (const entry of entries) {
				const current = this.pendingAcks.get(entry.channelId);
				if (current == null || current.messageId !== entry.messageId) {
					continue;
				}
				const attempt = current.attempt + 1;
				const retryDelay = Math.min(ACK_RETRY_MAX_DELAY_MS, ACK_RETRY_BASE_DELAY_MS * 2 ** (attempt - 1));
				current.attempt = attempt;
				current.deadline = now + retryDelay;
			}
		}
	}
}
