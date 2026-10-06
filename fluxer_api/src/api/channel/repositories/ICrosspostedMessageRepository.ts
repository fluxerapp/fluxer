// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID, WebhookID} from '@app/api/BrandedTypes';
import type {CrosspostedMessageRow} from '@app/api/database/types/ChannelTypes';

export interface CrosspostedMessageKey {
	sourceMessageId: MessageID;
	webhookId: WebhookID;
}

export interface CrosspostSyncState {
	targetMessageId: MessageID;
	sourceFingerprint: string | null;
}

export interface CrosspostSource {
	sourceChannelId: ChannelID;
	sourceMessageId: MessageID;
}

export abstract class ICrosspostedMessageRepository {
	abstract get(sourceMessageId: MessageID, webhookId: WebhookID): Promise<CrosspostedMessageRow | null>;

	abstract insertPending(row: CrosspostedMessageRow): Promise<boolean>;

	abstract reclaimPending(
		key: CrosspostedMessageKey,
		data: {
			fromTargetMessageId: MessageID;
			toTargetMessageId: MessageID;
			reservedAt: Date;
		},
	): Promise<boolean>;

	abstract markDelivered(key: CrosspostedMessageKey, data: CrosspostSyncState): Promise<boolean>;

	abstract updateSynced(key: CrosspostedMessageKey, data: CrosspostSyncState): Promise<boolean>;

	abstract listBySourceMessage(
		sourceMessageId: MessageID,
		options: {afterWebhookId?: WebhookID; limit: number},
	): Promise<Array<CrosspostedMessageRow>>;

	abstract delete(
		key: CrosspostedMessageKey,
		expected?: Partial<Pick<CrosspostedMessageRow, 'state' | 'target_message_id'>>,
	): Promise<boolean>;

	abstract addSource(source: CrosspostSource): Promise<void>;

	abstract listSourcesByChannel(
		sourceChannelId: ChannelID,
		options: {afterMessageId?: MessageID; limit: number},
	): Promise<Array<MessageID>>;

	abstract deleteSource(source: CrosspostSource): Promise<void>;
}
