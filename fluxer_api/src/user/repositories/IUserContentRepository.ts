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

import type {ChannelID, MessageID, UserID} from '~/BrandedTypes';
import type {
	BetaCodeRow,
	GiftCodeRow,
	PaymentBySubscriptionRow,
	PaymentRow,
	PushSubscriptionRow,
	RecentMentionRow,
} from '~/database/CassandraTypes';
import type {BetaCode, GiftCode, Payment, PushSubscription, RecentMention, SavedMessage, VisionarySlot} from '~/Models';

export interface IUserContentRepository {
	getRecentMention(userId: UserID, messageId: MessageID): Promise<RecentMention | null>;
	listRecentMentions(
		userId: UserID,
		includeEveryone: boolean,
		includeRole: boolean,
		includeGuilds: boolean,
		limit: number,
		before?: MessageID,
	): Promise<Array<RecentMention>>;
	createRecentMention(mention: RecentMentionRow): Promise<RecentMention>;
	createRecentMentions(mentions: Array<RecentMentionRow>): Promise<void>;
	deleteRecentMention(mention: RecentMention): Promise<void>;
	deleteAllRecentMentions(userId: UserID): Promise<void>;

	listSavedMessages(userId: UserID, limit?: number, before?: MessageID): Promise<Array<SavedMessage>>;
	createSavedMessage(userId: UserID, channelId: ChannelID, messageId: MessageID): Promise<SavedMessage>;
	deleteSavedMessage(userId: UserID, messageId: MessageID): Promise<void>;
	deleteAllSavedMessages(userId: UserID): Promise<void>;

	listBetaCodes(creatorId: UserID): Promise<Array<BetaCode>>;
	getBetaCode(code: string): Promise<BetaCode | null>;
	upsertBetaCode(betaCode: BetaCodeRow): Promise<BetaCode>;
	updateBetaCodeRedeemed(code: string, redeemerId: UserID, redeemedAt: Date): Promise<void>;
	deleteBetaCode(code: string, creatorId: UserID): Promise<void>;
	deleteAllBetaCodes(userId: UserID): Promise<void>;

	createGiftCode(data: GiftCodeRow): Promise<void>;
	findGiftCode(code: string): Promise<GiftCode | null>;
	findGiftCodeByPaymentIntent(paymentIntentId: string): Promise<GiftCode | null>;
	findGiftCodesByCreator(userId: UserID): Promise<Array<GiftCode>>;
	redeemGiftCode(code: string, userId: UserID): Promise<{applied: boolean}>;
	updateGiftCode(code: string, data: Partial<GiftCodeRow>): Promise<void>;
	linkGiftCodeToCheckoutSession(code: string, checkoutSessionId: string): Promise<void>;

	listPushSubscriptions(userId: UserID): Promise<Array<PushSubscription>>;
	createPushSubscription(data: PushSubscriptionRow): Promise<PushSubscription>;
	deletePushSubscription(userId: UserID, subscriptionId: string): Promise<void>;
	getBulkPushSubscriptions(userIds: Array<UserID>): Promise<Map<UserID, Array<PushSubscription>>>;
	deleteAllPushSubscriptions(userId: UserID): Promise<void>;

	createPayment(data: {
		checkout_session_id: string;
		user_id: UserID;
		price_id: string;
		product_type: string;
		status: string;
		is_gift: boolean;
		created_at: Date;
	}): Promise<void>;
	updatePayment(data: Partial<PaymentRow> & {checkout_session_id: string}): Promise<{applied: boolean}>;
	getPaymentByCheckoutSession(checkoutSessionId: string): Promise<Payment | null>;
	getPaymentByPaymentIntent(paymentIntentId: string): Promise<Payment | null>;
	getSubscriptionInfo(subscriptionId: string): Promise<PaymentBySubscriptionRow | null>;

	listVisionarySlots(): Promise<Array<VisionarySlot>>;
	expandVisionarySlots(byCount: number): Promise<void>;
	shrinkVisionarySlots(toCount: number): Promise<void>;
	reserveVisionarySlot(slotIndex: number, userId: UserID): Promise<void>;
	unreserveVisionarySlot(slotIndex: number, userId: UserID): Promise<void>;
}
