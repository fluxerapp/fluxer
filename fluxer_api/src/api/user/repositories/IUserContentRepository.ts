// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID, UserID} from '../../BrandedTypes';
import type {ExactRow} from '../../database/types/DatabaseRowTypes';
import type {GiftCodeRow, PaymentBySubscriptionRow, PaymentRow} from '../../database/types/PaymentTypes';
import type {PushSubscriptionRow, RecentMentionRow} from '../../database/types/UserTypes';
import type {GiftCode} from '../../models/GiftCode';
import type {Payment} from '../../models/Payment';
import type {PushSubscription} from '../../models/PushSubscription';
import type {RecentMention} from '../../models/RecentMention';
import type {SavedMessage} from '../../models/SavedMessage';
import type {VisionarySlot} from '../../models/VisionarySlot';

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
	createRecentMention(mention: ExactRow<RecentMentionRow>): Promise<RecentMention>;
	createRecentMentions(mentions: Array<ExactRow<RecentMentionRow>>): Promise<void>;
	deleteRecentMention(mention: RecentMention): Promise<void>;
	deleteRecentMentions(mentions: Array<RecentMention>): Promise<void>;
	deleteAllRecentMentions(userId: UserID): Promise<void>;
	listSavedMessages(userId: UserID, limit?: number, before?: MessageID): Promise<Array<SavedMessage>>;
	countSavedMessages(userId: UserID): Promise<number>;
	createSavedMessage(userId: UserID, channelId: ChannelID, messageId: MessageID): Promise<SavedMessage>;
	deleteSavedMessage(userId: UserID, messageId: MessageID): Promise<void>;
	deleteAllSavedMessages(userId: UserID): Promise<void>;
	createGiftCode(data: ExactRow<GiftCodeRow>): Promise<void>;
	findGiftCode(code: string): Promise<GiftCode | null>;
	findGiftCodeByPaymentIntent(paymentIntentId: string): Promise<GiftCode | null>;
	findGiftCodesByCreator(userId: UserID): Promise<Array<GiftCode>>;
	findGiftCodesByRedeemer(userId: UserID): Promise<Array<GiftCode>>;
	redeemGiftCode(code: string, userId: UserID): Promise<void>;
	unredeemGiftCode(code: string, userId: UserID): Promise<void>;
	revokeGiftCode(code: string): Promise<void>;
	updateGiftCode(code: string, data: Partial<GiftCodeRow>): Promise<void>;
	linkGiftCodeToCheckoutSession(code: string, checkoutSessionId: string): Promise<void>;
	listPushSubscriptions(userId: UserID): Promise<Array<PushSubscription>>;
	createPushSubscription(data: ExactRow<PushSubscriptionRow>): Promise<PushSubscription>;
	deletePushSubscription(userId: UserID, subscriptionId: string): Promise<void>;
	deletePushSubscriptionsForAuthSessions(
		userId: UserID,
		authSessionIdHashes: Array<string>,
		options: {deleteUnboundSubscriptions: boolean},
	): Promise<void>;
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
		purchase_geoip_country_code?: string | null;
		purchase_client_country_code?: string | null;
		eu_withdrawal_waiver_required?: boolean;
		eu_withdrawal_waiver_accepted?: boolean;
		eu_withdrawal_waiver_accepted_at?: Date | null;
		eu_withdrawal_waiver_text_version?: string | null;
	}): Promise<void>;
	updatePayment(
		data: Partial<PaymentRow> & {
			checkout_session_id: string;
		},
	): Promise<void>;
	getPaymentByCheckoutSession(checkoutSessionId: string): Promise<Payment | null>;
	getPaymentByPaymentIntent(paymentIntentId: string): Promise<Payment | null>;
	getSubscriptionInfo(subscriptionId: string): Promise<PaymentBySubscriptionRow | null>;
	listVisionarySlots(): Promise<Array<VisionarySlot>>;
	expandVisionarySlots(byCount: number): Promise<void>;
	shrinkVisionarySlots(toCount: number): Promise<void>;
	reserveVisionarySlot(slotIndex: number, userId: UserID): Promise<void>;
	unreserveVisionarySlot(slotIndex: number, userId: UserID): Promise<void>;
}
