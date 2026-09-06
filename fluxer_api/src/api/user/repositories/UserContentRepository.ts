// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID, UserID} from '../../BrandedTypes';
import type {GiftCodeRow, PaymentBySubscriptionRow, PaymentRow} from '../../database/types/PaymentTypes';
import type {PushSubscriptionRow, RecentMentionRow} from '../../database/types/UserTypes';
import type {GiftCode} from '../../models/GiftCode';
import type {Payment} from '../../models/Payment';
import type {PushSubscription} from '../../models/PushSubscription';
import type {RecentMention} from '../../models/RecentMention';
import type {SavedMessage} from '../../models/SavedMessage';
import type {VisionarySlot} from '../../models/VisionarySlot';
import {GiftCodeRepository} from './GiftCodeRepository';
import type {IUserContentRepository} from './IUserContentRepository';
import {PaymentRepository} from './PaymentRepository';
import {PushSubscriptionRepository} from './PushSubscriptionRepository';
import {RecentMentionRepository} from './RecentMentionRepository';
import {SavedMessageRepository} from './SavedMessageRepository';
import {VisionarySlotRepository} from './VisionarySlotRepository';

export class UserContentRepository implements IUserContentRepository {
	private giftCodeRepository: GiftCodeRepository;
	private paymentRepository: PaymentRepository;
	private pushSubscriptionRepository: PushSubscriptionRepository;
	private recentMentionRepository: RecentMentionRepository;
	private savedMessageRepository: SavedMessageRepository;
	private visionarySlotRepository: VisionarySlotRepository;

	constructor() {
		this.giftCodeRepository = new GiftCodeRepository();
		this.paymentRepository = new PaymentRepository();
		this.pushSubscriptionRepository = new PushSubscriptionRepository();
		this.recentMentionRepository = new RecentMentionRepository();
		this.savedMessageRepository = new SavedMessageRepository();
		this.visionarySlotRepository = new VisionarySlotRepository();
	}

	async createGiftCode(data: GiftCodeRow): Promise<void> {
		return this.giftCodeRepository.createGiftCode(data);
	}

	async findGiftCode(code: string): Promise<GiftCode | null> {
		return this.giftCodeRepository.findGiftCode(code);
	}

	async findGiftCodeByPaymentIntent(paymentIntentId: string): Promise<GiftCode | null> {
		return this.giftCodeRepository.findGiftCodeByPaymentIntent(paymentIntentId);
	}

	async findGiftCodesByCreator(userId: UserID): Promise<Array<GiftCode>> {
		return this.giftCodeRepository.findGiftCodesByCreator(userId);
	}

	async findGiftCodesByRedeemer(userId: UserID): Promise<Array<GiftCode>> {
		return this.giftCodeRepository.findGiftCodesByRedeemer(userId);
	}

	async redeemGiftCode(code: string, userId: UserID): Promise<void> {
		return this.giftCodeRepository.redeemGiftCode(code, userId);
	}

	async unredeemGiftCode(code: string, userId: UserID): Promise<void> {
		return this.giftCodeRepository.unredeemGiftCode(code, userId);
	}

	async revokeGiftCode(code: string): Promise<void> {
		return this.giftCodeRepository.revokeGiftCode(code);
	}

	async updateGiftCode(code: string, data: Partial<GiftCodeRow>): Promise<void> {
		return this.giftCodeRepository.updateGiftCode(code, data);
	}

	async linkGiftCodeToCheckoutSession(code: string, checkoutSessionId: string): Promise<void> {
		return this.giftCodeRepository.linkGiftCodeToCheckoutSession(code, checkoutSessionId);
	}

	async createPayment(data: {
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
	}): Promise<void> {
		return this.paymentRepository.createPayment(data);
	}

	async updatePayment(
		data: Partial<PaymentRow> & {
			checkout_session_id: string;
		},
	): Promise<void> {
		return this.paymentRepository.updatePayment(data);
	}

	async getPaymentByCheckoutSession(checkoutSessionId: string): Promise<Payment | null> {
		return this.paymentRepository.getPaymentByCheckoutSession(checkoutSessionId);
	}

	async getPaymentByPaymentIntent(paymentIntentId: string): Promise<Payment | null> {
		return this.paymentRepository.getPaymentByPaymentIntent(paymentIntentId);
	}

	async getSubscriptionInfo(subscriptionId: string): Promise<PaymentBySubscriptionRow | null> {
		return this.paymentRepository.getSubscriptionInfo(subscriptionId);
	}

	async listPushSubscriptions(userId: UserID): Promise<Array<PushSubscription>> {
		return this.pushSubscriptionRepository.listPushSubscriptions(userId);
	}

	async createPushSubscription(data: PushSubscriptionRow): Promise<PushSubscription> {
		return this.pushSubscriptionRepository.createPushSubscription(data);
	}

	async deletePushSubscription(userId: UserID, subscriptionId: string): Promise<void> {
		return this.pushSubscriptionRepository.deletePushSubscription(userId, subscriptionId);
	}

	async deletePushSubscriptionsForAuthSessions(
		userId: UserID,
		authSessionIdHashes: Array<string>,
		options: {deleteUnboundSubscriptions: boolean},
	): Promise<void> {
		return this.pushSubscriptionRepository.deletePushSubscriptionsForAuthSessions(userId, authSessionIdHashes, options);
	}

	async getBulkPushSubscriptions(userIds: Array<UserID>): Promise<Map<UserID, Array<PushSubscription>>> {
		return this.pushSubscriptionRepository.getBulkPushSubscriptions(userIds);
	}

	async deleteAllPushSubscriptions(userId: UserID): Promise<void> {
		return this.pushSubscriptionRepository.deleteAllPushSubscriptions(userId);
	}

	async getRecentMention(userId: UserID, messageId: MessageID): Promise<RecentMention | null> {
		return this.recentMentionRepository.getRecentMention(userId, messageId);
	}

	async listRecentMentions(
		userId: UserID,
		includeEveryone: boolean = true,
		includeRole: boolean = true,
		includeGuilds: boolean = true,
		limit: number = 25,
		before?: MessageID,
	): Promise<Array<RecentMention>> {
		return this.recentMentionRepository.listRecentMentions(
			userId,
			includeEveryone,
			includeRole,
			includeGuilds,
			limit,
			before,
		);
	}

	async createRecentMention(mention: RecentMentionRow): Promise<RecentMention> {
		return this.recentMentionRepository.createRecentMention(mention);
	}

	async createRecentMentions(mentions: Array<RecentMentionRow>): Promise<void> {
		return this.recentMentionRepository.createRecentMentions(mentions);
	}

	async deleteRecentMention(mention: RecentMention): Promise<void> {
		return this.recentMentionRepository.deleteRecentMention(mention);
	}

	async deleteRecentMentions(mentions: Array<RecentMention>): Promise<void> {
		return this.recentMentionRepository.deleteRecentMentions(mentions);
	}

	async deleteAllRecentMentions(userId: UserID): Promise<void> {
		return this.recentMentionRepository.deleteAllRecentMentions(userId);
	}

	async listSavedMessages(userId: UserID, limit: number = 25, before?: MessageID): Promise<Array<SavedMessage>> {
		return this.savedMessageRepository.listSavedMessages(userId, limit, before);
	}

	async countSavedMessages(userId: UserID): Promise<number> {
		return this.savedMessageRepository.countSavedMessages(userId);
	}

	async createSavedMessage(userId: UserID, channelId: ChannelID, messageId: MessageID): Promise<SavedMessage> {
		return this.savedMessageRepository.createSavedMessage(userId, channelId, messageId);
	}

	async deleteSavedMessage(userId: UserID, messageId: MessageID): Promise<void> {
		return this.savedMessageRepository.deleteSavedMessage(userId, messageId);
	}

	async deleteAllSavedMessages(userId: UserID): Promise<void> {
		return this.savedMessageRepository.deleteAllSavedMessages(userId);
	}

	async listVisionarySlots(): Promise<Array<VisionarySlot>> {
		return this.visionarySlotRepository.listVisionarySlots();
	}

	async expandVisionarySlots(byCount: number): Promise<void> {
		return this.visionarySlotRepository.expandVisionarySlots(byCount);
	}

	async shrinkVisionarySlots(toCount: number): Promise<void> {
		return this.visionarySlotRepository.shrinkVisionarySlots(toCount);
	}

	async reserveVisionarySlot(slotIndex: number, userId: UserID): Promise<void> {
		return this.visionarySlotRepository.reserveVisionarySlot(slotIndex, userId);
	}

	async unreserveVisionarySlot(slotIndex: number, userId: UserID): Promise<void> {
		return this.visionarySlotRepository.unreserveVisionarySlot(slotIndex, userId);
	}
}
