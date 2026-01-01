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

import type {ChannelID, GuildID, MessageID, PhoneVerificationToken, UserID} from '~/BrandedTypes';
import type {
	AuthSessionRow,
	BetaCodeRow,
	EmailRevertTokenRow,
	EmailVerificationTokenRow,
	GiftCodeRow,
	PasswordResetTokenRow,
	PaymentBySubscriptionRow,
	PaymentRow,
	PhoneTokenRow,
	PushSubscriptionRow,
	RecentMentionRow,
	RelationshipRow,
	UserGuildSettingsRow,
	UserRow,
	UserSettingsRow,
} from '~/database/CassandraTypes';
import type {
	AuthSession,
	BetaCode,
	Channel,
	EmailRevertToken,
	EmailVerificationToken,
	GiftCode,
	MfaBackupCode,
	PasswordResetToken,
	Payment,
	PushSubscription,
	ReadState,
	RecentMention,
	Relationship,
	SavedMessage,
	User,
	UserGuildSettings,
	UserNote,
	UserSettings,
	VisionarySlot,
	WebAuthnCredential,
} from '~/Models';
import {ReadStateRepository} from '~/read_state/ReadStateRepository';
import type {PrivateChannelSummary} from './IUserChannelRepository';
import type {IUserRepositoryAggregate} from './IUserRepositoryAggregate';
import {UserAccountRepository} from './UserAccountRepository';
import {UserAuthRepository} from './UserAuthRepository';
import {UserChannelRepository} from './UserChannelRepository';
import {UserContentRepository} from './UserContentRepository';
import {UserRelationshipRepository} from './UserRelationshipRepository';
import {UserSettingsRepository} from './UserSettingsRepository';

export class UserRepository implements IUserRepositoryAggregate {
	private accountRepo: UserAccountRepository;
	private settingsRepo: UserSettingsRepository;
	private authRepo: UserAuthRepository;
	private relationshipRepo: UserRelationshipRepository;
	private channelRepo: UserChannelRepository;
	private contentRepo: UserContentRepository;
	private readStateRepo: ReadStateRepository;

	constructor() {
		this.accountRepo = new UserAccountRepository();
		this.settingsRepo = new UserSettingsRepository();
		this.authRepo = new UserAuthRepository(this.accountRepo);
		this.relationshipRepo = new UserRelationshipRepository();
		this.channelRepo = new UserChannelRepository();
		this.contentRepo = new UserContentRepository();
		this.readStateRepo = new ReadStateRepository();
	}

	async create(data: UserRow): Promise<User> {
		return this.accountRepo.create(data);
	}

	async upsert(data: UserRow, oldData?: UserRow | null): Promise<User> {
		return this.accountRepo.upsert(data, oldData);
	}

	async patchUpsert(userId: UserID, patchData: Partial<UserRow>, oldData?: UserRow | null): Promise<User | null> {
		return this.accountRepo.patchUpsert(userId, patchData, oldData);
	}

	async findUnique(userId: UserID): Promise<User | null> {
		return this.accountRepo.findUnique(userId);
	}

	async findUniqueAssert(userId: UserID): Promise<User> {
		return this.accountRepo.findUniqueAssert(userId);
	}

	async findByUsernameDiscriminator(username: string, discriminator: number): Promise<User | null> {
		return this.accountRepo.findByUsernameDiscriminator(username, discriminator);
	}

	async findDiscriminatorsByUsername(username: string): Promise<Set<number>> {
		return this.accountRepo.findDiscriminatorsByUsername(username);
	}

	async findByEmail(email: string): Promise<User | null> {
		return this.accountRepo.findByEmail(email);
	}

	async findByPhone(phone: string): Promise<User | null> {
		return this.accountRepo.findByPhone(phone);
	}

	async findByStripeSubscriptionId(stripeSubscriptionId: string): Promise<User | null> {
		return this.accountRepo.findByStripeSubscriptionId(stripeSubscriptionId);
	}

	async findByStripeCustomerId(stripeCustomerId: string): Promise<User | null> {
		return this.accountRepo.findByStripeCustomerId(stripeCustomerId);
	}

	async listUsers(userIds: Array<UserID>): Promise<Array<User>> {
		return this.accountRepo.listUsers(userIds);
	}

	async listAllUsersPaginated(limit: number, lastUserId?: UserID): Promise<Array<User>> {
		return this.accountRepo.listAllUsersPaginated(limit, lastUserId);
	}

	async getUserGuildIds(userId: UserID): Promise<Array<GuildID>> {
		return this.accountRepo.getUserGuildIds(userId);
	}

	async getActivityTracking(userId: UserID): Promise<{last_active_at: Date | null; last_active_ip: string | null}> {
		return this.accountRepo.getActivityTracking(userId);
	}

	async addPendingDeletion(userId: UserID, pendingDeletionAt: Date, deletionReasonCode: number): Promise<void> {
		return this.accountRepo.addPendingDeletion(userId, pendingDeletionAt, deletionReasonCode);
	}

	async removePendingDeletion(userId: UserID, pendingDeletionAt: Date): Promise<void> {
		return this.accountRepo.removePendingDeletion(userId, pendingDeletionAt);
	}

	async findUsersPendingDeletion(now: Date): Promise<Array<User>> {
		return this.accountRepo.findUsersPendingDeletion(now);
	}

	async findUsersPendingDeletionByDate(
		deletionDate: string,
	): Promise<Array<{user_id: bigint; deletion_reason_code: number}>> {
		return this.accountRepo.findUsersPendingDeletionByDate(deletionDate);
	}

	async isUserPendingDeletion(userId: UserID, deletionDate: string): Promise<boolean> {
		return this.accountRepo.isUserPendingDeletion(userId, deletionDate);
	}

	async scheduleDeletion(userId: UserID, pendingDeletionAt: Date, deletionReasonCode: number): Promise<void> {
		return this.accountRepo.scheduleDeletion(userId, pendingDeletionAt, deletionReasonCode);
	}

	async deleteUserSecondaryIndices(userId: UserID): Promise<void> {
		return this.accountRepo.deleteUserSecondaryIndices(userId);
	}

	async removeFromAllGuilds(userId: UserID): Promise<void> {
		return this.accountRepo.removeFromAllGuilds(userId);
	}

	async updateLastActiveAt(params: {userId: UserID; lastActiveAt: Date; lastActiveIp?: string}): Promise<void> {
		return this.accountRepo.updateLastActiveAt(params);
	}

	async findSettings(userId: UserID): Promise<UserSettings | null> {
		return this.settingsRepo.findSettings(userId);
	}

	async upsertSettings(settings: UserSettingsRow): Promise<UserSettings> {
		return this.settingsRepo.upsertSettings(settings);
	}

	async deleteUserSettings(userId: UserID): Promise<void> {
		return this.settingsRepo.deleteUserSettings(userId);
	}

	async findGuildSettings(userId: UserID, guildId: GuildID | null): Promise<UserGuildSettings | null> {
		return this.settingsRepo.findGuildSettings(userId, guildId);
	}

	async findAllGuildSettings(userId: UserID): Promise<Array<UserGuildSettings>> {
		return this.settingsRepo.findAllGuildSettings(userId);
	}

	async upsertGuildSettings(settings: UserGuildSettingsRow): Promise<UserGuildSettings> {
		return this.settingsRepo.upsertGuildSettings(settings);
	}

	async deleteGuildSettings(userId: UserID, guildId: GuildID): Promise<void> {
		return this.settingsRepo.deleteGuildSettings(userId, guildId);
	}

	async deleteAllUserGuildSettings(userId: UserID): Promise<void> {
		return this.settingsRepo.deleteAllUserGuildSettings(userId);
	}

	async listAuthSessions(userId: UserID): Promise<Array<AuthSession>> {
		return this.authRepo.listAuthSessions(userId);
	}

	async getAuthSessionByToken(sessionIdHash: Buffer): Promise<AuthSession | null> {
		return this.authRepo.getAuthSessionByToken(sessionIdHash);
	}

	async createAuthSession(sessionData: AuthSessionRow): Promise<AuthSession> {
		return this.authRepo.createAuthSession(sessionData);
	}

	async updateAuthSessionLastUsed(sessionIdHash: Buffer): Promise<void> {
		return this.authRepo.updateAuthSessionLastUsed(sessionIdHash);
	}

	async deleteAuthSessions(userId: UserID, sessionIdHashes: Array<Buffer>): Promise<void> {
		return this.authRepo.deleteAuthSessions(userId, sessionIdHashes);
	}

	async revokeAuthSession(sessionIdHash: Buffer): Promise<void> {
		return this.authRepo.revokeAuthSession(sessionIdHash);
	}

	async deleteAllAuthSessions(userId: UserID): Promise<void> {
		return this.authRepo.deleteAllAuthSessions(userId);
	}

	async listMfaBackupCodes(userId: UserID): Promise<Array<MfaBackupCode>> {
		return this.authRepo.listMfaBackupCodes(userId);
	}

	async createMfaBackupCodes(userId: UserID, codes: Array<string>): Promise<Array<MfaBackupCode>> {
		return this.authRepo.createMfaBackupCodes(userId, codes);
	}

	async clearMfaBackupCodes(userId: UserID): Promise<void> {
		return this.authRepo.clearMfaBackupCodes(userId);
	}

	async consumeMfaBackupCode(userId: UserID, code: string): Promise<void> {
		return this.authRepo.consumeMfaBackupCode(userId, code);
	}

	async deleteAllMfaBackupCodes(userId: UserID): Promise<void> {
		return this.authRepo.deleteAllMfaBackupCodes(userId);
	}

	async getEmailVerificationToken(token: string): Promise<EmailVerificationToken | null> {
		return this.authRepo.getEmailVerificationToken(token);
	}

	async createEmailVerificationToken(tokenData: EmailVerificationTokenRow): Promise<EmailVerificationToken> {
		return this.authRepo.createEmailVerificationToken(tokenData);
	}

	async deleteEmailVerificationToken(token: string): Promise<void> {
		return this.authRepo.deleteEmailVerificationToken(token);
	}

	async getPasswordResetToken(token: string): Promise<PasswordResetToken | null> {
		return this.authRepo.getPasswordResetToken(token);
	}

	async createPasswordResetToken(tokenData: PasswordResetTokenRow): Promise<PasswordResetToken> {
		return this.authRepo.createPasswordResetToken(tokenData);
	}

	async deletePasswordResetToken(token: string): Promise<void> {
		return this.authRepo.deletePasswordResetToken(token);
	}

	async getEmailRevertToken(token: string): Promise<EmailRevertToken | null> {
		return this.authRepo.getEmailRevertToken(token);
	}

	async createEmailRevertToken(tokenData: EmailRevertTokenRow): Promise<EmailRevertToken> {
		return this.authRepo.createEmailRevertToken(tokenData);
	}

	async deleteEmailRevertToken(token: string): Promise<void> {
		return this.authRepo.deleteEmailRevertToken(token);
	}

	async createPhoneToken(token: PhoneVerificationToken, phone: string, userId: UserID | null): Promise<void> {
		return this.authRepo.createPhoneToken(token, phone, userId);
	}

	async getPhoneToken(token: PhoneVerificationToken): Promise<PhoneTokenRow | null> {
		return this.authRepo.getPhoneToken(token);
	}

	async deletePhoneToken(token: PhoneVerificationToken): Promise<void> {
		return this.authRepo.deletePhoneToken(token);
	}

	async updateUserActivity(userId: UserID, clientIp: string): Promise<void> {
		return this.authRepo.updateUserActivity(userId, clientIp);
	}

	async checkIpAuthorized(userId: UserID, ip: string): Promise<boolean> {
		return this.authRepo.checkIpAuthorized(userId, ip);
	}

	async createAuthorizedIp(userId: UserID, ip: string): Promise<void> {
		return this.authRepo.createAuthorizedIp(userId, ip);
	}

	async createIpAuthorizationToken(userId: UserID, token: string): Promise<void> {
		return this.authRepo.createIpAuthorizationToken(userId, token);
	}

	async authorizeIpByToken(token: string): Promise<{userId: UserID; email: string} | null> {
		return this.authRepo.authorizeIpByToken(token);
	}

	async getAuthorizedIps(userId: UserID): Promise<Array<{ip: string}>> {
		return this.authRepo.getAuthorizedIps(userId);
	}

	async deleteAllAuthorizedIps(userId: UserID): Promise<void> {
		return this.authRepo.deleteAllAuthorizedIps(userId);
	}

	async listWebAuthnCredentials(userId: UserID): Promise<Array<WebAuthnCredential>> {
		return this.authRepo.listWebAuthnCredentials(userId);
	}

	async getWebAuthnCredential(userId: UserID, credentialId: string): Promise<WebAuthnCredential | null> {
		return this.authRepo.getWebAuthnCredential(userId, credentialId);
	}

	async createWebAuthnCredential(
		userId: UserID,
		credentialId: string,
		publicKey: Buffer,
		counter: bigint,
		transports: Set<string> | null,
		name: string,
	): Promise<void> {
		return this.authRepo.createWebAuthnCredential(userId, credentialId, publicKey, counter, transports, name);
	}

	async updateWebAuthnCredentialCounter(userId: UserID, credentialId: string, counter: bigint): Promise<void> {
		return this.authRepo.updateWebAuthnCredentialCounter(userId, credentialId, counter);
	}

	async updateWebAuthnCredentialLastUsed(userId: UserID, credentialId: string): Promise<void> {
		return this.authRepo.updateWebAuthnCredentialLastUsed(userId, credentialId);
	}

	async updateWebAuthnCredentialName(userId: UserID, credentialId: string, name: string): Promise<void> {
		return this.authRepo.updateWebAuthnCredentialName(userId, credentialId, name);
	}

	async deleteWebAuthnCredential(userId: UserID, credentialId: string): Promise<void> {
		return this.authRepo.deleteWebAuthnCredential(userId, credentialId);
	}

	async getUserIdByCredentialId(credentialId: string): Promise<UserID | null> {
		return this.authRepo.getUserIdByCredentialId(credentialId);
	}

	async deleteAllWebAuthnCredentials(userId: UserID): Promise<void> {
		return this.authRepo.deleteAllWebAuthnCredentials(userId);
	}

	async createPendingVerification(userId: UserID, createdAt: Date, metadata: Map<string, string>): Promise<void> {
		return this.authRepo.createPendingVerification(userId, createdAt, metadata);
	}

	async deletePendingVerification(userId: UserID): Promise<void> {
		return this.authRepo.deletePendingVerification(userId);
	}

	async listRelationships(sourceUserId: UserID): Promise<Array<Relationship>> {
		return this.relationshipRepo.listRelationships(sourceUserId);
	}

	async getRelationship(sourceUserId: UserID, targetUserId: UserID, type: number): Promise<Relationship | null> {
		return this.relationshipRepo.getRelationship(sourceUserId, targetUserId, type);
	}

	async upsertRelationship(relationship: RelationshipRow): Promise<Relationship> {
		return this.relationshipRepo.upsertRelationship(relationship);
	}

	async deleteRelationship(sourceUserId: UserID, targetUserId: UserID, type: number): Promise<void> {
		return this.relationshipRepo.deleteRelationship(sourceUserId, targetUserId, type);
	}

	async deleteAllRelationships(userId: UserID): Promise<void> {
		return this.relationshipRepo.deleteAllRelationships(userId);
	}

	async getUserNote(sourceUserId: UserID, targetUserId: UserID): Promise<UserNote | null> {
		return this.relationshipRepo.getUserNote(sourceUserId, targetUserId);
	}

	async getUserNotes(sourceUserId: UserID): Promise<Map<UserID, string>> {
		return this.relationshipRepo.getUserNotes(sourceUserId);
	}

	async upsertUserNote(sourceUserId: UserID, targetUserId: UserID, note: string): Promise<UserNote> {
		return this.relationshipRepo.upsertUserNote(sourceUserId, targetUserId, note);
	}

	async clearUserNote(sourceUserId: UserID, targetUserId: UserID): Promise<void> {
		return this.relationshipRepo.clearUserNote(sourceUserId, targetUserId);
	}

	async deleteAllNotes(userId: UserID): Promise<void> {
		return this.relationshipRepo.deleteAllNotes(userId);
	}

	async listPrivateChannels(userId: UserID): Promise<Array<Channel>> {
		return this.channelRepo.listPrivateChannels(userId);
	}

	async listPrivateChannelSummaries(userId: UserID): Promise<Array<PrivateChannelSummary>> {
		return this.channelRepo.listPrivateChannelSummaries(userId);
	}

	async deleteAllPrivateChannels(userId: UserID): Promise<void> {
		return this.channelRepo.deleteAllPrivateChannels(userId);
	}

	async findExistingDmState(user1Id: UserID, user2Id: UserID): Promise<Channel | null> {
		return this.channelRepo.findExistingDmState(user1Id, user2Id);
	}

	async createDmChannelAndState(user1Id: UserID, user2Id: UserID, channelId: ChannelID): Promise<Channel> {
		return this.channelRepo.createDmChannelAndState(user1Id, user2Id, channelId);
	}

	async isDmChannelOpen(userId: UserID, channelId: ChannelID): Promise<boolean> {
		return this.channelRepo.isDmChannelOpen(userId, channelId);
	}

	async openDmForUser(userId: UserID, channelId: ChannelID, isGroupDm?: boolean): Promise<void> {
		return this.channelRepo.openDmForUser(userId, channelId, isGroupDm);
	}

	async closeDmForUser(userId: UserID, channelId: ChannelID): Promise<void> {
		return this.channelRepo.closeDmForUser(userId, channelId);
	}

	async getPinnedDms(userId: UserID): Promise<Array<ChannelID>> {
		return this.channelRepo.getPinnedDms(userId);
	}

	async getPinnedDmsWithDetails(userId: UserID): Promise<Array<{channel_id: ChannelID; sort_order: number}>> {
		return this.channelRepo.getPinnedDmsWithDetails(userId);
	}

	async addPinnedDm(userId: UserID, channelId: ChannelID): Promise<Array<ChannelID>> {
		return this.channelRepo.addPinnedDm(userId, channelId);
	}

	async removePinnedDm(userId: UserID, channelId: ChannelID): Promise<Array<ChannelID>> {
		return this.channelRepo.removePinnedDm(userId, channelId);
	}

	async deletePinnedDmsByUserId(userId: UserID): Promise<void> {
		return this.channelRepo.deletePinnedDmsByUserId(userId);
	}

	async deleteAllReadStates(userId: UserID): Promise<void> {
		return this.channelRepo.deleteAllReadStates(userId);
	}

	async getReadStates(userId: UserID): Promise<Array<ReadState>> {
		return this.readStateRepo.listReadStates(userId);
	}

	async getRecentMention(userId: UserID, messageId: MessageID): Promise<RecentMention | null> {
		return this.contentRepo.getRecentMention(userId, messageId);
	}

	async listRecentMentions(
		userId: UserID,
		includeEveryone: boolean,
		includeRole: boolean,
		includeGuilds: boolean,
		limit: number,
		before?: MessageID,
	): Promise<Array<RecentMention>> {
		return this.contentRepo.listRecentMentions(userId, includeEveryone, includeRole, includeGuilds, limit, before);
	}

	async createRecentMention(mention: RecentMentionRow): Promise<RecentMention> {
		return this.contentRepo.createRecentMention(mention);
	}

	async createRecentMentions(mentions: Array<RecentMentionRow>): Promise<void> {
		return this.contentRepo.createRecentMentions(mentions);
	}

	async deleteRecentMention(mention: RecentMention): Promise<void> {
		return this.contentRepo.deleteRecentMention(mention);
	}

	async deleteAllRecentMentions(userId: UserID): Promise<void> {
		return this.contentRepo.deleteAllRecentMentions(userId);
	}

	async listSavedMessages(userId: UserID, limit?: number, before?: MessageID): Promise<Array<SavedMessage>> {
		return this.contentRepo.listSavedMessages(userId, limit, before);
	}

	async createSavedMessage(userId: UserID, channelId: ChannelID, messageId: MessageID): Promise<SavedMessage> {
		return this.contentRepo.createSavedMessage(userId, channelId, messageId);
	}

	async deleteSavedMessage(userId: UserID, messageId: MessageID): Promise<void> {
		return this.contentRepo.deleteSavedMessage(userId, messageId);
	}

	async deleteAllSavedMessages(userId: UserID): Promise<void> {
		return this.contentRepo.deleteAllSavedMessages(userId);
	}

	async listBetaCodes(creatorId: UserID): Promise<Array<BetaCode>> {
		return this.contentRepo.listBetaCodes(creatorId);
	}

	async getBetaCode(code: string): Promise<BetaCode | null> {
		return this.contentRepo.getBetaCode(code);
	}

	async upsertBetaCode(betaCode: BetaCodeRow): Promise<BetaCode> {
		return this.contentRepo.upsertBetaCode(betaCode);
	}

	async updateBetaCodeRedeemed(code: string, redeemerId: UserID, redeemedAt: Date): Promise<void> {
		return this.contentRepo.updateBetaCodeRedeemed(code, redeemerId, redeemedAt);
	}

	async deleteBetaCode(code: string, creatorId: UserID): Promise<void> {
		return this.contentRepo.deleteBetaCode(code, creatorId);
	}

	async deleteAllBetaCodes(userId: UserID): Promise<void> {
		return this.contentRepo.deleteAllBetaCodes(userId);
	}

	async createGiftCode(data: GiftCodeRow): Promise<void> {
		return this.contentRepo.createGiftCode(data);
	}

	async findGiftCode(code: string): Promise<GiftCode | null> {
		return this.contentRepo.findGiftCode(code);
	}

	async findGiftCodeByPaymentIntent(paymentIntentId: string): Promise<GiftCode | null> {
		return this.contentRepo.findGiftCodeByPaymentIntent(paymentIntentId);
	}

	async findGiftCodesByCreator(userId: UserID): Promise<Array<GiftCode>> {
		return this.contentRepo.findGiftCodesByCreator(userId);
	}

	async redeemGiftCode(code: string, userId: UserID): Promise<{applied: boolean}> {
		return this.contentRepo.redeemGiftCode(code, userId);
	}

	async updateGiftCode(code: string, data: Partial<GiftCodeRow>): Promise<void> {
		return this.contentRepo.updateGiftCode(code, data);
	}

	async linkGiftCodeToCheckoutSession(code: string, checkoutSessionId: string): Promise<void> {
		return this.contentRepo.linkGiftCodeToCheckoutSession(code, checkoutSessionId);
	}

	async listPushSubscriptions(userId: UserID): Promise<Array<PushSubscription>> {
		return this.contentRepo.listPushSubscriptions(userId);
	}

	async createPushSubscription(data: PushSubscriptionRow): Promise<PushSubscription> {
		return this.contentRepo.createPushSubscription(data);
	}

	async deletePushSubscription(userId: UserID, subscriptionId: string): Promise<void> {
		return this.contentRepo.deletePushSubscription(userId, subscriptionId);
	}

	async getBulkPushSubscriptions(userIds: Array<UserID>): Promise<Map<UserID, Array<PushSubscription>>> {
		return this.contentRepo.getBulkPushSubscriptions(userIds);
	}

	async deleteAllPushSubscriptions(userId: UserID): Promise<void> {
		return this.contentRepo.deleteAllPushSubscriptions(userId);
	}

	async createPayment(data: {
		checkout_session_id: string;
		user_id: UserID;
		price_id: string;
		product_type: string;
		status: string;
		is_gift: boolean;
		created_at: Date;
	}): Promise<void> {
		return this.contentRepo.createPayment(data);
	}

	async updatePayment(data: Partial<PaymentRow> & {checkout_session_id: string}): Promise<{applied: boolean}> {
		return this.contentRepo.updatePayment(data);
	}

	async getPaymentByCheckoutSession(checkoutSessionId: string): Promise<Payment | null> {
		return this.contentRepo.getPaymentByCheckoutSession(checkoutSessionId);
	}

	async getPaymentByPaymentIntent(paymentIntentId: string): Promise<Payment | null> {
		return this.contentRepo.getPaymentByPaymentIntent(paymentIntentId);
	}

	async getSubscriptionInfo(subscriptionId: string): Promise<PaymentBySubscriptionRow | null> {
		return this.contentRepo.getSubscriptionInfo(subscriptionId);
	}

	async listVisionarySlots(): Promise<Array<VisionarySlot>> {
		return this.contentRepo.listVisionarySlots();
	}

	async expandVisionarySlots(byCount: number): Promise<void> {
		return this.contentRepo.expandVisionarySlots(byCount);
	}

	async shrinkVisionarySlots(toCount: number): Promise<void> {
		return this.contentRepo.shrinkVisionarySlots(toCount);
	}

	async reserveVisionarySlot(slotIndex: number, userId: UserID): Promise<void> {
		return this.contentRepo.reserveVisionarySlot(slotIndex, userId);
	}

	async unreserveVisionarySlot(slotIndex: number, userId: UserID): Promise<void> {
		return this.contentRepo.unreserveVisionarySlot(slotIndex, userId);
	}
}
