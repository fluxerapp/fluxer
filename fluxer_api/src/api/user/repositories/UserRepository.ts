// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, MessageID, UserID} from '@app/api/BrandedTypes';
import type {
	AuthSessionRow,
	EmailRevertTokenRow,
	EmailVerificationTokenRow,
	PasswordResetTokenRow,
} from '@app/api/database/types/AuthTypes';
import type {GiftCodeRow, PaymentBySubscriptionRow, PaymentRow} from '@app/api/database/types/PaymentTypes';
import type {
	PushSubscriptionRow,
	RecentMentionRow,
	RelationshipRow,
	UserGuildSettingsRow,
	UserRow,
	UserSettingsRow,
} from '@app/api/database/types/UserTypes';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import type {AuthSession, AuthSessionTombstone} from '@app/api/models/AuthSession';
import type {Channel} from '@app/api/models/Channel';
import type {EmailRevertToken} from '@app/api/models/EmailRevertToken';
import type {EmailVerificationToken} from '@app/api/models/EmailVerificationToken';
import type {GiftCode} from '@app/api/models/GiftCode';
import type {MfaBackupCode} from '@app/api/models/MfaBackupCode';
import type {PasswordResetToken} from '@app/api/models/PasswordResetToken';
import type {Payment} from '@app/api/models/Payment';
import type {PushSubscription} from '@app/api/models/PushSubscription';
import type {RecentMention} from '@app/api/models/RecentMention';
import type {Relationship} from '@app/api/models/Relationship';
import type {SavedMessage} from '@app/api/models/SavedMessage';
import type {User} from '@app/api/models/User';
import type {UserGuildSettings} from '@app/api/models/UserGuildSettings';
import type {UserNote} from '@app/api/models/UserNote';
import type {UserSettings} from '@app/api/models/UserSettings';
import type {VisionarySlot} from '@app/api/models/VisionarySlot';
import type {WebAuthnCredential} from '@app/api/models/WebAuthnCredential';
import {UserEmailOwnershipRepository} from '@app/api/user/repositories/account/crud/UserEmailOwnershipRepository';
import {
	UserAccountRepository,
	type UserDeletionScheduleUpdate,
} from '@app/api/user/repositories/account/UserAccountRepository';
import {UserDeletionRepository} from '@app/api/user/repositories/account/UserDeletionRepository';
import {UserGuildRepository} from '@app/api/user/repositories/account/UserGuildRepository';
import {UserLookupRepository} from '@app/api/user/repositories/account/UserLookupRepository';
import {AuthSessionRepository} from '@app/api/user/repositories/auth/AuthSessionRepository';
import {IpAuthorizationRepository} from '@app/api/user/repositories/auth/IpAuthorizationRepository';
import {MfaBackupCodeRepository} from '@app/api/user/repositories/auth/MfaBackupCodeRepository';
import {TokenRepository} from '@app/api/user/repositories/auth/TokenRepository';
import {WebAuthnRepository} from '@app/api/user/repositories/auth/WebAuthnRepository';
import {GiftCodeRepository} from '@app/api/user/repositories/GiftCodeRepository';
import {PaymentRepository} from '@app/api/user/repositories/PaymentRepository';
import {PushSubscriptionRepository} from '@app/api/user/repositories/PushSubscriptionRepository';
import {RecentMentionRepository} from '@app/api/user/repositories/RecentMentionRepository';
import {SavedMessageRepository} from '@app/api/user/repositories/SavedMessageRepository';
import {
	type HistoricalDmChannelSummary,
	type ListHistoricalDmChannelOptions,
	type PrivateChannelSummary,
	UserChannelRepository,
} from '@app/api/user/repositories/UserChannelRepository';
import {UserRelationshipRepository} from '@app/api/user/repositories/UserRelationshipRepository';
import {UserSettingsRepository} from '@app/api/user/repositories/UserSettingsRepository';
import {VisionarySlotRepository} from '@app/api/user/repositories/VisionarySlotRepository';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

export class UserRepository {
	private accountRepo: UserAccountRepository;
	private lookupRepo: UserLookupRepository;
	private deletionRepo = new UserDeletionRepository();
	private guildRepo = new UserGuildRepository();
	private tokenRepo = new TokenRepository();
	private settingsRepo = new UserSettingsRepository();
	private authSessionRepository = new AuthSessionRepository();
	private mfaBackupCodeRepository = new MfaBackupCodeRepository();
	private ipAuthorizationRepository: IpAuthorizationRepository;
	private webAuthnRepository = new WebAuthnRepository();
	private relationshipRepo = new UserRelationshipRepository();
	private channelRepo = new UserChannelRepository();
	private giftCodeRepository = new GiftCodeRepository();
	private paymentRepository = new PaymentRepository();
	private pushSubscriptionRepository = new PushSubscriptionRepository();
	private recentMentionRepository = new RecentMentionRepository();
	private savedMessageRepository = new SavedMessageRepository();
	private visionarySlotRepository = new VisionarySlotRepository();

	constructor(kv: IKVProvider = getKVClient()) {
		this.accountRepo = new UserAccountRepository(kv);
		const findUnique = this.accountRepo.findUnique.bind(this.accountRepo);
		this.lookupRepo = new UserLookupRepository(findUnique, new UserEmailOwnershipRepository(findUnique, kv));
		this.ipAuthorizationRepository = new IpAuthorizationRepository(this.accountRepo);
	}

	async create(data: UserRow): Promise<User> {
		return this.accountRepo.create(data);
	}

	async upsert(data: UserRow, oldData?: UserRow | null): Promise<User> {
		return this.accountRepo.upsert(data, oldData);
	}

	async patchUpsert(userId: UserID, patchData: Partial<UserRow>, oldData?: UserRow | null): Promise<User> {
		return this.accountRepo.patchUpsert(userId, patchData, oldData);
	}

	async compareAndSetFlags(user: User, flags: bigint): Promise<User | null> {
		return this.accountRepo.compareAndSetFlags(user, flags);
	}

	async updateFlags(userId: UserID, mutate: (flags: bigint) => bigint): Promise<User | null> {
		return this.accountRepo.updateFlags(userId, mutate);
	}

	async updateDeletionSchedule(user: User, patch: UserDeletionScheduleUpdate): Promise<User> {
		return this.accountRepo.updateDeletionSchedule(user, patch);
	}

	async startDeletion(userId: UserID, pendingDeletionAt: Date): Promise<User | null> {
		return this.accountRepo.startDeletion(userId, pendingDeletionAt);
	}

	async anonymizeForDeletion(user: User, patch: Partial<UserRow>): Promise<User> {
		return this.accountRepo.anonymizeForDeletion(user, patch);
	}

	async completeDeletion(user: User): Promise<void> {
		return this.accountRepo.completeDeletion(user);
	}

	async findUnique(userId: UserID): Promise<User | null> {
		return this.accountRepo.findUnique(userId);
	}

	async findUniqueAssert(userId: UserID): Promise<User> {
		return this.accountRepo.findUniqueAssert(userId);
	}

	async findByUsernameDiscriminator(username: string, discriminator: number): Promise<User | null> {
		return this.lookupRepo.findByUsernameDiscriminator(username, discriminator);
	}

	async findDiscriminatorsByUsername(username: string): Promise<Set<number>> {
		return this.lookupRepo.findDiscriminatorsByUsername(username);
	}

	async findUsersByUsername(username: string): Promise<Array<User>> {
		return this.lookupRepo.findUsersByUsername(username);
	}

	async findByEmail(email: string): Promise<User | null> {
		return this.lookupRepo.findByEmail(email);
	}

	async findByStripeSubscriptionId(stripeSubscriptionId: string): Promise<User | null> {
		return this.lookupRepo.findByStripeSubscriptionId(stripeSubscriptionId);
	}

	async findByStripeCustomerId(stripeCustomerId: string): Promise<User | null> {
		return this.lookupRepo.findByStripeCustomerId(stripeCustomerId);
	}

	async listUserIdsByLastActiveIp(
		lastActiveIp: string,
		limit: number,
		offset: number,
	): Promise<{
		userIds: Array<UserID>;
		total: number;
	}> {
		return this.lookupRepo.listUserIdsByLastActiveIp(lastActiveIp, limit, offset);
	}

	async listUsers(userIds: Array<UserID>): Promise<Array<User>> {
		return this.accountRepo.listUsers(userIds);
	}

	async scanAllUsersPage(
		limit: number,
		pageState?: string | null,
	): Promise<{
		users: Array<User>;
		pageState: string | null;
	}> {
		return this.accountRepo.scanAllUsersPage(limit, pageState);
	}

	async getUserGuildIds(userId: UserID): Promise<Array<GuildID>> {
		return this.guildRepo.getUserGuildIds(userId);
	}

	async getActivityTracking(userId: UserID): Promise<{
		last_active_at: Date | null;
		last_active_ip: string | null;
	}> {
		const result = await this.accountRepo.getActivityTracking(userId);
		return result ?? {last_active_at: null, last_active_ip: null};
	}

	async addPendingDeletion(userId: UserID, pendingDeletionAt: Date, deletionReasonCode: number): Promise<void> {
		return this.deletionRepo.addPendingDeletion(userId, pendingDeletionAt, deletionReasonCode);
	}

	async removePendingDeletion(userId: UserID, pendingDeletionAt: Date): Promise<void> {
		return this.deletionRepo.removePendingDeletion(userId, pendingDeletionAt);
	}

	async findUsersPendingDeletionByDate(deletionDate: string): Promise<
		Array<{
			user_id: bigint;
			deletion_reason_code: number;
		}>
	> {
		return this.deletionRepo.findUsersPendingDeletionByDate(deletionDate);
	}

	async scheduleDeletion(userId: UserID, pendingDeletionAt: Date, deletionReasonCode: number): Promise<void> {
		return this.deletionRepo.scheduleDeletion(userId, pendingDeletionAt, deletionReasonCode);
	}

	async deleteUserSecondaryIndices(userId: UserID): Promise<void> {
		return this.accountRepo.deleteUserSecondaryIndices(userId);
	}

	async removeFromAllGuilds(userId: UserID): Promise<void> {
		return this.guildRepo.removeFromAllGuilds(userId);
	}

	async updateLastActiveAt(params: {userId: UserID; lastActiveAt: Date; lastActiveIp?: string}): Promise<void> {
		return this.accountRepo.updateLastActiveAt(params);
	}

	async updateSubscriptionStatus(
		userId: UserID,
		updates: {
			premiumWillCancel: boolean;
			computedPremiumUntil: Date | null;
			periodStart: Date | null;
		},
	): Promise<{
		finalVersion: number | null;
	}> {
		return this.accountRepo.updateSubscriptionStatus(userId, updates);
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
		return this.authSessionRepository.listAuthSessions(userId);
	}

	async listAuthSessionTombstones(userId: UserID): Promise<Array<AuthSessionTombstone>> {
		return this.authSessionRepository.listAuthSessionTombstones(userId);
	}

	async getAuthSessionByToken(sessionIdHash: Buffer): Promise<AuthSession | null> {
		return this.authSessionRepository.getAuthSessionByToken(sessionIdHash);
	}

	async createAuthSession(sessionData: AuthSessionRow): Promise<AuthSession> {
		return this.authSessionRepository.createAuthSession(sessionData);
	}

	async updateAuthSessionLastUsed(sessionIdHash: Buffer): Promise<void> {
		const session = await this.authSessionRepository.getAuthSessionByToken(sessionIdHash);
		if (!session) return;
		await this.authSessionRepository.updateAuthSessionLastUsed(sessionIdHash);
	}

	async deleteAuthSessions(userId: UserID, sessionIdHashes: Array<Buffer>): Promise<void> {
		return this.authSessionRepository.deleteAuthSessions(userId, sessionIdHashes);
	}

	async deleteAllAuthSessions(userId: UserID): Promise<void> {
		return this.authSessionRepository.deleteAllAuthSessions(userId);
	}

	async listMfaBackupCodes(userId: UserID): Promise<Array<MfaBackupCode>> {
		return this.mfaBackupCodeRepository.listMfaBackupCodes(userId);
	}

	async createMfaBackupCodes(userId: UserID, codes: Array<string>): Promise<Array<MfaBackupCode>> {
		return this.mfaBackupCodeRepository.createMfaBackupCodes(userId, codes);
	}

	async clearMfaBackupCodes(userId: UserID): Promise<void> {
		return this.mfaBackupCodeRepository.clearMfaBackupCodes(userId);
	}

	async consumeMfaBackupCode(userId: UserID, code: string): Promise<void> {
		return this.mfaBackupCodeRepository.consumeMfaBackupCode(userId, code);
	}

	async deleteAllMfaBackupCodes(userId: UserID): Promise<void> {
		return this.mfaBackupCodeRepository.deleteAllMfaBackupCodes(userId);
	}

	async getEmailVerificationToken(token: string): Promise<EmailVerificationToken | null> {
		return this.tokenRepo.getEmailVerificationToken(token);
	}

	async createEmailVerificationToken(tokenData: EmailVerificationTokenRow): Promise<EmailVerificationToken> {
		return this.tokenRepo.createEmailVerificationToken(tokenData);
	}

	async deleteEmailVerificationToken(token: string): Promise<void> {
		return this.tokenRepo.deleteEmailVerificationToken(token);
	}

	async getPasswordResetToken(token: string): Promise<PasswordResetToken | null> {
		return this.tokenRepo.getPasswordResetToken(token);
	}

	async createPasswordResetToken(tokenData: PasswordResetTokenRow): Promise<PasswordResetToken> {
		return this.tokenRepo.createPasswordResetToken(tokenData);
	}

	async deleteAllPasswordResetTokens(userId: UserID): Promise<void> {
		return this.tokenRepo.deleteAllPasswordResetTokens(userId);
	}

	async getEmailRevertToken(token: string): Promise<EmailRevertToken | null> {
		return this.tokenRepo.getEmailRevertToken(token);
	}

	async createEmailRevertToken(tokenData: EmailRevertTokenRow): Promise<EmailRevertToken> {
		return this.tokenRepo.createEmailRevertToken(tokenData);
	}

	async deleteEmailRevertToken(token: string): Promise<void> {
		return this.tokenRepo.deleteEmailRevertToken(token);
	}

	async checkIpAuthorized(userId: UserID, ip: string): Promise<boolean> {
		return this.ipAuthorizationRepository.checkIpAuthorized(userId, ip);
	}

	async createAuthorizedIp(userId: UserID, ip: string): Promise<void> {
		return this.ipAuthorizationRepository.createAuthorizedIp(userId, ip);
	}

	async createIpAuthorizationToken(userId: UserID, token: string, email: string): Promise<void> {
		return this.ipAuthorizationRepository.createIpAuthorizationToken(userId, token, email);
	}

	async authorizeIpByToken(token: string): Promise<{
		userId: UserID;
		email: string;
	} | null> {
		return this.ipAuthorizationRepository.authorizeIpByToken(token);
	}

	async getAuthorizedIps(userId: UserID): Promise<
		Array<{
			ip: string;
		}>
	> {
		return this.ipAuthorizationRepository.getAuthorizedIps(userId);
	}

	async deleteAllAuthorizedIps(userId: UserID): Promise<void> {
		return this.ipAuthorizationRepository.deleteAllAuthorizedIps(userId);
	}

	async listWebAuthnCredentials(userId: UserID): Promise<Array<WebAuthnCredential>> {
		return this.webAuthnRepository.listWebAuthnCredentials(userId);
	}

	async getWebAuthnCredential(userId: UserID, credentialId: string): Promise<WebAuthnCredential | null> {
		return this.webAuthnRepository.getWebAuthnCredential(userId, credentialId);
	}

	async createWebAuthnCredential(
		userId: UserID,
		credentialId: string,
		publicKey: Buffer,
		counter: bigint,
		transports: Set<string> | null,
		name: string,
		rpId: string | null,
	): Promise<void> {
		return this.webAuthnRepository.createWebAuthnCredential(
			userId,
			credentialId,
			publicKey,
			counter,
			transports,
			name,
			rpId,
		);
	}

	async updateWebAuthnCredentialCounter(userId: UserID, credentialId: string, counter: bigint): Promise<void> {
		return this.webAuthnRepository.updateWebAuthnCredentialCounter(userId, credentialId, counter);
	}

	async updateWebAuthnCredentialLastUsed(userId: UserID, credentialId: string): Promise<void> {
		return this.webAuthnRepository.updateWebAuthnCredentialLastUsed(userId, credentialId);
	}

	async updateWebAuthnCredentialName(userId: UserID, credentialId: string, name: string): Promise<void> {
		return this.webAuthnRepository.updateWebAuthnCredentialName(userId, credentialId, name);
	}

	async setWebAuthnCredentialSupersededBy(userId: UserID, credentialId: string, supersededBy: string): Promise<void> {
		return this.webAuthnRepository.setWebAuthnCredentialSupersededBy(userId, credentialId, supersededBy);
	}

	async deleteWebAuthnCredential(userId: UserID, credentialId: string): Promise<void> {
		return this.webAuthnRepository.deleteWebAuthnCredential(userId, credentialId);
	}

	async getUserIdByCredentialId(credentialId: string): Promise<UserID | null> {
		return this.webAuthnRepository.getUserIdByCredentialId(credentialId);
	}

	async deleteAllWebAuthnCredentials(userId: UserID): Promise<void> {
		return this.webAuthnRepository.deleteAllWebAuthnCredentials(userId);
	}

	async listRelationships(sourceUserId: UserID): Promise<Array<Relationship>> {
		return this.relationshipRepo.listRelationships(sourceUserId);
	}

	async listBlockedUserIds(sourceUserId: UserID): Promise<Array<UserID>> {
		return this.relationshipRepo.listBlockedUserIds(sourceUserId);
	}

	async hasReachedRelationshipLimit(sourceUserId: UserID, limit: number): Promise<boolean> {
		return this.relationshipRepo.hasReachedRelationshipLimit(sourceUserId, limit);
	}

	async getRelationship(sourceUserId: UserID, targetUserId: UserID, type: number): Promise<Relationship | null> {
		return this.relationshipRepo.getRelationship(sourceUserId, targetUserId, type);
	}

	async upsertRelationship(relationship: RelationshipRow): Promise<Relationship> {
		return this.relationshipRepo.upsertRelationship(relationship);
	}

	async bulkUpdateFriendShareVoiceActivity(sourceUserId: UserID, value: boolean): Promise<Array<Relationship>> {
		return this.relationshipRepo.bulkUpdateFriendShareVoiceActivity(sourceUserId, value);
	}

	async deleteRelationship(sourceUserId: UserID, targetUserId: UserID, type: number): Promise<void> {
		return this.relationshipRepo.deleteRelationship(sourceUserId, targetUserId, type);
	}

	async deleteAllRelationships(userId: UserID): Promise<void> {
		return this.relationshipRepo.deleteAllRelationships(userId);
	}

	async listIncomingRequests(userId: UserID): Promise<Array<Relationship>> {
		return this.relationshipRepo.listIncomingRequests(userId);
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

	async listHistoricalDmChannelIds(userId: UserID): Promise<Array<ChannelID>> {
		return this.channelRepo.listHistoricalDmChannelIds(userId);
	}

	async listHistoricalDmChannelsPaginated(
		userId: UserID,
		options: ListHistoricalDmChannelOptions,
	): Promise<Array<HistoricalDmChannelSummary>> {
		return this.channelRepo.listHistoricalDmChannelsPaginated(userId, options);
	}

	async recordHistoricalDmChannel(userId: UserID, channelId: ChannelID, isGroupDm: boolean): Promise<void> {
		return this.channelRepo.recordHistoricalDmChannel(userId, channelId, isGroupDm);
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

	async createLocalOnlyDmChannel(ownerId: UserID, recipientId: UserID, channelId: ChannelID): Promise<Channel> {
		return this.channelRepo.createLocalOnlyDmChannel(ownerId, recipientId, channelId);
	}

	async isDmChannelOpen(userId: UserID, channelId: ChannelID): Promise<boolean> {
		return this.channelRepo.isDmChannelOpen(userId, channelId);
	}

	async openDmForUser(userId: UserID, channelId: ChannelID, isGroupDm?: boolean): Promise<void> {
		return this.channelRepo.openDmForUser(userId, channelId, isGroupDm);
	}

	async openPrivateChannelForUser(userId: UserID, channel: Channel): Promise<void> {
		return this.channelRepo.openPrivateChannelForUser(userId, channel);
	}

	async closeDmForUser(userId: UserID, channelId: ChannelID): Promise<void> {
		return this.channelRepo.closeDmForUser(userId, channelId);
	}

	async getPinnedDms(userId: UserID): Promise<Array<ChannelID>> {
		return this.channelRepo.getPinnedDms(userId);
	}

	async getPinnedDmsWithDetails(userId: UserID): Promise<
		Array<{
			channel_id: ChannelID;
			sort_order: number;
		}>
	> {
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

	async getRecentMention(userId: UserID, messageId: MessageID): Promise<RecentMention | null> {
		return this.recentMentionRepository.getRecentMention(userId, messageId);
	}

	async listRecentMentions(
		userId: UserID,
		includeEveryone: boolean,
		includeRole: boolean,
		includeGuilds: boolean,
		limit: number,
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

	async listSavedMessages(userId: UserID, limit?: number, before?: MessageID): Promise<Array<SavedMessage>> {
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

	async unrevokeGiftCode(code: string): Promise<void> {
		return this.giftCodeRepository.unrevokeGiftCode(code);
	}

	async markGiftPremiumReversed(gift: GiftCode, seconds: number): Promise<boolean> {
		return this.giftCodeRepository.markGiftPremiumReversed(gift, seconds);
	}

	async clearGiftPremiumReversed(code: string, seconds: number): Promise<boolean> {
		return this.giftCodeRepository.clearGiftPremiumReversed(code, seconds);
	}

	async linkGiftCodeToCheckoutSession(code: string, checkoutSessionId: string): Promise<void> {
		return this.giftCodeRepository.linkGiftCodeToCheckoutSession(code, checkoutSessionId);
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

	async findPaymentsByUserId(userId: UserID): Promise<Array<Payment>> {
		return this.paymentRepository.findPaymentsByUserId(userId);
	}

	async getPaymentByPaymentIntent(paymentIntentId: string): Promise<Payment | null> {
		return this.paymentRepository.getPaymentByPaymentIntent(paymentIntentId);
	}

	async getSubscriptionInfo(subscriptionId: string): Promise<PaymentBySubscriptionRow | null> {
		return this.paymentRepository.getSubscriptionInfo(subscriptionId);
	}

	async listVisionarySlots(): Promise<Array<VisionarySlot>> {
		return this.visionarySlotRepository.listVisionarySlots();
	}

	async expandVisionarySlots(byCount: number): Promise<void> {
		return this.visionarySlotRepository.expandVisionarySlots(byCount);
	}

	async reserveVisionarySlot(slotIndex: number, userId: UserID): Promise<void> {
		return this.visionarySlotRepository.reserveVisionarySlot(slotIndex, userId);
	}

	async unreserveVisionarySlot(slotIndex: number, userId: UserID): Promise<void> {
		return this.visionarySlotRepository.unreserveVisionarySlot(slotIndex, userId);
	}
}
