// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {ArchiveAttemptSupersededError} from '@app/api/archive/ArchiveAttemptSupersededError';
import {
	type ArchiveTaskHandler,
	ArchiveTerminalFailureError,
	createArchiveTask,
	throwIfArchiveTerminallyFailed,
} from '@app/api/archive/ArchiveTask';
import {
	type ChannelID,
	createAttachmentID,
	createChannelID,
	createUserID,
	type GuildID,
	type MessageID,
	type UserID,
} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {makeAttachmentCdnUrl} from '@app/api/channel/services/message/MessageHelpers';
import {
	isChannelEligible,
	isTimestampInWindow,
	type SelfMessageEligibilityContext,
	type SelfMessageFilter,
} from '@app/api/channel/services/message/SelfMessageFilter';
import type {UserConnectionRow} from '@app/api/database/types/ConnectionTypes';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {Logger} from '@app/api/Logger';
import type {Application} from '@app/api/models/Application';
import type {Attachment} from '@app/api/models/Attachment';
import type {AuthSession} from '@app/api/models/AuthSession';
import type {Channel} from '@app/api/models/Channel';
import type {FavoriteMeme} from '@app/api/models/FavoriteMeme';
import type {GiftCode} from '@app/api/models/GiftCode';
import type {Guild} from '@app/api/models/Guild';
import type {GuildMember} from '@app/api/models/GuildMember';
import type {MfaBackupCode} from '@app/api/models/MfaBackupCode';
import type {Payment} from '@app/api/models/Payment';
import type {PushSubscription} from '@app/api/models/PushSubscription';
import type {Relationship} from '@app/api/models/Relationship';
import type {SavedMessage} from '@app/api/models/SavedMessage';
import type {User} from '@app/api/models/User';
import type {UserGuildSettings} from '@app/api/models/UserGuildSettings';
import type {UserSettings} from '@app/api/models/UserSettings';
import type {WebAuthnCredential} from '@app/api/models/WebAuthnCredential';
import {buildHarvestDownloadUrl} from '@app/api/user/services/HarvestDownloadUrl';
import {mapWithConcurrency} from '@app/api/utils/ConcurrencyUtils';
import {resolveSessionClientInfo} from '@app/api/utils/SessionClientIdentity';
import {writeZipArchive} from '@app/api/worker/utils/ArchiveFile';
import {createArchiveJsonBuffer} from '@app/api/worker/utils/ArchiveJson';
import {
	appendAssetToArchive,
	buildHashedAssetKey,
	getAnimatedAssetExtension,
} from '@app/api/worker/utils/AssetArchiveHelpers';
import {ContentAddressedAttachmentCollector} from '@app/api/worker/utils/ContentAddressedAttachmentCollector';
import {deserializeSelfMessageFilter, SelfMessageFilterPayload} from '@app/api/worker/utils/SelfMessageFilterPayload';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerDependencies} from '@app/api/worker/WorkerDependencies';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {
	decodeSyncedPreferencesLenient,
	syncedPreferencesToJson,
} from '@fluxer/schema/src/domains/user/SyncedPreferencesCodec';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import {ms} from 'itty-time';
import {z} from 'zod';

const PayloadSchema = z.object({
	userId: z.string(),
	harvestId: z.string(),
	adminRequestedBy: z.string().optional(),
	includeAttachments: z.boolean().default(false),
	filter: SelfMessageFilterPayload.optional(),
});

interface HarvestedAttachment {
	attachment_id: string;
	filename: string;
	size: string;
	content_type: string;
	content_hash: string | null;
	archive_path: string | null;
	cdn_url: string;
	width: number | null;
	height: number | null;
}

interface HarvestedMessage {
	id: string;
	timestamp: string;
	content: string;
	attachments: Array<HarvestedAttachment>;
}

interface ChannelHarvestResult {
	channelId: string;
	messageData: HarvestedMessage;
}

interface GuildMembershipEntry {
	member: GuildMember | null;
	guild: Guild | null;
	guildId: GuildID;
}

interface HarvestMessageResult {
	channelMessagesMap: Map<string, Array<HarvestedMessage>>;
	totalMessages: number;
}

interface UserDataJsonParams {
	user: User;
	userId: UserID;
	productName: string;
	authSessions: Array<AuthSession>;
	relationships: Array<Relationship>;
	userNotes: Map<UserID, string>;
	userSettings: UserSettings | null;
	guildMemberships: Array<GuildMembershipEntry>;
	guildSettings: Array<UserGuildSettings | null>;
	savedMessages: Array<SavedMessage>;
	privateChannels: Array<Channel>;
	favoriteMemes: Array<FavoriteMeme>;
	pushSubscriptions: Array<PushSubscription>;
	webAuthnCredentials: Array<WebAuthnCredential>;
	mfaBackupCodes: Array<MfaBackupCode>;
	createdGiftCodes: Array<GiftCode>;
	payments: Array<Payment>;
	oauthClients: Array<Application>;
	connections: Array<UserConnectionRow>;
	pinnedDms: Array<{
		channel_id: bigint;
		sort_order: number;
	}>;
	authorizedIps: Array<{
		ip: string;
	}>;
	activityData: {
		last_active_at: Date | null;
		last_active_ip: string | null;
	};
}

interface ArchiveParams {
	userId: UserID;
	harvestId: bigint;
	attemptId: string;
	expiresAt: Date;
	isAdminArchive: boolean;
	includeAttachments: boolean;
	userDataJsonBuffer: Buffer;
	user: User;
	channelMessagesMap: Map<string, Array<HarvestedMessage>>;
	payments: Array<Payment>;
	oauthClients: Array<Application>;
	authorizedIps: Array<{
		ip: string;
	}>;
	activityData: {
		last_active_at: Date | null;
		last_active_ip: string | null;
	};
	storageService: IStorageService;
}

interface ArchiveResult {
	fileSize: bigint;
	storageKey: string;
	expiresAt: Date;
	downloadUrl: string;
}

interface PreparedUserArchive {
	user: User;
	totalMessages: number;
	archive: ArchiveResult;
}

interface HarvestProgressReporter {
	attemptId: string;
	expiresAt: Date | null;
	updateProgress(percent: number, step: string): Promise<void>;
	markAsCompleted(storageKey: string, fileSize: bigint, expiresAt: Date): Promise<void>;
	markAsFailed(message: string): Promise<void>;
	markAsTerminallyFailed(message: string): Promise<void>;
}

async function claimUserArchive(
	userId: UserID,
	harvestId: bigint,
	isAdminArchive: boolean,
	{userHarvestRepository, adminArchiveRepository}: WorkerDependencies,
): Promise<HarvestProgressReporter | null> {
	if (isAdminArchive) {
		const archive = await adminArchiveRepository.findBySubjectAndArchiveId('user', userId, harvestId);
		if (!archive) throw new Error(`Admin archive ${harvestId} for user ${userId} not found`);
		throwIfArchiveTerminallyFailed(archive);
		if (archive.completedAt) return null;
		const attempt = await adminArchiveRepository.markAsStarted(archive);
		assert(attempt.attemptId !== null && attempt.expiresAt !== null, 'Claimed admin archive is incomplete');
		return {
			attemptId: attempt.attemptId,
			expiresAt: attempt.expiresAt,
			updateProgress: (percent, step) => adminArchiveRepository.updateProgress(attempt, percent, step),
			markAsCompleted: (key, size, expiry) => adminArchiveRepository.markAsCompleted(attempt, key, size, expiry),
			markAsFailed: (message) => adminArchiveRepository.markAsFailed(attempt, message),
			markAsTerminallyFailed: (message) => adminArchiveRepository.markAsTerminallyFailed(attempt, message),
		};
	}
	const harvest = await userHarvestRepository.findByUserAndHarvestId(userId, harvestId);
	if (!harvest) throw new Error(`Harvest ${harvestId} for user ${userId} not found`);
	throwIfArchiveTerminallyFailed(harvest);
	if (harvest.completedAt) return null;
	const attempt = await userHarvestRepository.markAsStarted(harvest);
	assert(attempt.attemptId !== null, 'Claimed harvest has no attempt ID');
	return {
		attemptId: attempt.attemptId,
		expiresAt: null,
		updateProgress: (percent, step) => userHarvestRepository.updateProgress(attempt, percent, step),
		markAsCompleted: (key, size, expiry) => userHarvestRepository.markAsCompleted(attempt, key, size, expiry),
		markAsFailed: (message) => userHarvestRepository.markAsFailed(attempt, message),
		markAsTerminallyFailed: (message) => userHarvestRepository.markAsTerminallyFailed(attempt, message),
	};
}

// A harvest is every message the account wrote, so the read pages to the end of
// the account rather than stopping at a count. The page size is what bounds one
// query, not what bounds the archive.
const HARVEST_MESSAGE_CHUNK_SIZE = 1000;
const HARVEST_READ_CONCURRENCY = 10;
const INITIAL_PROGRESS = 5;
const MESSAGES_PROGRESS_MAX = 55;
const METADATA_PROGRESS = 60;
const ZIP_EXPIRY_MS = ms('7 days');

function mapPayment(payment: Payment) {
	return {
		checkout_session_id: payment.checkoutSessionId,
		amount_cents: payment.amountCents,
		currency: payment.currency,
		status: payment.status,
		subscription_id: payment.subscriptionId,
		payment_intent_id: payment.paymentIntentId,
		product_type: payment.productType,
		is_gift: payment.isGift,
		gift_code: payment.giftCode,
		created_at: payment.createdAt.toISOString(),
		completed_at: payment.completedAt?.toISOString() ?? null,
	};
}

function mapOAuthApplication(app: Application) {
	return {
		application_id: app.applicationId.toString(),
		name: app.name,
		redirect_uris: Array.from(app.oauth2RedirectUris),
	};
}

function mapSecurityData(params: {
	authorizedIps: Array<{
		ip: string;
	}>;
	activityData: {
		last_active_at: Date | null;
		last_active_ip: string | null;
	};
}) {
	return {
		authorized_ips: params.authorizedIps,
		activity_tracking: {
			last_active_at: params.activityData.last_active_at?.toISOString() ?? null,
			last_active_ip: params.activityData.last_active_ip,
		},
	};
}

interface HarvestMessagesFilterArgs {
	filter: SelfMessageFilter;
	context: SelfMessageEligibilityContext;
	findChannel: (channelId: ChannelID) => Promise<Channel | null>;
}

interface HarvestMessageReference {
	channelId: ChannelID;
	messageId: MessageID;
}

interface HarvestMessageRepository {
	listMessagesByAuthor(
		userId: UserID,
		limit: number,
		lastMessageId?: MessageID,
	): Promise<Array<HarvestMessageReference>>;
	getMessage(
		channelId: ChannelID,
		messageId: MessageID,
	): Promise<{content: string | null; attachments?: Array<Attachment>} | null>;
}

export async function harvestMessages(
	channelRepository: HarvestMessageRepository,
	userId: UserID,
	startTime: number,
	filterArgs: HarvestMessagesFilterArgs | null,
): Promise<HarvestMessageResult> {
	const channelMessagesMap = new Map<string, Array<HarvestedMessage>>();
	const channelEligibility = filterArgs ? new Map<string, boolean>() : null;
	Logger.debug('Fetching all user messages');
	const startFetchTime = Date.now();
	let lastMessageId: MessageID | undefined;
	let scannedMessages = 0;
	let totalMessages = 0;

	const readMessage = async ({channelId, messageId}: HarvestMessageReference): Promise<ChannelHarvestResult | null> => {
		const message = await channelRepository.getMessage(channelId, messageId);
		if (!message) {
			Logger.warn(
				{channelId: channelId.toString(), messageId: messageId.toString()},
				'Message not found during harvest',
			);
			return null;
		}
		return {
			channelId: channelId.toString(),
			messageData: {
				id: messageId.toString(),
				timestamp: snowflakeToDate(messageId).toISOString(),
				content: message.content ?? '',
				attachments: (message.attachments ?? []).map((attachment) => ({
					attachment_id: attachment.id.toString(),
					filename: attachment.filename,
					size: attachment.size.toString(),
					content_type: attachment.contentType,
					content_hash: null,
					archive_path: null,
					cdn_url: makeAttachmentCdnUrl(channelId, attachment.id, attachment.filename),
					width: attachment.width,
					height: attachment.height,
				})),
			},
		};
	};

	while (true) {
		const page = await channelRepository.listMessagesByAuthor(userId, HARVEST_MESSAGE_CHUNK_SIZE, lastMessageId);
		if (page.length === 0) {
			break;
		}
		scannedMessages += page.length;
		lastMessageId = page[page.length - 1].messageId;

		const pageRefs: Array<HarvestMessageReference> = [];
		for (const ref of page) {
			if (!filterArgs) {
				pageRefs.push(ref);
				continue;
			}
			const ts = snowflakeToDate(ref.messageId).getTime();
			if (!isTimestampInWindow(ts, filterArgs.filter)) {
				continue;
			}
			const channelIdStr = ref.channelId.toString();
			let eligible = channelEligibility!.get(channelIdStr);
			if (eligible === undefined) {
				const channel = await filterArgs.findChannel(ref.channelId);
				eligible = channel ? isChannelEligible(channel, userId, filterArgs.filter, filterArgs.context) : false;
				channelEligibility!.set(channelIdStr, eligible);
			}
			if (eligible) {
				pageRefs.push(ref);
			}
		}

		const pageResults = await mapWithConcurrency(pageRefs, HARVEST_READ_CONCURRENCY, readMessage);
		for (const result of pageResults) {
			if (result === null) continue;
			let bucket = channelMessagesMap.get(result.channelId);
			if (!bucket) {
				bucket = [];
				channelMessagesMap.set(result.channelId, bucket);
			}
			bucket.push(result.messageData);
			totalMessages++;
		}

		if (page.length < HARVEST_MESSAGE_CHUNK_SIZE) {
			break;
		}
	}

	Logger.debug(
		{
			scannedMessages,
			totalMessages,
			fetchElapsed: Date.now() - startFetchTime,
			totalElapsed: Date.now() - startTime,
		},
		'All messages retrieved',
	);
	return {channelMessagesMap, totalMessages};
}

function buildUserDataJson(params: UserDataJsonParams) {
	const {
		user,
		userId,
		productName,
		authSessions,
		relationships,
		userNotes,
		userSettings,
		guildMemberships,
		guildSettings,
		savedMessages,
		privateChannels,
		favoriteMemes,
		pushSubscriptions,
		webAuthnCredentials,
		mfaBackupCodes,
		createdGiftCodes,
		payments,
		oauthClients,
		connections,
		pinnedDms,
		authorizedIps,
		activityData,
	} = params;
	return {
		user: {
			id: user.id.toString(),
			username: user.username,
			discriminator: user.discriminator,
			bot: user.isBot,
			system: user.isSystem,
			email: user.email,
			email_verified: user.emailVerified,
			email_bounced: user.emailBounced,
			has_verified_phone: user.hasVerifiedPhone,
			avatar_hash: user.avatarHash,
			avatar_url: user.avatarHash
				? `${Config.endpoints.media}/avatars/${userId}/${user.avatarHash}.${user.avatarHash.startsWith('a_') ? 'gif' : 'png'}`
				: null,
			banner_hash: user.bannerHash,
			banner_url: user.bannerHash
				? `${Config.endpoints.media}/banners/${userId}/${user.bannerHash}.${user.bannerHash.startsWith('a_') ? 'gif' : 'png'}`
				: null,
			bio: user.bio,
			pronouns: user.pronouns,
			accent_color: user.accentColor,
			date_of_birth: user.dateOfBirth,
			locale: user.locale,
			flags: user.flags.toString(),
			premium_type: user.premiumType,
			premium_since: user.premiumSince?.toISOString() ?? null,
			premium_until: user.premiumUntil?.toISOString() ?? null,
			premium_lifetime_sequence: user.premiumLifetimeSequence,
			stripe_customer_id: user.stripeCustomerId,
			stripe_subscription_id: user.stripeSubscriptionId,
			terms_agreed_at: user.termsAgreedAt?.toISOString() ?? null,
			privacy_agreed_at: user.privacyAgreedAt?.toISOString() ?? null,
			last_active_at: user.lastActiveAt?.toISOString() ?? null,
			created_at: snowflakeToDate(user.id).toISOString(),
			mfa_enabled: user.authenticatorTypes.size > 0,
			authenticator_types: Array.from(user.authenticatorTypes),
		},
		auth_sessions: authSessions.map((session) => {
			const clientInfo = resolveSessionClientInfo({
				userAgent: session.clientUserAgent,
				reportedOs: session.clientOs ?? null,
				productName,
			});
			return {
				created_at: session.createdAt.toISOString(),
				approx_last_used_at: session.approximateLastUsedAt?.toISOString() ?? null,
				client_ip: session.clientIp,
				client_os: clientInfo.os,
				client_user_agent: session.clientUserAgent,
				client_platform: clientInfo.platform,
			};
		}),
		relationships: relationships.map((rel) => ({
			target_user_id: rel.targetUserId.toString(),
			type: rel.type,
			nickname: rel.nickname,
			since: rel.since?.toISOString() ?? null,
		})),
		notes: Array.from(userNotes.entries()).map(([targetUserId, note]) => ({
			target_user_id: targetUserId.toString(),
			note,
		})),
		user_settings: userSettings
			? {
					locale: userSettings.locale,
					theme: userSettings.theme,
					status: userSettings.status,
					custom_status: userSettings.customStatus
						? {
								text: userSettings.customStatus.text,
								emoji_id: userSettings.customStatus.emojiId?.toString() ?? null,
								emoji_name: userSettings.customStatus.emojiName,
								emoji_animated: userSettings.customStatus.emojiAnimated,
								expires_at: userSettings.customStatus.expiresAt?.toISOString() ?? null,
							}
						: null,
					developer_mode: userSettings.developerMode,
					message_display_compact: userSettings.compactMessageDisplay,
					animate_emoji: userSettings.animateEmoji,
					animate_stickers: userSettings.animateStickers,
					gif_auto_play: userSettings.gifAutoPlay,
					render_embeds: userSettings.renderEmbeds,
					render_reactions: userSettings.renderReactions,
					render_spoilers: userSettings.renderSpoilers,
					inline_attachment_media: userSettings.inlineAttachmentMedia,
					inline_embed_media: userSettings.inlineEmbedMedia,
					explicit_content_filter: userSettings.explicitContentFilter,
					friend_source_flags: userSettings.friendSourceFlags,
					default_guilds_restricted: userSettings.defaultGuildsRestricted,
					bot_default_guilds_restricted: userSettings.botDefaultGuildsRestricted,
					restricted_guilds: Array.from(userSettings.restrictedGuilds).map((id) => id.toString()),
					bot_restricted_guilds: Array.from(userSettings.botRestrictedGuilds).map((id) => id.toString()),
					guild_positions: userSettings.guildPositions.map((id) => id.toString()),
					guild_folders: userSettings.guildFolders.map((folder) => ({
						id: folder.folderId,
						name: folder.name,
						color: folder.color,
						flags: folder.flags,
						icon: folder.icon,
						guild_ids: folder.guildIds.map(String),
					})),
					afk_timeout: userSettings.afkTimeout,
					time_format: userSettings.timeFormat,
					suppress_unprivileged_self_mentions: userSettings.suppressUnprivilegedSelfMentions,
					suppress_unprivileged_self_mentions_bypass_user_ids: Array.from(
						userSettings.suppressUnprivilegedSelfMentionBypassUserIds,
					).map((id) => id.toString()),
					staff_dm_access_user_ids: Array.from(userSettings.staffDmAccessUserIds).map((id) => id.toString()),
					synced_preferences: syncedPreferencesToJson(decodeSyncedPreferencesLenient(userSettings.syncedPreferences)),
					profile_privacy: userSettings.profilePrivacy,
				}
			: null,
		guild_memberships: guildMemberships
			.filter((gm) => gm.member !== null)
			.map(({member, guild, guildId}) => ({
				guild_id: guildId.toString(),
				guild_name: guild?.name ?? null,
				joined_at: member!.joinedAt.toISOString(),
				nick: member!.nickname,
				avatar_hash: member!.avatarHash,
				avatar_url: member!.avatarHash
					? `${Config.endpoints.media}/guilds/${guildId}/users/${userId}/avatars/${member!.avatarHash}`
					: null,
				banner_hash: member!.bannerHash,
				banner_url: member!.bannerHash
					? `${Config.endpoints.media}/guilds/${guildId}/users/${userId}/banners/${member!.bannerHash}`
					: null,
				role_ids: Array.from(member!.roleIds).map((id) => id.toString()),
			})),
		user_guild_settings: guildSettings
			.filter((settings) => settings !== null)
			.map((settings) => ({
				guild_id: settings!.guildId.toString(),
				message_notifications: settings!.messageNotifications,
				muted: settings!.muted,
				mobile_push: settings!.mobilePush,
				suppress_everyone: settings!.suppressEveryone,
				suppress_roles: settings!.suppressRoles,
				hide_muted_channels: settings!.hideMutedChannels,
			})),
		saved_messages: savedMessages.map((msg) => ({
			channel_id: msg.channelId.toString(),
			message_id: msg.messageId.toString(),
			saved_at: msg.savedAt.toISOString(),
		})),
		private_channels: privateChannels.map((channel) => ({
			channel_id: channel.id.toString(),
			type: channel.type,
			name: channel.name,
			icon_hash: channel.iconHash,
			owner_id: channel.ownerId?.toString() ?? null,
			recipient_ids: Array.from(channel.recipientIds).map((id) => id.toString()),
			last_message_id: channel.lastMessageId?.toString() ?? null,
		})),
		favorite_memes: favoriteMemes.map((meme) => ({
			meme_id: meme.id.toString(),
			name: meme.name,
			alt_text: meme.altText,
			tags: meme.tags,
			filename: meme.filename,
			content_type: meme.contentType,
			size: meme.size.toString(),
			width: meme.width,
			height: meme.height,
			duration: meme.duration,
		})),
		push_subscriptions: pushSubscriptions.map((sub) => ({
			subscription_id: sub.subscriptionId,
			platform: sub.platform,
			app_id: sub.appId,
			provider_environment: sub.providerEnvironment,
			endpoint: sub.endpoint,
			user_agent: sub.userAgent,
		})),
		webauthn_credentials: webAuthnCredentials.map((cred) => ({
			credential_id: cred.credentialId,
			name: cred.name,
			transports: cred.transports ? Array.from(cred.transports) : [],
			created_at: cred.createdAt.toISOString(),
			last_used_at: cred.lastUsedAt?.toISOString() ?? null,
		})),
		mfa_backup_codes: {
			total_count: mfaBackupCodes.length,
			consumed_count: mfaBackupCodes.filter((code) => code.consumed).length,
			remaining_count: mfaBackupCodes.filter((code) => !code.consumed).length,
		},
		gift_codes_created: createdGiftCodes.map((gift) => ({
			code: gift.code,
			duration_months: gift.durationMonths,
			duration_type: gift.durationType,
			duration_quantity: gift.durationQuantity,
			created_at: gift.createdAt.toISOString(),
			redeemed_by_user_id: gift.redeemedByUserId?.toString() ?? null,
			redeemed_at: gift.redeemedAt?.toISOString() ?? null,
			stripe_payment_intent_id: gift.stripePaymentIntentId,
		})),
		payments: payments.map(mapPayment),
		oauth_applications: oauthClients.map(mapOAuthApplication),
		connections: connections.map((connection) => ({
			id: connection.connection_id,
			type: connection.connection_type,
			identifier: connection.identifier,
			name: connection.name,
			verified: connection.verified,
			visibility_flags: connection.visibility_flags,
			sort_order: connection.sort_order,
			created_at: connection.created_at.toISOString(),
			verified_at: connection.verified_at?.toISOString() ?? null,
			last_verified_at: connection.last_verified_at?.toISOString() ?? null,
		})),
		pinned_dms: pinnedDms.map((pin) => ({
			channel_id: pin.channel_id.toString(),
			sort_order: pin.sort_order,
		})),
		...mapSecurityData({authorizedIps, activityData}),
	};
}

async function createAndUploadArchive(params: ArchiveParams): Promise<ArchiveResult> {
	const {
		userId,
		harvestId,
		attemptId,
		expiresAt,
		isAdminArchive,
		includeAttachments,
		userDataJsonBuffer,
		user,
		channelMessagesMap,
		payments,
		oauthClients,
		authorizedIps,
		activityData,
		storageService,
	} = params;
	const userIdString = userId.toString();
	await using tempDir = await fs.promises.mkdtempDisposable(path.join(os.tmpdir(), 'fluxer-harvest-'));
	const zipPath = path.join(tempDir.path, `user-data-${userId}.zip`);
	await writeZipArchive(zipPath, async (archive) => {
		await archive.append(userDataJsonBuffer, {name: 'user.json'});
		if (user.avatarHash) {
			const avatarArchiveName = `assets/user/avatar.${getAnimatedAssetExtension(user.avatarHash)}`;
			const avatarStorageKey = buildHashedAssetKey('avatars', userIdString, user.avatarHash);
			await appendAssetToArchive({
				archive,
				storageService,
				storageKey: avatarStorageKey,
				archiveName: avatarArchiveName,
				label: 'user avatar',
				subjectId: userIdString,
			});
		}
		if (user.bannerHash) {
			const bannerArchiveName = `assets/user/banner.${getAnimatedAssetExtension(user.bannerHash)}`;
			const bannerStorageKey = buildHashedAssetKey('banners', userIdString, user.bannerHash);
			await appendAssetToArchive({
				archive,
				storageService,
				storageKey: bannerStorageKey,
				archiveName: bannerArchiveName,
				label: 'user banner',
				subjectId: userIdString,
			});
		}
		const collector = includeAttachments ? new ContentAddressedAttachmentCollector() : null;
		for (const [channelId, messages] of channelMessagesMap.entries()) {
			if (collector) {
				for (const message of messages) {
					for (const attachment of message.attachments) {
						const result = await collector.collect({
							storageService,
							archive,
							channelId: createChannelID(BigInt(channelId)),
							attachmentId: createAttachmentID(BigInt(attachment.attachment_id)),
							filename: attachment.filename,
						});
						if (result) {
							attachment.content_hash = result.hash;
							attachment.archive_path = result.archivePath;
						}
					}
				}
			}
			messages.sort((a, b) => a.timestamp.localeCompare(b.timestamp));
			await archive.append(createArchiveJsonBuffer(messages), {name: `channels/${channelId}/messages.json`});
		}
		if (collector) {
			const manifest = collector.getManifest();
			await archive.append(createArchiveJsonBuffer(manifest), {name: 'attachments_manifest.json'});
		}
		await archive.append(createArchiveJsonBuffer(payments.map(mapPayment)), {
			name: 'payments/payment_history.json',
		});
		await archive.append(createArchiveJsonBuffer({applications: oauthClients.map(mapOAuthApplication)}), {
			name: 'integrations/oauth.json',
		});
		await archive.append(createArchiveJsonBuffer(mapSecurityData({authorizedIps, activityData})), {
			name: 'account/security.json',
		});
	});
	const storageKey = isAdminArchive
		? `archives/users/${userId}/${harvestId}/${attemptId}/user-data.zip`
		: `exports/${userId}/${harvestId}/${attemptId}/user-data.zip`;
	const zipStat = await fs.promises.stat(zipPath);
	const fileSize = BigInt(zipStat.size);
	await storageService.uploadObjectFromFile({
		bucket: Config.s3.buckets.harvests,
		key: storageKey,
		filePath: zipPath,
		contentLength: zipStat.size,
		contentType: 'application/zip',
		expiresAt,
	});
	const downloadUrl = await buildHarvestDownloadUrl({
		userId,
		harvestId,
		storageKey,
		expiresInSeconds: ZIP_EXPIRY_MS / 1000,
		storageService,
	});
	return {fileSize, storageKey, expiresAt, downloadUrl};
}

const harvestUserData: ArchiveTaskHandler = async (payload, helpers, attempt) => {
	const validated = PayloadSchema.parse(payload);
	helpers.logger.debug({payload}, 'Processing harvestUserData task');
	const startTime = Date.now();
	const userId = createUserID(BigInt(validated.userId));
	const harvestId = BigInt(validated.harvestId);
	Logger.info({userId, harvestId, startTime: new Date(startTime).toISOString()}, 'Task started');
	const dependencies = getWorkerDependencies();
	const {
		channelRepository,
		guildRepository,
		userRepository,
		favoriteMemeRepository,
		paymentRepository,
		applicationRepository,
		connectionRepository,
		storageService,
		emailService,
		instanceConfigRepository,
	} = dependencies;
	const adminRequestedBy = validated.adminRequestedBy ? BigInt(validated.adminRequestedBy) : null;
	const isAdminArchive = adminRequestedBy !== null;
	const progressReporter = await claimUserArchive(userId, harvestId, isAdminArchive, dependencies);
	if (progressReporter === null) {
		Logger.info({userId, harvestId}, 'Harvest already completed, skipping');
		return;
	}
	let prepared: PreparedUserArchive;
	try {
		let filterArgs: HarvestMessagesFilterArgs | null = null;
		if (validated.filter) {
			const filter = deserializeSelfMessageFilter(validated.filter);
			const [privateChannelsForFilter, userGuildsForFilter] = await Promise.all([
				userRepository.listPrivateChannels(userId),
				guildRepository.listUserGuilds(userId),
			]);
			const openDmChannelIds = new Set<string>();
			for (const channel of privateChannelsForFilter) {
				if (channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM) {
					openDmChannelIds.add(channel.id.toString());
				}
			}
			const context: SelfMessageEligibilityContext = {
				currentGuildIds: new Set(userGuildsForFilter.map((guild) => guild.id.toString())),
				openDmChannelIds,
			};
			filterArgs = {
				filter,
				context,
				findChannel: (channelId) => channelRepository.findUnique(channelId),
			};
		}
		Logger.debug({userId, harvestId, elapsed: Date.now() - startTime}, 'Starting user data harvest');
		await progressReporter.updateProgress(INITIAL_PROGRESS, 'Harvesting messages');
		Logger.debug({elapsed: Date.now() - startTime}, 'Set progress to INITIAL_PROGRESS');
		const {channelMessagesMap, totalMessages} = await harvestMessages(channelRepository, userId, startTime, filterArgs);
		if (totalMessages > 0) {
			const progress = Math.min(INITIAL_PROGRESS + Math.floor((totalMessages / 10000) * 50), MESSAGES_PROGRESS_MAX);
			await progressReporter.updateProgress(progress, `Harvested ${totalMessages} messages`);
		}
		Logger.debug(
			{
				userId,
				harvestId,
				channelCount: channelMessagesMap.size,
				totalMessages,
				elapsed: Date.now() - startTime,
			},
			'Harvested all messages',
		);
		await progressReporter.updateProgress(METADATA_PROGRESS, 'Collecting user metadata');
		Logger.debug({elapsed: Date.now() - startTime}, 'Starting metadata collection');
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new Error(`User ${userId} not found`);
		}
		const [
			authSessions,
			relationships,
			userNotes,
			userSettings,
			guildIds,
			savedMessages,
			privateChannels,
			favoriteMemes,
			pushSubscriptions,
			webAuthnCredentials,
			mfaBackupCodes,
			createdGiftCodes,
			payments,
			oauthClients,
			connections,
			pinnedDms,
			authorizedIps,
			activityData,
		] = await Promise.all([
			userRepository.listAuthSessions(userId),
			userRepository.listRelationships(userId),
			userRepository.getUserNotes(userId),
			userRepository.findSettings(userId),
			userRepository.getUserGuildIds(userId),
			userRepository.listSavedMessages(userId, 1000),
			userRepository.listPrivateChannels(userId),
			favoriteMemeRepository.findByUserId(userId),
			userRepository.listPushSubscriptions(userId),
			userRepository.listWebAuthnCredentials(userId),
			userRepository.listMfaBackupCodes(userId),
			userRepository.findGiftCodesByCreator(userId),
			paymentRepository.findPaymentsByUserId(userId),
			applicationRepository.listApplicationsByOwner(userId),
			connectionRepository.findByUserId(userId),
			userRepository.getPinnedDmsWithDetails(userId),
			userRepository.getAuthorizedIps(userId),
			userRepository.getActivityTracking(userId),
		]);
		const guilds = await guildRepository.listGuilds(guildIds);
		const guildsMap = new Map(guilds.map((guild) => [guild.id.toString(), guild]));
		const guildMemberships = await mapWithConcurrency(guildIds, HARVEST_READ_CONCURRENCY, async (guildId) => {
			const member = await guildRepository.getMember(guildId, userId);
			const guild = guildsMap.get(guildId.toString()) ?? null;
			return {member, guild, guildId};
		});
		const guildSettings = await mapWithConcurrency(guildIds, HARVEST_READ_CONCURRENCY, (guildId) =>
			userRepository.findGuildSettings(userId, guildId),
		);
		const {branding} = await instanceConfigRepository.getAppPublicConfig();
		const userData = buildUserDataJson({
			user,
			userId,
			productName: branding.product_name,
			authSessions,
			relationships,
			userNotes,
			userSettings,
			guildMemberships,
			guildSettings,
			savedMessages,
			privateChannels,
			favoriteMemes,
			pushSubscriptions,
			webAuthnCredentials,
			mfaBackupCodes,
			createdGiftCodes,
			payments,
			oauthClients,
			connections,
			pinnedDms,
			authorizedIps,
			activityData,
		});
		const userDataJsonBuffer = createArchiveJsonBuffer(userData);
		Logger.debug({userId, harvestId, elapsed: Date.now() - startTime}, 'Collected user metadata');
		const includeAttachments = validated.includeAttachments && isAdminArchive;
		await progressReporter.updateProgress(METADATA_PROGRESS + 5, 'Downloading attachments and creating archive');
		Logger.debug({elapsed: Date.now() - startTime}, 'Starting ZIP creation');
		const archive = await createAndUploadArchive({
			userId,
			harvestId,
			attemptId: progressReporter.attemptId,
			expiresAt: progressReporter.expiresAt ?? new Date(Date.now() + ZIP_EXPIRY_MS),
			isAdminArchive,
			includeAttachments,
			userDataJsonBuffer,
			user,
			channelMessagesMap,
			payments,
			oauthClients,
			authorizedIps,
			activityData,
			storageService,
		});
		Logger.debug(
			{userId, harvestId, zipSize: archive.fileSize.toString(), elapsed: Date.now() - startTime},
			'Uploaded final ZIP to S3 with TTL',
		);
		prepared = {user, totalMessages, archive};
	} catch (error) {
		if (error instanceof ArchiveAttemptSupersededError) throw error;
		Logger.error(
			{
				error,
				userId,
				harvestId,
				elapsed: Date.now() - startTime,
			},
			'Failed to harvest user data',
		);
		const message = error instanceof Error ? error.message : String(error);
		try {
			if (attempt.isLastAttempt) {
				await progressReporter.markAsTerminallyFailed(message);
			} else {
				await progressReporter.markAsFailed(message);
			}
		} catch (statusError) {
			if (statusError instanceof ArchiveAttemptSupersededError) throw statusError;
			throw new AggregateError([error, statusError], 'Harvest preparation and failure recording both failed');
		}
		if (attempt.isLastAttempt) throw new ArchiveTerminalFailureError(message);
		throw error;
	}
	const {user, totalMessages, archive} = prepared;
	await progressReporter.markAsCompleted(archive.storageKey, archive.fileSize, archive.expiresAt);
	Logger.debug({userId, harvestId}, 'Marked harvest as completed');
	if (!isAdminArchive && user.email && (await instanceConfigRepository.isEmailEnabled())) {
		const sent = await emailService.sendHarvestCompletedEmail(
			user.email,
			user.username,
			archive.downloadUrl,
			totalMessages,
			Number(archive.fileSize),
			archive.expiresAt,
			user.locale,
		);
		if (!sent) throw new Error(`Completion email for harvest ${harvestId} was not sent`);
		Logger.debug({userId, harvestId, totalMessages}, 'Sent harvest completion email');
	}
	Logger.info(
		{
			userId,
			harvestId,
			totalElapsed: Date.now() - startTime,
			totalElapsedSeconds: Math.round((Date.now() - startTime) / 1000),
		},
		'User data harvest completed successfully',
	);
};

export default createArchiveTask(harvestUserData);
