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

import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import archiver from 'archiver';
import type {Task} from 'graphile-worker';
import {type ChannelID, createUserID, type MessageID} from '~/BrandedTypes';
import {Config} from '~/Config';
import {makeAttachmentCdnUrl} from '~/channel/services/message/MessageHelpers';
import {Logger} from '~/Logger';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';
import {resolveSessionClientInfo} from '~/utils/UserAgentUtils';
import {appendAssetToArchive, buildHashedAssetKey, getAnimatedAssetExtension} from '../utils/AssetArchiveHelpers';
import {getWorkerDependencies} from '../WorkerContext';

interface HarvestUserDataPayload {
	userId: string;
	harvestId: string;
	adminRequestedBy?: string;
}

interface HarvestedMessage {
	id: string;
	timestamp: string;
	content: string;
	attachments: Array<string>;
}

interface ChannelHarvestResult {
	channelId: string;
	messageData: HarvestedMessage;
}

const MESSAGE_CHUNK_SIZE = 100;
const INITIAL_PROGRESS = 5;
const MESSAGES_PROGRESS_MAX = 55;
const METADATA_PROGRESS = 60;
const ZIP_PROGRESS = 70;
const COMPLETE_PROGRESS = 100;

const ZIP_EXPIRY_DAYS = 7;
const ZIP_EXPIRY_SECONDS = ZIP_EXPIRY_DAYS * 24 * 60 * 60;
const ZIP_EXPIRY_MS = ZIP_EXPIRY_SECONDS * 1000;
const ADMIN_ARCHIVE_EXPIRY_MS = 365 * 24 * 60 * 60 * 1000;

function assertPayload(payload: unknown): asserts payload is HarvestUserDataPayload {
	if (typeof payload !== 'object' || payload === null) {
		throw new Error('invalid payload');
	}
	const data = payload as Record<string, unknown>;
	if (typeof data.userId !== 'string') {
		throw new Error('invalid userId');
	}
	if (typeof data.harvestId !== 'string') {
		throw new Error('invalid harvestId');
	}
	if (data.adminRequestedBy !== undefined && typeof data.adminRequestedBy !== 'string') {
		throw new Error('invalid adminRequestedBy');
	}
}

const harvestUserData: Task = async (payload, helpers) => {
	assertPayload(payload);
	helpers.logger.debug('Processing harvestUserData task', {payload});

	const startTime = Date.now();
	const userId = createUserID(BigInt(payload.userId));
	const harvestId = BigInt(payload.harvestId);
	const userIdString = userId.toString();

	console.log('[harvestUserData] Task started', {
		userId: userId.toString(),
		harvestId: harvestId.toString(),
		startTime: new Date(startTime).toISOString(),
	});

	const {
		channelRepository,
		guildRepository,
		userRepository,
		userHarvestRepository,
		adminArchiveRepository,
		favoriteMemeRepository,
		paymentRepository,
		applicationRepository,
		storageService,
		emailService,
	} = getWorkerDependencies();

	const adminRequestedBy = payload.adminRequestedBy ? BigInt(payload.adminRequestedBy) : null;
	const isAdminArchive = adminRequestedBy !== null;

	const adminArchiveRecord = isAdminArchive
		? await adminArchiveRepository.findBySubjectAndArchiveId('user', userId, harvestId)
		: null;

	if (isAdminArchive && !adminArchiveRecord) {
		throw new Error(`Admin archive ${harvestId.toString()} for user ${userId.toString()} not found`);
	}

	const progressReporter = {
		markAsStarted: () =>
			isAdminArchive
				? adminArchiveRepository.markAsStarted(adminArchiveRecord!)
				: userHarvestRepository.markAsStarted(userId, harvestId),
		updateProgress: (progressPercent: number, progressStep: string) =>
			isAdminArchive
				? adminArchiveRepository.updateProgress(adminArchiveRecord!, progressPercent, progressStep)
				: userHarvestRepository.updateProgress(userId, harvestId, progressPercent, progressStep),
		markAsCompleted: (storageKey: string, fileSize: bigint, expiresAt: Date) =>
			isAdminArchive
				? adminArchiveRepository.markAsCompleted(adminArchiveRecord!, storageKey, fileSize, expiresAt)
				: userHarvestRepository.markAsCompleted(userId, harvestId, storageKey, fileSize, expiresAt),
		markAsFailed: (message: string) =>
			isAdminArchive
				? adminArchiveRepository.markAsFailed(adminArchiveRecord!, message)
				: userHarvestRepository.markAsFailed(userId, harvestId, message),
		shouldSendEmail: !isAdminArchive,
	};

	try {
		await progressReporter.markAsStarted();
		Logger.debug({userId, harvestId}, 'Starting user data harvest');
		console.log('[harvestUserData] Marked as started, elapsed:', Date.now() - startTime, 'ms');

		const channelMessagesMap = new Map<string, Array<HarvestedMessage>>();
		let lastChannelId: ChannelID | undefined;
		let lastMessageId: MessageID | undefined;
		let totalMessages = 0;

		await progressReporter.updateProgress(INITIAL_PROGRESS, 'Harvesting messages');
		console.log('[harvestUserData] Set progress to INITIAL_PROGRESS, elapsed:', Date.now() - startTime, 'ms');

		console.log('[harvestUserData] Starting message collection loop');
		let loopCount = 0;

		while (true) {
			const loopStartTime = Date.now();
			loopCount++;

			const messageRefs = await channelRepository.listMessagesByAuthor(
				userId,
				MESSAGE_CHUNK_SIZE,
				lastChannelId,
				lastMessageId,
			);

			console.log('[harvestUserData] Message batch retrieved', {
				loopCount,
				batchSize: messageRefs.length,
				loopElapsed: Date.now() - loopStartTime,
				totalElapsed: Date.now() - startTime,
			});

			if (messageRefs.length === 0) {
				break;
			}

			const messagePromises = messageRefs.map(async ({channelId, messageId}): Promise<ChannelHarvestResult | null> => {
				const message = await channelRepository.getMessage(channelId, messageId);
				if (!message) {
					return null;
				}

				const timestamp = new Date(SnowflakeUtils.extractTimestamp(messageId));
				const attachments: Array<string> = [];

				if (message.attachments) {
					for (const attachment of message.attachments) {
						const attachmentUrl = makeAttachmentCdnUrl(channelId, attachment.id, attachment.filename);
						attachments.push(attachmentUrl);
					}
				}

				return {
					channelId: channelId.toString(),
					messageData: {
						id: messageId.toString(),
						timestamp: timestamp.toISOString(),
						content: message.content ?? '',
						attachments,
					},
				};
			});

			const messages = (await Promise.all(messagePromises)).filter((m): m is ChannelHarvestResult => m !== null);

			for (const {channelId, messageData} of messages) {
				if (!channelMessagesMap.has(channelId)) {
					channelMessagesMap.set(channelId, []);
				}
				channelMessagesMap.get(channelId)!.push(messageData);
			}

			totalMessages += messages.length;
			lastChannelId = messageRefs[messageRefs.length - 1].channelId;
			lastMessageId = messageRefs[messageRefs.length - 1].messageId;

			if (totalMessages % 1000 === 0) {
				const progress = Math.min(INITIAL_PROGRESS + Math.floor((totalMessages / 10000) * 50), MESSAGES_PROGRESS_MAX);
				await progressReporter.updateProgress(progress, `Harvested ${totalMessages} messages`);
			}
		}

		Logger.debug({userId, harvestId, channelCount: channelMessagesMap.size, totalMessages}, 'Harvested all messages');
		console.log('[harvestUserData] Message collection complete', {
			channelCount: channelMessagesMap.size,
			totalMessages,
			elapsed: Date.now() - startTime,
		});

		await progressReporter.updateProgress(METADATA_PROGRESS, 'Collecting user metadata');
		console.log('[harvestUserData] Starting metadata collection, elapsed:', Date.now() - startTime, 'ms');

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
			betaCodes,
			createdGiftCodes,
			payments,
			oauthClients,
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
			userRepository.listBetaCodes(userId),
			userRepository.findGiftCodesByCreator(userId),
			paymentRepository.findPaymentsByUserId(userId),
			applicationRepository.listApplicationsByOwner(userId),
			userRepository.getPinnedDmsWithDetails(userId),
			userRepository.getAuthorizedIps(userId),
			userRepository.getActivityTracking(userId),
		]);

		const guildMemberships = await Promise.all(
			guildIds.map(async (guildId) => {
				const member = await guildRepository.getMember(guildId, userId);
				const guild = await guildRepository.findUnique(guildId);
				return {member, guild, guildId};
			}),
		);

		const guildSettings = await Promise.all(
			guildIds.map((guildId) => userRepository.findGuildSettings(userId, guildId)),
		);

		const userData = {
			user: {
				id: user.id.toString(),
				username: user.username,
				discriminator: user.discriminator,
				bot: user.isBot,
				system: user.isSystem,
				email: user.email,
				email_verified: user.emailVerified,
				email_bounced: user.emailBounced,
				phone: user.phone,
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
				created_at: new Date(SnowflakeUtils.extractTimestamp(user.id)).toISOString(),
				mfa_enabled: user.authenticatorTypes.size > 0,
				authenticator_types: Array.from(user.authenticatorTypes),
			},
			auth_sessions: authSessions.map((session) => {
				const {clientOs, clientPlatform} = resolveSessionClientInfo({
					userAgent: session.clientUserAgent,
					isDesktopClient: session.clientIsDesktop,
				});
				return {
					created_at: session.createdAt.toISOString(),
					approx_last_used_at: session.approximateLastUsedAt?.toISOString() ?? null,
					client_ip: session.clientIp,
					client_os: clientOs,
					client_user_agent: session.clientUserAgent,
					client_platform: clientPlatform,
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
						restricted_guilds: Array.from(userSettings.restrictedGuilds).map((id) => id.toString()),
						guild_positions: userSettings.guildPositions.map((id) => id.toString()),
						guild_folders: userSettings.guildFolders,
						afk_timeout: userSettings.afkTimeout,
						time_format: userSettings.timeFormat,
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
			beta_codes: betaCodes.map((code) => ({
				code: code.code,
				created_at: code.createdAt.toISOString(),
				redeemer_id: code.redeemerId?.toString() ?? null,
				redeemed_at: code.redeemedAt?.toISOString() ?? null,
			})),
			gift_codes_created: createdGiftCodes.map((gift) => ({
				code: gift.code,
				duration_months: gift.durationMonths,
				created_at: gift.createdAt.toISOString(),
				redeemed_by_user_id: gift.redeemedByUserId?.toString() ?? null,
				redeemed_at: gift.redeemedAt?.toISOString() ?? null,
				stripe_payment_intent_id: gift.stripePaymentIntentId,
			})),
			payments: payments.map((payment) => ({
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
			})),
			oauth_applications: oauthClients.map((app) => ({
				application_id: app.applicationId.toString(),
				name: app.name,
				redirect_uris: Array.from(app.oauth2RedirectUris),
			})),
			pinned_dms: pinnedDms.map((pin) => ({
				channel_id: pin.channel_id.toString(),
				sort_order: pin.sort_order,
			})),
			authorized_ips: authorizedIps,
			activity_tracking: {
				last_active_at: activityData.last_active_at?.toISOString() ?? null,
				last_active_ip: activityData.last_active_ip,
			},
		};

		const userJsonBuffer = Buffer.from(JSON.stringify(userData, null, 2), 'utf-8');

		Logger.debug({userId, harvestId}, 'Collected user metadata');
		console.log('[harvestUserData] Metadata collection complete, elapsed:', Date.now() - startTime, 'ms');

		await progressReporter.updateProgress(METADATA_PROGRESS + 5, 'Downloading media assets');
		await progressReporter.updateProgress(ZIP_PROGRESS, 'Creating ZIP archive');
		console.log('[harvestUserData] Starting ZIP creation, elapsed:', Date.now() - startTime, 'ms');

		const tempDir = await fs.promises.mkdtemp(path.join(os.tmpdir(), 'fluxer-harvest-'));
		const zipPath = path.join(tempDir, `user-data-${userId}.zip`);

		try {
			console.log('[harvestUserData] Creating ZIP file', {zipPath, elapsed: Date.now() - startTime});

			const output = fs.createWriteStream(zipPath);
			const archive = archiver('zip', {zlib: {level: 9}});

			archive.pipe(output);

			archive.append(userJsonBuffer, {name: 'user.json'});
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
			console.log('[harvestUserData] Appended user.json, elapsed:', Date.now() - startTime, 'ms');

			for (const [channelId, messages] of channelMessagesMap.entries()) {
				messages.sort((a, b) => a.timestamp.localeCompare(b.timestamp));
				const messagesJson = JSON.stringify(messages, null, 2);
				archive.append(messagesJson, {
					name: `channels/${channelId}/messages.json`,
				});
			}

			const paymentData = payments.map((payment) => ({
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
			}));
			archive.append(JSON.stringify(paymentData, null, 2), {
				name: 'payments/payment_history.json',
			});

			const oauthData = {
				applications: oauthClients.map((app) => ({
					application_id: app.applicationId.toString(),
					name: app.name,
					redirect_uris: Array.from(app.oauth2RedirectUris),
				})),
			};
			archive.append(JSON.stringify(oauthData, null, 2), {
				name: 'integrations/oauth.json',
			});

			const securityData = {
				authorized_ips: authorizedIps,
				activity_tracking: {
					last_active_at: activityData.last_active_at?.toISOString() ?? null,
					last_active_ip: activityData.last_active_ip,
				},
			};
			archive.append(JSON.stringify(securityData, null, 2), {
				name: 'account/security.json',
			});

			console.log('[harvestUserData] Finalizing archive, elapsed:', Date.now() - startTime, 'ms');
			await archive.finalize();

			await new Promise<void>((resolve, reject) => {
				output.on('close', resolve);
				output.on('error', reject);
			});

			console.log('[harvestUserData] Archive finalized, reading from disk, elapsed:', Date.now() - startTime, 'ms');
			const zipBuffer = await fs.promises.readFile(zipPath);

			console.log(
				'[harvestUserData] ZIP file read, size:',
				zipBuffer.length,
				'bytes, elapsed:',
				Date.now() - startTime,
				'ms',
			);

			const finalKey = `exports/${userId}/${harvestId}/user-data.zip`;
			console.log('[harvestUserData] Starting S3 upload, elapsed:', Date.now() - startTime, 'ms');

			const expiresAt = new Date(Date.now() + (isAdminArchive ? ADMIN_ARCHIVE_EXPIRY_MS : ZIP_EXPIRY_MS));

			await storageService.uploadObject({
				bucket: Config.s3.buckets.harvests,
				key: finalKey,
				body: zipBuffer,
				contentType: 'application/zip',
				expiresAt: expiresAt,
			});

			Logger.debug({userId, harvestId, zipSize: zipBuffer.length}, 'Uploaded final ZIP to S3 with TTL');
			console.log('[harvestUserData] S3 upload complete, elapsed:', Date.now() - startTime, 'ms');
			const downloadUrl = await storageService.getPresignedDownloadURL({
				bucket: Config.s3.buckets.harvests,
				key: finalKey,
				expiresIn: ZIP_EXPIRY_SECONDS,
			});

			await progressReporter.markAsCompleted(finalKey, BigInt(zipBuffer.length), expiresAt);

			Logger.debug({userId, harvestId}, 'Marked harvest as completed');

			if (progressReporter.shouldSendEmail && user.email && Config.email.enabled) {
				await emailService.sendHarvestCompletedEmail(
					user.email,
					user.username,
					downloadUrl,
					totalMessages,
					zipBuffer.length,
					expiresAt,
					user.locale,
				);

				Logger.debug({userId, harvestId, email: user.email, totalMessages}, 'Sent harvest completion email');
			}

			await progressReporter.updateProgress(COMPLETE_PROGRESS, 'Completed');

			Logger.debug({userId, harvestId}, 'User data harvest completed successfully');
			console.log('[harvestUserData] Task completed successfully', {
				userId: userId.toString(),
				harvestId: harvestId.toString(),
				totalElapsed: Date.now() - startTime,
				totalElapsedSeconds: Math.round((Date.now() - startTime) / 1000),
			});
		} finally {
			await fs.promises.rm(tempDir, {recursive: true, force: true});
		}
	} catch (error) {
		Logger.error({error, userId, harvestId}, 'Failed to harvest user data');
		console.error('[harvestUserData] Task failed', {
			userId: userId.toString(),
			harvestId: harvestId.toString(),
			elapsed: Date.now() - startTime,
			error: error instanceof Error ? error.message : String(error),
		});
		await progressReporter.markAsFailed(String(error));
		throw error;
	}
};

export default harvestUserData;
