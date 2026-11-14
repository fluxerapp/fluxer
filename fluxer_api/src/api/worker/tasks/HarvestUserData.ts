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
import {makeDataPackageAttachmentCdnUrl} from '@app/api/attachment/AttachmentUrls';
import {findRecoveryKitCreatedAt} from '@app/api/auth/AuthRecoveryKit';
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
import type {ThreadRepository} from '@app/api/channel/repositories/ThreadRepository';
import {
	isChannelEligible,
	isTimestampInWindow,
	loadSelfMessageEligibilityContext,
	type SelfMessageEligibilityContext,
	type SelfMessageFilter,
} from '@app/api/channel/services/message/SelfMessageFilter';
import type {OperatorPassCertificateRow, OperatorPassUnitRow} from '@app/api/database/types/OperatorPassTypes';
import {isTainted} from '@app/api/experiment/ChannelThreadsGate';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {Logger} from '@app/api/Logger';
import {getOperatorPassRepository} from '@app/api/middleware/ServiceSingletons';
import type {Application} from '@app/api/models/Application';
import type {Attachment} from '@app/api/models/Attachment';
import type {Channel} from '@app/api/models/Channel';
import type {Payment} from '@app/api/models/Payment';
import type {ThreadMember} from '@app/api/models/ThreadMember';
import type {User} from '@app/api/models/User';
import {getOperatorPassVisionaryPassId} from '@app/api/operator_pass/OperatorPassVisionaryService';
import {listAppAttestKeysForExport} from '@app/api/user/AgeRange';
import {buildHarvestDownloadUrl} from '@app/api/user/services/HarvestDownloadUrl';
import {mapWithConcurrency} from '@app/api/utils/ConcurrencyUtils';
import {writeZipArchive} from '@app/api/worker/utils/ArchiveFile';
import {createArchiveJsonBuffer} from '@app/api/worker/utils/ArchiveJson';
import {
	appendAssetToArchive,
	buildHashedAssetKey,
	getAnimatedAssetExtension,
} from '@app/api/worker/utils/AssetArchiveHelpers';
import {ContentAddressedAttachmentCollector} from '@app/api/worker/utils/ContentAddressedAttachmentCollector';
import {
	buildUserDataJson,
	getOperatorPassCertificateArchivePath,
	mapOAuthApplication,
	mapPayment,
	mapSecurityData,
} from '@app/api/worker/utils/HarvestDataMappers';
import {deserializeSelfMessageFilter, SelfMessageFilterPayload} from '@app/api/worker/utils/SelfMessageFilterPayload';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerDependencies} from '@app/api/worker/WorkerDependencies';

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

interface HarvestMessageResult {
	channelMessagesMap: Map<string, Array<HarvestedMessage>>;
	totalMessages: number;
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
	operatorPassCertificates: Array<OperatorPassCertificateRow>;
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

const HARVEST_MESSAGE_CHUNK_SIZE = 1000;
const HARVEST_READ_CONCURRENCY = 10;
const INITIAL_PROGRESS = 5;
const MESSAGES_PROGRESS_MAX = 55;
const METADATA_PROGRESS = 60;
const ZIP_EXPIRY_MS = ms('7 days');

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

async function harvestMessages(
	channelRepository: HarvestMessageRepository,
	userId: UserID,
	startTime: number,
	filterArgs: HarvestMessagesFilterArgs | null,
): Promise<HarvestMessageResult> {
	const channelMessagesMap = new Map<string, Array<HarvestedMessage>>();
	const channelEligibility = new Map<string, boolean>();
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
					cdn_url: makeDataPackageAttachmentCdnUrl(channelId, attachment.id, attachment.filename),
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
			let eligible = channelEligibility.get(channelIdStr);
			if (eligible === undefined) {
				const channel = await filterArgs.findChannel(ref.channelId);
				eligible = channel ? isChannelEligible(channel, userId, filterArgs.filter, filterArgs.context) : false;
				channelEligibility.set(channelIdStr, eligible);
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

async function collectThreadMemberships(
	threads: Pick<ThreadRepository, 'listJoinedThreadIds' | 'getMember'>,
	userId: UserID,
	guildIds: ReadonlyArray<GuildID>,
): Promise<Array<ThreadMember>> {
	const perGuild = await mapWithConcurrency(guildIds, HARVEST_READ_CONCURRENCY, async (guildId) => {
		if (!(await isTainted(guildId, {fresh: true}))) return [];
		const threadIds = await threads.listJoinedThreadIds(userId, guildId);
		const members = await Promise.all(threadIds.map((threadId) => threads.getMember(threadId, userId)));
		return members.filter((member): member is ThreadMember => member !== null);
	});
	return perGuild.flat();
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
		operatorPassCertificates,
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
		for (const certificate of operatorPassCertificates) {
			if (certificate.pdf.length > 0) {
				await archive.append(Buffer.from(certificate.pdf), {name: getOperatorPassCertificateArchivePath(certificate)});
			}
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
		storeEntitlementService,
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
			const context = await loadSelfMessageEligibilityContext(userId, {userRepository, guildRepository});
			filterArgs = {
				filter,
				context,
				findChannel: (channelId) => channelRepository.channelData.findUnique(channelId),
			};
		}
		Logger.debug({userId, harvestId, elapsed: Date.now() - startTime}, 'Starting user data harvest');
		await progressReporter.updateProgress(INITIAL_PROGRESS, 'Harvesting messages');
		Logger.debug({elapsed: Date.now() - startTime}, 'Set progress to INITIAL_PROGRESS');
		const {channelMessagesMap, totalMessages} = await harvestMessages(
			channelRepository.messages,
			userId,
			startTime,
			filterArgs,
		);
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
			recoveryKitCreatedAt,
			appAttestKeys,
			createdGiftCodes,
			payments,
			storePurchases,
			operatorPass,
			operatorPassPurchases,
			operatorPassWaitlistEntry,
			operatorPassWall,
			operatorPassWallEvents,
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
			findRecoveryKitCreatedAt(userId),
			listAppAttestKeysForExport(userId),
			userRepository.findGiftCodesByCreator(userId),
			paymentRepository.findPaymentsByUserId(userId),
			storeEntitlementService.listStorePurchases(userId),
			getOperatorPassRepository().findOperatorPass(userId),
			getOperatorPassRepository().listPurchasesByPurchaser(userId),
			user.email ? getOperatorPassRepository().findWaitlistEntryByEmail(user.email.trim().toLowerCase()) : null,
			getOperatorPassRepository().findWallEntry(userId),
			getOperatorPassRepository().listWallEvents(userId),
			applicationRepository.listApplicationsByOwner(userId),
			connectionRepository.findByUserId(userId),
			userRepository.getPinnedDmsWithDetails(userId),
			userRepository.getAuthorizedIps(userId),
			userRepository.getActivityTracking(userId),
		]);
		const operatorPassRepository = getOperatorPassRepository();
		const operatorPassUnitRows = new Map<string, OperatorPassUnitRow>();
		for (const unit of await operatorPassRepository.listUnitsByHolder(userId)) {
			operatorPassUnitRows.set(unit.pass_id, unit);
		}
		for (const purchase of operatorPassPurchases) {
			for (const unit of await operatorPassRepository.listUnitsByPurchase(purchase.checkout_session_id)) {
				operatorPassUnitRows.set(unit.pass_id, unit);
			}
		}
		const operatorPassUnits = await Promise.all(
			[...operatorPassUnitRows.values()].map(async (unit) => ({
				unit,
				events: await operatorPassRepository.listUnitEvents(unit.pass_id),
			})),
		);
		const operatorPassOffers = await operatorPassRepository.listOffersByUser(userId);
		const operatorPassCertificates: Array<OperatorPassCertificateRow> = [];
		for (const purchase of operatorPassPurchases) {
			for (const page of await operatorPassRepository.listCertificatesByPurchase(purchase.checkout_session_id)) {
				operatorPassCertificates.push(...(await operatorPassRepository.listCertificateVersions(page.pass_id)));
			}
		}
		const operatorPassVisionaryActivation = await operatorPassRepository.findVisionaryActivation(userId);
		const operatorPassLabs = await operatorPassRepository.findLabsMember(userId);
		operatorPassCertificates.push(
			...(await operatorPassRepository.listCertificateVersions(getOperatorPassVisionaryPassId(userId))),
		);
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
		const threadMemberships = await collectThreadMemberships(channelRepository.threads, userId, guildIds);
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
			threadMemberships,
			savedMessages,
			privateChannels,
			favoriteMemes,
			pushSubscriptions,
			webAuthnCredentials,
			mfaBackupCodes,
			recoveryKitCreatedAt,
			appAttestKeys,
			createdGiftCodes,
			payments,
			storePurchases,
			operatorPass,
			operatorPassPurchases,
			operatorPassWaitlistEntry,
			operatorPassWall,
			operatorPassWallEvents,
			operatorPassUnits,
			operatorPassOffers,
			operatorPassCertificates,
			operatorPassVisionaryActivation,
			operatorPassLabs,
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
			operatorPassCertificates,
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
		const sent = await emailService.send(
			'harvest_completed',
			user.email,
			{
				downloadUrl: archive.downloadUrl,
				totalMessages,
				fileSize: Number(archive.fileSize),
				expiresAt: archive.expiresAt,
			},
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
