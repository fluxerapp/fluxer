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

import type {Context} from 'hono';
import {Redis} from 'ioredis';
import type {HonoApp, HonoEnv} from '~/App';
import {AttachmentDecayRepository} from '~/attachment/AttachmentDecayRepository';
import {AttachmentDecayService} from '~/attachment/AttachmentDecayService';
import type {IpAuthorizationTicketCache} from '~/auth/services/AuthLoginService';
import {
	createAttachmentID,
	createChannelID,
	createGuildID,
	createMessageID,
	createUserID,
	type MessageID,
	type UserID,
} from '~/BrandedTypes';
import {Config} from '~/Config';
import {ChannelTypes, SuspiciousActivityFlags, UserFlags} from '~/Constants';
import {mapMessageToResponse} from '~/channel/ChannelModel';
import {ChannelRepository} from '~/channel/ChannelRepository';
import {BatchBuilder, defineTable, deleteOneOrMany, fetchMany, fetchOne} from '~/database/Cassandra';
import type {
	ChannelEmptyBucketRow,
	ChannelMessageBucketRow,
	ChannelRow,
	ChannelStateRow,
	MessageRow,
} from '~/database/CassandraTypes';
import {
	CHANNEL_EMPTY_BUCKET_COLUMNS,
	CHANNEL_MESSAGE_BUCKET_COLUMNS,
	CHANNEL_STATE_COLUMNS,
	MESSAGE_COLUMNS,
} from '~/database/CassandraTypes';
import {
	AclsMustBeNonEmptyError,
	DeletionFailedError,
	EmailServiceNotTestableError,
	InputValidationError,
	InvalidAclsFormatError,
	InvalidBotFlagError,
	InvalidFlagsFormatError,
	InvalidSuspiciousFlagsFormatError,
	InvalidSystemFlagError,
	InvalidTimestampError,
	NoPendingDeletionError,
	ProcessingFailedError,
	TestHarnessDisabledError,
	TestHarnessForbiddenError,
	UnknownGuildError,
	UnknownGuildMemberError,
	UnknownHarvestError,
	UnknownMessageError,
	UnknownSuspiciousFlagError,
	UnknownUserError,
	UnknownUserFlagError,
	UpdateFailedError,
} from '~/Errors';
import {GuildRepository} from '~/guild/repositories/GuildRepository';
import type {IEmailService, ITestEmailService, SentEmailRecord} from '~/infrastructure/IEmailService';
import {RedisAccountDeletionQueueService} from '~/infrastructure/RedisAccountDeletionQueueService';
import {RedisActivityTracker} from '~/infrastructure/RedisActivityTracker';
import {SnowflakeService} from '~/infrastructure/SnowflakeService';
import {StorageService} from '~/infrastructure/StorageService';
import {Logger} from '~/Logger';
import {OAuth2AccessTokensByUser} from '~/Tables';
import {AuthSessionRepository} from '~/user/repositories/auth/AuthSessionRepository';
import {IpAuthorizationTokens} from '~/user/repositories/auth/IpAuthorizationRepository';
import {ScheduledMessageRepository} from '~/user/repositories/ScheduledMessageRepository';
import {UserChannelRepository} from '~/user/repositories/UserChannelRepository';
import {processUserDeletion} from '~/user/services/UserDeletionService';
import {UserHarvestRepository} from '~/user/UserHarvestRepository';
import {UserRepository} from '~/user/UserRepository';
import {getExpiryBucket} from '~/utils/AttachmentDecay';
import * as BucketUtils from '~/utils/BucketUtils';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';
import {ScheduledMessageExecutor} from '~/worker/executors/ScheduledMessageExecutor';
import {processExpiredAttachments} from '~/worker/tasks/expireAttachments';
import {processInactivityDeletionsCore} from '~/worker/tasks/processInactivityDeletions';
import {setWorkerDependencies} from '~/worker/WorkerContext';
import {initializeWorkerDependencies} from '~/worker/WorkerDependencies';

const TEST_EMAIL_ENDPOINT = '/test/emails';
const TEST_AUTH_HEADER = 'x-test-token';
const MAX_TEST_PRIVATE_CHANNELS = 1000;

const Messages = defineTable<MessageRow, 'channel_id' | 'bucket' | 'message_id'>({
	name: 'messages',
	columns: MESSAGE_COLUMNS,
	primaryKey: ['channel_id', 'bucket', 'message_id'],
	partitionKey: ['channel_id', 'bucket'],
});

const ChannelState = defineTable<ChannelStateRow, 'channel_id'>({
	name: 'channel_state',
	columns: CHANNEL_STATE_COLUMNS,
	primaryKey: ['channel_id'],
});

const ChannelMessageBuckets = defineTable<ChannelMessageBucketRow, 'channel_id' | 'bucket', 'channel_id'>({
	name: 'channel_message_buckets',
	columns: CHANNEL_MESSAGE_BUCKET_COLUMNS,
	primaryKey: ['channel_id', 'bucket'],
	partitionKey: ['channel_id'],
});

const ChannelEmptyBuckets = defineTable<ChannelEmptyBucketRow, 'channel_id' | 'bucket', 'channel_id'>({
	name: 'channel_empty_buckets',
	columns: CHANNEL_EMPTY_BUCKET_COLUMNS,
	primaryKey: ['channel_id', 'bucket'],
	partitionKey: ['channel_id'],
});

const FETCH_CHANNEL_STATE = ChannelState.select({
	where: ChannelState.where.eq('channel_id'),
	limit: 1,
});

const FETCH_CHANNEL_BUCKETS = ChannelMessageBuckets.select({
	columns: ['bucket', 'updated_at'],
	where: ChannelMessageBuckets.where.eq('channel_id'),
	orderBy: {col: 'bucket', direction: 'DESC'},
});

const FETCH_CHANNEL_EMPTY_BUCKETS = ChannelEmptyBuckets.select({
	columns: ['bucket', 'updated_at'],
	where: ChannelEmptyBuckets.where.eq('channel_id'),
	orderBy: {col: 'bucket', direction: 'DESC'},
});

const ensureHarnessAccess = (ctx: Context<HonoEnv>) => {
	if (!Config.dev.testModeEnabled) {
		throw new TestHarnessDisabledError();
	}

	if (Config.dev.testHarnessToken) {
		const headerValue = ctx.req.header(TEST_AUTH_HEADER) || ctx.req.header('authorization') || '';
		const bearer = headerValue.startsWith('Bearer ') ? headerValue.slice('Bearer '.length) : headerValue;
		if (bearer !== Config.dev.testHarnessToken) {
			throw new TestHarnessForbiddenError();
		}
	}
};

const isTestEmailService = (service: IEmailService): service is ITestEmailService => {
	return typeof (service as ITestEmailService).listSentEmails === 'function';
};

const serializeEmails = (emails: Array<SentEmailRecord>) =>
	emails.map((email) => ({
		...email,
		timestamp: email.timestamp.toISOString(),
	}));

const initializeWorkerDepsWithHarnessEmail = async (ctx: Context<HonoEnv>, snowflakeService: SnowflakeService) => {
	const workerDeps = await initializeWorkerDependencies(snowflakeService);
	const harnessEmailService = ctx.get('emailService');

	if (isTestEmailService(harnessEmailService) && harnessEmailService !== workerDeps.emailService) {
		workerDeps.emailService = harnessEmailService;
	}

	return workerDeps;
};

const userFlagEntries = Object.entries(UserFlags);
const suspiciousFlagEntries = Object.entries(SuspiciousActivityFlags);

const parseUserFlagNames = (names: Array<string>): bigint | null => {
	let mask = 0n;
	for (const name of names) {
		const entry = userFlagEntries.find(([flagName]) => flagName === name);
		if (!entry) {
			return null;
		}
		mask |= entry[1];
	}
	return mask;
};

const parseSuspiciousFlagNames = (names: Array<string>): number | null => {
	let mask = 0;
	for (const name of names) {
		const entry = suspiciousFlagEntries.find(([flagName]) => flagName === name);
		if (!entry) {
			return null;
		}
		mask |= entry[1];
	}
	return mask;
};

export const TestHarnessController = (app: HonoApp) => {
	app.get(TEST_EMAIL_ENDPOINT, (ctx) => {
		ensureHarnessAccess(ctx);

		const emailService = ctx.get('emailService');
		if (!isTestEmailService(emailService)) {
			throw new EmailServiceNotTestableError();
		}

		const emails = emailService.listSentEmails();
		const filter = ctx.req.query('recipient');

		let filtered = emails;
		if (filter) {
			filtered = emails.filter((email) => email.to === filter);
		}

		return ctx.json({emails: serializeEmails(filtered), count: filtered.length});
	});

	app.delete(TEST_EMAIL_ENDPOINT, (ctx) => {
		ensureHarnessAccess(ctx);

		const emailService = ctx.get('emailService');
		if (!isTestEmailService(emailService)) {
			throw new EmailServiceNotTestableError();
		}

		emailService.clearSentEmails();
		return ctx.json({cleared: true});
	});

	app.patch('/test/users/:userId/flags', async (ctx) => {
		ensureHarnessAccess(ctx);

		const userId = createUserID(BigInt(ctx.req.param('userId')));
		const body = await ctx.req.json();
		const {flags} = body;

		if (typeof flags !== 'string' && typeof flags !== 'number') {
			throw new InvalidFlagsFormatError();
		}

		const userRepository = ctx.get('userRepository');
		await userRepository.patchUpsert(userId, {flags: BigInt(flags)});

		return ctx.json({success: true});
	});

	app.post('/test/users/:userId/acls', async (ctx) => {
		ensureHarnessAccess(ctx);

		const userId = createUserID(BigInt(ctx.req.param('userId')));
		const body = await ctx.req.json();
		const {acls} = body;

		if (acls !== null && !Array.isArray(acls)) {
			throw new InvalidAclsFormatError();
		}

		let normalized: Set<string> | null = null;
		if (Array.isArray(acls)) {
			normalized = new Set<string>();
			for (const acl of acls) {
				if (typeof acl !== 'string' || !acl.trim()) {
					throw new AclsMustBeNonEmptyError();
				}
				normalized.add(acl);
			}
		}

		const userRepository = ctx.get('userRepository');
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		await userRepository.patchUpsert(userId, {acls: normalized});

		const userCacheService = ctx.get('userCacheService');
		await userCacheService.invalidateUserCache(userId);

		return ctx.json({success: true, count: normalized?.size ?? 0});
	});

	app.post('/test/users/:userId/security-flags', async (ctx) => {
		ensureHarnessAccess(ctx);

		const userId = createUserID(BigInt(ctx.req.param('userId')));
		const body = await ctx.req.json();
		const {
			set_flags: setFlags,
			clear_flags: clearFlags,
			suspicious_activity_flags: suspiciousFlagsValue,
			suspicious_activity_flag_names: suspiciousFlagNames,
			pending_manual_verification: pendingManualVerification,
		} = body as {
			set_flags?: Array<string>;
			clear_flags?: Array<string>;
			suspicious_activity_flags?: number | null;
			suspicious_activity_flag_names?: Array<string>;
			pending_manual_verification?: boolean | null;
		};

		const userRepository = ctx.get('userRepository');
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		let nextFlags = user.flags ?? 0n;
		const pendingUpdates: Record<string, unknown> = {};

		const applyFlagList = (names: Array<string> | undefined, op: 'set' | 'clear') => {
			if (!names) return true;
			const mask = parseUserFlagNames(names);
			if (mask === null) {
				return false;
			}
			nextFlags = op === 'set' ? nextFlags | mask : nextFlags & ~mask;
			return true;
		};

		if (!applyFlagList(setFlags, 'set') || !applyFlagList(clearFlags, 'clear')) {
			throw new UnknownUserFlagError();
		}

		if (typeof pendingManualVerification === 'boolean') {
			nextFlags = pendingManualVerification
				? nextFlags | UserFlags.PENDING_MANUAL_VERIFICATION
				: nextFlags & ~UserFlags.PENDING_MANUAL_VERIFICATION;
		}

		if (setFlags || clearFlags || typeof pendingManualVerification === 'boolean') {
			pendingUpdates.flags = nextFlags;
		}

		let suspiciousValue: number | null | undefined = suspiciousFlagsValue;
		if (Array.isArray(suspiciousFlagNames)) {
			const parsed = parseSuspiciousFlagNames(suspiciousFlagNames);
			if (parsed === null) {
				throw new UnknownSuspiciousFlagError();
			}
			suspiciousValue = parsed;
		}

		if (suspiciousValue !== undefined) {
			if (suspiciousValue !== null && typeof suspiciousValue !== 'number') {
				throw new InvalidSuspiciousFlagsFormatError();
			}
			pendingUpdates.suspicious_activity_flags = suspiciousValue;
		}

		if (Object.keys(pendingUpdates).length === 0) {
			return ctx.json({success: true, updated: false});
		}

		await userRepository.patchUpsert(userId, pendingUpdates);

		return ctx.json({
			success: true,
			updated: true,
			flags: (pendingUpdates.flags as bigint | undefined)?.toString() ?? user.flags?.toString(),
			suspicious_activity_flags: pendingUpdates.suspicious_activity_flags ?? user.suspiciousActivityFlags ?? null,
		});
	});

	app.post('/test/users/:userId/unclaim', async (ctx) => {
		ensureHarnessAccess(ctx);

		const userId = createUserID(BigInt(ctx.req.param('userId')));
		const userRepository = ctx.get('userRepository');
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		await userRepository.patchUpsert(userId, {
			email: null,
			email_verified: false,
			email_bounced: false,
			password_hash: null,
			password_last_changed_at: null,
			authenticator_types: null,
			totp_secret: null,
		});

		return ctx.json({success: true});
	});

	app.post('/test/users/:userId/set-pending-deletion', async (ctx) => {
		ensureHarnessAccess(ctx);

		const userId = createUserID(BigInt(ctx.req.param('userId')));
		const body = await ctx.req.json();
		const {pending_deletion_at: pendingDeletionAt, set_self_deleted_flag: setSelfDeletedFlag} = body as {
			pending_deletion_at?: string;
			set_self_deleted_flag?: boolean;
		};

		const userRepository = new UserRepository();
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		let date: Date;
		try {
			date = pendingDeletionAt ? new Date(pendingDeletionAt) : new Date();
			if (Number.isNaN(date.getTime())) {
				throw new InvalidTimestampError();
			}
		} catch {
			throw new InvalidTimestampError();
		}

		const updates: Record<string, unknown> = {pending_deletion_at: date};
		const shouldSetSelfDeleted = setSelfDeletedFlag !== false;
		if (shouldSetSelfDeleted) {
			const nextFlags = (user.flags ?? 0n) | UserFlags.SELF_DELETED;
			updates.flags = nextFlags;
		}

		const updated = await userRepository.patchUpsert(userId, updates);

		const redis = new Redis(Config.redis.url);
		const redisDeletionQueue = new RedisAccountDeletionQueueService(redis, userRepository);
		try {
			await redisDeletionQueue.scheduleDeletion(userId, date, user.deletionReasonCode ?? 0);
		} finally {
			await redis.quit();
		}

		return ctx.json(
			{
				success: true,
				pending_deletion_at: date.toISOString(),
				flags:
					(updates.flags as bigint | undefined)?.toString() ?? updated?.flags?.toString() ?? user.flags?.toString(),
			},
			200,
		);
	});

	app.post('/test/auth/ip-authorization', async (ctx) => {
		ensureHarnessAccess(ctx);

		const cacheService = ctx.get('cacheService');
		const body = await ctx.req.json();
		const {
			ticket,
			token,
			user_id: userId,
			email,
			username,
			client_ip: clientIp,
			user_agent: userAgent,
			client_location: clientLocation,
			platform,
			resend_used: resendUsed,
			invite_code: inviteCode,
			created_at: createdAtInput,
			ttl_seconds: ttlSeconds,
		} = body as Record<string, unknown>;

		if (!ticket || !token || !userId || !email || !username || !clientIp || !userAgent || !clientLocation) {
			return ctx.json({error: 'missing required fields'}, 400);
		}

		let createdAt = Date.now();
		if (typeof createdAtInput === 'number') {
			createdAt = createdAtInput;
		} else if (typeof createdAtInput === 'string') {
			const parsed = new Date(createdAtInput);
			if (!Number.isNaN(parsed.getTime())) {
				createdAt = parsed.getTime();
			}
		}

		const payload: IpAuthorizationTicketCache = {
			userId: String(userId),
			email: String(email),
			username: String(username),
			clientIp: String(clientIp),
			userAgent: String(userAgent),
			platform: platform ? String(platform) : null,
			authToken: String(token),
			clientLocation: String(clientLocation),
			inviteCode: inviteCode ? String(inviteCode) : null,
			resendUsed: Boolean(resendUsed),
			createdAt,
		};

		const ttl = typeof ttlSeconds === 'number' && ttlSeconds > 0 ? ttlSeconds : 60 * 15;

		await cacheService.set(`ip-auth-ticket:${ticket}`, payload, ttl);
		await cacheService.set(`ip-auth-token:${token}`, {ticket: String(ticket)}, ttl);

		return ctx.json(
			{
				success: true,
				ticket: String(ticket),
				token: String(token),
				ttl_seconds: ttl,
			},
			200,
		);
	});

	app.post('/test/auth/ip-authorization/publish', async (ctx) => {
		ensureHarnessAccess(ctx);

		const cacheService = ctx.get('cacheService');
		const body = await ctx.req.json();
		const {ticket, token, user_id: userId} = body as {ticket?: string; token?: string; user_id?: string};

		if (!ticket || !token || !userId) {
			return ctx.json({error: 'ticket, token, and user_id are required'}, 400);
		}

		const payload = JSON.stringify({token, user_id: userId});
		await cacheService.publish(`ip-auth:${ticket}`, payload);

		return ctx.json({success: true});
	});

	app.post('/test/auth/ip-authorization/expire', async (ctx) => {
		ensureHarnessAccess(ctx);

		const cacheService = ctx.get('cacheService');
		const body = await ctx.req.json();
		const {ticket, token} = body as {ticket?: string; token?: string};

		if (!ticket && !token) {
			return ctx.json({error: 'ticket or token is required'}, 400);
		}

		if (ticket) {
			await cacheService.delete(`ip-auth-ticket:${ticket}`);
			await cacheService.delete(`ip-auth:${ticket}`);
		}

		if (token) {
			await cacheService.delete(`ip-auth-token:${token}`);
			await deleteOneOrMany(
				IpAuthorizationTokens.deleteCql({
					where: IpAuthorizationTokens.where.eq('token_'),
				}),
				{token_: token},
			);
		}

		return ctx.json({success: true}, 200);
	});

	app.post('/test/auth/mfa-ticket', async (ctx) => {
		ensureHarnessAccess(ctx);

		const cacheService = ctx.get('cacheService');
		const userRepository = new UserRepository();
		const body = await ctx.req.json();
		const {
			ticket,
			user_id: userIdInput,
			ttl_seconds: ttlSeconds,
		} = body as {
			ticket?: string;
			user_id?: string;
			ttl_seconds?: number;
		};

		if (!ticket || !userIdInput) {
			return ctx.json({error: 'ticket and user_id are required'}, 400);
		}

		const userId = createUserID(BigInt(userIdInput));
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const ttl = typeof ttlSeconds === 'number' && ttlSeconds > 0 ? ttlSeconds : 60 * 5;
		await cacheService.set(`mfa-ticket:${ticket}`, userId.toString(), ttl);

		return ctx.json(
			{
				success: true,
				ticket,
				user_id: userId.toString(),
				ttl_seconds: ttl,
			},
			200,
		);
	});

	app.post('/test/users/:userId/premium', async (ctx) => {
		ensureHarnessAccess(ctx);

		const userId = createUserID(BigInt(ctx.req.param('userId')));
		const body = await ctx.req.json();
		const {premium_type: premiumType, premium_until: premiumUntil} = body as {
			premium_type?: number | null;
			premium_until?: string | null;
		};

		const userRepository = ctx.get('userRepository');
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const updates: Record<string, unknown> = {};

		if (premiumType !== undefined) {
			updates.premium_type = premiumType;
		}

		if (premiumUntil !== undefined) {
			updates.premium_until = premiumUntil ? new Date(premiumUntil) : null;
		}

		if (premiumType !== null && premiumType !== undefined && premiumType > 0) {
			if (!user.premiumSince) {
				updates.premium_since = new Date();
			}
		}

		if (Object.keys(updates).length === 0) {
			return ctx.json({success: true, updated: false});
		}

		await userRepository.patchUpsert(userId, updates);

		return ctx.json({
			success: true,
			updated: true,
			premium_type: updates.premium_type ?? user.premiumType,
			premium_until: updates.premium_until
				? (updates.premium_until as Date).toISOString()
				: (user.premiumUntil?.toISOString() ?? null),
		});
	});

	app.post('/test/users/:userId/private-channels', async (ctx) => {
		ensureHarnessAccess(ctx);

		const userId = createUserID(BigInt(ctx.req.param('userId')));
		const body = (await ctx.req.json()) as Record<string, unknown>;
		const parseCount = (value: unknown, name: string): number => {
			if (value === undefined || value === null) {
				return 0;
			}
			if (typeof value !== 'number' || !Number.isInteger(value) || value < 0 || value > MAX_TEST_PRIVATE_CHANNELS) {
				throw InputValidationError.create(
					name,
					`The value for ${name} must be an integer between 0 and ${MAX_TEST_PRIVATE_CHANNELS}.`,
				);
			}
			return value;
		};

		const dmCount = parseCount(body.dm_count, 'dm_count');
		const groupDmCount = parseCount(body.group_dm_count, 'group_dm_count');
		const recipientsInput = Array.isArray(body.recipients) ? body.recipients : [];
		if ((dmCount > 0 || groupDmCount > 0) && recipientsInput.length === 0) {
			throw InputValidationError.create('recipients', 'At least one recipient is required to seed private channels.');
		}

		const recipients = recipientsInput.map((raw) => {
			if (typeof raw !== 'string') {
				throw InputValidationError.create('recipients', 'Recipient IDs must be strings.');
			}
			const trimmed = raw.trim();
			if (trimmed === '') {
				throw InputValidationError.create('recipients', 'Recipient IDs cannot be empty.');
			}
			try {
				return createUserID(BigInt(trimmed));
			} catch {
				throw InputValidationError.create('recipients', 'Recipient IDs must be valid snowflakes.');
			}
		});

		const clearExisting = Boolean(body.clear_existing);
		const channelRepository = new ChannelRepository();
		const userRepository = ctx.get('userRepository');
		const snowflakeService = ctx.get('snowflakeService');

		if (clearExisting) {
			await userRepository.deleteAllPrivateChannels(userId);
		}

		const dmChannels: Array<{channel_id: string; last_message_id: string}> = [];
		for (let i = 0; i < dmCount; i++) {
			const recipientId = recipients[i % recipients.length];
			const channelId = createChannelID(snowflakeService.generate());
			const lastMessageId = createMessageID(snowflakeService.generate());
			const channelRow: ChannelRow = {
				channel_id: channelId,
				guild_id: null,
				type: ChannelTypes.DM,
				name: null,
				topic: null,
				icon_hash: null,
				url: null,
				parent_id: null,
				position: 0,
				owner_id: null,
				recipient_ids: new Set([userId, recipientId]),
				nsfw: null,
				rate_limit_per_user: null,
				bitrate: null,
				user_limit: null,
				rtc_region: null,
				last_message_id: lastMessageId,
				last_pin_timestamp: null,
				permission_overwrites: null,
				nicks: null,
				soft_deleted: false,
				indexed_at: null,
				version: 1,
			};

			await channelRepository.upsert(channelRow);
			await userRepository.openDmForUser(userId, channelId);

			dmChannels.push({
				channel_id: channelId.toString(),
				last_message_id: lastMessageId.toString(),
			});
		}

		const groupDmChannels: Array<{channel_id: string; last_message_id: string}> = [];
		if (groupDmCount > 0) {
			const recipientsSet = new Set<UserID>([userId, ...recipients]);
			for (let i = 0; i < groupDmCount; i++) {
				const channelId = createChannelID(snowflakeService.generate());
				const lastMessageId = createMessageID(snowflakeService.generate());
				const channelRow: ChannelRow = {
					channel_id: channelId,
					guild_id: null,
					type: ChannelTypes.GROUP_DM,
					name: null,
					topic: null,
					icon_hash: null,
					url: null,
					parent_id: null,
					position: 0,
					owner_id: userId,
					recipient_ids: new Set(recipientsSet),
					nsfw: false,
					rate_limit_per_user: 0,
					bitrate: null,
					user_limit: null,
					rtc_region: null,
					last_message_id: lastMessageId,
					last_pin_timestamp: null,
					permission_overwrites: null,
					nicks: null,
					soft_deleted: false,
					indexed_at: null,
					version: 1,
				};

				await channelRepository.upsert(channelRow);
				await userRepository.openDmForUser(userId, channelId);

				groupDmChannels.push({
					channel_id: channelId.toString(),
					last_message_id: lastMessageId.toString(),
				});
			}
		}

		return ctx.json({
			dms: dmChannels,
			group_dms: groupDmChannels,
		});
	});

	app.post('/test/guilds/:guildId/features', async (ctx) => {
		ensureHarnessAccess(ctx);

		const guildId = createGuildID(BigInt(ctx.req.param('guildId')));
		const body = await ctx.req.json();
		const {add_features: addFeatures, remove_features: removeFeatures} = body as {
			add_features?: Array<string>;
			remove_features?: Array<string>;
		};

		const guildRepository = new GuildRepository();
		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			throw new UnknownGuildError();
		}

		const newFeatures = new Set(guild.features);

		if (Array.isArray(addFeatures)) {
			for (const feature of addFeatures) {
				if (typeof feature === 'string' && feature.trim()) {
					newFeatures.add(feature);
				}
			}
		}

		if (Array.isArray(removeFeatures)) {
			for (const feature of removeFeatures) {
				if (typeof feature === 'string') {
					newFeatures.delete(feature);
				}
			}
		}

		const guildRow = guild.toRow();
		await guildRepository.upsert({
			...guildRow,
			features: newFeatures,
		});

		return ctx.json({
			success: true,
			features: Array.from(newFeatures),
		});
	});

	app.get('/test/verify-asset/user/:userId/avatar', async (ctx) => {
		ensureHarnessAccess(ctx);

		const userId = createUserID(BigInt(ctx.req.param('userId')));
		const userRepository = new UserRepository();
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		if (!user.avatarHash) {
			return ctx.json({
				hash: null,
				existsInS3: null,
				message: 'User has no avatar set',
			});
		}

		const storageService = new StorageService();
		const hashWithoutPrefix = user.avatarHash.startsWith('a_') ? user.avatarHash.slice(2) : user.avatarHash;
		const s3Key = `avatars/${userId}/${hashWithoutPrefix}`;

		try {
			const metadata = await storageService.getObjectMetadata(Config.s3.buckets.cdn, s3Key);
			return ctx.json({
				hash: user.avatarHash,
				s3Key,
				existsInS3: metadata !== null,
				metadata,
			});
		} catch (error) {
			return ctx.json({
				hash: user.avatarHash,
				s3Key,
				existsInS3: false,
				error: error instanceof Error ? error.message : 'unknown error',
			});
		}
	});

	app.get('/test/verify-asset/user/:userId/banner', async (ctx) => {
		ensureHarnessAccess(ctx);

		const userId = createUserID(BigInt(ctx.req.param('userId')));
		const userRepository = new UserRepository();
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		if (!user.bannerHash) {
			return ctx.json({
				hash: null,
				existsInS3: null,
				message: 'User has no banner set',
			});
		}

		const storageService = new StorageService();
		const hashWithoutPrefix = user.bannerHash.startsWith('a_') ? user.bannerHash.slice(2) : user.bannerHash;
		const s3Key = `banners/${userId}/${hashWithoutPrefix}`;

		try {
			const metadata = await storageService.getObjectMetadata(Config.s3.buckets.cdn, s3Key);
			return ctx.json({
				hash: user.bannerHash,
				s3Key,
				existsInS3: metadata !== null,
				metadata,
			});
		} catch (error) {
			return ctx.json({
				hash: user.bannerHash,
				s3Key,
				existsInS3: false,
				error: error instanceof Error ? error.message : 'unknown error',
			});
		}
	});

	app.get('/test/verify-asset/guild/:guildId/icon', async (ctx) => {
		ensureHarnessAccess(ctx);

		const guildId = createGuildID(BigInt(ctx.req.param('guildId')));
		const guildRepository = new GuildRepository();
		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			throw new UnknownGuildError();
		}

		if (!guild.iconHash) {
			return ctx.json({
				hash: null,
				existsInS3: null,
				message: 'Guild has no icon set',
			});
		}

		const storageService = new StorageService();
		const hashWithoutPrefix = guild.iconHash.startsWith('a_') ? guild.iconHash.slice(2) : guild.iconHash;
		const s3Key = `icons/${guildId}/${hashWithoutPrefix}`;

		try {
			const metadata = await storageService.getObjectMetadata(Config.s3.buckets.cdn, s3Key);
			return ctx.json({
				hash: guild.iconHash,
				s3Key,
				existsInS3: metadata !== null,
				metadata,
			});
		} catch (error) {
			return ctx.json({
				hash: guild.iconHash,
				s3Key,
				existsInS3: false,
				error: error instanceof Error ? error.message : 'unknown error',
			});
		}
	});

	app.get('/test/verify-asset/guild/:guildId/banner', async (ctx) => {
		ensureHarnessAccess(ctx);

		const guildId = createGuildID(BigInt(ctx.req.param('guildId')));
		const guildRepository = new GuildRepository();
		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			throw new UnknownGuildError();
		}

		if (!guild.bannerHash) {
			return ctx.json({
				hash: null,
				existsInS3: null,
				message: 'Guild has no banner set',
			});
		}

		const storageService = new StorageService();
		const hashWithoutPrefix = guild.bannerHash.startsWith('a_') ? guild.bannerHash.slice(2) : guild.bannerHash;
		const s3Key = `banners/${guildId}/${hashWithoutPrefix}`;

		try {
			const metadata = await storageService.getObjectMetadata(Config.s3.buckets.cdn, s3Key);
			return ctx.json({
				hash: guild.bannerHash,
				s3Key,
				existsInS3: metadata !== null,
				metadata,
			});
		} catch (error) {
			return ctx.json({
				hash: guild.bannerHash,
				s3Key,
				existsInS3: false,
				error: error instanceof Error ? error.message : 'unknown error',
			});
		}
	});

	app.get('/test/verify-asset/guild/:guildId/splash', async (ctx) => {
		ensureHarnessAccess(ctx);

		const guildId = createGuildID(BigInt(ctx.req.param('guildId')));
		const guildRepository = new GuildRepository();
		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			throw new UnknownGuildError();
		}

		if (!guild.splashHash) {
			return ctx.json({
				hash: null,
				existsInS3: null,
				message: 'Guild has no splash set',
			});
		}

		const storageService = new StorageService();
		const hashWithoutPrefix = guild.splashHash.startsWith('a_') ? guild.splashHash.slice(2) : guild.splashHash;
		const s3Key = `splashes/${guildId}/${hashWithoutPrefix}`;

		try {
			const metadata = await storageService.getObjectMetadata(Config.s3.buckets.cdn, s3Key);
			return ctx.json({
				hash: guild.splashHash,
				s3Key,
				existsInS3: metadata !== null,
				metadata,
			});
		} catch (error) {
			return ctx.json({
				hash: guild.splashHash,
				s3Key,
				existsInS3: false,
				error: error instanceof Error ? error.message : 'unknown error',
			});
		}
	});

	app.get('/test/verify-asset/guild/:guildId/member/:userId/avatar', async (ctx) => {
		ensureHarnessAccess(ctx);

		const guildId = createGuildID(BigInt(ctx.req.param('guildId')));
		const userId = createUserID(BigInt(ctx.req.param('userId')));
		const guildRepository = new GuildRepository();
		const member = await guildRepository.getMember(guildId, userId);
		if (!member) {
			throw new UnknownGuildMemberError();
		}

		if (!member.avatarHash) {
			return ctx.json({
				hash: null,
				existsInS3: null,
				message: 'Member has no guild avatar set',
			});
		}

		const storageService = new StorageService();
		const hashWithoutPrefix = member.avatarHash.startsWith('a_') ? member.avatarHash.slice(2) : member.avatarHash;
		const s3Key = `guilds/${guildId}/users/${userId}/avatars/${hashWithoutPrefix}`;

		try {
			const metadata = await storageService.getObjectMetadata(Config.s3.buckets.cdn, s3Key);
			return ctx.json({
				hash: member.avatarHash,
				s3Key,
				existsInS3: metadata !== null,
				metadata,
			});
		} catch (error) {
			return ctx.json({
				hash: member.avatarHash,
				s3Key,
				existsInS3: false,
				error: error instanceof Error ? error.message : 'unknown error',
			});
		}
	});

	app.get('/test/verify-asset/guild/:guildId/member/:userId/banner', async (ctx) => {
		ensureHarnessAccess(ctx);

		const guildId = createGuildID(BigInt(ctx.req.param('guildId')));
		const userId = createUserID(BigInt(ctx.req.param('userId')));
		const guildRepository = new GuildRepository();
		const member = await guildRepository.getMember(guildId, userId);
		if (!member) {
			throw new UnknownGuildMemberError();
		}

		if (!member.bannerHash) {
			return ctx.json({
				hash: null,
				existsInS3: null,
				message: 'Member has no guild banner set',
			});
		}

		const storageService = new StorageService();
		const hashWithoutPrefix = member.bannerHash.startsWith('a_') ? member.bannerHash.slice(2) : member.bannerHash;
		const s3Key = `guilds/${guildId}/users/${userId}/banners/${hashWithoutPrefix}`;

		try {
			const metadata = await storageService.getObjectMetadata(Config.s3.buckets.cdn, s3Key);
			return ctx.json({
				hash: member.bannerHash,
				s3Key,
				existsInS3: metadata !== null,
				metadata,
			});
		} catch (error) {
			return ctx.json({
				hash: member.bannerHash,
				s3Key,
				existsInS3: false,
				error: error instanceof Error ? error.message : 'unknown error',
			});
		}
	});

	app.get('/test/users/:userId/data-exists', async (ctx) => {
		ensureHarnessAccess(ctx);

		const userId = createUserID(BigInt(ctx.req.param('userId')));
		console.log('[test/users/:userId/data-exists] Request received', {userId: userId.toString()});

		const userRepository = new UserRepository();
		const userChannelRepository = new UserChannelRepository();
		const authSessionRepository = new AuthSessionRepository();
		const user = await userRepository.findUnique(userId);

		if (!user) {
			console.log('[test/users/:userId/data-exists] User not found', {userId: userId.toString()});
			return ctx.json({user_exists: false}, 200);
		}

		console.log('[test/users/:userId/data-exists] User found', {
			userId: userId.toString(),
			flags: user.flags?.toString() ?? 'null',
			pendingDeletionAt: user.pendingDeletionAt?.toISOString() ?? 'null',
		});

		const [relationships, sessions, oauthAccessTokens, pinnedDms, savedMessages] = await Promise.all([
			userRepository.listRelationships(userId),
			authSessionRepository.listAuthSessions(userId),
			fetchMany<{token_: string}>(
				OAuth2AccessTokensByUser.selectCql({
					columns: ['token_'],
					where: OAuth2AccessTokensByUser.where.eq('user_id'),
				}),
				{user_id: userId},
			),
			userChannelRepository.getPinnedDms(userId),
			userRepository.listSavedMessages(userId),
		]);

		const hasSelfDeletedFlag = user.flags ? (user.flags & UserFlags.SELF_DELETED) !== 0n : false;
		const hasDeletedFlag = user.flags ? (user.flags & UserFlags.DELETED) !== 0n : false;

		console.log('[test/users/:userId/data-exists] Flag check results', {
			userId: userId.toString(),
			rawFlags: user.flags?.toString() ?? 'null',
			SELF_DELETED_FLAG_VALUE: UserFlags.SELF_DELETED.toString(),
			DELETED_FLAG_VALUE: UserFlags.DELETED.toString(),
			hasSelfDeletedFlag,
			hasDeletedFlag,
		});

		const response = {
			user_exists: true,
			email_cleared: user.email === null,
			phone_cleared: user.phone === null,
			password_cleared: user.passwordHash === null,
			flags: user.flags?.toString(),
			has_deleted_flag: hasDeletedFlag,
			has_self_deleted_flag: hasSelfDeletedFlag,
			pending_deletion_at: user.pendingDeletionAt ? user.pendingDeletionAt.toISOString() : null,
			relationships_count: relationships.length,
			sessions_count: sessions.length,
			oauth_tokens_count: oauthAccessTokens.length,
			pinned_dms_count: pinnedDms.length,
			saved_messages_count: savedMessages.length,
		};

		console.log('[test/users/:userId/data-exists] Returning response', {userId: userId.toString(), response});

		return ctx.json(response, 200);
	});

	app.get('/test/users/:userId/relationships', async (ctx) => {
		ensureHarnessAccess(ctx);

		const userId = createUserID(BigInt(ctx.req.param('userId')));
		const userRepository = new UserRepository();

		const relationships = await userRepository.listRelationships(userId);

		return ctx.json({relationships}, 200);
	});

	app.get('/test/users/:userId/sessions', async (ctx) => {
		ensureHarnessAccess(ctx);

		const userId = createUserID(BigInt(ctx.req.param('userId')));
		const authSessionRepository = new AuthSessionRepository();

		const sessions = await authSessionRepository.listAuthSessions(userId);

		return ctx.json({sessions, count: sessions.length}, 200);
	});

	app.get('/test/users/:userId/messages/count', async (ctx) => {
		ensureHarnessAccess(ctx);

		const userId = createUserID(BigInt(ctx.req.param('userId')));
		const channelRepository = new ChannelRepository();

		const messages = await channelRepository.listMessagesByAuthor(userId);

		return ctx.json({count: messages.length}, 200);
	});

	app.post('/test/worker/process-pending-deletions', async (ctx) => {
		ensureHarnessAccess(ctx);

		console.log('[test/worker/process-pending-deletions] Request received');

		const userRepository = new UserRepository();
		const redis = new Redis(Config.redis.url);
		const redisDeletionQueue = new RedisAccountDeletionQueueService(redis, userRepository);

		const snowflakeService = new SnowflakeService(redis);
		await snowflakeService.initialize();

		const workerDeps = await initializeWorkerDepsWithHarnessEmail(ctx, snowflakeService);

		const now = Date.now();
		console.log('[test/worker/process-pending-deletions] Looking for ready deletions', {
			now: new Date(now).toISOString(),
		});

		const readyDeletions = await redisDeletionQueue.getReadyDeletions(now, 100);

		console.log('[test/worker/process-pending-deletions] Found ready deletions from Redis', {
			count: readyDeletions.length,
		});

		let processed = 0;
		const errors: Array<{userId: string; error: string}> = [];

		for (const deletion of readyDeletions) {
			try {
				const userId = createUserID(deletion.userId);

				console.log('[test/worker/process-pending-deletions] Processing deletion', {
					userId: userId.toString(),
					deletionReasonCode: deletion.deletionReasonCode,
				});

				const user = await userRepository.findUnique(userId);
				if (!user || !user.pendingDeletionAt) {
					console.log('[test/worker/process-pending-deletions] User not found or no pending deletion', {
						userId: userId.toString(),
						userFound: !!user,
						hasPendingDeletion: !!user?.pendingDeletionAt,
					});
					continue;
				}

				const pendingDeletionAt = user.pendingDeletionAt;

				await processUserDeletion(userId, deletion.deletionReasonCode, workerDeps);

				await userRepository.removePendingDeletion(userId, pendingDeletionAt);
				await redisDeletionQueue.removeFromQueue(userId);

				console.log('[test/worker/process-pending-deletions] Deletion completed', {userId: userId.toString()});

				processed++;
			} catch (error) {
				const errorMessage = error instanceof Error ? error.message : String(error);
				console.error('[test/worker/process-pending-deletions] Error processing deletion', {
					userId: deletion.userId.toString(),
					error: errorMessage,
				});
				errors.push({
					userId: deletion.userId.toString(),
					error: errorMessage,
				});
			}
		}

		console.log('[test/worker/process-pending-deletions] Completed', {processed, total: readyDeletions.length});

		await snowflakeService.shutdown();
		await redis.quit();

		return ctx.json(
			{
				scheduled: processed,
				total: readyDeletions.length,
				errors: errors.length > 0 ? errors : undefined,
			},
			200,
		);
	});

	app.post('/test/worker/process-pending-deletion/:userId', async (ctx) => {
		ensureHarnessAccess(ctx);

		const userId = createUserID(BigInt(ctx.req.param('userId')));

		console.log('[test/worker/process-pending-deletion/:userId] Request received', {userId: userId.toString()});

		const userRepository = new UserRepository();
		const user = await userRepository.findUnique(userId);

		if (!user) {
			console.log('[test/worker/process-pending-deletion/:userId] User not found', {userId: userId.toString()});
			throw new UnknownUserError();
		}

		if (!user.pendingDeletionAt) {
			console.log('[test/worker/process-pending-deletion/:userId] User has no pending deletion', {
				userId: userId.toString(),
			});
			throw new NoPendingDeletionError();
		}

		const deletionReasonCode = user.deletionReasonCode ?? 0;

		console.log('[test/worker/process-pending-deletion/:userId] Running deletion synchronously', {
			userId: userId.toString(),
			deletionReasonCode,
			pendingDeletionAt: user.pendingDeletionAt.toISOString(),
		});

		try {
			const redis = new Redis(Config.redis.url);
			const snowflakeService = new SnowflakeService(redis);
			await snowflakeService.initialize();

			const workerDeps = await initializeWorkerDepsWithHarnessEmail(ctx, snowflakeService);

			await processUserDeletion(userId, deletionReasonCode, workerDeps);

			console.log('[test/worker/process-pending-deletion/:userId] Deletion completed successfully', {
				userId: userId.toString(),
			});

			return ctx.json(
				{
					success: true,
					userId: userId.toString(),
					deletionReasonCode,
				},
				200,
			);
		} catch (error) {
			console.error('[test/worker/process-pending-deletion/:userId] Deletion failed', {
				userId: userId.toString(),
				error: error instanceof Error ? error.message : String(error),
			});

			throw new DeletionFailedError(error instanceof Error ? error.message : String(error));
		}
	});

	app.post('/test/worker/process-inactivity-deletions', async (ctx) => {
		ensureHarnessAccess(ctx);

		console.log('[test/worker/process-inactivity-deletions] Request received');

		const redis = new Redis(Config.redis.url);
		const snowflakeService = new SnowflakeService(redis);
		await snowflakeService.initialize();

		try {
			const workerDeps = await initializeWorkerDepsWithHarnessEmail(ctx, snowflakeService);

			setWorkerDependencies(workerDeps);

			const result = await processInactivityDeletionsCore({
				redis: workerDeps.redis,
				userRepository: workerDeps.userRepository,
				emailService: workerDeps.emailService,
				activityTracker: workerDeps.activityTracker,
				deletionEligibilityService: workerDeps.deletionEligibilityService,
			});

			console.log('[test/worker/process-inactivity-deletions] Completed successfully', result);

			return ctx.json(
				{
					success: true,
					processed: result.warningsSent + result.deletionsScheduled,
					warnings_sent: result.warningsSent,
					deletions_scheduled: result.deletionsScheduled,
				},
				200,
			);
		} catch (error) {
			console.error('[test/worker/process-inactivity-deletions] Failed', {
				error: error instanceof Error ? error.message : String(error),
			});

			throw new ProcessingFailedError(error instanceof Error ? error.message : String(error));
		} finally {
			await snowflakeService.shutdown();
			await redis.quit();
		}
	});

	app.post('/test/worker/send-scheduled-message/:userId/:scheduledMessageId', async (ctx) => {
		ensureHarnessAccess(ctx);

		const userId = createUserID(BigInt(ctx.req.param('userId')));
		const scheduledMessageId = createMessageID(BigInt(ctx.req.param('scheduledMessageId')));

		const redis = new Redis(Config.redis.url);
		const snowflakeService = new SnowflakeService(redis);
		await snowflakeService.initialize();

		try {
			const workerDeps = await initializeWorkerDepsWithHarnessEmail(ctx, snowflakeService);
			setWorkerDependencies(workerDeps);

			const scheduledMessageRepository = new ScheduledMessageRepository();
			const scheduledMessage = await scheduledMessageRepository.getScheduledMessage(userId, scheduledMessageId);
			if (!scheduledMessage) {
				return ctx.json({success: false, reason: 'scheduled message not found'}, 404);
			}

			const logger = {
				debug: (message: string, extra?: object) => Logger.debug(extra ?? {}, message),
				info: (message: string, extra?: object) => Logger.info(extra ?? {}, message),
				warn: (message: string, extra?: object) => Logger.warn(extra ?? {}, message),
				error: (message: string, extra?: object) => Logger.error(extra ?? {}, message),
			};

			const executor = new ScheduledMessageExecutor(workerDeps, logger, scheduledMessageRepository);
			await executor.execute({
				userId: userId.toString(),
				scheduledMessageId: scheduledMessageId.toString(),
				expectedScheduledAt: scheduledMessage.scheduledAt.toISOString(),
			});

			return ctx.json({success: true}, 200);
		} finally {
			await snowflakeService.shutdown();
			await redis.quit();
		}
	});

	app.post('/test/attachment-decay/rows', async (ctx) => {
		ensureHarnessAccess(ctx);

		const payload = (await ctx.req.json()) as {
			rows?: Array<{
				attachment_id?: string;
				channel_id?: string;
				message_id?: string;
				expires_at?: string;
				uploaded_at?: string;
				last_accessed_at?: string;
				filename?: string;
				size_bytes?: string | number;
				cost?: number;
				lifetime_days?: number;
				status?: string | null;
			}>;
		};

		if (!payload.rows || payload.rows.length === 0) {
			throw InputValidationError.create('rows', 'rows is required');
		}

		const repo = new AttachmentDecayRepository();
		let inserted = 0;

		for (const row of payload.rows) {
			if (!row.attachment_id || !row.channel_id || !row.message_id || !row.expires_at) {
				throw InputValidationError.create(
					'attachment_id',
					'attachment_id, channel_id, message_id, and expires_at are required',
				);
			}

			let attachmentIdNum: bigint;
			let channelIdNum: bigint;
			let messageIdNum: bigint;

			try {
				attachmentIdNum = BigInt(row.attachment_id);
				channelIdNum = BigInt(row.channel_id);
				messageIdNum = BigInt(row.message_id);
			} catch {
				throw InputValidationError.create(
					'attachment_id',
					'attachment_id, channel_id, and message_id must be valid integers',
				);
			}

			const expiresAt = new Date(row.expires_at);
			if (Number.isNaN(expiresAt.getTime())) {
				throw new InvalidTimestampError('expires_at must be a valid timestamp');
			}

			const uploadedAt = row.uploaded_at ? new Date(row.uploaded_at) : expiresAt;
			const lastAccessedAt = row.last_accessed_at ? new Date(row.last_accessed_at) : uploadedAt;
			if (Number.isNaN(uploadedAt.getTime()) || Number.isNaN(lastAccessedAt.getTime())) {
				throw new InvalidTimestampError('uploaded_at and last_accessed_at must be valid timestamps');
			}

			const sizeInput = row.size_bytes ?? '1024';
			let sizeBytes: bigint;
			try {
				sizeBytes = typeof sizeInput === 'number' ? BigInt(sizeInput) : BigInt(sizeInput);
			} catch {
				throw InputValidationError.create('size_bytes', 'size_bytes must be a valid integer');
			}

			await repo.upsert({
				attachment_id: createAttachmentID(attachmentIdNum),
				channel_id: createChannelID(channelIdNum),
				message_id: createMessageID(messageIdNum),
				filename: row.filename ?? 'attachment-decay-test.bin',
				size_bytes: sizeBytes,
				uploaded_at: uploadedAt,
				expires_at: expiresAt,
				last_accessed_at: lastAccessedAt,
				cost: row.cost ?? 1,
				lifetime_days: row.lifetime_days ?? 1,
				status: row.status ?? null,
				expiry_bucket: getExpiryBucket(expiresAt),
			});

			inserted++;
		}

		return ctx.json({inserted}, 200);
	});

	app.post('/test/attachment-decay/clear', async (ctx) => {
		ensureHarnessAccess(ctx);

		const repo = new AttachmentDecayRepository();

		const deleted = await repo.clearAll(30);

		return ctx.json({deleted}, 200);
	});

	app.post('/test/attachment-decay/query', async (ctx) => {
		ensureHarnessAccess(ctx);

		const payload = (await ctx.req.json()) as {
			bucket?: number | string;
			current_time?: string;
			limit?: number;
		};

		const bucketValue = payload.bucket ? Number(payload.bucket) : undefined;
		if (!bucketValue) {
			throw InputValidationError.create('bucket', 'bucket is required');
		}

		const currentTime = payload.current_time ? new Date(payload.current_time) : new Date();
		if (Number.isNaN(currentTime.getTime())) {
			throw new InvalidTimestampError('current_time must be a valid timestamp');
		}

		const repo = new AttachmentDecayRepository();
		const rows = await repo.fetchExpiredByBucket(bucketValue, currentTime, payload.limit ?? 200);

		return ctx.json(
			{
				rows: rows.map((row) => ({
					attachment_id: row.attachment_id.toString(),
					channel_id: row.channel_id.toString(),
					message_id: row.message_id.toString(),
					expires_at: row.expires_at.toISOString(),
					expiry_bucket: row.expiry_bucket,
				})),
			},
			200,
		);
	});

	app.get('/test/attachment-decay/:attachment_id', async (ctx) => {
		ensureHarnessAccess(ctx);

		const attachmentId = createAttachmentID(BigInt(ctx.req.param('attachment_id')));
		const repo = new AttachmentDecayRepository();
		const row = await repo.fetchById(attachmentId);

		if (!row) {
			return ctx.json({row: null}, 200);
		}

		return ctx.json(
			{
				row: {
					attachment_id: row.attachment_id.toString(),
					channel_id: row.channel_id.toString(),
					message_id: row.message_id.toString(),
					filename: row.filename,
					size_bytes: row.size_bytes.toString(),
					uploaded_at: row.uploaded_at.toISOString(),
					expires_at: row.expires_at.toISOString(),
					last_accessed_at: row.last_accessed_at.toISOString(),
					cost: row.cost,
					lifetime_days: row.lifetime_days,
					status: row.status,
				},
			},
			200,
		);
	});

	app.get('/test/messages/:channel_id/:message_id/with-reference', async (ctx) => {
		ensureHarnessAccess(ctx);

		const channelId = createChannelID(BigInt(ctx.req.param('channel_id')));
		const messageId = createMessageID(BigInt(ctx.req.param('message_id')));
		const channelRepository = new ChannelRepository();
		const message = await channelRepository.messages.getMessage(channelId, messageId);
		if (!message) {
			throw new UnknownMessageError();
		}

		const reference = message.reference;
		const referencedMessage =
			reference?.channelId && reference?.messageId
				? await channelRepository.messages.getMessage(reference.channelId, reference.messageId)
				: null;

		const decayTargets = [
			...message.attachments.map((att) => ({attachmentId: att.id})),
			...(referencedMessage?.attachments.map((att) => ({attachmentId: att.id})) ?? []),
		];
		const attachmentDecayMap = decayTargets.length
			? await new AttachmentDecayService().fetchMetadata(decayTargets)
			: undefined;

		const requestCache = ctx.get('requestCache');
		const userCacheService = ctx.get('userCacheService');
		const mediaService = ctx.get('mediaService');

		const messageResponse = await mapMessageToResponse({
			message,
			userCacheService,
			requestCache,
			mediaService,
			attachmentDecayMap,
			getReferencedMessage: async (refChannelId, referencedMessageId) => {
				if (referencedMessage && referencedMessage.id === referencedMessageId) {
					return referencedMessage;
				}
				return channelRepository.messages.getMessage(refChannelId, referencedMessageId);
			},
		});

		return ctx.json(messageResponse, 200);
	});

	app.post('/test/worker/expire-attachments', async (ctx) => {
		ensureHarnessAccess(ctx);

		console.log('[test/worker/expire-attachments] Request received');

		const redis = new Redis(Config.redis.url);
		const snowflakeService = new SnowflakeService(redis);
		await snowflakeService.initialize();

		try {
			const workerDeps = await initializeWorkerDepsWithHarnessEmail(ctx, snowflakeService);

			setWorkerDependencies(workerDeps);

			await processExpiredAttachments();

			console.log('[test/worker/expire-attachments] Completed successfully');

			return ctx.json({success: true}, 200);
		} finally {
			await snowflakeService.shutdown();
			await redis.quit();
		}
	});

	app.post('/test/users/:userId/set-last-active-at', async (ctx) => {
		ensureHarnessAccess(ctx);

		const userId = createUserID(BigInt(ctx.req.param('userId')));
		const body = await ctx.req.json();
		const {timestamp} = body as {timestamp?: string};

		if (!timestamp) {
			throw new InvalidTimestampError('Timestamp is required');
		}

		let date: Date;
		try {
			date = new Date(timestamp);
			if (Number.isNaN(date.getTime())) {
				throw new InvalidTimestampError();
			}
		} catch {
			throw new InvalidTimestampError();
		}

		console.log('[test/users/:userId/set-last-active-at] Request received', {
			userId: userId.toString(),
			timestamp: date.toISOString(),
		});

		try {
			const userRepository = new UserRepository();
			const redis = new Redis(Config.redis.url);

			try {
				await userRepository.patchUpsert(userId, {
					last_active_at: date,
				});

				const activityTracker = new RedisActivityTracker(redis);
				await activityTracker.updateActivity(userId, date);

				console.log('[test/users/:userId/set-last-active-at] Updated successfully', {
					userId: userId.toString(),
					timestamp: date.toISOString(),
				});

				return ctx.json(
					{
						success: true,
						userId: userId.toString(),
						last_active_at: date.toISOString(),
					},
					200,
				);
			} finally {
				await redis.quit();
			}
		} catch (error) {
			console.error('[test/users/:userId/set-last-active-at] Failed', {
				userId: userId.toString(),
				error: error instanceof Error ? error.message : String(error),
			});

			throw new UpdateFailedError(error instanceof Error ? error.message : String(error));
		}
	});

	app.post('/test/users/:userId/set-bot-flag', async (ctx) => {
		ensureHarnessAccess(ctx);

		const userId = createUserID(BigInt(ctx.req.param('userId')));
		const body = await ctx.req.json();
		const {is_bot: isBot} = body as {is_bot?: boolean};

		if (typeof isBot !== 'boolean') {
			throw new InvalidBotFlagError();
		}

		console.log('[test/users/:userId/set-bot-flag] Request received', {
			userId: userId.toString(),
			isBot,
		});

		try {
			const userRepository = new UserRepository();
			const user = await userRepository.findUnique(userId);

			if (!user) {
				throw new UnknownUserError();
			}

			await userRepository.patchUpsert(userId, {
				bot: isBot,
			});

			console.log('[test/users/:userId/set-bot-flag] Updated successfully', {
				userId: userId.toString(),
				isBot,
			});

			return ctx.json(
				{
					success: true,
					userId: userId.toString(),
					is_bot: isBot,
				},
				200,
			);
		} catch (error) {
			console.error('[test/users/:userId/set-bot-flag] Failed', {
				userId: userId.toString(),
				error: error instanceof Error ? error.message : String(error),
			});

			throw new UpdateFailedError(error instanceof Error ? error.message : String(error));
		}
	});

	app.post('/test/users/:userId/set-system-flag', async (ctx) => {
		ensureHarnessAccess(ctx);

		const userId = createUserID(BigInt(ctx.req.param('userId')));
		const body = await ctx.req.json();
		const {is_system: isSystem} = body as {is_system?: boolean};

		if (typeof isSystem !== 'boolean') {
			throw new InvalidSystemFlagError();
		}

		console.log('[test/users/:userId/set-system-flag] Request received', {
			userId: userId.toString(),
			isSystem,
		});

		try {
			const userRepository = new UserRepository();
			const user = await userRepository.findUnique(userId);

			if (!user) {
				throw new UnknownUserError();
			}

			await userRepository.patchUpsert(userId, {
				system: isSystem,
			});

			console.log('[test/users/:userId/set-system-flag] Updated successfully', {
				userId: userId.toString(),
				isSystem,
			});

			return ctx.json(
				{
					success: true,
					userId: userId.toString(),
					is_system: isSystem,
				},
				200,
			);
		} catch (error) {
			console.error('[test/users/:userId/set-system-flag] Failed', {
				userId: userId.toString(),
				error: error instanceof Error ? error.message : String(error),
			});

			throw new UpdateFailedError(error instanceof Error ? error.message : String(error));
		}
	});

	app.post('/test/users/:userId/harvest/:harvestId/set-expiration', async (ctx) => {
		ensureHarnessAccess(ctx);

		const userId = createUserID(BigInt(ctx.req.param('userId')));
		const harvestId = BigInt(ctx.req.param('harvestId'));
		const body = await ctx.req.json();
		const {expires_at} = body as {expires_at?: string};

		if (!expires_at) {
			throw new InvalidTimestampError('expires_at is required');
		}

		let date: Date;
		try {
			date = new Date(expires_at);
			if (Number.isNaN(date.getTime())) {
				throw new InvalidTimestampError();
			}
		} catch {
			throw new InvalidTimestampError();
		}

		const harvestRepository = new UserHarvestRepository();
		const harvest = await harvestRepository.findByUserAndHarvestId(userId, harvestId);

		if (!harvest) {
			throw new UnknownHarvestError();
		}

		harvest.downloadUrlExpiresAt = date;
		await harvestRepository.update(harvest);

		return ctx.json({success: true}, 200);
	});

	app.get('/test/users/:userId/presence/has-active', async (ctx) => {
		ensureHarnessAccess(ctx);

		const userId = createUserID(BigInt(ctx.req.param('userId')));

		try {
			const gatewayService = ctx.get('gatewayService');
			const hasActive = await gatewayService.hasActivePresence(userId);

			return ctx.json(
				{
					user_id: userId.toString(),
					has_active: hasActive,
				},
				200,
			);
		} catch (error) {
			console.error('[test/users/:userId/presence/has-active] Error checking presence', {
				userId: userId.toString(),
				error: error instanceof Error ? error.message : String(error),
			});

			return ctx.json(
				{
					user_id: userId.toString(),
					has_active: false,
					error: error instanceof Error ? error.message : String(error),
				},
				200,
			);
		}
	});

	app.post('/test/messages/seed', async (ctx) => {
		ensureHarnessAccess(ctx);

		interface SeedMessageInput {
			message_id?: string;
			timestamp?: string;
			content?: string;
			author_id?: string;
		}

		interface SeedMessagesRequest {
			channel_id: string;
			messages: Array<SeedMessageInput>;
			author_id?: string;
			clear_existing?: boolean;
			skip_bucket_index?: boolean;
		}

		const body = (await ctx.req.json()) as SeedMessagesRequest;

		if (!body.channel_id) {
			throw InputValidationError.create('channel_id', 'channel_id is required');
		}

		if (!body.messages || !Array.isArray(body.messages) || body.messages.length === 0) {
			throw InputValidationError.create('messages', 'messages array is required and must not be empty');
		}

		const channelId = createChannelID(BigInt(body.channel_id));
		const defaultAuthorId = body.author_id ? createUserID(BigInt(body.author_id)) : null;
		const skipBucketIndex = body.skip_bucket_index ?? false;

		if (body.clear_existing) {
			await deleteOneOrMany(ChannelMessageBuckets.deletePartition({channel_id: channelId}));
			await deleteOneOrMany(ChannelEmptyBuckets.deletePartition({channel_id: channelId}));
		}

		const seededMessages: Array<{message_id: string; bucket: number; timestamp: string}> = [];
		const bucketsPopulated = new Set<number>();
		let latestMessageId: MessageID | null = null;
		let latestBucket: number | null = null;

		const batch = new BatchBuilder();

		for (let i = 0; i < body.messages.length; i++) {
			const input = body.messages[i];

			let messageId: MessageID;
			let timestamp: number;

			if (input.message_id) {
				messageId = createMessageID(BigInt(input.message_id));
				timestamp = SnowflakeUtils.extractTimestamp(messageId);
			} else if (input.timestamp) {
				const parsedDate = new Date(input.timestamp);
				if (Number.isNaN(parsedDate.getTime())) {
					throw new InvalidTimestampError(`Invalid timestamp at index ${i}: ${input.timestamp}`);
				}
				timestamp = parsedDate.getTime();
				const baseSnowflake = SnowflakeUtils.getSnowflake(timestamp);
				messageId = createMessageID(baseSnowflake | BigInt(i & 0xfff));
			} else {
				timestamp = Date.now() + i;
				messageId = createMessageID(SnowflakeUtils.getSnowflake(timestamp) | BigInt(i & 0xfff));
			}

			const bucket = BucketUtils.makeBucket(messageId);
			const authorId = input.author_id ? createUserID(BigInt(input.author_id)) : defaultAuthorId;
			const content = input.content ?? `Test message ${i + 1}`;

			const messageRow: MessageRow = {
				channel_id: channelId,
				bucket,
				message_id: messageId,
				author_id: authorId,
				type: 0,
				webhook_id: null,
				webhook_name: null,
				webhook_avatar_hash: null,
				content,
				edited_timestamp: null,
				pinned_timestamp: null,
				flags: 0,
				mention_everyone: false,
				mention_users: null,
				mention_roles: null,
				mention_channels: null,
				attachments: null,
				embeds: null,
				sticker_items: null,
				message_reference: null,
				message_snapshots: null,
				call: null,
				has_reaction: false,
				version: 1,
			};

			batch.addPrepared(Messages.upsertAll(messageRow));

			bucketsPopulated.add(bucket);

			if (latestMessageId === null || messageId > latestMessageId) {
				latestMessageId = messageId;
				latestBucket = bucket;
			}

			seededMessages.push({
				message_id: messageId.toString(),
				bucket,
				timestamp: new Date(timestamp).toISOString(),
			});
		}

		if (!skipBucketIndex) {
			for (const bucket of bucketsPopulated) {
				batch.addPrepared(
					ChannelMessageBuckets.upsertAll({
						channel_id: channelId,
						bucket,
						updated_at: new Date(),
					}),
				);

				batch.addPrepared(
					ChannelEmptyBuckets.deleteByPk({
						channel_id: channelId,
						bucket,
					}),
				);
			}
		}

		if (latestMessageId !== null && latestBucket !== null) {
			const createdBucket = Math.min(BucketUtils.makeBucket(channelId), ...bucketsPopulated);

			const existingState = await fetchOne<ChannelStateRow>(FETCH_CHANNEL_STATE.bind({channel_id: channelId}));

			const finalCreatedBucket =
				existingState?.created_bucket !== undefined
					? Math.min(createdBucket, existingState.created_bucket)
					: createdBucket;

			const finalLastMessageId =
				existingState?.last_message_id && existingState.last_message_id > latestMessageId
					? existingState.last_message_id
					: latestMessageId;

			const finalLastMessageBucket =
				existingState?.last_message_id && existingState.last_message_id > latestMessageId
					? existingState.last_message_bucket
					: skipBucketIndex
						? null
						: latestBucket;

			batch.addPrepared(
				ChannelState.upsertAll({
					channel_id: channelId,
					created_bucket: finalCreatedBucket,
					has_messages: true,
					last_message_id: finalLastMessageId,
					last_message_bucket: finalLastMessageBucket,
					updated_at: new Date(),
				}),
			);
		}

		await batch.execute();

		return ctx.json(
			{
				messages: seededMessages,
				buckets_populated: skipBucketIndex ? [] : Array.from(bucketsPopulated).sort((a, b) => b - a),
				channel_state_updated: latestMessageId !== null,
			},
			200,
		);
	});

	app.get('/test/channels/:channelId/state', async (ctx) => {
		ensureHarnessAccess(ctx);

		const channelId = createChannelID(BigInt(ctx.req.param('channelId')));

		const state = await fetchOne<ChannelStateRow>(FETCH_CHANNEL_STATE.bind({channel_id: channelId}));

		if (!state) {
			return ctx.json(
				{
					channel_id: channelId.toString(),
					exists: false,
				},
				200,
			);
		}

		return ctx.json(
			{
				channel_id: channelId.toString(),
				exists: true,
				created_bucket: state.created_bucket,
				has_messages: state.has_messages,
				last_message_id: state.last_message_id?.toString() ?? null,
				last_message_bucket: state.last_message_bucket,
				updated_at: state.updated_at.toISOString(),
			},
			200,
		);
	});

	app.get('/test/channels/:channelId/buckets', async (ctx) => {
		ensureHarnessAccess(ctx);

		const channelId = createChannelID(BigInt(ctx.req.param('channelId')));

		const rows = await fetchMany<Pick<ChannelMessageBucketRow, 'bucket' | 'updated_at'>>(
			FETCH_CHANNEL_BUCKETS.bind({channel_id: channelId}),
		);

		return ctx.json(
			{
				channel_id: channelId.toString(),
				buckets: rows.map((r) => ({
					bucket: r.bucket,
					updated_at: r.updated_at.toISOString(),
				})),
				count: rows.length,
			},
			200,
		);
	});

	app.get('/test/channels/:channelId/empty-buckets', async (ctx) => {
		ensureHarnessAccess(ctx);

		const channelId = createChannelID(BigInt(ctx.req.param('channelId')));

		const rows = await fetchMany<Pick<ChannelEmptyBucketRow, 'bucket' | 'updated_at'>>(
			FETCH_CHANNEL_EMPTY_BUCKETS.bind({channel_id: channelId}),
		);

		return ctx.json(
			{
				channel_id: channelId.toString(),
				empty_buckets: rows.map((r) => ({
					bucket: r.bucket,
					updated_at: r.updated_at.toISOString(),
				})),
				count: rows.length,
			},
			200,
		);
	});

	app.delete('/test/channels/:channelId/messages', async (ctx) => {
		ensureHarnessAccess(ctx);

		const channelId = createChannelID(BigInt(ctx.req.param('channelId')));

		const batch = new BatchBuilder();

		batch.addPrepared(ChannelMessageBuckets.deletePartition({channel_id: channelId}));
		batch.addPrepared(ChannelEmptyBuckets.deletePartition({channel_id: channelId}));

		const createdBucket = BucketUtils.makeBucket(channelId);
		batch.addPrepared(
			ChannelState.upsertAll({
				channel_id: channelId,
				created_bucket: createdBucket,
				has_messages: false,
				last_message_id: null,
				last_message_bucket: null,
				updated_at: new Date(),
			}),
		);

		await batch.execute();

		return ctx.json(
			{
				channel_id: channelId.toString(),
				cleared: true,
			},
			200,
		);
	});
};
