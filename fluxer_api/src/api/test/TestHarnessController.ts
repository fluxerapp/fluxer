// SPDX-License-Identifier: AGPL-3.0-or-later

import {AttachmentDecayRepository} from '@app/api/attachment/AttachmentDecayRepository';
import type {IpAuthorizationTicketCache} from '@app/api/auth/AuthLogin';
import {getTicketCacheKey} from '@app/api/auth/AuthLogin';
import {
	createApplicationID,
	createAttachmentID,
	createChannelID,
	createGuildID,
	createMessageID,
	createUserID,
	type MessageID,
	type UserID,
} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {BatchBuilder, deleteOneOrMany, fetchMany, fetchOne} from '@app/api/database/CassandraQueryExecution';
import {defineTable} from '@app/api/database/CassandraTableDsl';
import type {ChannelRow} from '@app/api/database/types/ChannelTypes';
import {
	CHANNEL_EMPTY_BUCKET_COLUMNS,
	CHANNEL_MESSAGE_BUCKET_COLUMNS,
	CHANNEL_STATE_COLUMNS,
	type ChannelEmptyBucketRow,
	type ChannelMessageBucketRow,
	type ChannelStateRow,
	MESSAGE_BY_AUTHOR_COLUMNS,
	MESSAGE_COLUMNS,
	type MessageByAuthorRow,
	type MessageRow,
} from '@app/api/database/types/MessageTypes';
import type {GiftCodeDurationType} from '@app/api/database/types/PaymentTypes';
import {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import {KVActivityTracker} from '@app/api/infrastructure/KVActivityTracker';
import {Logger} from '@app/api/Logger';
import {getKVClient, getSnowflakeService} from '@app/api/middleware/ServiceRegistry';
import {mapGiftDurationMonthsToFields} from '@app/api/models/GiftCode';
import {OAuth2TokenRepository} from '@app/api/oauth/repositories/OAuth2TokenRepository';
import {IpAuthorizationTokens, OAuth2AccessTokensByUser} from '@app/api/Tables';
import type {HonoApp, HonoEnv} from '@app/api/types/HonoEnv';
import {UserSearchRepository} from '@app/api/user/repositories/account/crud/UserSearchRepository';
import {AuthSessionRepository} from '@app/api/user/repositories/auth/AuthSessionRepository';
import type {UserDeletionScheduleUpdate} from '@app/api/user/repositories/IUserAccountRepository';
import {UserChannelRepository} from '@app/api/user/repositories/UserChannelRepository';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {processUserDeletion} from '@app/api/user/services/UserDeletionService';
import {parseReportedClientOs} from '@app/api/utils/SessionClientIdentity';
import {processInactivityDeletionsCore} from '@app/api/worker/tasks/ProcessInactivityDeletions';
import {setWorkerDependencies} from '@app/api/worker/WorkerContext';
import {initializeWorkerDependencies} from '@app/api/worker/WorkerDependencies';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {MAX_GUILD_MEMBERS_VERY_LARGE_GUILD} from '@fluxer/constants/src/LimitConstants';
import {PremiumFlags, UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {EmailServiceNotTestableError} from '@fluxer/errors/src/domains/auth/EmailServiceNotTestableError';
import {AclsMustBeNonEmptyError} from '@fluxer/errors/src/domains/core/AclsMustBeNonEmptyError';
import {DeletionFailedError} from '@fluxer/errors/src/domains/core/DeletionFailedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {InvalidAclsFormatError} from '@fluxer/errors/src/domains/core/InvalidAclsFormatError';
import {InvalidFlagsFormatError} from '@fluxer/errors/src/domains/core/InvalidFlagsFormatError';
import {InvalidSystemFlagError} from '@fluxer/errors/src/domains/core/InvalidSystemFlagError';
import {InvalidTimestampError} from '@fluxer/errors/src/domains/core/InvalidTimestampError';
import {NoPendingDeletionError} from '@fluxer/errors/src/domains/core/NoPendingDeletionError';
import {ProcessingFailedError} from '@fluxer/errors/src/domains/core/ProcessingFailedError';
import {TestHarnessDisabledError} from '@fluxer/errors/src/domains/core/TestHarnessDisabledError';
import {TestHarnessForbiddenError} from '@fluxer/errors/src/domains/core/TestHarnessForbiddenError';
import {UpdateFailedError} from '@fluxer/errors/src/domains/core/UpdateFailedError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {InvalidBotFlagError} from '@fluxer/errors/src/domains/oauth/InvalidBotFlagError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import {UnknownUserFlagError} from '@fluxer/errors/src/domains/user/UnknownUserFlagError';
import {RpcRequest} from '@fluxer/schema/src/domains/rpc/RpcSchemas';
import {createSnowflakeFromTimestamp, snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import * as BucketUtils from '@fluxer/snowflake/src/SnowflakeBuckets';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import type {ITestEmailService, SentEmailRecord} from '@pkgs/email/src/ITestEmailService';
import type {Context} from 'hono';
import {seconds} from 'itty-time';

const TEST_EMAIL_ENDPOINT = '/test/emails';
const TEST_AUTH_HEADER = 'x-test-token';
const MAX_TEST_PRIVATE_CHANNELS = 1000;
const GIFT_DURATION_TYPES = new Set<GiftCodeDurationType>(['days', 'weeks', 'months', 'years']);

interface GiftDurationInput {
	duration_months?: number;
	duration_type?: string;
	duration_quantity?: number;
}

interface ParsedGiftDuration {
	durationType: GiftCodeDurationType;
	durationQuantity: number;
}

function isGiftCodeDurationType(value: string): value is GiftCodeDurationType {
	return GIFT_DURATION_TYPES.has(value as GiftCodeDurationType);
}

function parseGiftDurationInput(body: GiftDurationInput): ParsedGiftDuration {
	const durationTypeInput = body.duration_type;
	const durationQuantityInput = body.duration_quantity;
	if (durationTypeInput !== undefined || durationQuantityInput !== undefined) {
		if (durationTypeInput === undefined || durationQuantityInput === undefined) {
			throw new Error('duration_type and duration_quantity must both be provided');
		}
		if (!isGiftCodeDurationType(durationTypeInput)) {
			throw new Error('duration_type must be one of: days, weeks, months, years');
		}
		if (
			typeof durationQuantityInput !== 'number' ||
			!Number.isInteger(durationQuantityInput) ||
			durationQuantityInput < 0
		) {
			throw new Error('duration_quantity must be a non-negative integer');
		}
		return {
			durationType: durationTypeInput,
			durationQuantity: durationQuantityInput,
		};
	}
	if (typeof body.duration_months !== 'number' || !Number.isInteger(body.duration_months) || body.duration_months < 0) {
		throw new Error('duration_months is required when duration_type and duration_quantity are not provided');
	}
	const legacyDuration = mapGiftDurationMonthsToFields(body.duration_months);
	return {
		durationType: legacyDuration.durationType,
		durationQuantity: legacyDuration.durationQuantity,
	};
}

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
const MessagesByAuthorV2 = defineTable<MessageByAuthorRow, 'author_id' | 'message_id'>({
	name: 'messages_by_author_id_v2',
	columns: MESSAGE_BY_AUTHOR_COLUMNS,
	primaryKey: ['author_id', 'message_id'],
	partitionKey: ['author_id'],
});
const FETCH_CHANNEL_STATE = ChannelState.select({
	where: ChannelState.where.eq('channel_id'),
	limit: 1,
});

function ensureHarnessAccess(ctx: Context<HonoEnv>) {
	if (!Config.dev.testModeEnabled && Config.nodeEnv !== 'development') {
		throw new TestHarnessDisabledError();
	}
	if (Config.dev.testHarnessToken) {
		const headerValue = ctx.req.header(TEST_AUTH_HEADER) || ctx.req.header('authorization') || '';
		const bearer = headerValue.startsWith('Bearer ') ? headerValue.slice('Bearer '.length) : headerValue;
		if (bearer !== Config.dev.testHarnessToken) {
			throw new TestHarnessForbiddenError();
		}
	}
}

function isTestEmailService(service: IEmailService): service is ITestEmailService {
	return typeof (service as ITestEmailService).listSentEmails === 'function';
}

const serializeEmails = (emails: Array<SentEmailRecord>) =>
	emails.map((email) => ({
		...email,
		timestamp: email.timestamp.toISOString(),
	}));

async function initializeWorkerDepsWithHarnessEmail(ctx: Context<HonoEnv>, snowflakeService: ISnowflakeService) {
	const workerDeps = await initializeWorkerDependencies(snowflakeService);
	const harnessEmailService = ctx.get('emailService');
	if (isTestEmailService(harnessEmailService) && harnessEmailService !== workerDeps.emailService) {
		workerDeps.emailService = harnessEmailService;
	}
	return workerDeps;
}

const userFlagEntries = Object.entries(UserFlags);
const premiumFlagEntries = Object.entries(PremiumFlags);

function parseUserFlagNames(names: Array<string>): bigint | null {
	let mask = 0n;
	for (const name of names) {
		const entry = userFlagEntries.find(([flagName]) => flagName === name);
		if (!entry) {
			return null;
		}
		mask |= entry[1];
	}
	return mask;
}

function parsePremiumFlagNames(names: Array<string>): number | null {
	let mask = 0;
	for (const name of names) {
		const entry = premiumFlagEntries.find(([flagName]) => flagName === name);
		if (!entry) return null;
		mask |= entry[1] as number;
	}
	return mask;
}

export function TestHarnessController(app: HonoApp) {
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
		return ctx.body(null, 204);
	});
	app.patch('/test/users/:userId/flags', async (ctx) => {
		ensureHarnessAccess(ctx);
		const params = ctx.req.param() as {
			userId?: string;
		};
		const userIdParam = typeof params === 'object' && params !== null ? params.userId : undefined;
		if (!userIdParam || userIdParam === 'undefined') {
			throw new Error(
				`Missing userId parameter. path=${ctx.req.path}, paramsType=${typeof params}, params=${JSON.stringify(params)}`,
			);
		}
		const userId = createUserID(BigInt(userIdParam));
		const body = await ctx.req.json();
		const {flags} = body;
		if (typeof flags !== 'string' && typeof flags !== 'number') {
			throw new InvalidFlagsFormatError();
		}
		const userRepository = ctx.get('userRepository');
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		await userRepository.patchUpsert(userId, {flags: BigInt(flags)}, user.toRow());
		return ctx.json({success: true});
	});
	app.patch('/test/users/:userId/premium-flags', async (ctx) => {
		ensureHarnessAccess(ctx);
		const params = ctx.req.param() as {userId?: string};
		const userIdParam = typeof params === 'object' && params !== null ? params.userId : undefined;
		if (!userIdParam || userIdParam === 'undefined') {
			throw new Error('Missing userId parameter');
		}
		const userId = createUserID(BigInt(userIdParam));
		const body = await ctx.req.json();
		const {premium_flags: premiumFlags} = body;
		if (typeof premiumFlags !== 'string' && typeof premiumFlags !== 'number') {
			throw new InvalidFlagsFormatError();
		}
		const userRepository = ctx.get('userRepository');
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		await userRepository.patchUpsert(userId, {premium_flags: Number(premiumFlags)}, user.toRow());
		return ctx.json({success: true});
	});
	app.patch('/test/users/:userId/discriminator', async (ctx) => {
		ensureHarnessAccess(ctx);
		const params = ctx.req.param() as {
			userId?: string;
		};
		const userIdParam = typeof params === 'object' && params !== null ? params.userId : undefined;
		if (!userIdParam || userIdParam === 'undefined') {
			throw new Error(
				`Missing userId parameter. path=${ctx.req.path}, paramsType=${typeof params}, params=${JSON.stringify(params)}`,
			);
		}
		const userId = createUserID(BigInt(userIdParam));
		const body = (await ctx.req.json()) as {
			discriminator?: unknown;
		};
		const raw = body.discriminator;
		if (raw === undefined || raw === null) {
			throw InputValidationError.fromCode('discriminator', ValidationErrorCodes.DISCRIMINATOR_INVALID_FORMAT);
		}
		let discriminator: number;
		if (typeof raw === 'number') {
			discriminator = raw;
		} else if (typeof raw === 'string') {
			if (!/^\d{1,4}$/.test(raw)) {
				throw InputValidationError.fromCode('discriminator', ValidationErrorCodes.DISCRIMINATOR_INVALID_FORMAT);
			}
			discriminator = Number.parseInt(raw, 10);
		} else {
			throw InputValidationError.fromCode('discriminator', ValidationErrorCodes.DISCRIMINATOR_INVALID_FORMAT);
		}
		if (!Number.isInteger(discriminator) || discriminator < 0 || discriminator > 9999) {
			throw InputValidationError.fromCode('discriminator', ValidationErrorCodes.DISCRIMINATOR_OUT_OF_RANGE);
		}
		const userRepository = ctx.get('userRepository');
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		await userRepository.patchUpsert(userId, {discriminator}, user.toRow());
		const userCacheService = ctx.get('userCacheService');
		await userCacheService.invalidateUserCache(userId);
		return ctx.json({success: true, discriminator});
	});
	app.post('/test/users/:userId/acls', async (ctx) => {
		ensureHarnessAccess(ctx);
		const params = ctx.req.param() as {
			userId?: string;
		};
		const userIdParam = params.userId;
		if (!userIdParam) {
			throw new Error('Missing userId parameter');
		}
		const userId = createUserID(BigInt(userIdParam));
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
		await userRepository.patchUpsert(userId, {acls: normalized}, user.toRow());
		const userCacheService = ctx.get('userCacheService');
		await userCacheService.invalidateUserCache(userId);
		return ctx.json({success: true, count: normalized?.size ?? 0});
	});
	app.post('/test/users/:userId/security-flags', async (ctx) => {
		ensureHarnessAccess(ctx);
		const params = ctx.req.param() as {
			userId?: string;
		};
		const userIdParam = params.userId;
		if (!userIdParam) {
			throw new Error('Missing userId parameter');
		}
		const userId = createUserID(BigInt(userIdParam));
		const body = await ctx.req.json();
		const {
			set_flags: setFlags,
			clear_flags: clearFlags,
			set_premium_flags: setPremiumFlags,
			clear_premium_flags: clearPremiumFlags,
			email_bounced: emailBounced,
			email_verified: emailVerified,
		} = body as {
			set_flags?: Array<string>;
			clear_flags?: Array<string>;
			set_premium_flags?: Array<string>;
			clear_premium_flags?: Array<string>;
			email_bounced?: boolean;
			email_verified?: boolean;
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
		if (setFlags || clearFlags) {
			pendingUpdates['flags'] = nextFlags;
		}
		let nextPremiumFlags = user.premiumFlags ?? 0;
		const applyPremiumFlagList = (names: Array<string> | undefined, op: 'set' | 'clear') => {
			if (!names) return true;
			const mask = parsePremiumFlagNames(names);
			if (mask === null) return false;
			nextPremiumFlags = op === 'set' ? nextPremiumFlags | mask : nextPremiumFlags & ~mask;
			return true;
		};
		if (!applyPremiumFlagList(setPremiumFlags, 'set') || !applyPremiumFlagList(clearPremiumFlags, 'clear')) {
			throw new UnknownUserFlagError();
		}
		if (setPremiumFlags || clearPremiumFlags) {
			pendingUpdates['premium_flags'] = nextPremiumFlags;
		}
		if (emailBounced !== undefined) {
			if (typeof emailBounced !== 'boolean') {
				throw new InvalidFlagsFormatError();
			}
			pendingUpdates['email_bounced'] = emailBounced;
		}
		if (emailVerified !== undefined) {
			if (typeof emailVerified !== 'boolean') {
				throw new InvalidFlagsFormatError();
			}
			pendingUpdates['email_verified'] = emailVerified;
		}
		if (Object.keys(pendingUpdates).length === 0) {
			return ctx.json({success: true, updated: false});
		}
		await userRepository.patchUpsert(userId, pendingUpdates, user.toRow());
		return ctx.json({
			success: true,
			updated: true,
			flags: (pendingUpdates['flags'] as bigint | undefined)?.toString() ?? user.flags?.toString(),
			email_bounced: pendingUpdates['email_bounced'] ?? user.emailBounced,
			email_verified: pendingUpdates['email_verified'] ?? user.emailVerified,
		});
	});
	app.post('/test/users/:userId/unclaim', async (ctx) => {
		ensureHarnessAccess(ctx);
		const params = ctx.req.param() as {
			userId?: string;
		};
		const userIdParam = params.userId;
		if (!userIdParam) {
			throw new Error('Missing userId parameter');
		}
		const userId = createUserID(BigInt(userIdParam));
		const userRepository = ctx.get('userRepository');
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		await userRepository.patchUpsert(
			userId,
			{
				email: null,
				email_verified: false,
				email_bounced: false,
				password_hash: null,
				password_last_changed_at: null,
				authenticator_types: null,
				totp_secret: null,
			},
			user.toRow(),
		);
		return ctx.json({success: true});
	});
	app.post('/test/users/:userId/set-pending-deletion', async (ctx) => {
		ensureHarnessAccess(ctx);
		const params = ctx.req.param() as {
			userId?: string;
		};
		const userIdParam = params.userId;
		if (!userIdParam) {
			throw new Error('Missing userId parameter');
		}
		const userId = createUserID(BigInt(userIdParam));
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
		const updates: UserDeletionScheduleUpdate = {pending_deletion_at: date};
		const shouldSetSelfDeleted = setSelfDeletedFlag !== false;
		if (shouldSetSelfDeleted) {
			const nextFlags = (user.flags ?? 0n) | UserFlags.SELF_DELETED;
			updates['flags'] = nextFlags;
		}
		const updated = await userRepository.updateDeletionSchedule(user, updates);
		const kvClient = getKVClient();
		const kvDeletionQueue = new KVAccountDeletionQueueService(kvClient, userRepository);
		try {
			await kvDeletionQueue.scheduleDeletion(userId, date, user.deletionReasonCode ?? 0);
		} finally {
		}
		return ctx.json(
			{
				success: true,
				pending_deletion_at: date.toISOString(),
				flags:
					(updates['flags'] as bigint | undefined)?.toString() ?? updated?.flags?.toString() ?? user.flags?.toString(),
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
			client_properties: clientProperties,
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
			origin: {
				ip: String(clientIp),
				userAgent: userAgent ? String(userAgent) : null,
				clientOs: parseReportedClientOs(clientProperties ? String(clientProperties) : null),
			},
			authToken: String(token),
			clientLocation: String(clientLocation),
			inviteCode: inviteCode ? String(inviteCode) : null,
			resendUsed: Boolean(resendUsed),
			createdAt,
		};
		const ttl = typeof ttlSeconds === 'number' && ttlSeconds > 0 ? ttlSeconds : seconds('15 minutes');
		await cacheService.set(getTicketCacheKey(String(ticket)), payload, ttl);
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
		const {
			ticket,
			token,
			user_id: userId,
		} = body as {
			ticket?: string;
			token?: string;
			user_id?: string;
		};
		if (!ticket || !token || !userId) {
			return ctx.json({error: 'ticket, token, and user_id are required'}, 400);
		}
		const payload = JSON.stringify({token, user_id: userId});
		await cacheService.publish(`ip-auth:${ticket}`, payload);
		await cacheService.set(`ip-auth-result:${ticket}`, payload, 60);
		return ctx.json({success: true});
	});
	app.post('/test/auth/ip-authorization/expire', async (ctx) => {
		ensureHarnessAccess(ctx);
		const cacheService = ctx.get('cacheService');
		const body = await ctx.req.json();
		const {ticket, token} = body as {
			ticket?: string;
			token?: string;
		};
		if (!ticket && !token) {
			return ctx.json({error: 'ticket or token is required'}, 400);
		}
		if (ticket) {
			await cacheService.delete(getTicketCacheKey(String(ticket)));
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
		const ttl = typeof ttlSeconds === 'number' && ttlSeconds > 0 ? ttlSeconds : seconds('5 minutes');
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
		const params = ctx.req.param() as {
			userId?: string;
		};
		const userIdParam = params.userId;
		if (!userIdParam) {
			throw new Error('Missing userId parameter');
		}
		const userId = createUserID(BigInt(userIdParam));
		const body = await ctx.req.json();
		const {
			premium_type: premiumType,
			premium_until: premiumUntil,
			premium_gift_extension_ends_at: premiumGiftExtensionEndsAt,
			premium_since: premiumSince,
			stripe_subscription_id: stripeSubscriptionId,
			stripe_customer_id: stripeCustomerId,
			premium_will_cancel: premiumWillCancel,
			premium_billing_cycle: premiumBillingCycle,
			premium_lifetime_sequence: premiumLifetimeSequence,
			first_refund_at: firstRefundAt,
			has_ever_purchased: hasEverPurchased,
		} = body as {
			premium_type?: number | null;
			premium_until?: string | null;
			premium_gift_extension_ends_at?: string | null;
			premium_since?: string | null;
			stripe_subscription_id?: string | null;
			stripe_customer_id?: string | null;
			premium_will_cancel?: boolean | null;
			premium_billing_cycle?: string | null;
			premium_lifetime_sequence?: number | null;
			first_refund_at?: string | null;
			has_ever_purchased?: boolean | null;
		};
		const userRepository = ctx.get('userRepository');
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const updates: Record<string, unknown> = {};
		if (premiumType !== undefined) {
			updates['premium_type'] = premiumType;
		}
		if (premiumUntil !== undefined) {
			updates['premium_until'] = premiumUntil ? new Date(premiumUntil) : null;
		}
		if (premiumGiftExtensionEndsAt !== undefined) {
			updates['premium_gift_extension_ends_at'] = premiumGiftExtensionEndsAt
				? new Date(premiumGiftExtensionEndsAt)
				: null;
		}
		if (premiumSince !== undefined) {
			updates['premium_since'] = premiumSince ? new Date(premiumSince) : null;
		}
		if (stripeSubscriptionId !== undefined) {
			updates['stripe_subscription_id'] = stripeSubscriptionId;
		}
		if (stripeCustomerId !== undefined) {
			updates['stripe_customer_id'] = stripeCustomerId;
		}
		if (premiumWillCancel !== undefined) {
			updates['premium_will_cancel'] = premiumWillCancel;
		}
		if (premiumBillingCycle !== undefined) {
			updates['premium_billing_cycle'] = premiumBillingCycle;
		}
		if (premiumLifetimeSequence !== undefined) {
			updates['premium_lifetime_sequence'] = premiumLifetimeSequence;
		}
		if (firstRefundAt !== undefined) {
			updates['first_refund_at'] = firstRefundAt ? new Date(firstRefundAt) : null;
		}
		if (hasEverPurchased !== undefined) {
			updates['has_ever_purchased'] = hasEverPurchased;
		}
		if (premiumType !== null && premiumType !== undefined && premiumType > 0) {
			if (!user.premiumSince && updates['premium_since'] === undefined) {
				updates['premium_since'] = new Date();
			}
		}
		if (Object.keys(updates).length === 0) {
			return ctx.json({success: true, updated: false});
		}
		await userRepository.patchUpsert(userId, updates, user.toRow());
		return ctx.json({
			success: true,
			updated: true,
			premium_type: updates['premium_type'] ?? user.premiumType,
			premium_until: updates['premium_until']
				? (updates['premium_until'] as Date).toISOString()
				: (user.premiumUntil?.toISOString() ?? null),
			has_ever_purchased: updates['has_ever_purchased'] ?? user.hasEverPurchased,
		});
	});
	app.post('/test/gifts/:code', async (ctx) => {
		ensureHarnessAccess(ctx);
		const params = ctx.req.param() as {
			code?: string;
		};
		const code = params.code;
		if (!code) {
			throw new Error('Missing code parameter');
		}
		const body = await ctx.req.json();
		const {
			duration_months: durationMonths,
			duration_type: durationTypeInput,
			duration_quantity: durationQuantityInput,
			created_by_user_id: createdByUserIdParam,
			visionary_sequence_number: visionarySequenceNumber,
		} = body as {
			duration_months?: number;
			duration_type?: string;
			duration_quantity?: number;
			created_by_user_id?: string;
			visionary_sequence_number?: number | null;
		};
		const {durationType, durationQuantity} = parseGiftDurationInput({
			duration_months: durationMonths,
			duration_type: durationTypeInput,
			duration_quantity: durationQuantityInput,
		});
		if (!createdByUserIdParam) {
			throw new Error('Missing created_by_user_id');
		}
		const createdByUserId = createUserID(BigInt(createdByUserIdParam));
		const userRepository = ctx.get('userRepository');
		await userRepository.createGiftCode({
			code,
			duration_months: null,
			duration_type: durationType,
			duration_quantity: durationQuantity,
			created_at: new Date(),
			created_by_user_id: createdByUserId,
			redeemed_at: null,
			redeemed_by_user_id: null,
			stripe_payment_intent_id: null,
			visionary_sequence_number: visionarySequenceNumber ?? null,
			checkout_session_id: null,
			version: 1,
		});
		return ctx.json({success: true, code});
	});
	app.post('/test/users/:userId/extend-subscription-trial', async (ctx) => {
		ensureHarnessAccess(ctx);
		const params = ctx.req.param() as {
			userId?: string;
		};
		const userIdParam = params.userId;
		if (!userIdParam) {
			throw new Error('Missing userId parameter');
		}
		const userId = createUserID(BigInt(userIdParam));
		const body = (await ctx.req.json()) as {
			duration_months?: number;
			duration_type?: string;
			duration_quantity?: number;
			idempotency_key?: string;
		};
		const idempotencyKey = body.idempotency_key;
		if (!idempotencyKey) {
			throw new Error('Missing idempotency_key');
		}
		const {durationType, durationQuantity} = parseGiftDurationInput(body);
		const userRepository = ctx.get('userRepository');
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const stripeService = ctx.get('stripeService');
		await stripeService.extendSubscriptionWithGiftTrialDuration(user, durationType, durationQuantity, idempotencyKey);
		return ctx.body(null, 204);
	});
	app.post('/test/users/:userId/private-channels', async (ctx) => {
		ensureHarnessAccess(ctx);
		const params = ctx.req.param() as {
			userId?: string;
		};
		const userIdParam = params.userId;
		if (!userIdParam) {
			throw new Error('Missing userId parameter');
		}
		const userId = createUserID(BigInt(userIdParam));
		const body = (await ctx.req.json()) as Record<string, unknown>;
		const parseCount = (value: unknown, name: string): number => {
			if (value === undefined || value === null) {
				return 0;
			}
			if (typeof value !== 'number' || !Number.isInteger(value) || value < 0 || value > MAX_TEST_PRIVATE_CHANNELS) {
				throw InputValidationError.fromCode(name, ValidationErrorCodes.VALUE_MUST_BE_INTEGER_IN_RANGE, {
					name,
					minValue: 0,
					maxValue: MAX_TEST_PRIVATE_CHANNELS,
				});
			}
			return value;
		};
		const dmCount = parseCount(body['dm_count'], 'dm_count');
		const groupDmCount = parseCount(body['group_dm_count'], 'group_dm_count');
		const recipientsInput = Array.isArray(body['recipients']) ? body['recipients'] : [];
		if ((dmCount > 0 || groupDmCount > 0) && recipientsInput.length === 0) {
			throw InputValidationError.fromCode('recipients', ValidationErrorCodes.AT_LEAST_ONE_RECIPIENT_REQUIRED);
		}
		const recipients = recipientsInput.map((raw) => {
			if (typeof raw !== 'string') {
				throw InputValidationError.fromCode('recipients', ValidationErrorCodes.RECIPIENT_IDS_MUST_BE_STRINGS);
			}
			const trimmed = raw.trim();
			if (trimmed === '') {
				throw InputValidationError.fromCode('recipients', ValidationErrorCodes.RECIPIENT_IDS_CANNOT_BE_EMPTY);
			}
			try {
				return createUserID(BigInt(trimmed));
			} catch {
				throw InputValidationError.fromCode('recipients', ValidationErrorCodes.RECIPIENT_IDS_MUST_BE_VALID_SNOWFLAKES);
			}
		});
		const clearExisting = Boolean(body['clear_existing']);
		const channelRepository = new ChannelRepository();
		const userRepository = ctx.get('userRepository');
		const snowflakeService = ctx.get('snowflakeService');
		if (clearExisting) {
			await userRepository.deleteAllPrivateChannels(userId);
		}
		const dmChannels: Array<{
			channel_id: string;
			last_message_id: string;
		}> = [];
		for (let i = 0; i < dmCount; i++) {
			const recipientId = recipients[i % recipients.length];
			const channelId = createChannelID(await snowflakeService.generate());
			const lastMessageId = createMessageID(await snowflakeService.generateForChannel(channelId));
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
				voice_connection_limit: null,
				rtc_region: null,
				rtc_p2p: null,
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
		const groupDmChannels: Array<{
			channel_id: string;
			last_message_id: string;
		}> = [];
		if (groupDmCount > 0) {
			const recipientsSet = new Set<UserID>([userId, ...recipients]);
			for (let i = 0; i < groupDmCount; i++) {
				const channelId = createChannelID(await snowflakeService.generate());
				const lastMessageId = createMessageID(await snowflakeService.generateForChannel(channelId));
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
					voice_connection_limit: null,
					rtc_region: null,
					rtc_p2p: null,
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
		const params = ctx.req.param() as {
			guildId?: string;
		};
		const guildIdParam = params.guildId;
		if (!guildIdParam) {
			throw new Error('Missing guildId parameter');
		}
		const guildId = createGuildID(BigInt(guildIdParam));
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
		await guildRepository.upsertPartial(guildId, {features: newFeatures}, guild.toRow());
		return ctx.json({
			success: true,
			features: Array.from(newFeatures),
		});
	});
	app.post('/test/guilds/:guildId/member-count', async (ctx) => {
		ensureHarnessAccess(ctx);
		const params = ctx.req.param() as {
			guildId?: string;
		};
		const guildIdParam = params.guildId;
		if (!guildIdParam) {
			throw new Error('Missing guildId parameter');
		}
		const guildId = createGuildID(BigInt(guildIdParam));
		const body = await ctx.req.json();
		const memberCount = (
			body as {
				member_count?: unknown;
			}
		).member_count;
		if (
			typeof memberCount !== 'number' ||
			!Number.isInteger(memberCount) ||
			memberCount < 0 ||
			memberCount > MAX_GUILD_MEMBERS_VERY_LARGE_GUILD
		) {
			throw InputValidationError.fromCode('member_count', ValidationErrorCodes.VALUE_MUST_BE_INTEGER_IN_RANGE, {
				name: 'member_count',
				minValue: 0,
				maxValue: MAX_GUILD_MEMBERS_VERY_LARGE_GUILD,
			});
		}
		const guildRepository = new GuildRepository();
		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			throw new UnknownGuildError();
		}
		await guildRepository.upsertPartial(guildId, {member_count: memberCount}, guild.toRow());
		return ctx.json({
			success: true,
			member_count: memberCount,
		});
	});
	app.get('/test/users/:userId/data-exists', async (ctx) => {
		ensureHarnessAccess(ctx);
		const params = ctx.req.param() as {
			userId?: string;
		};
		const userIdParam = params.userId;
		if (!userIdParam) {
			throw new Error('Missing userId parameter');
		}
		const userId = createUserID(BigInt(userIdParam));
		Logger.info({userId: userId.toString()}, '[test/users/:userId/data-exists] Request received');
		const userRepository = new UserRepository();
		const userChannelRepository = new UserChannelRepository();
		const authSessionRepository = new AuthSessionRepository();
		const user = await userRepository.findUnique(userId);
		if (!user) {
			Logger.info({userId: userId.toString()}, '[test/users/:userId/data-exists] User not found');
			return ctx.json({user_exists: false}, 200);
		}
		Logger.info(
			{
				userId: userId.toString(),
				flags: user.flags?.toString() ?? 'null',
				pendingDeletionAt: user.pendingDeletionAt?.toISOString() ?? 'null',
			},
			'[test/users/:userId/data-exists] User found',
		);
		const [relationships, sessions, oauthAccessTokens, pinnedDms, savedMessages] = await Promise.all([
			userRepository.listRelationships(userId),
			authSessionRepository.listAuthSessions(userId),
			fetchMany<{
				token_: string;
			}>(
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
		Logger.info(
			{
				userId: userId.toString(),
				rawFlags: user.flags?.toString() ?? 'null',
				SELF_DELETED_FLAG_VALUE: UserFlags.SELF_DELETED.toString(),
				DELETED_FLAG_VALUE: UserFlags.DELETED.toString(),
				hasSelfDeletedFlag,
				hasDeletedFlag,
			},
			'[test/users/:userId/data-exists] Flag check results',
		);
		const response = {
			user_exists: true,
			email_cleared: user.email === null,
			password_cleared: user.passwordHash === null,
			flags: user.flags?.toString(),
			has_deleted_flag: hasDeletedFlag,
			has_self_deleted_flag: hasSelfDeletedFlag,
			pending_deletion_at: user.pendingDeletionAt ? user.pendingDeletionAt.toISOString() : null,
			deletion_reason_code: user.deletionReasonCode ?? null,
			relationships_count: relationships.length,
			sessions_count: sessions.length,
			oauth_tokens_count: oauthAccessTokens.length,
			pinned_dms_count: pinnedDms.length,
			saved_messages_count: savedMessages.length,
		};
		Logger.info({userId: userId.toString(), response}, '[test/users/:userId/data-exists] Returning response');
		return ctx.json(response, 200);
	});
	app.get('/test/users/:userId/messages/count', async (ctx) => {
		ensureHarnessAccess(ctx);
		const params = ctx.req.param() as {
			userId?: string;
		};
		const userIdParam = params.userId;
		if (!userIdParam) {
			throw new Error('Missing userId parameter');
		}
		const userId = createUserID(BigInt(userIdParam));
		const channelRepository = new ChannelRepository();
		const messages = await channelRepository.listMessagesByAuthor(userId);
		return ctx.json({count: messages.length}, 200);
	});
	app.post('/test/worker/process-pending-deletions', async (ctx) => {
		ensureHarnessAccess(ctx);
		Logger.info({}, '[test/worker/process-pending-deletions] Request received');
		const userRepository = new UserRepository();
		const kvClient = getKVClient();
		const kvDeletionQueue = new KVAccountDeletionQueueService(kvClient, userRepository);
		const snowflakeService = getSnowflakeService();
		const workerDeps = await initializeWorkerDepsWithHarnessEmail(ctx, snowflakeService);
		const now = Date.now();
		Logger.info(
			{now: new Date(now).toISOString()},
			'[test/worker/process-pending-deletions] Looking for ready deletions',
		);
		const readyDeletions = await kvDeletionQueue.getReadyDeletions(now, 100);
		Logger.info(
			{count: readyDeletions.length},
			'[test/worker/process-pending-deletions] Found ready deletions from KV',
		);
		let processed = 0;
		const errors: Array<{
			userId: string;
			error: string;
		}> = [];
		for (const deletion of readyDeletions) {
			try {
				const userId = createUserID(deletion.userId);
				Logger.info(
					{userId: userId.toString(), deletionReasonCode: deletion.deletionReasonCode},
					'[test/worker/process-pending-deletions] Processing deletion',
				);
				const user = await userRepository.findUnique(userId);
				if (!user?.pendingDeletionAt) {
					Logger.info(
						{
							userId: userId.toString(),
							userFound: !!user,
							hasPendingDeletion: !!user?.pendingDeletionAt,
						},
						'[test/worker/process-pending-deletions] User not found or no pending deletion',
					);
					continue;
				}
				const pendingDeletionAt = user.pendingDeletionAt;
				await processUserDeletion(userId, pendingDeletionAt, deletion.deletionReasonCode, workerDeps);
				await kvDeletionQueue.removeFromQueue(userId);
				Logger.info({userId: userId.toString()}, '[test/worker/process-pending-deletions] Deletion completed');
				processed++;
			} catch (error) {
				const errorMessage = error instanceof Error ? error.message : String(error);
				Logger.error(
					{userId: deletion.userId.toString(), error: errorMessage},
					'[test/worker/process-pending-deletions] Error processing deletion',
				);
				errors.push({
					userId: deletion.userId.toString(),
					error: errorMessage,
				});
			}
		}
		Logger.info({processed, total: readyDeletions.length}, '[test/worker/process-pending-deletions] Completed');
		await snowflakeService.shutdown();
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
		const params = ctx.req.param() as {
			userId?: string;
		};
		const userIdParam = params.userId;
		if (!userIdParam) {
			throw new Error('Missing userId parameter');
		}
		const userId = createUserID(BigInt(userIdParam));
		Logger.info({userId: userId.toString()}, '[test/worker/process-pending-deletion/:userId] Request received');
		const userRepository = new UserRepository();
		const user = await userRepository.findUnique(userId);
		if (!user) {
			Logger.info({userId: userId.toString()}, '[test/worker/process-pending-deletion/:userId] User not found');
			throw new UnknownUserError();
		}
		if (!user.pendingDeletionAt) {
			Logger.info(
				{userId: userId.toString()},
				'[test/worker/process-pending-deletion/:userId] User has no pending deletion',
			);
			throw new NoPendingDeletionError();
		}
		const deletionReasonCode = user.deletionReasonCode ?? 0;
		Logger.info(
			{
				userId: userId.toString(),
				deletionReasonCode,
				pendingDeletionAt: user.pendingDeletionAt.toISOString(),
			},
			'[test/worker/process-pending-deletion/:userId] Running deletion synchronously',
		);
		try {
			const snowflakeService = getSnowflakeService();
			const workerDeps = await initializeWorkerDepsWithHarnessEmail(ctx, snowflakeService);
			await processUserDeletion(userId, user.pendingDeletionAt, deletionReasonCode, workerDeps);
			Logger.info(
				{userId: userId.toString()},
				'[test/worker/process-pending-deletion/:userId] Deletion completed successfully',
			);
			return ctx.json(
				{
					success: true,
					userId: userId.toString(),
					deletionReasonCode,
				},
				200,
			);
		} catch (error) {
			Logger.error(
				{userId: userId.toString(), error: error instanceof Error ? error.message : String(error)},
				'[test/worker/process-pending-deletion/:userId] Deletion failed',
			);
			throw new DeletionFailedError(error instanceof Error ? error.message : String(error));
		}
	});
	app.post('/test/worker/process-inactivity-deletions', async (ctx) => {
		ensureHarnessAccess(ctx);
		Logger.info({}, '[test/worker/process-inactivity-deletions] Request received');
		const snowflakeService = getSnowflakeService();
		try {
			const workerDeps = await initializeWorkerDepsWithHarnessEmail(ctx, snowflakeService);
			setWorkerDependencies(workerDeps);
			const result = await processInactivityDeletionsCore({
				kvClient: workerDeps.kvClient,
				userRepository: workerDeps.userRepository,
				deletionQueueService: workerDeps.deletionQueueService,
				gatewayService: workerDeps.gatewayService,
				emailService: workerDeps.emailService,
				isEmailEnabled: () => workerDeps.instanceConfigRepository.isEmailEnabled(),
				activityTracker: workerDeps.activityTracker,
				deletionEligibilityService: workerDeps.deletionEligibilityService,
			});
			Logger.info(result, '[test/worker/process-inactivity-deletions] Completed successfully');
			return ctx.json(
				{
					success: true,
					processed: result.warningsSent + result.deletionsScheduled,
					warnings_sent: result.warningsSent,
					deletions_scheduled: result.deletionsScheduled,
					errors: result.errors,
				},
				200,
			);
		} catch (error) {
			Logger.error(
				{error: error instanceof Error ? error.message : String(error)},
				'[test/worker/process-inactivity-deletions] Failed',
			);
			throw new ProcessingFailedError(error instanceof Error ? error.message : String(error));
		} finally {
			await snowflakeService.shutdown();
		}
	});
	app.get('/test/attachment-decay/:attachment_id', async (ctx) => {
		ensureHarnessAccess(ctx);
		const params = ctx.req.param() as {
			attachment_id?: string;
		};
		const attachmentIdParam = params.attachment_id;
		if (!attachmentIdParam) {
			throw new Error('Missing attachment_id parameter');
		}
		const attachmentId = createAttachmentID(BigInt(attachmentIdParam));
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
	app.post('/test/users/:userId/set-last-active-at', async (ctx) => {
		ensureHarnessAccess(ctx);
		const params = ctx.req.param() as {
			userId?: string;
		};
		const userIdParam = params.userId;
		if (!userIdParam) {
			throw new Error('Missing userId parameter');
		}
		const userId = createUserID(BigInt(userIdParam));
		const body = await ctx.req.json();
		const {timestamp} = body as {
			timestamp?: string;
		};
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
		Logger.info(
			{userId: userId.toString(), timestamp: date.toISOString()},
			'[test/users/:userId/set-last-active-at] Request received',
		);
		try {
			const userRepository = new UserRepository();
			const kvClient = getKVClient();
			try {
				const user = await userRepository.findUnique(userId);
				if (!user) {
					throw new UnknownUserError();
				}
				await userRepository.patchUpsert(
					userId,
					{
						last_active_at: date,
					},
					user.toRow(),
				);
				const activityTracker = new KVActivityTracker(kvClient);
				await activityTracker.updateActivity(userId, date);
				Logger.info(
					{userId: userId.toString(), timestamp: date.toISOString()},
					'[test/users/:userId/set-last-active-at] Updated successfully',
				);
				return ctx.json(
					{
						success: true,
						userId: userId.toString(),
						last_active_at: date.toISOString(),
					},
					200,
				);
			} finally {
			}
		} catch (error) {
			Logger.error(
				{userId: userId.toString(), error: error instanceof Error ? error.message : String(error)},
				'[test/users/:userId/set-last-active-at] Failed',
			);
			throw new UpdateFailedError(error instanceof Error ? error.message : String(error));
		}
	});
	app.post('/test/users/:userId/set-bot-flag', async (ctx) => {
		ensureHarnessAccess(ctx);
		const params = ctx.req.param() as {
			userId?: string;
		};
		const userIdParam = params.userId;
		if (!userIdParam) {
			throw new Error('Missing userId parameter');
		}
		const userId = createUserID(BigInt(userIdParam));
		const body = await ctx.req.json();
		const {is_bot: isBot} = body as {
			is_bot?: boolean;
		};
		if (typeof isBot !== 'boolean') {
			throw new InvalidBotFlagError();
		}
		Logger.info({userId: userId.toString(), isBot}, '[test/users/:userId/set-bot-flag] Request received');
		try {
			const userRepository = new UserRepository();
			const user = await userRepository.findUnique(userId);
			if (!user) {
				throw new UnknownUserError();
			}
			await userRepository.patchUpsert(
				userId,
				{
					bot: isBot,
				},
				user.toRow(),
			);
			Logger.info({userId: userId.toString(), isBot}, '[test/users/:userId/set-bot-flag] Updated successfully');
			return ctx.json(
				{
					success: true,
					userId: userId.toString(),
					is_bot: isBot,
				},
				200,
			);
		} catch (error) {
			Logger.error(
				{userId: userId.toString(), error: error instanceof Error ? error.message : String(error)},
				'[test/users/:userId/set-bot-flag] Failed',
			);
			throw new UpdateFailedError(error instanceof Error ? error.message : String(error));
		}
	});
	app.post('/test/users/:userId/set-system-flag', async (ctx) => {
		ensureHarnessAccess(ctx);
		const params = ctx.req.param() as {
			userId?: string;
		};
		const userIdParam = params.userId;
		if (!userIdParam) {
			throw new Error('Missing userId parameter');
		}
		const userId = createUserID(BigInt(userIdParam));
		const body = await ctx.req.json();
		const {is_system: isSystem} = body as {
			is_system?: boolean;
		};
		if (typeof isSystem !== 'boolean') {
			throw new InvalidSystemFlagError();
		}
		Logger.info({userId: userId.toString(), isSystem}, '[test/users/:userId/set-system-flag] Request received');
		try {
			const userRepository = new UserRepository();
			const user = await userRepository.findUnique(userId);
			if (!user) {
				throw new UnknownUserError();
			}
			await userRepository.patchUpsert(
				userId,
				{
					system: isSystem,
				},
				user.toRow(),
			);
			Logger.info({userId: userId.toString(), isSystem}, '[test/users/:userId/set-system-flag] Updated successfully');
			return ctx.json(
				{
					success: true,
					userId: userId.toString(),
					is_system: isSystem,
				},
				200,
			);
		} catch (error) {
			Logger.error(
				{userId: userId.toString(), error: error instanceof Error ? error.message : String(error)},
				'[test/users/:userId/set-system-flag] Failed',
			);
			throw new UpdateFailedError(error instanceof Error ? error.message : String(error));
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
			throw InputValidationError.fromCode('channel_id', ValidationErrorCodes.CHANNEL_ID_IS_REQUIRED);
		}
		if (!body.messages || !Array.isArray(body.messages) || body.messages.length === 0) {
			throw InputValidationError.fromCode('messages', ValidationErrorCodes.MESSAGES_ARRAY_REQUIRED_AND_NOT_EMPTY);
		}
		const channelId = createChannelID(BigInt(body.channel_id));
		const defaultAuthorId = body.author_id ? createUserID(BigInt(body.author_id)) : null;
		const skipBucketIndex = body.skip_bucket_index ?? false;
		if (body.clear_existing) {
			await deleteOneOrMany(ChannelMessageBuckets.deletePartition({channel_id: channelId}));
			await deleteOneOrMany(ChannelEmptyBuckets.deletePartition({channel_id: channelId}));
		}
		const seededMessages: Array<{
			message_id: string;
			bucket: number;
			timestamp: string;
		}> = [];
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
				timestamp = snowflakeToDate(messageId).getTime();
			} else if (input.timestamp) {
				const parsedDate = new Date(input.timestamp);
				if (Number.isNaN(parsedDate.getTime())) {
					throw new InvalidTimestampError(`Invalid timestamp at index ${i}: ${input.timestamp}`);
				}
				timestamp = parsedDate.getTime();
				const baseSnowflake = createSnowflakeFromTimestamp(timestamp);
				messageId = createMessageID(baseSnowflake | BigInt(i & 0xfff));
			} else {
				timestamp = Date.now() + i;
				messageId = createMessageID(createSnowflakeFromTimestamp(timestamp) | BigInt(i & 0xfff));
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
			if (authorId != null) {
				batch.addPrepared(
					MessagesByAuthorV2.upsertAll({
						author_id: authorId,
						channel_id: channelId,
						message_id: messageId,
					}),
				);
			}
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
	app.post('/test/channels/:channelId/mark-indexed', async (ctx) => {
		ensureHarnessAccess(ctx);
		const params = ctx.req.param() as {
			channelId?: string;
		};
		const channelIdParam = params.channelId;
		if (!channelIdParam) {
			throw new Error('Missing channelId parameter');
		}
		const channelId = createChannelID(BigInt(channelIdParam));
		const channelRepository = new ChannelRepository();
		const userRepository = new UserRepository();
		const channel = await channelRepository.findUnique(channelId);
		if (!channel) {
			return ctx.json({error: 'Channel not found'}, 404);
		}
		const {getMessageSearchService} = await import('@app/api/SearchFactory');
		const searchService = getMessageSearchService();
		let messagesIndexed = 0;
		if (searchService) {
			const BATCH_SIZE = 100;
			let lastMessageId: MessageID | undefined;
			let hasMore = true;
			while (hasMore) {
				const messages = await channelRepository.listMessages(channelId, lastMessageId, BATCH_SIZE);
				if (messages.length === 0) {
					hasMore = false;
					break;
				}
				const authorIds = new Set(messages.map((m) => m.authorId).filter((id): id is UserID => id !== null));
				const authorBotMap = new Map<UserID, boolean>();
				for (const authorId of authorIds) {
					const user = await userRepository.findUnique(authorId);
					if (user) {
						authorBotMap.set(authorId, user.isBot);
					}
				}
				await searchService.indexMessages(messages, authorBotMap);
				messagesIndexed += messages.length;
				if (messages.length < BATCH_SIZE) {
					hasMore = false;
				} else {
					lastMessageId = messages[messages.length - 1]!.id;
				}
			}
		}
		await channelRepository.upsert({
			...channel.toRow(),
			indexed_at: new Date(),
		});
		return ctx.json(
			{
				channel_id: channelId.toString(),
				indexed_at: new Date().toISOString(),
				messages_indexed: messagesIndexed,
			},
			200,
		);
	});
	app.post('/test/guilds/:guildId/mark-members-indexed', async (ctx) => {
		ensureHarnessAccess(ctx);
		const params = ctx.req.param() as {
			guildId?: string;
		};
		const guildIdParam = params.guildId;
		if (!guildIdParam) {
			throw new Error('Missing guildId parameter');
		}
		const guildId = createGuildID(BigInt(guildIdParam));
		const guildRepository = new GuildRepository();
		const userRepository = new UserRepository();
		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			return ctx.json({error: 'Guild not found'}, 404);
		}
		const {getGuildMemberSearchService} = await import('@app/api/SearchFactory');
		const searchService = getGuildMemberSearchService();
		let membersIndexed = 0;
		if (searchService) {
			const members = await guildRepository.listMembers(guildId);
			const membersWithUsers = [];
			for (const member of members) {
				const user = await userRepository.findUnique(member.userId);
				if (user) {
					membersWithUsers.push({member, user});
				}
			}
			if (membersWithUsers.length > 0) {
				await searchService.indexMembers(membersWithUsers);
				membersIndexed = membersWithUsers.length;
			}
		}
		await guildRepository.upsertPartial(guildId, {members_indexed_at: new Date()}, guild.toRow());
		return ctx.json(
			{
				guild_id: guildId.toString(),
				indexed_at: new Date().toISOString(),
				members_indexed: membersIndexed,
			},
			200,
		);
	});
	app.post('/test/oauth2/access-token', async (ctx) => {
		ensureHarnessAccess(ctx);
		const body = await ctx.req.json();
		const {
			user_id: userIdInput,
			scopes: scopesInput,
			application_id: applicationIdInput,
		} = body as {
			user_id?: string;
			scopes?: Array<string>;
			application_id?: string;
		};
		if (!userIdInput) {
			return ctx.json({error: 'user_id is required'}, 400);
		}
		const userId = createUserID(BigInt(userIdInput));
		const userRepository = new UserRepository();
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const scopeSet = new Set(scopesInput ?? ['identify', 'email']);
		const applicationId = applicationIdInput
			? createApplicationID(BigInt(applicationIdInput))
			: createApplicationID(BigInt(1));
		const snowflakeService = ctx.get('snowflakeService');
		const tokenId = await snowflakeService.generate();
		const token = `test_oauth_${tokenId.toString()}`;
		const oauth2TokenRepository = new OAuth2TokenRepository();
		await oauth2TokenRepository.createAccessToken({
			token_: token,
			application_id: applicationId,
			user_id: userId,
			scope: scopeSet,
			created_at: new Date(),
		});
		return ctx.json(
			{
				token,
				user_id: userId.toString(),
				scopes: Array.from(scopeSet),
				application_id: applicationId.toString(),
			},
			200,
		);
	});
	app.post('/test/users/:userId/set-contact-info', async (ctx) => {
		ensureHarnessAccess(ctx);
		const params = ctx.req.param() as {
			userId?: string;
		};
		const userIdParam = params.userId;
		if (!userIdParam) {
			throw new Error('Missing userId parameter');
		}
		const userId = createUserID(BigInt(userIdParam));
		const body = await ctx.req.json();
		const {email} = body as {
			email?: string | null;
		};
		const userRepository = new UserRepository();
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const updates: Record<string, unknown> = {};
		if (email !== undefined) {
			updates['email'] = email;
		}
		if (Object.keys(updates).length === 0) {
			return ctx.json({success: true, updated: false});
		}
		await userRepository.patchUpsert(userId, updates, user.toRow());
		const updatedUser = await userRepository.findUnique(userId);
		if (updatedUser) {
			const searchRepo = new UserSearchRepository();
			await searchRepo.indexUser(updatedUser);
		}
		return ctx.json({
			success: true,
			updated: true,
			email: updates['email'] ?? user.email,
		});
	});
	app.post('/test/cache-clear', async (ctx) => {
		ensureHarnessAccess(ctx);
		const cacheService = ctx.get('cacheService');
		let totalDeleted = 0;
		let deleted: number;
		do {
			deleted = await cacheService.deletePattern('*');
			totalDeleted += deleted;
		} while (deleted > 0);
		Logger.info({totalDeleted}, 'Cleared KV cache via test harness');
		return ctx.json({cleared: true, deleted_count: totalDeleted});
	});
	app.post('/test/rpc-session-init', async (ctx) => {
		ensureHarnessAccess(ctx);
		const request = RpcRequest.parse(await ctx.req.json());
		const response = await ctx.get('rpcService').handleRpcRequest({request, requestCache: ctx.get('requestCache')});
		return ctx.json(response);
	});
}
