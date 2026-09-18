// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AttachmentID, ChannelID, GuildID, MessageID, UserID} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';

const WINDOW_MS = 600000;
const USER_THRESHOLD = 60;
const GUILD_THRESHOLD = 120;
const FRESH_GUILD_THRESHOLD = 20;
const FRESH_GUILD_MAX_AGE_MS = 604800000;
const SEGMENT_MAX_BYTES = 16 * 1024 * 1024;
const SEGMENT_MAX_DURATION_SECONDS = 30;

const KEY_PREFIX = 'abuse:upload_segment:';
const WINDOW_TTL_SECONDS = WINDOW_MS / 1000;
const MAX_IN_FLIGHT_SIGNALS = 32;
const SURFACE = 'message_attachment';
const SIGNAL_MESSAGE = 'content_moderation.upload_segment_pattern';
const FAILURE_MESSAGE = 'content_moderation.upload_segment_signal_failed';
const DROPPED_MESSAGE = 'content_moderation.upload_segment_signal_dropped';
const PLAYLIST_CONTENT_TYPES = new Set(['application/vnd.apple.mpegurl', 'application/x-mpegurl']);

export type UploadSegmentScope = 'user' | 'guild' | 'fresh_guild';

export interface UploadSegmentAttachment {
	attachmentId: AttachmentID;
	uploadKey: string;
	filename: string;
	contentType: string;
	size: bigint;
	duration: number | null;
	waveform: string | null;
	sniffedContentType: string | null;
	requestIp: string | null;
}

export interface UploadSegmentSignalInput {
	userId: UserID;
	guildId: GuildID | null;
	guildOwnerId: UserID | null;
	channelId: ChannelID;
	messageId: MessageID;
	attachments: ReadonlyArray<UploadSegmentAttachment>;
}

interface CountedScope {
	scope: UploadSegmentScope;
	key: string;
	threshold: number;
}

function baseContentType(contentType: string): string {
	const [base] = contentType.split(';');
	return (base ?? '').trim().toLowerCase();
}

export function isSegmentShapedAttachment(attachment: UploadSegmentAttachment): boolean {
	if (attachment.waveform !== null) {
		return false;
	}
	if (attachment.sniffedContentType !== null) {
		return true;
	}
	const contentType = baseContentType(attachment.contentType);
	const isSegmentType =
		contentType.startsWith('video/') || contentType.startsWith('audio/') || PLAYLIST_CONTENT_TYPES.has(contentType);
	if (!isSegmentType) {
		return false;
	}
	if (attachment.size > BigInt(SEGMENT_MAX_BYTES)) {
		return false;
	}
	return attachment.duration === null || attachment.duration <= SEGMENT_MAX_DURATION_SECONDS;
}

export function isRungValue(count: number, threshold: number): boolean {
	if (count < threshold || count % threshold !== 0) {
		return false;
	}
	const multiple = count / threshold;
	return (multiple & (multiple - 1)) === 0;
}

function uploaderOwnsGuild(input: UploadSegmentSignalInput): boolean {
	return input.guildOwnerId !== null && input.guildOwnerId === input.userId;
}

function resolveScopes(input: UploadSegmentSignalInput, windowIndex: number, nowMs: number): Array<CountedScope> {
	const scopes: Array<CountedScope> = [
		{
			scope: 'user',
			key: `${KEY_PREFIX}user:${input.userId.toString()}:${windowIndex}`,
			threshold: USER_THRESHOLD,
		},
	];
	if (input.guildId === null) {
		return scopes;
	}
	const guildAgeMs = nowMs - snowflakeToDate(input.guildId).getTime();
	const isFreshGuild = uploaderOwnsGuild(input) && guildAgeMs < FRESH_GUILD_MAX_AGE_MS;
	scopes.push({
		scope: isFreshGuild ? 'fresh_guild' : 'guild',
		key: `${KEY_PREFIX}guild:${input.guildId.toString()}:${windowIndex}`,
		threshold: isFreshGuild ? FRESH_GUILD_THRESHOLD : GUILD_THRESHOLD,
	});
	return scopes;
}

function buildSignalFields(params: {
	input: UploadSegmentSignalInput;
	segments: ReadonlyArray<UploadSegmentAttachment>;
	windowIndex: number;
	scope: CountedScope;
	count: number;
}): Record<string, unknown> {
	const {input, segments, windowIndex, scope, count} = params;
	const requestIps = new Set<string>();
	for (const segment of segments) {
		if (segment.requestIp !== null) {
			requestIps.add(segment.requestIp);
		}
	}
	return {
		surface: SURFACE,
		scope: scope.scope,
		count,
		threshold: scope.threshold,
		windowMs: WINDOW_MS,
		windowStartedAt: new Date(windowIndex * WINDOW_MS).toISOString(),
		userId: input.userId.toString(),
		userCreatedAt: snowflakeToDate(input.userId).toISOString(),
		guildId: input.guildId === null ? null : input.guildId.toString(),
		guildCreatedAt: input.guildId === null ? null : snowflakeToDate(input.guildId).toISOString(),
		guildOwnerId: input.guildOwnerId === null ? null : input.guildOwnerId.toString(),
		uploaderOwnsGuild: uploaderOwnsGuild(input),
		channelId: input.channelId.toString(),
		messageId: input.messageId.toString(),
		attachmentIds: segments.map((segment) => segment.attachmentId.toString()),
		uploadKeys: segments.map((segment) => segment.uploadKey),
		requestIps: Array.from(requestIps),
		filenames: segments.map((segment) => segment.filename),
		contentTypes: segments.map((segment) => segment.contentType),
		disguisedCount: segments.filter((segment) => segment.sniffedContentType !== null).length,
	};
}

function scopeFields(input: UploadSegmentSignalInput): Record<string, unknown> {
	return {
		surface: SURFACE,
		userId: input.userId.toString(),
		guildId: input.guildId === null ? null : input.guildId.toString(),
		channelId: input.channelId.toString(),
		messageId: input.messageId.toString(),
	};
}

const inFlightSignals = new Set<Promise<void>>();

export function scheduleUploadSegmentSignal(input: UploadSegmentSignalInput): void {
	if (!input.attachments.some(isSegmentShapedAttachment)) {
		return;
	}
	if (inFlightSignals.size >= MAX_IN_FLIGHT_SIGNALS) {
		Logger.warn(scopeFields(input), DROPPED_MESSAGE);
		return;
	}
	const pending = recordUploadSegmentSignal(input).finally(() => {
		inFlightSignals.delete(pending);
	});
	inFlightSignals.add(pending);
}

export async function flushUploadSegmentSignals(): Promise<void> {
	while (inFlightSignals.size > 0) {
		await Promise.all([...inFlightSignals]);
	}
}

export async function recordUploadSegmentSignal(input: UploadSegmentSignalInput): Promise<void> {
	try {
		const segments = input.attachments.filter(isSegmentShapedAttachment);
		if (segments.length === 0) {
			return;
		}
		const nowMs = Date.now();
		const windowIndex = Math.floor(nowMs / WINDOW_MS);
		const scopes = resolveScopes(input, windowIndex, nowMs);
		const kv = getKVClient();
		const counts: Array<number> = [];
		for (const scope of scopes) {
			const count = await kv.incr(scope.key);
			counts.push(count);
			if (count === 1) {
				await kv.expire(scope.key, WINDOW_TTL_SECONDS);
			}
		}
		for (const [index, scope] of scopes.entries()) {
			const count = counts[index];
			if (!isRungValue(count, scope.threshold)) {
				continue;
			}
			Logger.warn(buildSignalFields({input, segments, windowIndex, scope, count}), SIGNAL_MESSAGE);
		}
	} catch (error) {
		Logger.warn({error, ...scopeFields(input)}, FAILURE_MESSAGE);
	}
}
