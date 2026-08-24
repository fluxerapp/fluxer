// SPDX-License-Identifier: AGPL-3.0-or-later

import {MS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';
import {compare as compareSnowflakes, extractTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';

export const OLD_MESSAGE_AGE_THRESHOLD = 7 * MS_PER_DAY;
export const RECENT_MESSAGE_THRESHOLD = 3 * MS_PER_DAY;
export const ACK_BATCH_DELAY_MS = 3000;
export const ACK_BATCH_SIZE = 100;
export const ACK_RETRY_BASE_DELAY_MS = 5000;
export const ACK_RETRY_MAX_DELAY_MS = 60000;

export type Timer = NodeJS.Timeout;

export interface GatewayReadState {
	id: string;
	mention_count?: number;
	last_message_id?: string | null;
	last_pin_timestamp?: string | null;
	version?: string;
}

export interface ReadStateAckRequestEntry {
	channel_id: string;
	message_id: string;
	mention_count?: number;
	manual?: boolean;
}

export interface ReadStateAckResponse {
	read_states: Array<GatewayReadState>;
	read_state_proto?: string;
}

export interface ChannelPayload {
	id: string;
	type: number;
	guild_id?: string;
	last_message_id?: string | null;
	last_pin_timestamp?: string | null;
}

export interface AckOptions {
	messageId?: string | null;
	local?: boolean;
	immediate?: boolean;
	force?: boolean;
	isExplicitUserAction?: boolean;
	preserveStickyUnread?: boolean;
}

export interface AppliedAck {
	acked: boolean;
	messageId: string | null;
	hadMentions: boolean;
}

export interface PendingAck {
	channelId: string;
	messageId: string;
	deadline: number;
	attempt: number;
}

export interface ArchivedReadState {
	ackMessageId: string | null;
	acknowledgedPinTimestamp: number;
	readStateKnown: boolean;
}

export function parseTimestamp(timestamp?: string | null): number {
	if (timestamp == null) return 0;
	const parsed = Date.parse(timestamp);
	return Number.isNaN(parsed) ? 0 : parsed;
}

export function snowflakeTimestamp(messageId?: string | null): number {
	if (messageId == null) return 0;
	const timestamp = extractTimestamp(messageId);
	return Number.isFinite(timestamp) ? timestamp : 0;
}

export function compareMessageIds(a?: string | null, b?: string | null): number {
	if (a == null && b == null) return 0;
	if (a == null) return -1;
	if (b == null) return 1;
	return compareSnowflakes(a, b);
}

export function isNewerMessageId(a?: string | null, b?: string | null): boolean {
	return compareMessageIds(a, b) > 0;
}

export function normalizeCount(count: number | undefined | null): number {
	if (count == null || !Number.isFinite(count)) return 0;
	return Math.max(0, Math.floor(count));
}

export function compareReadStateVersions(a?: string | null, b?: string | null): number {
	const left = normalizeVersion(a);
	const right = normalizeVersion(b);
	if (left == null || right == null) return 0;
	if (left.length !== right.length) return left.length < right.length ? -1 : 1;
	return left === right ? 0 : left < right ? -1 : 1;
}

export function chunkEntries<T>(entries: Array<T>, size: number): Array<Array<T>> {
	const chunks: Array<Array<T>> = [];
	for (let i = 0; i < entries.length; i += size) {
		chunks.push(entries.slice(i, i + size));
	}
	return chunks;
}

function normalizeVersion(version?: string | null): string | null {
	if (version == null || !/^\d+$/.test(version)) return null;
	return version.replace(/^0+/, '') || '0';
}
