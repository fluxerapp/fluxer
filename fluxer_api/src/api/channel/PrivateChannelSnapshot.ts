// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChannelType, ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {UserID} from '../BrandedTypes';
import {Db, type DbOp} from '../database/CassandraTypes';
import type {ChannelRow, PrivateChannelRow} from '../database/types/ChannelTypes';

type SnapshotPatch = Partial<{
	is_gdm: DbOp<boolean>;
	channel_type: DbOp<number>;
	channel_name: DbOp<string | null>;
	channel_icon_hash: DbOp<string | null>;
	channel_owner_id: DbOp<UserID | null>;
	channel_recipient_ids: DbOp<Set<UserID> | null>;
	channel_last_message_id: DbOp<ChannelRow['last_message_id']>;
	channel_last_pin_timestamp: DbOp<Date | null>;
	channel_nicks: DbOp<Map<string, string> | null>;
	channel_rate_limit_per_user: DbOp<number | null>;
	channel_nsfw: DbOp<boolean | null>;
	channel_version: DbOp<number>;
	snapshot_at: DbOp<Date>;
}>;

export function isPrivateChannelType(type: number): boolean {
	return type === ChannelTypes.DM || type === ChannelTypes.GROUP_DM || type === ChannelTypes.DM_PERSONAL_NOTES;
}

export function privateChannelFanOutTargets(row: ChannelRow): Array<UserID> {
	if (!isPrivateChannelType(row.type)) {
		return [];
	}
	const targets = new Set<UserID>(row.recipient_ids ?? []);
	if (row.type === ChannelTypes.DM_PERSONAL_NOTES && row.owner_id != null) {
		targets.add(row.owner_id);
	}
	return Array.from(targets);
}

function nonEmptySet(value: Set<UserID> | null | undefined): Set<UserID> | null {
	return value && value.size > 0 ? value : null;
}

function nonEmptyMap(value: Map<string, string> | null | undefined): Map<string, string> | null {
	return value && value.size > 0 ? value : null;
}

function snapshotMetadataPatch(row: ChannelRow): SnapshotPatch {
	return {
		is_gdm: Db.set(row.type === ChannelTypes.GROUP_DM),
		channel_type: Db.set(row.type),
		channel_name: Db.set(row.name ?? null),
		channel_icon_hash: Db.set(row.icon_hash ?? null),
		channel_owner_id: Db.set(row.owner_id ?? null),
		channel_recipient_ids: Db.set(nonEmptySet(row.recipient_ids)),
		channel_last_pin_timestamp: Db.set(row.last_pin_timestamp ?? null),
		channel_nicks: Db.set(nonEmptyMap(row.nicks)),
		channel_rate_limit_per_user: Db.set(row.rate_limit_per_user ?? null),
		channel_nsfw: Db.set(row.nsfw ?? null),
		channel_version: Db.set(row.version ?? 0),
	};
}

export function privateChannelMetadataPatch(row: ChannelRow): SnapshotPatch {
	return snapshotMetadataPatch(row);
}

export function privateChannelHydrationPatch(row: ChannelRow, hydratedAt: Date): SnapshotPatch {
	return {
		...snapshotMetadataPatch(row),
		channel_last_message_id: Db.set(row.last_message_id ?? null),
		snapshot_at: Db.set(hydratedAt),
	};
}

export function privateChannelLastMessageIdPatch(messageId: ChannelRow['last_message_id']): SnapshotPatch {
	return {channel_last_message_id: Db.set(messageId)};
}

export function channelRowFromPrivateChannelSnapshot(row: PrivateChannelRow): ChannelRow | null {
	if (row.snapshot_at == null || row.channel_type == null) {
		return null;
	}
	return {
		channel_id: row.channel_id,
		guild_id: null,
		type: row.channel_type as ChannelType,
		name: row.channel_name ?? null,
		topic: null,
		icon_hash: row.channel_icon_hash ?? null,
		url: null,
		parent_id: null,
		position: null,
		owner_id: row.channel_owner_id ?? null,
		recipient_ids: row.channel_recipient_ids ?? null,
		nsfw: row.channel_nsfw ?? null,
		content_warning_level: null,
		content_warning_text: null,
		rate_limit_per_user: row.channel_rate_limit_per_user ?? null,
		bitrate: null,
		user_limit: null,
		voice_connection_limit: null,
		rtc_region: null,
		last_message_id: row.channel_last_message_id ?? null,
		last_pin_timestamp: row.channel_last_pin_timestamp ?? null,
		permission_overwrites: null,
		nicks: row.channel_nicks ?? null,
		soft_deleted: false,
		indexed_at: null,
		version: row.channel_version ?? 0,
		thread_metadata: null,
		message_count: null,
		total_message_sent: null,
		member_count: null,
	};
}
