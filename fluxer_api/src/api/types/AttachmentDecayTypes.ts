// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AttachmentID, ChannelID, MessageID} from '@app/api/BrandedTypes';

export interface AttachmentDecayRow {
	attachment_id: AttachmentID;
	channel_id: ChannelID;
	message_id: MessageID;
	filename: string;
	size_bytes: bigint;
	uploaded_at: Date;
	expires_at: Date;
	last_accessed_at: Date;
	cost: number;
	lifetime_days: number;
	status: string | null;
}

export const ATTACHMENT_DECAY_COLUMNS = [
	'attachment_id',
	'channel_id',
	'message_id',
	'filename',
	'size_bytes',
	'uploaded_at',
	'expires_at',
	'last_accessed_at',
	'cost',
	'lifetime_days',
	'status',
] as const satisfies ReadonlyArray<keyof AttachmentDecayRow>;
