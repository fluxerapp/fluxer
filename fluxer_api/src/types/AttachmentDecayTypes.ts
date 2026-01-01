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

import type {AttachmentID, ChannelID, MessageID} from '~/BrandedTypes';

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
