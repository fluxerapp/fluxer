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

import {type Message, MessageRecord} from '~/records/MessageRecord';

export interface SavedMessageEntryResponse {
	id: string;
	channel_id: string;
	message_id: string;
	status: SavedMessageStatus;
	message: Message | null;
}

export type SavedMessageStatus = 'available' | 'missing_permissions';

export interface SavedMessageMissingEntry {
	id: string;
	channelId: string;
	messageId: string;
}

export class SavedMessageEntryRecord {
	readonly id: string;
	readonly channelId: string;
	readonly messageId: string;
	readonly status: SavedMessageStatus;
	readonly message: MessageRecord | null;

	constructor(data: SavedMessageEntryResponse) {
		this.id = data.id;
		this.channelId = data.channel_id;
		this.messageId = data.message_id;
		this.status = data.status;
		this.message = data.message ? new MessageRecord(data.message) : null;
	}

	static fromResponse(response: SavedMessageEntryResponse): SavedMessageEntryRecord {
		return new SavedMessageEntryRecord(response);
	}

	toMissingEntry(): SavedMessageMissingEntry {
		return {
			id: this.id,
			channelId: this.channelId,
			messageId: this.messageId,
		};
	}
}
