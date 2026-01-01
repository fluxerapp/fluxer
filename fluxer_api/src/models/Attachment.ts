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

import type {MessageAttachment} from '~/database/CassandraTypes';
import type {AttachmentID} from '../BrandedTypes';

export class Attachment {
	readonly id: AttachmentID;
	readonly filename: string;
	readonly size: bigint;
	readonly title: string | null;
	readonly description: string | null;
	readonly width: number | null;
	readonly height: number | null;
	readonly contentType: string;
	readonly contentHash: string | null;
	readonly placeholder: string | null;
	readonly flags: number;
	readonly duration: number | null;
	readonly nsfw: boolean | null;

	constructor(attachment: MessageAttachment) {
		this.id = attachment.attachment_id;
		this.filename = attachment.filename;
		this.size = attachment.size;
		this.title = attachment.title ?? null;
		this.description = attachment.description ?? null;
		this.width = attachment.width ?? null;
		this.height = attachment.height ?? null;
		this.contentType = attachment.content_type;
		this.contentHash = attachment.content_hash ?? null;
		this.placeholder = attachment.placeholder ?? null;
		this.flags = attachment.flags ?? 0;
		this.duration = attachment.duration ?? null;
		this.nsfw = attachment.nsfw ?? null;
	}

	toMessageAttachment(): MessageAttachment {
		return {
			attachment_id: this.id,
			filename: this.filename,
			size: this.size,
			title: this.title,
			description: this.description,
			width: this.width,
			height: this.height,
			content_type: this.contentType,
			content_hash: this.contentHash,
			placeholder: this.placeholder,
			flags: this.flags,
			duration: this.duration,
			nsfw: this.nsfw,
		};
	}
}
