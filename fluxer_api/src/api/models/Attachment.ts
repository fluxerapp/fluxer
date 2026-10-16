// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AttachmentID} from '@app/api/BrandedTypes';
import type {MessageAttachment} from '@app/api/database/types/MessageTypes';

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
	readonly waveform: string | null;

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
		this.waveform = attachment.waveform ?? null;
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
			waveform: this.waveform,
		};
	}
}
