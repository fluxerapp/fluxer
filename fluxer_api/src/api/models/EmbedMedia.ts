// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageEmbedMedia} from '@app/api/database/types/MessageTypes';

export class EmbedMedia {
	readonly url: string | null;
	readonly width: number | null;
	readonly height: number | null;
	readonly description: string | null;
	readonly contentType: string | null;
	readonly contentHash: string | null;
	readonly placeholder: string | null;
	readonly flags: number;
	readonly duration: number | null;

	constructor(media: MessageEmbedMedia) {
		this.url = media.url ?? null;
		this.width = media.width ?? null;
		this.height = media.height ?? null;
		this.description = media.description ?? null;
		this.contentType = media.content_type ?? null;
		this.contentHash = media.content_hash ?? null;
		this.placeholder = media.placeholder ?? null;
		this.flags = media.flags ?? 0;
		this.duration = media.duration ?? null;
	}

	toMessageEmbedMedia(): MessageEmbedMedia {
		return {
			url: this.url,
			width: this.width,
			height: this.height,
			description: this.description,
			content_type: this.contentType,
			content_hash: this.contentHash,
			placeholder: this.placeholder,
			flags: this.flags,
			duration: this.duration,
		};
	}
}
