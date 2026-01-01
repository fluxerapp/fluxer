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

import type {MessageEmbedMedia} from '~/database/CassandraTypes';

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
