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

import type {MessageEmbed} from '~/database/CassandraTypes';
import {EmbedAuthor} from './EmbedAuthor';
import {EmbedField} from './EmbedField';
import {EmbedFooter} from './EmbedFooter';
import {EmbedMedia} from './EmbedMedia';
import {EmbedProvider} from './EmbedProvider';

export class Embed {
	readonly type: string | null;
	readonly title: string | null;
	readonly description: string | null;
	readonly url: string | null;
	readonly timestamp: Date | null;
	readonly color: number | null;
	readonly author: EmbedAuthor | null;
	readonly provider: EmbedProvider | null;
	readonly thumbnail: EmbedMedia | null;
	readonly image: EmbedMedia | null;
	readonly video: EmbedMedia | null;
	readonly footer: EmbedFooter | null;
	readonly fields: Array<EmbedField>;
	readonly nsfw: boolean | null;

	constructor(embed: MessageEmbed) {
		this.type = embed.type ?? null;
		this.title = embed.title ?? null;
		this.description = embed.description ?? null;
		this.url = embed.url ?? null;
		this.timestamp = embed.timestamp ? new Date(embed.timestamp) : null;
		this.color = embed.color ?? null;
		this.author = embed.author ? new EmbedAuthor(embed.author) : null;
		this.provider = embed.provider ? new EmbedProvider(embed.provider) : null;
		this.thumbnail = embed.thumbnail ? new EmbedMedia(embed.thumbnail) : null;
		this.image = embed.image ? new EmbedMedia(embed.image) : null;
		this.video = embed.video ? new EmbedMedia(embed.video) : null;
		this.footer = embed.footer ? new EmbedFooter(embed.footer) : null;
		this.fields = (embed.fields ?? []).map((field) => new EmbedField(field));
		this.nsfw = embed.nsfw ?? null;
	}

	toMessageEmbed(): MessageEmbed {
		return {
			type: this.type,
			title: this.title,
			description: this.description,
			url: this.url,
			timestamp: this.timestamp,
			color: this.color,
			author: this.author?.toMessageEmbedAuthor() ?? null,
			provider: this.provider?.toMessageEmbedProvider() ?? null,
			thumbnail: this.thumbnail?.toMessageEmbedMedia() ?? null,
			image: this.image?.toMessageEmbedMedia() ?? null,
			video: this.video?.toMessageEmbedMedia() ?? null,
			footer: this.footer?.toMessageEmbedFooter() ?? null,
			fields: this.fields.length > 0 ? this.fields.map((field) => field.toMessageEmbedField()) : null,
			nsfw: this.nsfw,
		};
	}
}
