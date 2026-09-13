// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageEmbed, MessageEmbedChild} from '@app/api/database/types/MessageTypes';
import {EmbedAuthor} from '@app/api/models/EmbedAuthor';
import {EmbedField} from '@app/api/models/EmbedField';
import {EmbedFooter} from '@app/api/models/EmbedFooter';
import {EmbedMedia} from '@app/api/models/EmbedMedia';
import {EmbedProvider} from '@app/api/models/EmbedProvider';
import {sanitizeOptionalAbsoluteUrlOrNull} from '@app/api/utils/UrlSanitizer';

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
	readonly audio: EmbedMedia | null;
	readonly footer: EmbedFooter | null;
	readonly fields: Array<EmbedField>;
	readonly html: string | null;
	readonly htmlWidth: number | null;
	readonly htmlHeight: number | null;
	readonly children: Array<Embed>;
	readonly nsfw: boolean | null;

	constructor(embed: MessageEmbed | MessageEmbedChild, allowChildren: boolean = true) {
		this.type = embed.type ?? null;
		this.title = embed.title ?? null;
		this.description = embed.description ?? null;
		this.url = sanitizeOptionalAbsoluteUrlOrNull(embed.url);
		this.timestamp = embed.timestamp ? new Date(embed.timestamp) : null;
		this.color = embed.color ?? null;
		this.author = embed.author ? new EmbedAuthor(embed.author) : null;
		this.provider = embed.provider ? new EmbedProvider(embed.provider) : null;
		this.thumbnail = embed.thumbnail ? new EmbedMedia(embed.thumbnail) : null;
		this.image = embed.image ? new EmbedMedia(embed.image) : null;
		this.video = embed.video ? new EmbedMedia(embed.video) : null;
		this.audio = embed.audio ? new EmbedMedia(embed.audio) : null;
		this.footer = embed.footer ? new EmbedFooter(embed.footer) : null;
		this.fields = (embed.fields ?? []).map((field) => new EmbedField(field));
		this.html = embed.html ?? null;
		this.htmlWidth = embed.html_width ?? null;
		this.htmlHeight = embed.html_height ?? null;
		this.children =
			allowChildren && 'children' in embed && embed.children
				? embed.children.map((child) => new Embed(child, false))
				: [];
		this.nsfw = embed.nsfw ?? null;
	}

	toMessageEmbed(): MessageEmbed {
		return {
			...this.toMessageEmbedChild(),
			children: this.children.length > 0 ? this.children.map((child) => child.toMessageEmbedChild()) : null,
		};
	}

	private toMessageEmbedChild(): MessageEmbedChild {
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
			audio: this.audio?.toMessageEmbedMedia() ?? null,
			footer: this.footer?.toMessageEmbedFooter() ?? null,
			fields: this.fields.length > 0 ? this.fields.map((field) => field.toMessageEmbedField()) : null,
			html: this.html,
			html_width: this.htmlWidth,
			html_height: this.htmlHeight,
			nsfw: this.nsfw,
		};
	}
}
