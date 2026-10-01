// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {MessageEmbed, MessageEmbedChild, MessageEmbedMedia} from '@app/api/database/types/MessageTypes';
import type {Message} from '@app/api/models/Message';

const EMBED_MEDIA_FIELDS = ['image', 'thumbnail', 'video', 'audio'] as const;

export type EmbedMediaField = (typeof EMBED_MEDIA_FIELDS)[number];

function attachmentPrefix(channelId: ChannelID): string {
	return `${Config.endpoints.media}/attachments/${channelId}/`;
}

export function parseAttachmentUrl(
	url: string | null | undefined,
	channelId: ChannelID,
): {id: string; filename: string} | null {
	const prefix = attachmentPrefix(channelId);
	if (!url?.startsWith(prefix)) return null;
	const rest = url.slice(prefix.length);
	const separator = rest.indexOf('/');
	if (separator <= 0) return null;
	const id = rest.slice(0, separator);
	const filename = rest.slice(separator + 1).split('?')[0] ?? '';
	if (!/^\d+$/.test(id) || filename.length === 0) return null;
	return {id, filename};
}

export function forEachEmbedMedia(
	embeds: ReadonlyArray<MessageEmbed>,
	visit: (media: MessageEmbedMedia, owner: MessageEmbedChild, field: EmbedMediaField) => void,
): void {
	const visitOwner = (owner: MessageEmbedChild) => {
		for (const field of EMBED_MEDIA_FIELDS) {
			const media = owner[field];
			if (media) visit(media, owner, field);
		}
	};
	for (const embed of embeds) {
		visitOwner(embed);
		for (const child of embed.children ?? []) {
			visitOwner(child);
		}
	}
}

export function collectEmbedContentHashes(message: Message): Array<string> {
	const hashes: Array<string> = [];
	forEachEmbedMedia(
		message.embeds.map((embed) => embed.toMessageEmbed()),
		(media) => {
			if (media.content_hash) hashes.push(media.content_hash);
		},
	);
	return hashes;
}
