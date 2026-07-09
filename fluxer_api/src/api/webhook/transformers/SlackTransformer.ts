// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	SlackWebhookRequest,
	WebhookMessageRequest,
} from '@fluxer/schema/src/domains/webhook/WebhookRequestSchemas';
import {ColorType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {safeUrl} from '../../utils/StringUtils';

type SlackAttachment = NonNullable<SlackWebhookRequest['attachments']>[number];
type SlackAttachmentField = NonNullable<SlackAttachment['fields']>[number];
type WebhookEmbed = NonNullable<WebhookMessageRequest['embeds']>[number];
type WebhookEmbedField = NonNullable<NonNullable<WebhookEmbed['fields']>[number]>;

export function transformSlackWebhookRequest(payload: SlackWebhookRequest): WebhookMessageRequest {
	const embeds: Array<WebhookEmbed> = [];
	for (const att of payload.attachments ?? []) {
		const embed = transformSlackAttachmentToEmbed(att);
		if (embed) embeds.push(embed);
	}
	const content = payload.text ?? (embeds.length > 0 ? '' : undefined);
	return {
		content,
		username: payload.username,
		avatar_url: safeUrl(payload.icon_url),
		embeds: embeds.length > 0 ? embeds : undefined,
	};
}

function transformSlackAttachmentToEmbed(att: SlackAttachment): WebhookEmbed | undefined {
	const embed: Partial<WebhookEmbed> = {};
	if (att.title) embed.title = att.title;
	const titleUrl = safeUrl(att.title_link);
	if (titleUrl) embed.url = titleUrl;
	const description = buildAttachmentDescription(att);
	if (description) embed.description = description;
	if (att.author_name) {
		embed.author = {
			name: att.author_name,
			url: safeUrl(att.author_link),
			icon_url: safeUrl(att.author_icon),
		};
	}
	const fields: Array<WebhookEmbedField> = [];
	for (const field of att.fields ?? []) {
		const embedField = toEmbedField(field);
		if (embedField) fields.push(embedField);
	}
	if (fields.length > 0) embed.fields = fields;
	if (att.footer) embed.footer = {text: att.footer};
	if (typeof att.ts === 'number') {
		embed.timestamp = new Date(att.ts * 1000);
	}
	const imageUrl = safeUrl(att.image_url);
	if (imageUrl) embed.image = {url: imageUrl};
	const thumbUrl = safeUrl(att.thumb_url);
	if (thumbUrl) embed.thumbnail = {url: thumbUrl};
	const color = safeHexColor(att.color);
	if (color != null) embed.color = color;
	return Object.keys(embed).length > 0 ? (embed as WebhookEmbed) : undefined;
}

function toEmbedField(field: SlackAttachmentField): WebhookEmbedField | undefined {
	if (!field.title || !field.value) return undefined;
	return {
		name: field.title,
		value: field.value,
		inline: field.short ?? false,
	};
}

function buildAttachmentDescription(att: SlackAttachment): string | undefined {
	const parts: Array<string> = [];
	if (att.pretext) parts.push(att.pretext);
	if (att.text) parts.push(att.text);
	if (parts.length === 0 && att.fallback) parts.push(att.fallback);
	if (parts.length === 0) return undefined;
	const combined = parts.join('\n');
	return combined.length > 0 ? combined : undefined;
}

function safeHexColor(value: unknown): number | undefined {
	if (typeof value !== 'string' || value.length === 0) return undefined;
	const match = value.match(/^#?([0-9a-fA-F]{6})$/);
	if (!match) return undefined;
	const num = Number.parseInt(match[1], 16);
	const validated = ColorType.safeParse(num);
	return validated.success ? validated.data : undefined;
}
