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

import {ColorType, coerceNumberFromString, createUnboundedStringType, URLType, WebhookNameType, z} from '~/Schema';
import type {WebhookMessageRequest} from '~/webhook/WebhookModel';

const SlackAttachmentFieldSchema = z.object({
	title: createUnboundedStringType().optional(),
	value: createUnboundedStringType().optional(),
	short: z.boolean().optional(),
});

const SlackUnixSecondsSchema = coerceNumberFromString(z.number().int().nonnegative());

const SlackAttachmentSchema = z.object({
	fallback: createUnboundedStringType().optional(),
	pretext: createUnboundedStringType().optional(),
	text: createUnboundedStringType().optional(),

	color: createUnboundedStringType().optional(),
	title: createUnboundedStringType().optional(),
	title_link: createUnboundedStringType().optional(),
	fields: z.array(SlackAttachmentFieldSchema).optional(),

	footer: createUnboundedStringType().optional(),
	ts: SlackUnixSecondsSchema.optional(),

	author_name: createUnboundedStringType().optional(),
	author_link: createUnboundedStringType().optional(),
	author_icon: createUnboundedStringType().optional(),

	image_url: createUnboundedStringType().optional(),
	thumb_url: createUnboundedStringType().optional(),
});

export const SlackWebhookRequest = z.object({
	text: createUnboundedStringType().optional(),
	username: WebhookNameType.optional(),
	icon_url: createUnboundedStringType().optional(),
	attachments: z.array(SlackAttachmentSchema).optional(),
});

export type SlackWebhookRequest = z.infer<typeof SlackWebhookRequest>;

type SlackAttachment = z.infer<typeof SlackAttachmentSchema>;
type SlackAttachmentField = z.infer<typeof SlackAttachmentFieldSchema>;

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

function safeUrl(value: unknown): string | undefined {
	if (typeof value !== 'string' || value.length === 0) return undefined;
	if (!value.startsWith('http://') && !value.startsWith('https://')) return undefined;

	const parsed = URLType.safeParse(value);
	return parsed.success ? parsed.data : undefined;
}

function safeHexColor(value: unknown): number | undefined {
	if (typeof value !== 'string' || value.length === 0) return undefined;

	const match = value.match(/^#?([0-9a-fA-F]{6})$/);
	if (!match) return undefined;

	const num = Number.parseInt(match[1], 16);
	const validated = ColorType.safeParse(num);
	return validated.success ? validated.data : undefined;
}
