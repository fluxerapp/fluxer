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

import {createStringType, createUnboundedStringType, z} from '~/Schema';
import type {WebhookMessageRequest} from '~/webhook/WebhookModel';

const SlackAttachmentField = z.object({
	title: createUnboundedStringType().optional(),
	value: createUnboundedStringType().optional(),
	short: z.boolean().optional(),
});

const SlackAttachmentTs = z.union([z.number().int(), z.string().regex(/^\d+$/)]).optional();

const SlackAttachment = z.object({
	fallback: createUnboundedStringType().optional(),
	pretext: createUnboundedStringType().optional(),
	text: createUnboundedStringType().optional(),

	color: createUnboundedStringType().optional(),
	title: createUnboundedStringType().optional(),
	title_link: createUnboundedStringType().optional(),
	fields: z.array(SlackAttachmentField).optional(),

	footer: createUnboundedStringType().optional(),
	ts: SlackAttachmentTs,

	author_name: createUnboundedStringType().optional(),
	author_link: createUnboundedStringType().optional(),
	author_icon: createUnboundedStringType().optional(),

	image_url: createUnboundedStringType().optional(),
	thumb_url: createUnboundedStringType().optional(),
});

export const SlackWebhookRequest = z.object({
	text: createUnboundedStringType().optional(),
	username: createStringType(1, 80).optional(),
	icon_url: createUnboundedStringType().optional(),
	attachments: z.array(SlackAttachment).optional(),
});

export type SlackWebhookRequest = z.infer<typeof SlackWebhookRequest>;

type SlackAttachmentType = z.infer<typeof SlackAttachment>;
type SlackAttachmentFieldType = z.infer<typeof SlackAttachmentField>;
type WebhookEmbed = NonNullable<WebhookMessageRequest['embeds']>[number];

export function transformSlackWebhookRequest(payload: SlackWebhookRequest): WebhookMessageRequest {
	const attachments: ReadonlyArray<SlackAttachmentType> = payload.attachments ?? [];
	const embeds = attachments
		.map((attachment) => transformSlackAttachmentToEmbed(attachment))
		.filter((embed): embed is WebhookEmbed => embed != null);

	const content = payload.text !== undefined ? payload.text : embeds.length > 0 ? '' : undefined;

	return {
		content,
		username: payload.username,
		avatar_url: parseUrl(payload.icon_url) ?? undefined,
		embeds: embeds.length > 0 ? embeds : undefined,
	};
}

function transformSlackAttachmentToEmbed(att: SlackAttachmentType): WebhookEmbed | undefined {
	const description = buildAttachmentDescription(att);

	const embed: Partial<WebhookEmbed> = {};

	if (att.title && att.title.length > 0) {
		embed.title = att.title;
	}

	const titleLink = parseUrl(att.title_link);
	if (titleLink) {
		embed.url = titleLink;
	}

	if (description && description.length > 0) {
		embed.description = description;
	}

	const authorName = att.author_name?.trim();
	if (authorName) {
		embed.author = {
			name: authorName,
			url: parseUrl(att.author_link) ?? undefined,
			icon_url: parseUrl(att.author_icon) ?? undefined,
		};
	}

	const fieldsSource: ReadonlyArray<SlackAttachmentFieldType> = att.fields ?? [];
	const fields = fieldsSource
		.map<{
			name: string;
			value: string;
			inline: boolean;
		} | null>((field) => {
			const name = (field.title ?? '').trim();
			const value = (field.value ?? '').trim();
			if (!name || !value) return null;
			return {
				name,
				value,
				inline: field.short ?? false,
			};
		})
		.filter(
			(
				field,
			): field is {
				name: string;
				value: string;
				inline: boolean;
			} => field != null,
		);

	if (fields.length > 0) {
		embed.fields = fields;
	}

	const footerText = att.footer?.trim();
	if (footerText) {
		embed.footer = {text: footerText};
	}

	const tsSeconds = parseUnixSeconds(att.ts);
	if (tsSeconds != null) {
		embed.timestamp = new Date(tsSeconds * 1000);
	}

	const imageUrl = parseUrl(att.image_url);
	if (imageUrl) {
		embed.image = {url: imageUrl};
	}

	const thumbUrl = parseUrl(att.thumb_url);
	if (thumbUrl) {
		embed.thumbnail = {url: thumbUrl};
	}

	const explicitColor = parseHexColor(att.color);
	if (explicitColor != null) {
		embed.color = explicitColor;
	}

	const hasAnyRenderable =
		!!embed.title ||
		!!embed.description ||
		!!embed.url ||
		!!embed.author ||
		!!embed.fields ||
		!!embed.footer ||
		!!embed.timestamp ||
		!!embed.image ||
		!!embed.thumbnail;

	if (!hasAnyRenderable) return undefined;

	return embed as WebhookEmbed;
}

function buildAttachmentDescription(att: SlackAttachmentType): string | undefined {
	const parts: Array<string> = [];
	if (att.pretext && att.pretext.length > 0) parts.push(att.pretext);
	if (att.text && att.text.length > 0) parts.push(att.text);

	if (parts.length === 0 && att.fallback && att.fallback.length > 0) {
		parts.push(att.fallback);
	}

	const combined = parts.join('\n').trim();
	return combined.length > 0 ? combined : undefined;
}

function parseUnixSeconds(value: unknown): number | undefined {
	if (typeof value === 'number' && Number.isFinite(value)) return Math.trunc(value);
	if (typeof value === 'string' && /^\d+$/.test(value)) return Number.parseInt(value, 10);
	return undefined;
}

function parseHexColor(value: unknown): number | undefined {
	if (typeof value !== 'string') return undefined;
	const raw = value.trim();
	const match = raw.match(/^#?([0-9a-fA-F]{6})$/);
	if (!match) return undefined;
	const num = Number.parseInt(match[1], 16);
	return Number.isFinite(num) ? num : undefined;
}

function parseUrl(value: unknown): string | undefined {
	if (typeof value !== 'string') return undefined;
	const trimmed = value.trim();
	if (!trimmed) return undefined;
	try {
		return new URL(trimmed).toString();
	} catch {
		return undefined;
	}
}
