// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RichEmbedRequest} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import type {LibredeskWebhook} from '@fluxer/schema/src/domains/webhook/LibredeskWebhookSchemas';

type LibredeskPayload = LibredeskWebhook['payload'];
type LibredeskHeadline = Pick<LibredeskPayload, 'inbox_name' | 'reference_number' | 'subject'>;
type LibredeskPerson = NonNullable<LibredeskPayload['author']>;
type EmbedField = NonNullable<RichEmbedRequest['fields']>[number];

const COLOUR_NEW = 0x3b82f6 as const;
const COLOUR_RESOLVED = 0x3ecf8e as const;
const COLOUR_WAITING = 0xf5a623 as const;
const COLOUR_CONTACT = 0xf97316 as const;
const COLOUR_NOTE = 0xfcbd1f as const;
const COLOUR_FAILED = 0xe23c39 as const;
const COLOUR_NEUTRAL = 0x5c6bc0 as const;

const INBOX_MAX_LENGTH = 48;
const REFERENCE_MAX_LENGTH = 32;
const EVENT_MAX_LENGTH = 32;
const SUBJECT_MAX_LENGTH = 128;
const STATUS_MAX_LENGTH = 64;
const NAME_MAX_LENGTH = 128;
const EXCERPT_MAX_LENGTH = 1000;
const FIELD_VALUE_MAX_LENGTH = 256;
const FOOTER_MAX_LENGTH = 128;
const ELLIPSIS = '...';

const graphemeSegmenter = new Intl.Segmenter('en', {granularity: 'grapheme'});
const WHITESPACE_REGEX = /[\s\p{Cc}]+/gu;
const BIDI_CONTROL_REGEX = /\p{Bidi_Control}/gu;
const MARKDOWN_TRIGGER_REGEX = /[\\<@[\]]/g;
const URL_REGEX = /https?:\/\/\S+/giu;
const MAX_TIMESTAMP_SECONDS = 8_640_000_000_000;

function escapeMarkdown(segment: string, inUrl: boolean): string {
	return inUrl
		? segment.replace(MARKDOWN_TRIGGER_REGEX, (char) => `%${char.charCodeAt(0).toString(16).toUpperCase()}`)
		: segment.replace(MARKDOWN_TRIGGER_REGEX, '\\$&');
}

function urlRanges(text: string): Array<[number, number]> {
	return Array.from(text.matchAll(URL_REGEX), (match) => [match.index, match.index + match[0].length]);
}

function clampText(value: string | null | undefined, maxLength: number, markdown: boolean): string | undefined {
	if (typeof value !== 'string') return undefined;
	const window = maxLength * 4;
	let truncated = value.length > window;
	const source = (truncated ? value.slice(0, window) : value).toWellFormed();
	const text = source.replace(BIDI_CONTROL_REGEX, '').replace(WHITESPACE_REGEX, ' ').trim();
	if (text.length === 0) return undefined;
	const budget = maxLength - ELLIPSIS.length;
	const urls = markdown ? urlRanges(text) : [];
	let range = 0;
	let output = '';
	let cut = 0;
	for (const {segment, index} of graphemeSegmenter.segment(text)) {
		while (range < urls.length && urls[range]![1] <= index) range++;
		const inUrl = range < urls.length && urls[range]![0] < index + segment.length;
		const piece = markdown ? escapeMarkdown(segment, inUrl) : segment;
		if (output.length + piece.length > maxLength) {
			truncated = true;
			break;
		}
		output += piece;
		if (output.length <= budget) cut = output.length;
	}
	return truncated ? `${output.slice(0, cut).trimEnd()}${ELLIPSIS}` : output;
}

function markdownText(value: string | null | undefined, maxLength: number): string | undefined {
	return clampText(value, maxLength, true);
}

function plainText(value: string | null | undefined, maxLength: number): string | undefined {
	return clampText(value, maxLength, false);
}

function personName(person: LibredeskPerson | null | undefined): string | undefined {
	const parts = [person?.first_name, person?.last_name].filter((part) => part && part.trim().length > 0);
	return plainText(parts.join(' '), NAME_MAX_LENGTH);
}

function conversationTitle(action: string, conversation: LibredeskHeadline | null | undefined): string {
	const inbox = markdownText(conversation?.inbox_name, INBOX_MAX_LENGTH);
	const reference = markdownText(conversation?.reference_number, REFERENCE_MAX_LENGTH);
	const subject = markdownText(conversation?.subject, SUBJECT_MAX_LENGTH);
	const headline = [reference ? `#${reference}` : undefined, subject].filter(Boolean).join(' ');
	const label = headline ? `${action}: ${headline}` : action;
	return inbox ? `\\[${inbox}\\] ${label}` : label;
}

function conversationFooter(uuid: string | null | undefined): RichEmbedRequest['footer'] {
	const text = plainText(uuid, FOOTER_MAX_LENGTH);
	return text ? {text: `Conversation ${text}`} : undefined;
}

function statusColour(category: string | null | undefined): number {
	switch (category) {
		case 'open':
			return COLOUR_NEW;
		case 'waiting':
			return COLOUR_WAITING;
		case 'resolved':
			return COLOUR_RESOLVED;
		default:
			return COLOUR_NEUTRAL;
	}
}

function snoozeMarkup(value: string | null | undefined): string | undefined {
	if (!value) return undefined;
	const seconds = Math.floor(new Date(value).getTime() / 1000);
	return seconds >= 1 && seconds <= MAX_TIMESTAMP_SECONDS ? `<t:${seconds}:f>` : undefined;
}

function tagList(tags: ReadonlyArray<string>): string | undefined {
	return markdownText(tags.join(', '), FIELD_VALUE_MAX_LENGTH);
}

function transformConversationCreated(payload: LibredeskPayload): RichEmbedRequest {
	const name = personName(payload.contact);
	return {
		title: conversationTitle('New conversation', payload),
		color: COLOUR_NEW,
		author: name ? {name} : undefined,
		footer: conversationFooter(payload.uuid),
	};
}

function transformStatusChanged(payload: LibredeskPayload): RichEmbedRequest | null {
	const next = markdownText(payload.new_status, STATUS_MAX_LENGTH);
	if (!next) return null;
	const previous = markdownText(payload.previous_status, STATUS_MAX_LENGTH);
	const until = snoozeMarkup(payload.snooze_until);
	const change = previous ? `${previous} to ${next}` : next;
	return {
		title: conversationTitle('Status changed', payload.conversation),
		color: statusColour(payload.conversation?.status_category),
		description: until ? `${change} until ${until}` : change,
		footer: conversationFooter(payload.conversation_uuid),
	};
}

function transformTagsChanged(payload: LibredeskPayload): RichEmbedRequest | null {
	const previous = new Set(payload.previous_tags ?? []);
	const next = new Set(payload.new_tags ?? []);
	const added = tagList([...next].filter((tag) => !previous.has(tag)));
	const removed = tagList([...previous].filter((tag) => !next.has(tag)));
	if (!added && !removed) return null;
	const fields: Array<EmbedField> = [];
	if (added) fields.push({name: 'Added', value: added, inline: true});
	if (removed) fields.push({name: 'Removed', value: removed, inline: true});
	return {
		title: conversationTitle('Tags changed', payload.conversation),
		color: COLOUR_NEUTRAL,
		fields,
		footer: conversationFooter(payload.conversation_uuid),
	};
}

function transformAssigned(payload: LibredeskPayload): RichEmbedRequest {
	const assignee = payload.assigned_to;
	return {
		title: conversationTitle('Conversation assigned', payload.conversation),
		color: COLOUR_NEUTRAL,
		fields: assignee != null ? [{name: 'Agent ID', value: String(assignee), inline: true}] : undefined,
		footer: conversationFooter(payload.conversation_uuid),
	};
}

function transformUnassigned(payload: LibredeskPayload): RichEmbedRequest {
	return {
		title: conversationTitle('Conversation unassigned', payload.conversation),
		color: COLOUR_NEUTRAL,
		footer: conversationFooter(payload.conversation_uuid),
	};
}

function isHiddenMessage(payload: LibredeskPayload): boolean {
	return payload.type === 'activity' || payload.meta?.is_csat === true || payload.meta?.continuity_email === true;
}

function attachmentsField(payload: LibredeskPayload): EmbedField | undefined {
	const names = (payload.attachments ?? []).map((attachment) => attachment.name?.trim()).filter(Boolean);
	const value = markdownText(names.join(', '), FIELD_VALUE_MAX_LENGTH);
	return value ? {name: 'Attachments', value, inline: false} : undefined;
}

function messageHeading(payload: LibredeskPayload): {title: string; color: number; fallbackName: string} {
	if (payload.sender_type === 'contact') return {title: 'New message', color: COLOUR_CONTACT, fallbackName: 'Contact'};
	if (payload.private === true) return {title: 'New private note', color: COLOUR_NOTE, fallbackName: 'Agent'};
	if (payload.meta?.is_automated === true)
		return {title: 'Automated reply', color: COLOUR_NEUTRAL, fallbackName: 'Agent'};
	return {title: 'New reply', color: COLOUR_NEW, fallbackName: 'Agent'};
}

function transformMessageCreated(payload: LibredeskPayload): RichEmbedRequest | null {
	if (isHiddenMessage(payload)) return null;
	const {title, color, fallbackName} = messageHeading(payload);
	const attachments = attachmentsField(payload);
	return {
		title,
		color,
		description: markdownText(payload.text_content, EXCERPT_MAX_LENGTH),
		author: {name: personName(payload.author) ?? fallbackName},
		fields: attachments ? [attachments] : undefined,
		footer: conversationFooter(payload.conversation_uuid),
	};
}

function transformMessageUpdated(payload: LibredeskPayload): RichEmbedRequest | null {
	if (payload.status !== 'failed' || isHiddenMessage(payload)) return null;
	return {
		title: 'Reply failed to send',
		color: COLOUR_FAILED,
		description: markdownText(payload.text_content, EXCERPT_MAX_LENGTH),
		author: {name: personName(payload.author) ?? 'Agent'},
		footer: conversationFooter(payload.conversation_uuid),
	};
}

function transformTest(payload: LibredeskPayload): RichEmbedRequest {
	return {
		title: 'Test delivery received',
		color: COLOUR_NEUTRAL,
		description: markdownText(payload.name, NAME_MAX_LENGTH),
	};
}

function transformCustomEvent(event: string, payload: LibredeskPayload): RichEmbedRequest | null {
	const conversation = payload.conversation;
	if (!conversation) return null;
	const name = markdownText(event, EVENT_MAX_LENGTH) ?? 'Event';
	return {
		title: conversationTitle(name, conversation),
		color: COLOUR_NEUTRAL,
		footer: conversationFooter(conversation.uuid),
	};
}

export function transformLibredeskWebhook(body: LibredeskWebhook): RichEmbedRequest | null {
	const {event, payload} = body;
	const hasConversationUuid = payload.conversation_uuid != null;
	switch (event) {
		case 'conversation.created':
			if (payload.uuid != null) return transformConversationCreated(payload);
			break;
		case 'conversation.status_changed':
			if (hasConversationUuid) return transformStatusChanged(payload);
			break;
		case 'conversation.tags_changed':
			if (hasConversationUuid) return transformTagsChanged(payload);
			break;
		case 'conversation.assigned':
			if (hasConversationUuid) return transformAssigned(payload);
			break;
		case 'conversation.unassigned':
			if (hasConversationUuid) return transformUnassigned(payload);
			break;
		case 'message.created':
			if (hasConversationUuid) return transformMessageCreated(payload);
			break;
		case 'message.updated':
			if (hasConversationUuid) return transformMessageUpdated(payload);
			break;
		case 'webhook.test':
			if (payload.name != null) return transformTest(payload);
			break;
	}
	return transformCustomEvent(event, payload);
}
