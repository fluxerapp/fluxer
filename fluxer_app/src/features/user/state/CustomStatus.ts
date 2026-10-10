// SPDX-License-Identifier: AGPL-3.0-or-later

import UnicodeEmojis from '@app/features/expressions/utils/UnicodeEmojis';

export interface CustomStatus {
	text: string | null;
	expiresAt: string | null;
	emojiId: string | null;
	emojiName: string | null;
	emojiAnimated?: boolean | null;
}

export interface GatewayCustomStatusPayload {
	text?: string | null;
	expires_at?: string | null;
	emoji_id?: string | null;
	emoji_name?: string | null;
	emoji_animated?: boolean | null;
}

export interface ApiCustomStatusPayload {
	text?: string | null;
	expires_at?: string | null;
	emoji_id?: string;
	emoji_name?: string;
}

const CUSTOM_STATUS_TEXT_LIMIT = 128;

function trimToNonEmpty(value: string | null | undefined): string | null {
	if (value == null) return null;
	const t = value.trim();
	return t.length === 0 ? null : t;
}

function clampText(value: string | null | undefined): string | null {
	const t = trimToNonEmpty(value);
	return t == null ? null : t.slice(0, CUSTOM_STATUS_TEXT_LIMIT);
}

function expiryMs(expiresAt: string | null | undefined): number | null {
	if (expiresAt == null) return null;
	const ms = Date.parse(expiresAt);
	return Number.isFinite(ms) ? ms : null;
}

export function isCustomStatusExpired(status: CustomStatus | null, referenceTime: number = Date.now()): boolean {
	const ms = expiryMs(status?.expiresAt);
	if (ms == null) return false;
	return ms <= referenceTime;
}

export function normalizeCustomStatus(status: CustomStatus | null | undefined): CustomStatus | null {
	if (status == null) return null;
	const text = clampText(status.text);
	const emojiId = trimToNonEmpty(status.emojiId);
	const rawEmojiName = trimToNonEmpty(status.emojiName);
	const emojiName =
		rawEmojiName == null
			? null
			: emojiId != null
				? rawEmojiName
				: UnicodeEmojis.normalizeEmojiNameToSurrogate(rawEmojiName);
	if (text == null && emojiId == null && emojiName == null) return null;
	const normalized: CustomStatus = {
		text,
		expiresAt: status.expiresAt ?? null,
		emojiId,
		emojiName,
		emojiAnimated: status.emojiAnimated ?? null,
	};
	return isCustomStatusExpired(normalized) ? null : normalized;
}

export function toGatewayCustomStatus(status: CustomStatus | null | undefined): GatewayCustomStatusPayload | null {
	if (status == null) return null;
	return {
		text: status.text,
		expires_at: status.expiresAt,
		emoji_id: status.emojiId,
		emoji_name: status.emojiName,
		emoji_animated: status.emojiAnimated ?? undefined,
	};
}

export function toApiCustomStatusPayload(status: CustomStatus | null | undefined): ApiCustomStatusPayload | null {
	const normalized = normalizeCustomStatus(status);
	if (normalized == null) return null;
	const payload: ApiCustomStatusPayload = {text: normalized.text, expires_at: normalized.expiresAt};
	if (normalized.emojiId != null) {
		payload.emoji_id = normalized.emojiId;
	} else if (normalized.emojiName != null) {
		payload.emoji_name = UnicodeEmojis.normalizeEmojiNameToSurrogate(normalized.emojiName);
	}
	return payload;
}

export function fromGatewayCustomStatus(payload: GatewayCustomStatusPayload | null | undefined): CustomStatus | null {
	if (payload == null) return null;
	return normalizeCustomStatus({
		text: payload.text ?? null,
		expiresAt: payload.expires_at ?? null,
		emojiId: payload.emoji_id ?? null,
		emojiName: payload.emoji_name ?? null,
		emojiAnimated: payload.emoji_animated ?? null,
	});
}

export function customStatusToKey(status: CustomStatus | null | undefined): string {
	if (status == null) return '';
	const fields: ReadonlyArray<string> = [
		status.text ?? '',
		status.emojiId ?? '',
		status.emojiName ?? '',
		String(status.emojiAnimated ?? ''),
		status.expiresAt ?? '',
	];
	return fields.join('|');
}

export function getCustomStatusText(status: CustomStatus | null | undefined): string | null {
	return trimToNonEmpty(status?.text);
}

export const buildDraftStatus = (params: {
	text: string;
	emojiId: string | null;
	emojiName: string | null;
	expiresAt: string | null;
}): CustomStatus | null => {
	return normalizeCustomStatus({
		text: params.text || null,
		emojiId: params.emojiId,
		emojiName: params.emojiName,
		expiresAt: params.expiresAt,
	});
};
