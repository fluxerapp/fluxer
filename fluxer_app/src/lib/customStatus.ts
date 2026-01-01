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

export const CUSTOM_STATUS_TEXT_LIMIT = 128;

export const isCustomStatusExpired = (status: CustomStatus | null, referenceTime = Date.now()): boolean => {
	if (!status?.expiresAt) {
		return false;
	}

	const expiresAt = Date.parse(status.expiresAt);
	if (Number.isNaN(expiresAt)) {
		return false;
	}

	return expiresAt <= referenceTime;
};

function normalizeText(text: string | null | undefined): string | null {
	const trimmed = text?.trim() ?? null;
	if (!trimmed) {
		return null;
	}
	return trimmed.slice(0, CUSTOM_STATUS_TEXT_LIMIT);
}

function normalizeEmojiName(name: string | null | undefined): string | null {
	const trimmed = name?.trim() ?? null;
	return trimmed || null;
}

export const normalizeCustomStatus = (status: CustomStatus | null | undefined): CustomStatus | null => {
	if (!status) {
		return null;
	}

	const text = normalizeText(status.text);
	const emojiId = status.emojiId?.trim() ?? null;
	const emojiName = normalizeEmojiName(status.emojiName);
	const expiresAt = status.expiresAt ?? null;

	if (!text && !emojiId && !emojiName) {
		return null;
	}

	const normalized: CustomStatus = {
		text,
		expiresAt,
		emojiId,
		emojiName,
		emojiAnimated: status.emojiAnimated ?? null,
	};

	if (isCustomStatusExpired(normalized)) {
		return null;
	}

	return normalized;
};

export const toGatewayCustomStatus = (status: CustomStatus | null | undefined): GatewayCustomStatusPayload | null => {
	if (!status) {
		return null;
	}

	return {
		text: status.text,
		expires_at: status.expiresAt,
		emoji_id: status.emojiId,
		emoji_name: status.emojiName,
		emoji_animated: status.emojiAnimated ?? undefined,
	};
};

export const fromGatewayCustomStatus = (
	payload: GatewayCustomStatusPayload | null | undefined,
): CustomStatus | null => {
	if (!payload) {
		return null;
	}

	const customStatus: CustomStatus = {
		text: payload.text ?? null,
		expiresAt: payload.expires_at ?? null,
		emojiId: payload.emoji_id ?? null,
		emojiName: payload.emoji_name ?? null,
		emojiAnimated: payload.emoji_animated ?? null,
	};

	return normalizeCustomStatus(customStatus);
};

export const customStatusToKey = (status: CustomStatus | null | undefined): string => {
	if (!status) {
		return '';
	}

	return `${status.text ?? ''}|${status.emojiId ?? ''}|${status.emojiName ?? ''}|${status.emojiAnimated ?? ''}|${status.expiresAt ?? ''}`;
};

export const getCustomStatusText = (status: CustomStatus | null | undefined): string | null => {
	const normalized = status?.text ? status.text.trim() : null;
	return normalized || null;
};
