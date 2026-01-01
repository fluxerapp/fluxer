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

import {msg} from '@lingui/core/macro';
import i18n from '~/i18n';
import {TimestampStyle} from '~/lib/markdown/parser/types/enums';
import {formatTimestamp} from '~/lib/markdown/utils/date-formatter';
import type {MessageAttachment} from '~/records/MessageRecord';
import {getFormattedShortDate} from '~/utils/DateUtils';

export interface AttachmentExpiryOverride {
	expired?: boolean;
	expiresAt?: string | null;
}

interface FormatExpiryParams {
	expiresAt: Date | null;
	isExpired?: boolean;
}

export interface AttachmentExpirySummary {
	expiresAt: Date | null;
	latestAt: Date | null;
	isExpired: boolean;
}

export function formatAttachmentExpiryTooltip({expiresAt, isExpired = false}: FormatExpiryParams): string | null {
	if (!expiresAt) return null;

	const timestampSeconds = Math.floor(expiresAt.getTime() / 1000);
	const absolute = formatTimestamp(timestampSeconds, TimestampStyle.LongDateTime, i18n);
	const relativeText = formatTimestamp(timestampSeconds, TimestampStyle.RelativeTime, i18n);

	return isExpired ? i18n._(msg`Expired ${absolute}`) : i18n._(msg`Expires ${absolute} (${relativeText})`);
}

export function getEarliestAttachmentExpiry(attachments: ReadonlyArray<MessageAttachment>): AttachmentExpirySummary {
	let earliest: Date | null = null;
	let latest: Date | null = null;
	let isExpired = false;

	for (const att of attachments) {
		let attDate: Date | null = null;

		if (att.expires_at) {
			attDate = new Date(att.expires_at);
		}

		if (!attDate && att.expired) {
			attDate = new Date();
		}

		if (!attDate) continue;

		if (!earliest || attDate.getTime() < earliest.getTime()) {
			earliest = attDate;
		}
		if (!latest || attDate.getTime() > latest.getTime()) {
			latest = attDate;
		}
		if (att.expired || attDate.getTime() <= Date.now()) {
			isExpired = true;
		}
	}

	return {
		expiresAt: earliest,
		latestAt: latest,
		isExpired,
	};
}

export function formatAttachmentDate(date: Date | null): string | null {
	if (!date) return null;
	return getFormattedShortDate(date);
}

export interface AttachmentExpiryResult {
	attachment: MessageAttachment;
	expiresAt: Date | null;
	isExpired: boolean;
}

export function getEffectiveAttachmentExpiry(
	attachment: MessageAttachment,
	override?: AttachmentExpiryOverride,
	now = Date.now(),
): AttachmentExpiryResult {
	const expiresAt = attachment.expires_at ? new Date(attachment.expires_at) : null;
	const overrideExpiresAt = override?.expiresAt ? new Date(override.expiresAt) : null;
	const effectiveExpiresAt = overrideExpiresAt ?? expiresAt;

	const baseExpired = Boolean(attachment.expired) || (expiresAt ? expiresAt.getTime() <= now : false);
	const effectiveExpired =
		(override?.expired ?? baseExpired) || (effectiveExpiresAt ? effectiveExpiresAt.getTime() <= now : false);

	return {
		attachment: {
			...attachment,
			expired: effectiveExpired,
			expires_at: effectiveExpiresAt?.toISOString() ?? attachment.expires_at ?? null,
		},
		expiresAt: effectiveExpiresAt,
		isExpired: effectiveExpired,
	};
}

export function mapAttachmentsWithExpiry(
	attachments: ReadonlyArray<MessageAttachment>,
	overrides?: Record<string, AttachmentExpiryOverride>,
	now = Date.now(),
): ReadonlyArray<AttachmentExpiryResult> {
	return attachments.map((att) => getEffectiveAttachmentExpiry(att, overrides?.[att.id], now));
}

export function filterExpiredAttachments(
	results: ReadonlyArray<AttachmentExpiryResult>,
): ReadonlyArray<MessageAttachment> {
	return results.filter((r) => !r.isExpired).map((r) => r.attachment);
}
