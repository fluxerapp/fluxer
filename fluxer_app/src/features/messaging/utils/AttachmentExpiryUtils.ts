// SPDX-License-Identifier: AGPL-3.0-or-later
import {getFormattedShortDate} from '@app/features/user/utils/DateFormatting';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

export interface AttachmentExpiryOverride {
	expired?: boolean;
	expiresAt?: Date | string | null;
}

export interface AttachmentExpirySummary {
	expiresAt: Date | null;
	latestAt: Date | null;
	isExpired: boolean;
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
