// SPDX-License-Identifier: AGPL-3.0-or-later

import {getNativeLocale, t} from '@electron/main/MainI18n';

export const UNREAD_BADGE_DOT = -1;

export function parseUnreadBadge(value: unknown): number | null {
	if (typeof value !== 'number' || !Number.isSafeInteger(value) || value < UNREAD_BADGE_DOT) {
		return null;
	}
	return value;
}

function formatBadgeCount(count: number): string {
	try {
		return count.toLocaleString(getNativeLocale());
	} catch {
		return String(count);
	}
}

export function describeUnreadBadge(badge: number): string | null {
	if (badge === UNREAD_BADGE_DOT) {
		return t('desktop.badge.unreadMessages');
	}
	if (badge <= 0) {
		return null;
	}
	return t('desktop.badge.unreadMessagesCount', {count: formatBadgeCount(badge)});
}
