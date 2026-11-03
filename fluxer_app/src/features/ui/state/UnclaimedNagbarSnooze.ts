// SPDX-License-Identifier: AGPL-3.0-or-later

import {getProtectedLocalStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {MS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';

const logger = new Logger('UnclaimedNagbarSnooze');

const SNOOZE_KEY_PREFIX = 'fluxer.unclaimed_nagbar_snoozed_until';
const UNCLAIMED_NAGBAR_SNOOZE_MS = 3 * MS_PER_DAY;

function snoozeKey(userId: string): string {
	return `${SNOOZE_KEY_PREFIX}:${userId}`;
}

export function snoozeUnclaimedNagbar(userId: string): void {
	const now = Date.now();
	try {
		getProtectedLocalStorage()?.setItem(snoozeKey(userId), String(now + UNCLAIMED_NAGBAR_SNOOZE_MS));
	} catch (error) {
		logger.warn('Failed to persist the unclaimed account banner snooze', error);
	}
}

export function isUnclaimedNagbarSnoozed(userId: string): boolean {
	const now = Date.now();
	try {
		const until = Number(getProtectedLocalStorage()?.getItem(snoozeKey(userId)) ?? '0');
		return Number.isFinite(until) && until > now;
	} catch (error) {
		logger.warn('Failed to read the unclaimed account banner snooze', error);
		return false;
	}
}
