// SPDX-License-Identifier: AGPL-3.0-or-later

import type {InstanceNotice} from '@fluxer/schema/src/domains/instance/InstanceSchemas';

function timeOf(value: string | null): number | null {
	if (value === null) return null;
	const parsed = Date.parse(value);
	return Number.isFinite(parsed) ? parsed : null;
}

export function isInstanceNoticeLive(notice: InstanceNotice, nowMs: number): boolean {
	const startsAt = timeOf(notice.starts_at);
	const endsAt = timeOf(notice.ends_at);
	return (startsAt === null || startsAt <= nowMs) && (endsAt === null || nowMs < endsAt);
}

export function nextInstanceNoticeChangeAt(notice: InstanceNotice, nowMs: number): number | null {
	const upcoming = [timeOf(notice.starts_at), timeOf(notice.ends_at)].filter(
		(time): time is number => time !== null && time > nowMs,
	);
	return upcoming.length === 0 ? null : Math.min(...upcoming);
}
