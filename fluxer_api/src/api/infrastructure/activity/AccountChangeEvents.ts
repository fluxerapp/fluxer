// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import {currentAccountChangeSource} from '@app/api/infrastructure/activity/ActivityMeta';
import type {AdminActionKind, ChangeSource} from '@app/api/infrastructure/activity/Contract.generated';

function timeMs(value: Date | null | undefined): number | null {
	return value ? value.getTime() : null;
}

function isRelevantChange(previous: UserRow, updated: UserRow): boolean {
	return (
		(previous.flags ?? 0n) !== (updated.flags ?? 0n) ||
		timeMs(previous.temp_banned_until) !== timeMs(updated.temp_banned_until) ||
		timeMs(previous.pending_deletion_at) !== timeMs(updated.pending_deletion_at) ||
		(previous.deletion_reason_code ?? null) !== (updated.deletion_reason_code ?? null)
	);
}

export async function emitAccountChangedIfRelevant(
	previous: UserRow | null,
	updated: UserRow,
	source?: ChangeSource,
): Promise<void> {
	if (!previous || !isRelevantChange(previous, updated)) return;
	const userId = updated.user_id.toString();
	const data = {
		user_id: userId,
		source: source ?? currentAccountChangeSource(),
		flags: (updated.flags ?? 0n).toString(),
		flags_before: (previous.flags ?? 0n).toString(),
		temp_banned_until_ms: timeMs(updated.temp_banned_until),
		pending_deletion_at_ms: timeMs(updated.pending_deletion_at),
		deletion_reason_code: updated.deletion_reason_code ?? null,
	};
	const digest = createHash('sha256').update(JSON.stringify(data)).digest('base64url').slice(0, 16);
	await emitActivity('account_changed', userId, data, null, `${userId}:${updated.version}:${digest}`);
}

export async function emitAdminAction(
	adminId: bigint,
	targetId: bigint,
	action: AdminActionKind,
	details: {reasonCode?: number | null; durationHours?: number | null; ips?: Iterable<string>} = {},
): Promise<void> {
	const target = targetId.toString();
	await emitActivity('admin_action', target, {
		admin_id: adminId.toString(),
		target_id: target,
		action,
		reason_code: details.reasonCode ?? null,
		duration_h: details.durationHours ?? null,
		ips: [...(details.ips ?? [])],
	});
}
