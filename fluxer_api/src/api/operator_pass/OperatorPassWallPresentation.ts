// SPDX-License-Identifier: AGPL-3.0-or-later

import type {OperatorPassWallEntryRow} from '@app/api/database/types/OperatorPassTypes';
import type {User} from '@app/api/models/User';
import {isOperatorPassReservedName} from '@app/api/operator_pass/OperatorPassCertificateNames';
import {
	OPERATOR_PASS_WALL_AVATAR_SIZES,
	operatorPassWallSnapshotUrl,
} from '@app/api/operator_pass/OperatorPassWallAvatarStore';
import {
	OPERATOR_PASS_WALL_FALLBACK_NAME,
	OPERATOR_PASS_WALL_REVIEW_TARGET_MS,
} from '@app/api/operator_pass/OperatorPassWallConstants';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {OperatorPassWallNameRejectedError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassWallNameRejectedError';
import {OperatorPassWallNameRequiredError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassWallNameRequiredError';
import type {OperatorPassListingResponse} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';

const OFF_WALL_FLAGS = UserFlags.DELETED | UserFlags.SELF_DELETED | UserFlags.SPAMMER;

export function isOperatorPassWallStanding(
	user: Pick<User, 'flags' | 'tempBannedUntil' | 'pendingDeletionAt' | 'deletionStartedAt'>,
	now = Date.now(),
): boolean {
	if ((user.flags & OFF_WALL_FLAGS) !== 0n || user.pendingDeletionAt !== null || user.deletionStartedAt !== null) {
		return false;
	}
	if ((user.flags & UserFlags.DISABLED) === 0n) {
		return true;
	}
	return user.tempBannedUntil !== null && user.tempBannedUntil.getTime() <= now;
}

export interface OperatorPassWallChoice {
	name: string | null;
	optedOut: boolean;
}

export function normalizeOperatorPassWallChoice({
	name,
	optOut,
}: {
	name: string | null | undefined;
	optOut: boolean | null | undefined;
}): OperatorPassWallChoice {
	if (optOut === true) {
		return {name: null, optedOut: true};
	}
	const trimmed = name?.trim() ?? '';
	if (trimmed.length === 0) {
		throw new OperatorPassWallNameRequiredError();
	}
	if (isOperatorPassReservedName(trimmed)) {
		throw new OperatorPassWallNameRejectedError();
	}
	return {name: trimmed, optedOut: false};
}

export function hasOperatorPassWallChoice(entry: OperatorPassWallEntryRow | null): boolean {
	return entry != null && (entry.listed_at != null || entry.left_wall_at != null || entry.delivered_at != null);
}

export function isOperatorPassWallListed(entry: OperatorPassWallEntryRow): boolean {
	return !entry.opted_out && entry.listed_at != null;
}

export function operatorPassWallShownName(entry: OperatorPassWallEntryRow): string {
	return entry.live_name || OPERATOR_PASS_WALL_FALLBACK_NAME;
}

export function operatorPassWallReviewState(
	entry: OperatorPassWallEntryRow,
): 'pending' | 'approved' | 'declined' | null {
	if (entry.pending_revision_id != null) {
		return 'pending';
	}
	if (entry.declined_revision_id != null) {
		return 'declined';
	}
	if (entry.live_revision_id != null) {
		return 'approved';
	}
	return null;
}

export function operatorPassWallDisplayState(
	entry: OperatorPassWallEntryRow,
	{removed}: {removed: boolean},
): 'live' | 'fallback' | 'hidden' | 'removed' {
	if (removed) {
		return 'removed';
	}
	if (entry.hidden) {
		return 'hidden';
	}
	return entry.live_revision_id != null ? 'live' : 'fallback';
}

export function mapOperatorPassListing(
	entry: OperatorPassWallEntryRow | null,
	passCount: number,
	{removed = false}: {removed?: boolean} = {},
): OperatorPassListingResponse {
	const listed = entry != null && isOperatorPassWallListed(entry);
	return {
		wall_name: entry?.opted_out ? null : (entry?.pending_name ?? entry?.live_name ?? entry?.name ?? null),
		wall_opt_out: entry?.opted_out ?? true,
		keep_as_is: entry?.keep_as_is ?? false,
		release_list_opt_in: entry?.release_list_opt_in ?? false,
		status: listed ? 'listed' : 'not_listed',
		display_state: listed && entry ? operatorPassWallDisplayState(entry, {removed}) : null,
		review_state: listed && entry ? operatorPassWallReviewState(entry) : null,
		shown_name: listed && entry ? operatorPassWallShownName(entry) : null,
		shown_avatar_url:
			listed && entry?.live_avatar
				? operatorPassWallSnapshotUrl(entry.live_avatar, Math.max(...OPERATOR_PASS_WALL_AVATAR_SIZES))
				: null,
		pending_name: listed ? (entry?.pending_name ?? null) : null,
		pending_since: listed ? (entry?.pending_since?.toISOString() ?? null) : null,
		review_due_at:
			listed && entry?.pending_since
				? new Date(entry.pending_since.getTime() + OPERATOR_PASS_WALL_REVIEW_TARGET_MS).toISOString()
				: null,
		decline_reason: listed ? (entry?.decline_reason ?? null) : null,
		decline_note: listed ? (entry?.decline_note ?? null) : null,
		declined_name: listed ? (entry?.declined_name ?? null) : null,
		listed_at: listed ? (entry?.listed_at?.toISOString() ?? null) : null,
		release_year: entry?.release_erased_at ? null : (entry?.release_published_year ?? null),
		has_wall: passCount > 0,
		pass_count: passCount,
	};
}
