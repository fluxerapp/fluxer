// SPDX-License-Identifier: AGPL-3.0-or-later

import type {OperatorPassWallEntryRow} from '@app/api/database/types/OperatorPassTypes';
import type {User} from '@app/api/models/User';
import {isOperatorPassReservedName} from '@app/api/operator_pass/OperatorPassCertificateNames';
import {
	OPERATOR_PASS_WALL_AVATAR_SIZES,
	operatorPassWallSnapshotUrl,
} from '@app/api/operator_pass/OperatorPassWallAvatarStore';
import {OPERATOR_PASS_WALL_FALLBACK_NAME} from '@app/api/operator_pass/OperatorPassWallConstants';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {OperatorPassWallNameRejectedError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassWallNameRejectedError';
import {OperatorPassWallNameRequiredError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassWallNameRequiredError';
import {OperatorPassWallQuoteInvalidError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassWallQuoteInvalidError';
import {OperatorPassWallQuoteRejectedError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassWallQuoteRejectedError';
import type {OperatorPassListingResponse} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {
	getOperatorPassWallQuoteProblem,
	normalizeOperatorPassWallQuote,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassWallQuote';

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

export function parseOperatorPassWallQuote(raw: string | null): string | null {
	const quote = normalizeOperatorPassWallQuote(raw);
	if (quote === null) {
		return null;
	}
	const problem = getOperatorPassWallQuoteProblem(quote);
	if (problem === 'link') {
		throw new OperatorPassWallQuoteRejectedError();
	}
	if (problem !== null) {
		throw new OperatorPassWallQuoteInvalidError();
	}
	return quote;
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
		wall_name: entry?.opted_out ? null : (entry?.live_name ?? entry?.name ?? null),
		wall_quote: entry?.quote ?? null,
		wall_opt_out: entry?.opted_out ?? true,
		keep_as_is: entry?.keep_as_is ?? false,
		release_list_opt_in: entry?.release_list_opt_in ?? false,
		status: listed ? 'listed' : 'not_listed',
		display_state: listed && entry ? operatorPassWallDisplayState(entry, {removed}) : null,
		shown_name: listed && entry ? operatorPassWallShownName(entry) : null,
		shown_avatar_url:
			listed && entry?.live_avatar
				? operatorPassWallSnapshotUrl(entry.live_avatar, Math.max(...OPERATOR_PASS_WALL_AVATAR_SIZES))
				: null,
		listed_at: listed ? (entry?.listed_at?.toISOString() ?? null) : null,
		release_year: entry?.release_erased_at ? null : (entry?.release_published_year ?? null),
		has_wall: passCount > 0,
		pass_count: passCount,
	};
}
