// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {
	OperatorPassOfferRow,
	OperatorPassPurchaseRow,
	OperatorPassReleaseRow,
	OperatorPassRemovalHistoryRow,
	OperatorPassRemovalRow,
	OperatorPassRow,
	OperatorPassUnitEventRow,
	OperatorPassUnitRow,
	OperatorPassWallDecisionRow,
	OperatorPassWallEntryRow,
	OperatorPassWallEventRow,
} from '@app/api/database/types/OperatorPassTypes';
import type {User} from '@app/api/models/User';
import {
	OPERATOR_PASS_WALL_AVATAR_SIZES,
	operatorPassWallSnapshotUrl,
} from '@app/api/operator_pass/OperatorPassWallAvatarStore';
import {OPERATOR_PASS_WALL_REVIEW_TARGET_MS} from '@app/api/operator_pass/OperatorPassWallConstants';
import {
	isOperatorPassWallListed,
	operatorPassWallDisplayState,
	operatorPassWallReviewState,
	operatorPassWallShownName,
} from '@app/api/operator_pass/OperatorPassWallPresentation';
import {getAnimatedAssetExtension} from '@app/api/worker/utils/AssetArchiveHelpers';
import {toIsoOrNull} from '@fluxer/date_utils/src/DateSerialization';
import type {
	AdminOperatorPassReleaseFileResponse,
	AdminOperatorPassRemovalResponse,
	AdminOperatorPassUserResponse,
	AdminOperatorPassWallEntryResponse,
	AdminOperatorPassWallEventResponse,
	AdminOperatorPassWallTimelineResponse,
} from '@fluxer/schema/src/domains/admin/AdminOperatorPassSchemas';
import type {z} from 'zod';

export function mapRemoval(removal: OperatorPassRemovalRow): AdminOperatorPassRemovalResponse {
	return {
		state: removal.removal_state,
		rule: removal.rule,
		facts: removal.facts,
		automated: removal.automated,
		serious: removal.serious ?? null,
		removed_until: toIsoOrNull(removal.removed_until),
		notice_sent_at: toIsoOrNull(removal.notice_sent_at),
		removed_at: toIsoOrNull(removal.removed_at),
		admin_user_id: removal.admin_user_id.toString(),
	};
}

export function mapRemovalHistory(
	entry: OperatorPassRemovalHistoryRow,
): AdminOperatorPassUserResponse['removal_history'][number] {
	return {
		removed_at: entry.removed_at.toISOString(),
		removed_until: toIsoOrNull(entry.removed_until),
		rule: entry.rule,
		facts: entry.facts,
		automated: entry.automated,
		serious: entry.serious,
		notice_sent_at: toIsoOrNull(entry.notice_sent_at),
		admin_user_id: entry.admin_user_id.toString(),
		ended_at: toIsoOrNull(entry.ended_at),
		end_reason: entry.end_reason ?? null,
	};
}

export function mapPass(
	pass: OperatorPassRow,
	purchases: ReadonlyArray<OperatorPassPurchaseRow>,
): NonNullable<AdminOperatorPassUserResponse['pass']> {
	return {
		granted_at: pass.granted_at.toISOString(),
		source: pass.source,
		paused: pass.paused_at != null,
		checkout_session_id: pass.checkout_session_id ?? null,
		invoice_number:
			purchases.find((purchase) => purchase.checkout_session_id === pass.checkout_session_id)?.invoice_number ?? null,
		pass_count: pass.pass_count ?? 1,
		terms_version: pass.terms_version ?? null,
		waiver_consent_at: toIsoOrNull(pass.waiver_consent_at),
		waiver_consent_version: pass.waiver_consent_version ?? null,
		waiver_consent_ip: pass.waiver_consent_ip ?? null,
		waiver_user_agent: pass.waiver_consent_user_agent ?? null,
	};
}

export function mapUnit(
	unit: OperatorPassUnitRow,
	events: ReadonlyArray<OperatorPassUnitEventRow>,
): AdminOperatorPassUserResponse['units'][number] {
	return {
		id: unit.pass_id,
		purchase_id: unit.purchase_id ?? null,
		buyer_id: unit.buyer_id?.toString() ?? null,
		holder_id: unit.holder_id.toString(),
		source: unit.source,
		status: unit.status,
		transferable: unit.transferable,
		delivered_at: unit.delivered_at.toISOString(),
		first_delivered_at: unit.first_delivered_at.toISOString(),
		paused_at: toIsoOrNull(unit.paused_at),
		ended_at: toIsoOrNull(unit.ended_at),
		end_reason: unit.end_reason ?? null,
		withdrawal_country: unit.withdrawal_country ?? null,
		withdrawal_days: unit.withdrawal_days ?? null,
		events: events.map((event) => ({
			occurred_at: event.occurred_at.toISOString(),
			kind: event.kind,
			payment_id: event.payment_id ?? null,
			purchase_id: event.purchase_id ?? null,
			sender_id: event.sender_id?.toString() ?? null,
			recipient_id: event.recipient_id?.toString() ?? null,
			offer_id: event.offer_id ?? null,
			country: event.country ?? null,
			days: event.days ?? null,
			rule_version: event.rule_version ?? null,
			reason: event.reason ?? null,
		})),
	};
}

export function mapOffer(offer: OperatorPassOfferRow): AdminOperatorPassUserResponse['offers'][number] {
	return {
		id: offer.offer_id,
		pass_id: offer.pass_id,
		status: offer.status,
		sender_id: offer.sender_id.toString(),
		recipient_id: offer.recipient_id.toString(),
		created_at: offer.created_at.toISOString(),
		accepted_at: toIsoOrNull(offer.accepted_at),
		arrives_at: toIsoOrNull(offer.completes_at),
		ended_at: toIsoOrNull(offer.ended_at),
		terms_version: offer.terms_version ?? null,
	};
}

export function wallAvatarRoute(userId: UserID | string, sha256: string | null | undefined): string | null {
	return sha256 ? `/admin/users/${userId}/operator-pass/wall/avatars/${sha256}` : null;
}

export function mapWallEntry(entry: OperatorPassWallEntryRow, user: User | null): AdminOperatorPassWallEntryResponse {
	const listed = isOperatorPassWallListed(entry);
	return {
		user_id: entry.user_id.toString(),
		username: user?.username ?? null,
		avatar_url: user?.avatarHash
			? `${Config.endpoints.media}/avatars/${user.id}/${user.avatarHash}.${getAnimatedAssetExtension(user.avatarHash)}`
			: null,
		wall_name: entry.name ?? null,
		wall_opt_out: entry.opted_out,
		keep_as_is: entry.keep_as_is,
		release_list_opt_in: entry.release_list_opt_in ?? false,
		display_state: listed ? operatorPassWallDisplayState(entry, {removed: false}) : null,
		review_state: listed ? operatorPassWallReviewState(entry) : null,
		shown_name: listed && user ? operatorPassWallShownName(entry, user) : null,
		shown_avatar_url: entry.live_avatar
			? operatorPassWallSnapshotUrl(entry.live_avatar, Math.max(...OPERATOR_PASS_WALL_AVATAR_SIZES))
			: null,
		live_revision_id: entry.live_revision_id?.toString() ?? null,
		pending_revision_id: entry.pending_revision_id?.toString() ?? null,
		pending_name: entry.pending_name ?? null,
		pending_avatar_url: wallAvatarRoute(entry.user_id, entry.pending_avatar),
		live_avatar_private_url: wallAvatarRoute(entry.user_id, entry.live_avatar),
		business: entry.live_business ?? false,
		pending_source: entry.pending_source ?? null,
		pending_since: toIsoOrNull(entry.pending_since),
		review_due_at: entry.pending_since
			? new Date(entry.pending_since.getTime() + OPERATOR_PASS_WALL_REVIEW_TARGET_MS).toISOString()
			: null,
		declined_revision_id: entry.declined_revision_id?.toString() ?? null,
		declined_name: entry.declined_name ?? null,
		decline_reason: entry.decline_reason ?? null,
		decline_note: entry.decline_note ?? null,
		username_declined: entry.username_declined,
		hidden: entry.hidden,
		legal_hold: entry.legal_hold,
		left_wall_at: toIsoOrNull(entry.left_wall_at),
		reviewed_at: toIsoOrNull(entry.reviewed_at),
		listed_at: toIsoOrNull(entry.listed_at),
		delivered_at: toIsoOrNull(entry.delivered_at),
		release_published_year: entry.release_published_year ?? null,
		release_published_at: toIsoOrNull(entry.release_published_at),
		release_erased_at: toIsoOrNull(entry.release_erased_at),
	};
}

export function mapWallDecision(
	decision: OperatorPassWallDecisionRow,
): z.infer<typeof AdminOperatorPassWallTimelineResponse>['decisions'][number] {
	return {
		id: decision.decision_id.toString(),
		revision_id: decision.revision_id?.toString() ?? null,
		kind: decision.kind,
		decided_by: decision.decided_by?.toString() ?? null,
		decided_at: decision.decided_at.toISOString(),
		reason: decision.reason ?? null,
		note: decision.note ?? null,
		statement_id: decision.statement_id ?? null,
	};
}

export function mapWallEvent(event: OperatorPassWallEventRow): AdminOperatorPassWallEventResponse {
	return {
		occurred_at: event.occurred_at.toISOString(),
		kind: event.kind,
		name: event.name ?? null,
		reason: event.reason ?? null,
		cause: event.cause ?? null,
		purchase_id: event.purchase_id ?? null,
		release_year: event.release_year ?? null,
		release_tag: event.release_tag ?? null,
		actor_id: event.actor_id?.toString() ?? null,
		pass_count: event.pass_count ?? null,
		first_pass_at: event.first_pass_at?.toISOString() ?? null,
	};
}

export function mapRelease(release: OperatorPassReleaseRow): AdminOperatorPassReleaseFileResponse['release'] {
	return {
		year: release.year,
		tag: release.tag,
		released_at: release.released_at.toISOString(),
		published_at: release.published_at.toISOString(),
		published_by: release.published_by.toString(),
		cutoff_at: release.cutoff_at.toISOString(),
		names: release.names,
		erased_names: release.erased_names ?? [],
	};
}
