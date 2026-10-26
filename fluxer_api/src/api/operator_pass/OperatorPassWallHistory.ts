// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {
	OperatorPassWallDecisionRow,
	OperatorPassWallEventRow,
	OperatorPassWallRevisionRow,
	OperatorPassWallRevisionStatus,
} from '@app/api/database/types/OperatorPassTypes';
import {getOperatorPassWallTier} from '@app/api/operator_pass/OperatorPassPrices';
import {OPERATOR_PASS_WALL_FALLBACK_NAME} from '@app/api/operator_pass/OperatorPassWallConstants';
import {compareSnowflakes} from '@fluxer/snowflake/src/SnowflakeComparison';

type WallHistoryItem =
	| {kind: 'revision'; at: bigint; revision: OperatorPassWallRevisionRow}
	| {kind: 'decision'; at: bigint; decision: OperatorPassWallDecisionRow};

export function revisionStatuses(
	revisions: ReadonlyArray<OperatorPassWallRevisionRow>,
	decisions: ReadonlyArray<OperatorPassWallDecisionRow>,
): Map<bigint, {status: OperatorPassWallRevisionStatus; decision: OperatorPassWallDecisionRow | null}> {
	const result = new Map<
		bigint,
		{status: OperatorPassWallRevisionStatus; decision: OperatorPassWallDecisionRow | null}
	>(revisions.map((revision) => [revision.revision_id, {status: 'pending', decision: null}]));
	let live: bigint | null = null;
	const timeline = [
		...revisions.map((revision): WallHistoryItem => ({kind: 'revision', at: revision.revision_id, revision})),
		...decisions.map((decision): WallHistoryItem => ({kind: 'decision', at: decision.decision_id, decision})),
	].sort((left, right) => compareSnowflakes(left.at, right.at));
	for (const item of timeline) {
		if (item.kind === 'revision') {
			for (const [id, value] of result) {
				if (value.status === 'pending' && id < item.revision.revision_id) {
					result.set(id, {status: 'superseded', decision: null});
				}
			}
			continue;
		}
		const decision = item.decision;
		const target = decision.revision_id ?? null;
		if (decision.kind === 'force_fallback') {
			if (live != null) {
				result.set(live, {status: 'reverted', decision});
				live = null;
			}
			continue;
		}
		if (target == null) continue;
		switch (decision.kind) {
			case 'approve':
				result.set(target, {status: 'approved', decision});
				live = target;
				break;
			case 'decline':
				result.set(target, {status: 'declined', decision});
				break;
			case 'revert':
				if (live != null && live !== target) {
					result.set(live, {status: 'reverted', decision});
				}
				live = target;
				break;
		}
	}
	return result;
}

export interface OperatorPassWallAsOfEntry {
	userId: UserID;
	name: string;
	avatar: string | null;
	count: number;
	tier: number;
	firstPassAt: Date;
	deleted: boolean;
}

export function reconstructOperatorPassWallEntry(params: {
	userId: UserID;
	at: Date;
	username: string | null;
	revisions: ReadonlyArray<OperatorPassWallRevisionRow>;
	decisions: ReadonlyArray<OperatorPassWallDecisionRow>;
	events: ReadonlyArray<OperatorPassWallEventRow>;
	tierMinimums: ReadonlyArray<number>;
	userDeleted?: boolean;
}): OperatorPassWallAsOfEntry | null {
	const atMs = params.at.getTime();
	const events = params.events.filter((event) => event.occurred_at.getTime() <= atMs);
	let count = 0;
	let firstPassAt: Date | null = null;
	for (const event of events.filter((event) => event.kind === 'count_changed')) {
		const next = event.pass_count ?? 0;
		if (count === 0 && next > 0) firstPassAt = event.occurred_at;
		if (event.first_pass_at != null) firstPassAt = event.first_pass_at;
		if (next === 0) firstPassAt = null;
		count = next;
	}
	const listing = events.findLast(
		(event) => event.kind === 'listed' || event.kind === 'changed' || event.kind === 'opted_out',
	);
	if (count === 0 || listing == null || listing.kind === 'opted_out' || !firstPassAt) {
		return null;
	}
	const deleted =
		params.userDeleted === true ||
		params.events.some((event) => event.kind === 'taken_down' && event.cause === 'deletion');
	const revisions = params.revisions.filter((revision) => revision.created_at.getTime() <= atMs);
	const decisions = params.decisions.filter((decision) => decision.decided_at.getTime() <= atMs);
	let liveId: bigint | null = null;
	let hidden = false;
	let usernameDeclined = false;
	for (const decision of decisions) {
		switch (decision.kind) {
			case 'approve':
				liveId = decision.revision_id ?? liveId;
				usernameDeclined = false;
				break;
			case 'revert':
				liveId = decision.revision_id ?? liveId;
				break;
			case 'force_fallback':
				liveId = null;
				break;
			case 'hide':
				hidden = true;
				break;
			case 'unhide':
				hidden = false;
				break;
		}
		usernameDeclined = decision.username_declined === true || usernameDeclined;
	}
	if (hidden) {
		return null;
	}
	const live = liveId != null ? revisions.find((revision) => revision.revision_id === liveId) : undefined;
	const tier = getOperatorPassWallTier(count, params.tierMinimums);
	if (deleted || live?.name === '') {
		return {
			userId: params.userId,
			name: OPERATOR_PASS_WALL_FALLBACK_NAME,
			avatar: null,
			count,
			tier,
			firstPassAt,
			deleted: true,
		};
	}
	let name = live?.name;
	if (name == null) {
		name = usernameDeclined || !params.username ? OPERATOR_PASS_WALL_FALLBACK_NAME : params.username;
	}
	return {userId: params.userId, name, avatar: live?.avatar ?? null, count, tier, firstPassAt, deleted: false};
}
