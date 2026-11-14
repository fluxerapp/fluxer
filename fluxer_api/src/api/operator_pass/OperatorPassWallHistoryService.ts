// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {
	OperatorPassWallDecisionRow,
	OperatorPassWallEntryRow,
	OperatorPassWallEventRow,
	OperatorPassWallRevisionRow,
	OperatorPassWallRevisionStatus,
} from '@app/api/database/types/OperatorPassTypes';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {OPERATOR_PASS_WALL_FALLBACK_NAME} from '@app/api/operator_pass/OperatorPassWallConstants';
import type {OperatorPassWallEntryStore} from '@app/api/operator_pass/OperatorPassWallEntryStore';
import {
	type OperatorPassWallAsOfEntry,
	reconstructOperatorPassWallEntry,
	revisionStatuses,
} from '@app/api/operator_pass/OperatorPassWallHistory';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {compareSnowflakes} from '@fluxer/snowflake/src/SnowflakeComparison';

const AS_OF_BATCH_SIZE = 25;

interface OperatorPassWallTimelineRevision {
	revision: OperatorPassWallRevisionRow;
	status: OperatorPassWallRevisionStatus;
	decision: OperatorPassWallDecisionRow | null;
}

interface OperatorPassWallTimeline {
	entry: OperatorPassWallEntryRow | null;
	revisions: Array<OperatorPassWallTimelineRevision>;
	decisions: Array<OperatorPassWallDecisionRow>;
	events: Array<OperatorPassWallEventRow>;
}

export class OperatorPassWallHistoryService {
	constructor(
		private readonly repository: OperatorPassRepository,
		private readonly userRepository: UserRepository,
		private readonly entries: OperatorPassWallEntryStore,
	) {}

	async timeline(userId: UserID): Promise<OperatorPassWallTimeline> {
		const [entry, revisions, decisions, events] = await Promise.all([
			this.repository.findWallEntry(userId),
			this.repository.listWallRevisions(userId),
			this.repository.listWallDecisions(userId),
			this.repository.listWallEvents(userId),
		]);
		const statuses = revisionStatuses(revisions, decisions);
		return {
			entry,
			revisions: revisions.map((revision) => ({
				revision,
				status: statuses.get(revision.revision_id)?.status ?? 'pending',
				decision: statuses.get(revision.revision_id)?.decision ?? null,
			})),
			decisions,
			events,
		};
	}

	async wallAsOf(at: Date): Promise<Array<OperatorPassWallAsOfEntry>> {
		const result: Array<OperatorPassWallAsOfEntry> = [];
		const entries = await this.entries.scanAll();
		for (let index = 0; index < entries.length; index += AS_OF_BATCH_SIZE) {
			const items = await Promise.all(
				entries.slice(index, index + AS_OF_BATCH_SIZE).map(async (entry) => {
					const [revisions, decisions, events, user] = await Promise.all([
						this.repository.listWallRevisions(entry.user_id),
						this.repository.listWallDecisions(entry.user_id),
						this.repository.listWallEvents(entry.user_id),
						this.userRepository.findUnique(entry.user_id),
					]);
					return reconstructOperatorPassWallEntry({
						userId: entry.user_id,
						at,
						revisions,
						decisions,
						events,
						tierMinimums: Config.operatorPass.wallTierMinimums,
						userDeleted: !user || (user.flags & UserFlags.DELETED) !== 0n,
					});
				}),
			);
			for (const item of items) {
				if (item) result.push(item);
			}
		}
		for (const tombstone of await this.repository.listWallTombstones()) {
			const tokenId = tombstone.token_ as UserID;
			const item = reconstructOperatorPassWallEntry({
				userId: tokenId,
				at,
				revisions: [],
				decisions: [],
				events: await this.repository.listWallEvents(tokenId),
				tierMinimums: Config.operatorPass.wallTierMinimums,
			});
			if (item) result.push({...item, name: OPERATOR_PASS_WALL_FALLBACK_NAME, avatar: null, deleted: true});
		}
		result.sort(
			(left, right) =>
				right.tier - left.tier ||
				left.firstPassAt.getTime() - right.firstPassAt.getTime() ||
				compareSnowflakes(left.userId, right.userId),
		);
		return result;
	}
}
