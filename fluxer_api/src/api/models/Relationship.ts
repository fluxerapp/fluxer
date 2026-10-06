// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {RelationshipRow} from '@app/api/database/types/UserTypes';
import type {RelationshipType} from '@fluxer/constants/src/UserConstants';

export class Relationship {
	readonly sourceUserId: UserID;
	readonly targetUserId: UserID;
	readonly type: RelationshipType;
	readonly nickname: string | null;
	readonly since: Date | null;
	readonly shareVoiceActivity: boolean;
	readonly version: number;

	constructor(row: RelationshipRow) {
		this.sourceUserId = row.source_user_id;
		this.targetUserId = row.target_user_id;
		this.type = row.type as RelationshipType;
		this.nickname = row.nickname ?? null;
		this.since = row.since ?? null;
		this.shareVoiceActivity = row.share_voice_activity ?? true;
		this.version = row.version;
	}

	toRow(): RelationshipRow {
		return {
			source_user_id: this.sourceUserId,
			target_user_id: this.targetUserId,
			type: this.type,
			nickname: this.nickname,
			since: this.since,
			share_voice_activity: this.shareVoiceActivity,
			version: this.version,
		};
	}
}
