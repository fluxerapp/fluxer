// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EntranceSoundID, UserID} from '@app/api/BrandedTypes';
import type {UserEntranceSoundRow, UserEntranceSoundSelectionRow} from '@app/api/database/types/UserTypes';

export class EntranceSound {
	readonly userId: UserID;
	readonly soundId: EntranceSoundID;
	readonly name: string;
	readonly hash: string;
	readonly extension: string;
	readonly contentType: string;
	readonly durationMs: number;
	readonly sizeBytes: number;
	readonly createdAt: Date;
	readonly version: number;

	constructor(row: UserEntranceSoundRow) {
		this.userId = row.user_id;
		this.soundId = row.sound_id;
		this.name = row.name;
		this.hash = row.hash;
		this.extension = row.extension;
		this.contentType = row.content_type;
		this.durationMs = row.duration_ms;
		this.sizeBytes = row.size_bytes;
		this.createdAt = row.created_at;
		this.version = row.version;
	}

	toRow(): UserEntranceSoundRow {
		return {
			user_id: this.userId,
			sound_id: this.soundId,
			name: this.name,
			hash: this.hash,
			extension: this.extension,
			content_type: this.contentType,
			duration_ms: this.durationMs,
			size_bytes: this.sizeBytes,
			created_at: this.createdAt,
			version: this.version,
		};
	}
}

export class EntranceSoundSelection {
	readonly userId: UserID;
	readonly scopeId: string;
	readonly soundId: EntranceSoundID;

	constructor(row: UserEntranceSoundSelectionRow) {
		this.userId = row.user_id;
		this.scopeId = row.scope_id;
		this.soundId = row.sound_id;
	}

	toRow(): UserEntranceSoundSelectionRow {
		return {
			user_id: this.userId,
			scope_id: this.scopeId,
			sound_id: this.soundId,
		};
	}
}
