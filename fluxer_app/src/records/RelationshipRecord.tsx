/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {UserPartial, UserRecord} from '~/records/UserRecord';
import UserStore from '~/stores/UserStore';

export type Relationship = Readonly<{
	id: string;
	type: number;
	user?: UserPartial;
	since: string;
	nickname?: string | null;
}>;

export class RelationshipRecord {
	readonly id: string;
	readonly type: number;
	readonly userId: string;
	readonly since: Date;
	readonly nickname: string | null;

	constructor(relationship: Relationship) {
		if (relationship.user) {
			UserStore.cacheUsers([relationship.user]);
			this.userId = relationship.user.id;
		} else {
			this.userId = relationship.id;
		}
		this.id = relationship.id;
		this.type = relationship.type;
		this.since = new Date(relationship.since);
		this.nickname = relationship.nickname ?? null;
	}

	get user(): UserRecord {
		return UserStore.getUser(this.userId)!;
	}

	withUpdates(relationship: Relationship): RelationshipRecord {
		const mergedUser = relationship.user
			? {
					...this.user?.toJSON(),
					...relationship.user,
				}
			: this.user?.toJSON();

		return new RelationshipRecord({
			id: relationship.id ?? this.id,
			type: relationship.type ?? this.type,
			since: relationship.since ?? this.since.toISOString(),
			nickname: relationship.nickname ?? this.nickname,
			user: mergedUser,
		});
	}
}
