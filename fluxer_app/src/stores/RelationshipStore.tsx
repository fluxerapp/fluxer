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

import {makeAutoObservable} from 'mobx';
import {RelationshipTypes} from '~/Constants';
import {type Relationship, RelationshipRecord} from '~/records/RelationshipRecord';

class RelationshipStore {
	relationships: Record<string, RelationshipRecord> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	loadRelationships(relationships: ReadonlyArray<Relationship>): void {
		const newRelationships: Record<string, RelationshipRecord> = {};

		for (const relationship of relationships) {
			newRelationships[relationship.id] = new RelationshipRecord(relationship);
		}

		this.relationships = newRelationships;
	}

	updateRelationship(relationship: Relationship): void {
		const existingRelationship = this.relationships[relationship.id];

		if (existingRelationship) {
			this.relationships = {
				...this.relationships,
				[relationship.id]: existingRelationship.withUpdates(relationship),
			};
		} else {
			this.relationships = {
				...this.relationships,
				[relationship.id]: new RelationshipRecord(relationship),
			};
		}
	}

	removeRelationship(relationshipId: string): void {
		const {[relationshipId]: _, ...remainingRelationships} = this.relationships;
		this.relationships = remainingRelationships;
	}

	getRelationship(relationshipId: string): RelationshipRecord | undefined {
		return this.relationships[relationshipId];
	}

	getRelationships(): ReadonlyArray<RelationshipRecord> {
		return Object.values(this.relationships);
	}

	isBlocked(userId: string): boolean {
		const relationship = this.relationships[userId];
		return relationship?.type === RelationshipTypes.BLOCKED;
	}
}

export default new RelationshipStore();
