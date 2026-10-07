// SPDX-License-Identifier: AGPL-3.0-or-later

import {Relationship, type RelationshipWire} from '@app/features/relationship/models/Relationship';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {makeAutoObservable} from 'mobx';

class Relationships {
	relationships: Record<string, Relationship> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	loadRelationships(relationships: ReadonlyArray<RelationshipWire>): void {
		const newRelationships: Record<string, Relationship> = {};
		for (const relationship of relationships) {
			newRelationships[relationship.id] = new Relationship(relationship);
		}
		this.relationships = newRelationships;
	}

	hydrateFromSnapshot(relationships: ReadonlyArray<RelationshipWire>): void {
		this.loadRelationships(relationships);
	}

	updateRelationship(relationship: RelationshipWire): void {
		const existingRelationship = this.relationships[relationship.id];
		if (existingRelationship) {
			this.relationships = {
				...this.relationships,
				[relationship.id]: existingRelationship.withUpdates(relationship),
			};
		} else {
			this.relationships = {
				...this.relationships,
				[relationship.id]: new Relationship(relationship),
			};
		}
	}

	removeRelationship(relationshipId: string): void {
		const {[relationshipId]: _, ...remainingRelationships} = this.relationships;
		this.relationships = remainingRelationships;
	}

	getRelationship(relationshipId: string): Relationship | undefined {
		return this.relationships[relationshipId];
	}

	get relationshipList(): ReadonlyArray<Relationship> {
		return Object.values(this.relationships);
	}

	getRelationships(): ReadonlyArray<Relationship> {
		return this.relationshipList;
	}

	isBlocked(userId: string): boolean {
		const relationship = this.relationships[userId];
		return relationship?.type === RelationshipTypes.BLOCKED;
	}
}

export default new Relationships();
