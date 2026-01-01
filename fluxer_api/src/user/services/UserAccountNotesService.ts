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

import type {UserID} from '~/BrandedTypes';
import {UnknownUserError} from '~/Errors';
import type {IUserAccountRepository} from '../repositories/IUserAccountRepository';
import type {IUserRelationshipRepository} from '../repositories/IUserRelationshipRepository';
import type {UserAccountUpdatePropagator} from './UserAccountUpdatePropagator';

interface UserAccountNotesServiceDeps {
	userAccountRepository: IUserAccountRepository;
	userRelationshipRepository: IUserRelationshipRepository;
	updatePropagator: UserAccountUpdatePropagator;
}

export class UserAccountNotesService {
	constructor(private readonly deps: UserAccountNotesServiceDeps) {}

	async getUserNote(params: {userId: UserID; targetId: UserID}): Promise<{note: string} | null> {
		const {userId, targetId} = params;
		const note = await this.deps.userRelationshipRepository.getUserNote(userId, targetId);
		return note ? {note: note.note} : null;
	}

	async getUserNotes(userId: UserID): Promise<Record<string, string>> {
		const notes = await this.deps.userRelationshipRepository.getUserNotes(userId);
		return Object.fromEntries(Array.from(notes.entries()).map(([k, v]) => [k.toString(), v]));
	}

	async setUserNote(params: {userId: UserID; targetId: UserID; note: string | null}): Promise<void> {
		const {userId, targetId, note} = params;
		const targetUser = await this.deps.userAccountRepository.findUnique(targetId);
		if (!targetUser) throw new UnknownUserError();

		if (note) {
			await this.deps.userRelationshipRepository.upsertUserNote(userId, targetId, note);
		} else {
			await this.deps.userRelationshipRepository.clearUserNote(userId, targetId);
		}

		await this.deps.updatePropagator.dispatchUserNoteUpdate({userId, targetId, note: note ?? ''});
	}
}
