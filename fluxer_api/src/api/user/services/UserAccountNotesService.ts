// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {IUserAccountRepository} from '@app/api/user/repositories/IUserAccountRepository';
import type {IUserRelationshipRepository} from '@app/api/user/repositories/IUserRelationshipRepository';
import type {UserAccountUpdatePropagator} from '@app/api/user/services/UserAccountUpdatePropagator';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';

interface UserAccountNotesServiceDeps {
	userAccountRepository: IUserAccountRepository;
	userRelationshipRepository: IUserRelationshipRepository;
	updatePropagator: UserAccountUpdatePropagator;
}

export class UserAccountNotesService {
	constructor(private readonly deps: UserAccountNotesServiceDeps) {}

	async getUserNote(params: {userId: UserID; targetId: UserID}): Promise<{
		note: string;
	} | null> {
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
