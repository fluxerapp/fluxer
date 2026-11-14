// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import type {UserAccountUpdatePropagator} from '@app/api/user/services/UserAccountUpdatePropagator';

interface UserAccountNotesServiceDeps {
	userAccountRepository: UserRepository;

	updatePropagator: UserAccountUpdatePropagator;
}

export class UserAccountNotesService {
	constructor(private readonly deps: UserAccountNotesServiceDeps) {}

	async getUserNote(params: {userId: UserID; targetId: UserID}): Promise<{
		note: string;
	} | null> {
		const {userId, targetId} = params;
		const note = await this.deps.userAccountRepository.getUserNote(userId, targetId);
		return note ? {note: note.note} : null;
	}

	async getUserNotes(userId: UserID): Promise<Record<string, string>> {
		const notes = await this.deps.userAccountRepository.getUserNotes(userId);
		return Object.fromEntries(Array.from(notes.entries()).map(([k, v]) => [k.toString(), v]));
	}

	async setUserNote(params: {userId: UserID; targetId: UserID; note: string | null}): Promise<void> {
		const {userId, targetId, note} = params;
		await this.deps.userAccountRepository.findUniqueAssert(targetId);
		if (note) {
			await this.deps.userAccountRepository.upsertUserNote(userId, targetId, note);
		} else {
			await this.deps.userAccountRepository.clearUserNote(userId, targetId);
		}
		await this.deps.updatePropagator.dispatchUserNoteUpdate({userId, targetId, note: note ?? ''});
	}
}
