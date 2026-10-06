// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EntranceSoundID, UserID} from '@app/api/BrandedTypes';
import {deleteOneOrMany, fetchMany, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {UserEntranceSoundRow, UserEntranceSoundSelectionRow} from '@app/api/database/types/UserTypes';
import {EntranceSound, EntranceSoundSelection} from '@app/api/models/EntranceSound';
import {UserEntranceSoundSelections, UserEntranceSounds} from '@app/api/Tables';

const LIST_SOUNDS_CQL = UserEntranceSounds.select({
	where: UserEntranceSounds.where.eq('user_id'),
});
const FETCH_SOUND_CQL = UserEntranceSounds.select({
	where: [UserEntranceSounds.where.eq('user_id'), UserEntranceSounds.where.eq('sound_id')],
	limit: 1,
});
const LIST_SELECTIONS_CQL = UserEntranceSoundSelections.select({
	where: UserEntranceSoundSelections.where.eq('user_id'),
});
const FETCH_SELECTION_CQL = UserEntranceSoundSelections.select({
	where: [UserEntranceSoundSelections.where.eq('user_id'), UserEntranceSoundSelections.where.eq('scope_id')],
	limit: 1,
});

export class EntranceSoundRepository {
	async listSounds(userId: UserID): Promise<Array<EntranceSound>> {
		const rows = await fetchMany<UserEntranceSoundRow>(LIST_SOUNDS_CQL.bind({user_id: userId}));
		return rows.map((row) => new EntranceSound(row));
	}

	async getSound(userId: UserID, soundId: EntranceSoundID): Promise<EntranceSound | null> {
		const row = await fetchOne<UserEntranceSoundRow>(FETCH_SOUND_CQL.bind({user_id: userId, sound_id: soundId}));
		return row ? new EntranceSound(row) : null;
	}

	async upsertSound(sound: EntranceSound): Promise<EntranceSound> {
		await upsertOne(UserEntranceSounds.upsertAll(sound.toRow()));
		return sound;
	}

	async deleteSound(userId: UserID, soundId: EntranceSoundID): Promise<void> {
		await deleteOneOrMany(UserEntranceSounds.deleteByPk({user_id: userId, sound_id: soundId}));
	}

	async deleteAllSoundsForUser(userId: UserID): Promise<void> {
		await deleteOneOrMany(
			UserEntranceSounds.delete({where: UserEntranceSounds.where.eq('user_id')}).bind({user_id: userId}),
		);
	}

	async listSelections(userId: UserID): Promise<Array<EntranceSoundSelection>> {
		const rows = await fetchMany<UserEntranceSoundSelectionRow>(LIST_SELECTIONS_CQL.bind({user_id: userId}));
		return rows.map((row) => new EntranceSoundSelection(row));
	}

	async getSelection(userId: UserID, scopeId: string): Promise<EntranceSoundSelection | null> {
		const row = await fetchOne<UserEntranceSoundSelectionRow>(
			FETCH_SELECTION_CQL.bind({user_id: userId, scope_id: scopeId}),
		);
		return row ? new EntranceSoundSelection(row) : null;
	}

	async upsertSelection(selection: EntranceSoundSelection): Promise<EntranceSoundSelection> {
		await upsertOne(UserEntranceSoundSelections.upsertAll(selection.toRow()));
		return selection;
	}

	async deleteSelection(userId: UserID, scopeId: string): Promise<void> {
		await deleteOneOrMany(UserEntranceSoundSelections.deleteByPk({user_id: userId, scope_id: scopeId}));
	}

	async deleteSelectionsForSound(userId: UserID, soundId: EntranceSoundID): Promise<void> {
		const selections = await this.listSelections(userId);
		for (const selection of selections) {
			if (selection.soundId === soundId) {
				await this.deleteSelection(userId, selection.scopeId);
			}
		}
	}

	async deleteAllSelectionsForUser(userId: UserID): Promise<void> {
		await deleteOneOrMany(
			UserEntranceSoundSelections.delete({where: UserEntranceSoundSelections.where.eq('user_id')}).bind({
				user_id: userId,
			}),
		);
	}
}
