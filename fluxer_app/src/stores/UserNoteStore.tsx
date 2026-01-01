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
import {Logger} from '~/lib/Logger';

const logger = new Logger('UserNoteStore');

class UserNoteStore {
	notes: Record<string, string> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	loadNotes(notes: Record<string, string>): void {
		logger.debug('Loading user notes');
		this.notes = {...notes};
	}

	updateUserNote(userId: string, note: string): void {
		if (!note) {
			const {[userId]: _, ...remainingNotes} = this.notes;
			this.notes = remainingNotes;
			logger.debug(`Removed note for user ${userId}`);
		} else if (this.notes[userId] !== note) {
			this.notes = {
				...this.notes,
				[userId]: note,
			};
			logger.debug(`Updated note for user ${userId}`);
		}
	}

	clearNote(userId: string): void {
		this.updateUserNote(userId, '');
	}

	getUserNote(userId: string): string {
		return this.notes[userId] ?? '';
	}

	hasNote(userId: string): boolean {
		return userId in this.notes && this.notes[userId].length > 0;
	}
}

export default new UserNoteStore();
