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

import {action, makeAutoObservable} from 'mobx';
import {makePersistent} from '~/lib/MobXPersistence';

class DraftStore {
	drafts: Record<string, string> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'DraftStore', ['drafts']);
	}

	@action
	createDraft(channelId: string, content: string): void {
		if (!content || content === this.drafts[channelId]) {
			return;
		}

		this.drafts = {
			...this.drafts,
			[channelId]: content,
		};
	}

	@action
	deleteDraft(channelId: string): void {
		if (!this.drafts[channelId]) {
			return;
		}

		const {[channelId]: _, ...remainingDrafts} = this.drafts;
		this.drafts = remainingDrafts;
	}

	@action
	deleteChannelDraft(channelId: string): void {
		this.deleteDraft(channelId);
	}

	getDraft(channelId: string): string {
		return this.drafts[channelId] ?? '';
	}

	@action
	cleanupEmptyDrafts(): void {
		this.drafts = Object.fromEntries(Object.entries(this.drafts).filter(([_, content]) => content.trim().length > 0));
	}

	getAllDrafts(): ReadonlyArray<[string, string]> {
		return Object.entries(this.drafts);
	}

	hasDraft(channelId: string): boolean {
		return channelId in this.drafts;
	}

	getDraftCount(): number {
		return Object.keys(this.drafts).length;
	}
}

export default new DraftStore();
