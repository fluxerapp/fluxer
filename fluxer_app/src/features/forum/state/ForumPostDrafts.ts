// SPDX-License-Identifier: AGPL-3.0-or-later

import {ResettableStates} from '@app/features/app/state/ResettableStates';
import {makeAutoObservable} from 'mobx';

interface ForumPostDraft {
	title: string;
	tags: Array<string>;
}

class ForumPostDrafts {
	drafts: Record<string, ForumPostDraft> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get(forumId: string): ForumPostDraft | undefined {
		return this.drafts[forumId];
	}

	open(forumId: string, title: string): void {
		this.drafts[forumId] = {title, tags: []};
	}

	update(forumId: string, changes: Partial<ForumPostDraft>): void {
		const draft = this.drafts[forumId];
		if (draft) Object.assign(draft, changes);
	}

	close(forumId: string): void {
		delete this.drafts[forumId];
	}

	reset(): void {
		this.drafts = {};
	}
}

const forumPostDrafts = new ForumPostDrafts();

ResettableStates.register(forumPostDrafts, () => forumPostDrafts.reset());

export default forumPostDrafts;
