// SPDX-License-Identifier: AGPL-3.0-or-later

import {action, makeAutoObservable, observable} from 'mobx';

export interface PendingThread {
	channelId: string;
	sourceMessageId?: string;
	sourceMessagePreview?: string;
	sourceMessageAuthor?: string;
}

class ThreadCreationStore {
	@observable pending: PendingThread | null = null;
	@observable createdThreadId: string | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	@action
	open(params: PendingThread): void {
		this.pending = params;
		this.createdThreadId = null;
	}

	@action
	setCreated(threadId: string): void {
		this.createdThreadId = threadId;
		this.pending = null;
	}

	@action
	close(): void {
		this.pending = null;
		this.createdThreadId = null;
	}
}

export default new ThreadCreationStore();
