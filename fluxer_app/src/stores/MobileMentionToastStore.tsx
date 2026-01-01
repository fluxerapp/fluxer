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

import {makeAutoObservable, observable} from 'mobx';
import type {MessageRecord} from '~/records/MessageRecord';

const MAX_QUEUE_LENGTH = 5;

class MobileMentionToastStore {
	queue: Array<MessageRecord> = [];

	constructor() {
		makeAutoObservable(this, {queue: observable.shallow}, {autoBind: true});
	}

	enqueue(message: MessageRecord): void {
		if (this.queue.some((entry) => entry.id === message.id)) {
			return;
		}

		this.queue.push(message);

		if (this.queue.length > MAX_QUEUE_LENGTH) {
			this.queue.shift();
		}
	}

	dequeue(targetId?: string): void {
		if (!targetId) {
			this.queue.shift();
			return;
		}

		if (this.queue[0]?.id === targetId) {
			this.queue.shift();
			return;
		}

		this.queue = this.queue.filter((entry) => entry.id !== targetId);
	}

	get current(): MessageRecord | undefined {
		return this.queue[0];
	}

	get count(): number {
		return this.queue.length;
	}
}

export default new MobileMentionToastStore();
