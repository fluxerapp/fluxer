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
import type {MessageRecord} from '~/records/MessageRecord';

export type MediaViewerItem = Readonly<{
	src: string;
	originalSrc: string;
	naturalWidth: number;
	naturalHeight: number;
	type: 'image' | 'gif' | 'gifv' | 'video' | 'audio';
	contentHash?: string | null;
	attachmentId?: string;
	embedIndex?: number;
	filename?: string;
	fileSize?: number;
	duration?: number;
	expiresAt?: string | null;
	expired?: boolean;
}>;

class MediaViewerStore {
	isOpen: boolean = false;
	items: ReadonlyArray<MediaViewerItem> = [];
	currentIndex: number = 0;
	channelId?: string = undefined;
	messageId?: string = undefined;
	message?: MessageRecord = undefined;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	open(
		items: ReadonlyArray<MediaViewerItem>,
		currentIndex: number,
		channelId?: string,
		messageId?: string,
		message?: MessageRecord,
	): void {
		this.isOpen = true;
		this.items = items;
		this.currentIndex = currentIndex;
		this.channelId = channelId;
		this.messageId = messageId;
		this.message = message;
	}

	close(): void {
		this.isOpen = false;
		this.items = [];
		this.currentIndex = 0;
		this.channelId = undefined;
		this.messageId = undefined;
		this.message = undefined;
	}

	navigate(index: number): void {
		if (index < 0 || index >= this.items.length) {
			return;
		}

		this.currentIndex = index;
	}

	getCurrentItem(): MediaViewerItem | undefined {
		if (!this.isOpen || this.items.length === 0) {
			return;
		}
		return this.items[this.currentIndex];
	}

	canNavigatePrevious(): boolean {
		return this.isOpen && this.currentIndex > 0;
	}

	canNavigateNext(): boolean {
		return this.isOpen && this.currentIndex < this.items.length - 1;
	}
}

export default new MediaViewerStore();
