// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {makeAutoObservable, observable} from 'mobx';

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
	contentType?: string;
	duration?: number;
	expiresAt?: string | null;
	expired?: boolean;
	animated?: boolean;
	providerName?: string;
	initialTime?: number;
}>;

function copyMediaViewerItems(items: ReadonlyArray<MediaViewerItem>): ReadonlyArray<MediaViewerItem> {
	return Object.freeze(items.map((item) => Object.freeze({...item})));
}

class MediaViewer {
	isOpen: boolean = false;
	items: ReadonlyArray<MediaViewerItem> = [];
	currentIndex: number = 0;
	channelId?: string = undefined;
	messageId?: string = undefined;
	message?: Message = undefined;
	sourceChannel?: Channel = undefined;

	constructor() {
		makeAutoObservable(
			this,
			{
				items: observable.ref,
				message: observable.ref,
				sourceChannel: observable.ref,
			},
			{autoBind: true},
		);
	}

	open(
		items: ReadonlyArray<MediaViewerItem>,
		currentIndex: number,
		channelId?: string,
		messageId?: string,
		message?: Message,
		sourceChannel?: Channel | null,
	): void {
		this.isOpen = true;
		this.items = copyMediaViewerItems(items);
		this.currentIndex = currentIndex;
		this.channelId = channelId;
		this.messageId = messageId;
		this.message = message;
		this.sourceChannel = sourceChannel ?? undefined;
	}

	close(): void {
		this.isOpen = false;
		this.items = [];
		this.currentIndex = 0;
		this.channelId = undefined;
		this.messageId = undefined;
		this.message = undefined;
		this.sourceChannel = undefined;
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

export default new MediaViewer();
