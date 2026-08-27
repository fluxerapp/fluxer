// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import MediaViewer, {type MediaViewerItem} from '@app/features/ui/state/MediaViewer';

interface MediaViewerOpenOptions {
	channelId?: string;
	messageId?: string;
	message?: Message;
	sourceChannel?: Channel | null;
	allowAttachmentDelete?: boolean;
}

type MediaViewerIntent =
	| {kind: 'open'; items: ReadonlyArray<MediaViewerItem>; currentIndex: number; options?: MediaViewerOpenOptions}
	| {kind: 'close'}
	| {kind: 'navigate'; index: number};

function dispatchMediaViewerIntent(intent: MediaViewerIntent): void {
	switch (intent.kind) {
		case 'open':
			MediaViewer.open(
				intent.items,
				intent.currentIndex,
				intent.options?.channelId,
				intent.options?.messageId,
				intent.options?.message,
				intent.options?.sourceChannel,
				intent.options?.allowAttachmentDelete,
			);
			return;
		case 'close':
			MediaViewer.close();
			return;
		case 'navigate':
			MediaViewer.navigate(intent.index);
			return;
	}
}

export function openMediaViewer(
	items: ReadonlyArray<MediaViewerItem>,
	currentIndex: number,
	options?: MediaViewerOpenOptions,
): void {
	dispatchMediaViewerIntent({kind: 'open', items, currentIndex, options});
}

export function closeMediaViewer(): void {
	dispatchMediaViewerIntent({kind: 'close'});
}

export function navigateMediaViewer(index: number): void {
	dispatchMediaViewerIntent({kind: 'navigate', index});
}
