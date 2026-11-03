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

export function openMediaViewer(
	items: ReadonlyArray<MediaViewerItem>,
	currentIndex: number,
	options?: MediaViewerOpenOptions,
): void {
	MediaViewer.open(
		items,
		currentIndex,
		options?.channelId,
		options?.messageId,
		options?.message,
		options?.sourceChannel,
		options?.allowAttachmentDelete,
	);
}

export function closeMediaViewer(): void {
	MediaViewer.close();
}

export function navigateMediaViewer(index: number): void {
	MediaViewer.navigate(index);
}
