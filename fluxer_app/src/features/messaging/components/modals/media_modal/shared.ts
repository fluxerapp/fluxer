// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';
import type {MouseEventHandler, ReactNode} from 'react';

export type ZoomState = 'fit' | 'zoomed';

export interface MediaThumbnail {
	src: string;
	alt?: string;
	type?: 'image' | 'gif' | 'gifv' | 'video' | 'audio';
}

export interface MediaModalProps {
	title: string;
	fileName?: string;
	fileSize?: string;
	dimensions?: string;
	expiryInfo?: {expiresAt: Date | null; isExpired: boolean};
	isFavorited?: boolean;
	onFavorite?: () => void;
	onDownload?: () => void;
	onOpenInBrowser?: () => void;
	onCopyLink?: () => void;
	onCopyMedia?: () => void;
	onDeleteAttachment?: MouseEventHandler<HTMLButtonElement>;
	onReply?: () => void;
	onForward?: () => void;
	onInfo?: () => void;
	additionalActions?: ReactNode;
	children: ReactNode;
	enablePanZoom?: boolean;
	currentIndex?: number;
	totalAttachments?: number;
	onPrevious?: () => void;
	onNext?: () => void;
	thumbnails?: ReadonlyArray<MediaThumbnail>;
	onSelectThumbnail?: (index: number) => void;
	providerName?: string;
	videoSrc?: string;
	initialTime?: number;
	mediaType?: 'image' | 'video' | 'audio';
	onMenuOpen?: () => void;
}

export const clamp = (value: number, min: number, max: number) => Math.min(max, Math.max(min, value));
export const getViewportPadding = () => {
	const minSide = Math.min(window.innerWidth, window.innerHeight);
	return Math.round(clamp(minSide * 0.05, 16, 64));
};
export const getNativeTitlebarHeight = () => {
	if (typeof document === 'undefined') return 0;
	const raw = getComputedStyle(document.documentElement).getPropertyValue('--native-titlebar-height').trim();
	const parsed = Number.parseFloat(raw);
	return Number.isFinite(parsed) ? parsed : 0;
};
export const PREVIOUS_ATTACHMENT_DESCRIPTOR = msg({
	message: 'Previous attachment',
	comment: 'Media viewer navigation button label for the previous attachment.',
});
export const MESSAGE_DESCRIPTOR = msg({
	message: '{currentAttachmentNumber}/{totalAttachments}',
	comment: 'Compact media viewer attachment position, such as 2/5.',
});
export const NEXT_ATTACHMENT_DESCRIPTOR = msg({
	message: 'Next attachment',
	comment: 'Media viewer navigation button label for the next attachment.',
});
export const MEDIA_CONTROLS_DESCRIPTOR = msg({
	message: 'Media controls',
	comment: 'Accessible label for the floating media controls toolbar in the media viewer modal.',
});
export const DOWNLOAD_MEDIA_DESCRIPTOR = msg({
	message: 'Download media',
	comment: 'Tooltip on the download button in the media viewer modal.',
});
export const OPEN_IN_BROWSER_DESCRIPTOR = msg({
	message: 'Open in browser',
	comment: 'Media viewer action that opens the current media in the system default browser.',
});
export const SHOW_MEDIA_INFORMATION_DESCRIPTOR = msg({
	message: 'Show media information',
	comment: 'Tooltip on the info button in the media viewer modal. Toggles a metadata sidebar.',
});
export const COPY_MEDIA_TO_CLIPBOARD_DESCRIPTOR = msg({
	message: 'Copy media to clipboard',
	comment: 'Tooltip on the media viewer button that copies the current media item to the system clipboard.',
});
export const RESET_MEDIA_POSITION_DESCRIPTOR = msg({
	message: 'Reset media position',
	comment: 'Tooltip on the media viewer button that recenters the current media item and restores the default zoom.',
});
export const ROTATE_CLOCKWISE_DESCRIPTOR = msg({
	message: 'Rotate clockwise',
	comment: 'Tooltip on the media viewer button that rotates the current media item 90 degrees clockwise.',
});
export const ROTATE_ANTICLOCKWISE_DESCRIPTOR = msg({
	message: 'Rotate anticlockwise',
	comment: 'Tooltip on the media viewer button that rotates the current media item 90 degrees anticlockwise.',
});
export const FORWARD_MEDIA_DESCRIPTOR = msg({
	message: 'Forward media',
	comment: 'Tooltip on the media viewer button that forwards only the current media item.',
});
export const REPLY_TO_MEDIA_MESSAGE_DESCRIPTOR = msg({
	message: 'Reply to message',
	comment: 'Tooltip on the media viewer button that closes the viewer and replies to the message containing the media.',
});
export const CLOSE_MODAL_DESCRIPTOR = msg({
	message: 'Close modal',
	comment: 'Generic accessible label for the close button on the media viewer modal backdrop.',
});
export const CLOSE_MEDIA_VIEWER_DESCRIPTOR = msg({
	message: 'Close media viewer',
	comment: 'Accessible label for the dedicated close button at the top of the media viewer modal.',
});
export const PREVIOUS_ATTACHMENT_2_DESCRIPTOR = msg({
	message: 'Previous attachment',
	comment: 'Accessible label for the previous-attachment navigation button in the media viewer.',
});
export const NEXT_ATTACHMENT_2_DESCRIPTOR = msg({
	message: 'Next attachment',
	comment: 'Accessible label for the next-attachment navigation button in the media viewer.',
});
export const ATTACHMENT_THUMBNAILS_DESCRIPTOR = msg({
	message: 'Attachment thumbnails',
	comment: 'Accessible label for the thumbnail strip at the bottom of the media viewer.',
});
export const ATTACHMENT_DESCRIPTOR = msg({
	message: 'Attachment {index1}',
	comment: 'Accessible label for a thumbnail in the media viewer. index1 is the 1-based attachment index.',
});
export const VIDEO_PREVIEW_DESCRIPTOR = msg({
	message: 'Video preview',
	comment: 'Accessible label for a video thumbnail in the media viewer thumbnail strip.',
});
export const PREVIOUS_ATTACHMENT_3_DESCRIPTOR = msg({
	message: 'Previous attachment',
	comment: 'Media viewer navigation tooltip for the previous attachment.',
});
export const OF_DESCRIPTOR = msg({
	message: '{currentAttachmentNumber} of {totalAttachments}',
	comment: 'Media viewer attachment position text, such as 2 of 5.',
});
export const NEXT_ATTACHMENT_3_DESCRIPTOR = msg({
	message: 'Next attachment',
	comment: 'Media viewer navigation tooltip for the next attachment.',
});
