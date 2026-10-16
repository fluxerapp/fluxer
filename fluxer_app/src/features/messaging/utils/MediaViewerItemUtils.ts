// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildMediaProxyURL,
	resolvePreferredImageFormat,
	stripMediaProxyParams,
} from '@app/features/messaging/utils/MediaProxyUtils';
import type {MediaViewerItem} from '@app/features/ui/state/MediaViewer';
import {MessageAttachmentFlags} from '@fluxer/constants/src/ChannelConstants';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

export function determineMediaType(attachment: MessageAttachment): 'audio' | 'video' | 'gifv' | 'gif' | 'image' {
	if (attachment.content_type?.startsWith('audio/')) {
		return 'audio';
	}
	if (attachment.content_type?.startsWith('video/') && (attachment.flags & MessageAttachmentFlags.IS_ANIMATED) !== 0) {
		return 'gifv';
	}
	if ((attachment.flags & MessageAttachmentFlags.IS_ANIMATED) !== 0 || attachment.content_type === 'image/gif') {
		return 'gif';
	}
	if (attachment.content_type?.startsWith('video/')) {
		return 'video';
	}
	return 'image';
}

export function attachmentToViewerItem(
	attachment: MessageAttachment,
	overrides?: Partial<MediaViewerItem>,
): MediaViewerItem {
	const type = determineMediaType(attachment);
	return {
		src: attachment.proxy_url ?? attachment.url ?? '',
		originalSrc: attachment.url ?? '',
		naturalWidth: attachment.width || 0,
		naturalHeight: attachment.height || 0,
		type,
		contentHash: attachment.content_hash,
		attachmentId: attachment.id,
		filename: attachment.filename,
		fileSize: attachment.size,
		contentType: attachment.content_type,
		duration: attachment.duration,
		expiresAt: attachment.expires_at ?? null,
		expired: attachment.expired ?? false,
		animated: type === 'gif' || type === 'gifv',
		...overrides,
	};
}

interface AttachmentsToViewerItemsOptions {
	filterType?: 'video';
	initialTimeForId?: {
		attachmentId: string;
		time: number;
	};
}

export function attachmentsToViewerItems(
	attachments: ReadonlyArray<MessageAttachment>,
	options?: AttachmentsToViewerItemsOptions,
): Array<MediaViewerItem> {
	const filtered = options?.filterType
		? attachments.filter((att) => att.content_type?.startsWith(`${options.filterType}/`))
		: attachments;
	return filtered.map((att) => {
		const initialTimeMatch = options?.initialTimeForId?.attachmentId === att.id;
		return attachmentToViewerItem(att, initialTimeMatch ? {initialTime: options!.initialTimeForId!.time} : undefined);
	});
}

export function findViewerItemIndex(items: ReadonlyArray<MediaViewerItem>, attachmentId?: string): number {
	return Math.max(
		0,
		items.findIndex((item) => item.attachmentId === attachmentId),
	);
}

export function getBaseProxyURL(src: string): string {
	if (src.startsWith('blob:')) {
		return src;
	}
	return stripMediaProxyParams(src);
}

function normalizeContentType(contentType?: string): string | undefined {
	return contentType?.toLowerCase().split(';')[0]?.trim() || undefined;
}

function inferImageContentTypeFromURL(src: string): string | undefined {
	try {
		const path = new URL(src).pathname.toLowerCase();
		if (path.endsWith('.png')) return 'image/png';
		if (path.endsWith('.jpg') || path.endsWith('.jpeg')) return 'image/jpeg';
		if (path.endsWith('.webp')) return 'image/webp';
		if (path.endsWith('.gif')) return 'image/gif';
		if (path.endsWith('.svg')) return 'image/svg+xml';
		if (path.endsWith('.avif')) return 'image/avif';
		if (path.endsWith('.jxl')) return 'image/jxl';
	} catch {
		return undefined;
	}
	return undefined;
}

function resolveViewerStaticImageFormat(contentType: string | undefined, src: string): 'webp' | undefined {
	const normalizedContentType = normalizeContentType(contentType) ?? inferImageContentTypeFromURL(src);
	switch (normalizedContentType) {
		case 'image/png':
		case 'image/jpeg':
		case 'image/webp':
		case 'image/gif':
		case 'image/svg+xml':
			return undefined;
		default:
			return resolvePreferredImageFormat(normalizedContentType);
	}
}

function buildViewerStaticImageURL(item: MediaViewerItem): string {
	if (item.src.startsWith('blob:')) {
		return item.src;
	}
	const baseProxyURL = getBaseProxyURL(item.src);
	return buildMediaProxyURL(baseProxyURL, {
		format: resolveViewerStaticImageFormat(item.contentType, baseProxyURL),
	});
}

export function isGifvRenderedAsImage(item: MediaViewerItem): boolean {
	return item.type === 'gifv' && (item.src.endsWith('.gif') || item.originalSrc.endsWith('.gif'));
}

export function isViewerImageItem(item: MediaViewerItem): boolean {
	return item.type === 'image' || item.type === 'gif' || isGifvRenderedAsImage(item);
}

export function buildViewerMediaURL(item: MediaViewerItem): string {
	if (item.src.startsWith('blob:')) {
		return item.src;
	}
	const baseProxyURL = getBaseProxyURL(item.src);
	if (item.animated || item.type === 'gif') {
		return buildMediaProxyURL(baseProxyURL, {
			format: resolvePreferredImageFormat(item.contentType),
			animated: true,
		});
	}
	if (item.type === 'gifv' || item.type === 'video' || item.type === 'audio') {
		return baseProxyURL;
	}
	return buildViewerStaticImageURL(item);
}
