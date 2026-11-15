// SPDX-License-Identifier: AGPL-3.0-or-later

import {formatDuration as formatDurationBase} from '@fluxer/date_utils/src/DateDuration';

export const formatDuration = (seconds: number | null | undefined, locale: string): string => {
	if (!seconds || seconds <= 0) return formatDurationBase(0, locale);
	return formatDurationBase(seconds, locale);
};
export const getFileExtension = (filename: string, contentType: string): string => {
	const extension = filename.split('.').pop()?.toUpperCase();
	if (extension && extension.length <= 4) return extension;
	const typeMatch = contentType.match(/\/([^;]+)/);
	return typeMatch?.[1]?.toUpperCase() || 'FILE';
};

export function filterMemesByContentType<T extends {contentType: string; isGifv: boolean}>(
	memes: ReadonlyArray<T>,
	filter: 'all' | 'image' | 'video' | 'audio' | 'gif',
): Array<T> {
	if (filter === 'all') return [...memes];
	return memes.filter((meme) => {
		const contentType = meme.contentType.toLowerCase();
		switch (filter) {
			case 'image':
				return contentType.startsWith('image/') && !contentType.includes('gif') && !meme.isGifv;
			case 'video':
				return contentType.startsWith('video/') && !meme.isGifv;
			case 'audio':
				return contentType.startsWith('audio/');
			case 'gif':
				return contentType.includes('gif') || meme.isGifv;
			default:
				return true;
		}
	});
}
