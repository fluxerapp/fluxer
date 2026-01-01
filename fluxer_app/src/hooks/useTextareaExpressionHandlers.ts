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

import React from 'react';
import type {TenorGif} from '~/actions/TenorActionCreators';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import type {FavoriteMemeRecord} from '~/records/FavoriteMemeRecord';
import type {GuildStickerRecord} from '~/records/GuildStickerRecord';
import UserStore from '~/stores/UserStore';
import type {MentionSegment} from '~/utils/TextareaSegmentManager';

interface UseTextareaExpressionHandlersOptions {
	setValue: React.Dispatch<React.SetStateAction<string>>;
	textareaRef: React.RefObject<HTMLTextAreaElement | null>;
	insertSegment: (
		text: string,
		position: number,
		displayText: string,
		actualText: string,
		type: MentionSegment['type'],
		id: string,
	) => {newText: string; newSegments: Array<MentionSegment>};
	previousValueRef: React.MutableRefObject<string>;
	sendOptimisticMessage: (
		messageData: {content: string; stickers?: Array<any>; attachments?: Array<any>},
		sendOptions: {hasAttachments: boolean; favoriteMemeId?: string},
	) => void;
}

export const useTextareaExpressionHandlers = ({
	setValue,
	textareaRef,
	insertSegment,
	previousValueRef,
	sendOptimisticMessage,
}: UseTextareaExpressionHandlersOptions) => {
	React.useEffect(() => {
		const handleGifSelect = (payload?: unknown) => {
			const {gif, autoSend} = (payload ?? {}) as {gif?: TenorGif; autoSend?: boolean};
			if (!gif) return;
			if (autoSend) {
				sendOptimisticMessage({content: gif.url}, {hasAttachments: false});
			} else {
				setValue((prevValue) => `${prevValue}${prevValue.length === 0 ? '' : ' '}${gif.url} `);
				textareaRef.current?.focus();
			}
		};

		return ComponentDispatch.subscribe('GIF_SELECT', handleGifSelect);
	}, [sendOptimisticMessage, setValue, textareaRef]);

	React.useEffect(() => {
		const handleStickerSelect = (payload?: unknown) => {
			const {sticker} = (payload ?? {}) as {sticker?: GuildStickerRecord};
			if (!sticker) return;
			sendOptimisticMessage({content: '', stickers: [sticker.toJSON()]}, {hasAttachments: false});
		};

		return ComponentDispatch.subscribe('STICKER_SELECT', handleStickerSelect);
	}, [sendOptimisticMessage]);

	React.useEffect(() => {
		const handleFavoriteMemeSelect = (payload?: unknown) => {
			const {meme, autoSend} = (payload ?? {}) as {meme?: FavoriteMemeRecord; autoSend?: boolean};
			if (!meme) return;
			if (autoSend) {
				if (meme.tenorId) {
					const tenorUrl = `https://tenor.com/view/${meme.tenorId}`;
					sendOptimisticMessage({content: tenorUrl}, {hasAttachments: false});
				} else {
					const uploadingAttachment = {
						id: 'uploading',
						filename: meme.filename,
						title: meme.name,
						size: meme.size,
						url: '',
						proxy_url: '',
						content_type: meme.contentType,
						flags: 0x1000,
					};

					sendOptimisticMessage(
						{content: '', attachments: [uploadingAttachment]},
						{hasAttachments: false, favoriteMemeId: meme.id},
					);
				}
			} else {
				if (meme.tenorId) {
					const tenorUrl = `https://tenor.com/view/${meme.tenorId}`;
					setValue((prevValue) => `${prevValue}${prevValue.length === 0 ? '' : ' '}${tenorUrl} `);
				} else {
					setValue((prevValue) => `${prevValue}${prevValue.length === 0 ? '' : ' '}${meme.url} `);
				}
				textareaRef.current?.focus();
			}
		};

		return ComponentDispatch.subscribe('FAVORITE_MEME_SELECT', handleFavoriteMemeSelect);
	}, [sendOptimisticMessage, setValue, textareaRef]);

	React.useEffect(() => {
		const handleInsertMention = (payload?: unknown) => {
			const {userId} = (payload ?? {}) as {userId?: string};
			if (!userId) return;
			setValue((prevValue) => {
				const user = UserStore.getUser(userId);
				if (!user) {
					return prevValue;
				}

				const actualText = `<@${userId}>`;
				const displayText = `@${user.tag}`;
				const needsSpace = prevValue.length > 0 && !prevValue.endsWith(' ');
				const prefix = prevValue.length === 0 ? '' : needsSpace ? ' ' : '';
				const insertPosition = prevValue.length + prefix.length;

				const {newText} = insertSegment(prevValue + prefix, insertPosition, displayText, actualText, 'user', userId);

				if (previousValueRef.current !== null) {
					previousValueRef.current = newText;
				}
				return newText;
			});
			textareaRef.current?.focus();
		};

		return ComponentDispatch.subscribe('INSERT_MENTION', handleInsertMention);
	}, [insertSegment, previousValueRef, setValue, textareaRef]);
};
