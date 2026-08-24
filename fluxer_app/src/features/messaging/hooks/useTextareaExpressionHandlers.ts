// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Gif} from '@app/features/expressions/commands/GifCommands';
import type {FavoriteMeme} from '@app/features/expressions/models/FavoriteMeme';
import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import * as GifSlugUtils from '@app/features/expressions/utils/GifSlugUtils';
import {UploadingAttachment} from '@app/features/messaging/models/UploadingAttachment';
import {
	applyTextareaTextChange,
	type PrepareTextareaTextChange,
} from '@app/features/messaging/utils/TextareaNativeEditUtils';
import {type MentionSegment, TextareaSegmentManager} from '@app/features/messaging/utils/TextareaSegmentManager';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import type {MessageAttachment, MessageStickerItem} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {useCallback, useEffect} from 'react';

interface UseTextareaExpressionHandlersOptions {
	setValue: React.Dispatch<React.SetStateAction<string>>;
	textareaRef: React.RefObject<HTMLTextAreaElement | null>;
	canSendFavoriteMemeId: boolean;
	insertSegment: (
		text: string,
		position: number,
		displayText: string,
		actualText: string,
		type: MentionSegment['type'],
		id: string,
	) => {
		newText: string;
		newSegments: Array<MentionSegment>;
	};
	previousValueRef: React.MutableRefObject<string>;
	prepareTextChange: PrepareTextareaTextChange;
	segmentManagerRef: React.MutableRefObject<TextareaSegmentManager>;
	sendOptimisticMessage: (
		messageData: {
			content: string;
			stickers?: Array<MessageStickerItem>;
			attachments?: Array<MessageAttachment>;
		},
		sendOptions: {
			hasAttachments: boolean;
			favoriteMemeId?: string;
		},
	) => void;
	enabled?: boolean;
}

export const useTextareaExpressionHandlers = ({
	setValue,
	textareaRef,
	canSendFavoriteMemeId,
	insertSegment,
	previousValueRef,
	prepareTextChange,
	segmentManagerRef,
	sendOptimisticMessage,
	enabled = true,
}: UseTextareaExpressionHandlersOptions) => {
	const appendText = useCallback(
		(text: string) => {
			if (!enabled) return;
			const prevValue = textareaRef.current?.value ?? previousValueRef.current;
			const prefix = prevValue.length === 0 ? '' : ' ';
			const nextValue = `${prevValue}${prefix}${text} `;
			applyTextareaTextChange({
				textareaRef,
				setValue,
				segmentManagerRef,
				previousValueRef,
				prepareTextChange,
				nextValue,
				nextSegments: segmentManagerRef.current.getSegmentsCopy(),
				selectionStart: nextValue.length,
			});
		},
		[enabled, prepareTextChange, previousValueRef, segmentManagerRef, setValue, textareaRef],
	);
	useEffect(() => {
		const handleGifSelect = (payload?: unknown) => {
			if (!enabled) return;
			const {gif, autoSend} = (payload ?? {}) as {
				gif?: Gif;
				autoSend?: boolean;
			};
			if (!gif) return;
			const gifUrl = GifSlugUtils.resolveShareUrl(gif.provider, {url: gif.url, slug: gif.slug});
			if (autoSend) {
				sendOptimisticMessage({content: gifUrl}, {hasAttachments: false});
			} else {
				appendText(gifUrl);
			}
		};
		return ComponentBus.subscribe('GIF_SELECT', handleGifSelect);
	}, [appendText, sendOptimisticMessage, enabled]);
	useEffect(() => {
		const handleStickerSelect = (payload?: unknown) => {
			if (!enabled) return;
			const {sticker} = (payload ?? {}) as {
				sticker?: GuildSticker;
			};
			if (!sticker) return;
			sendOptimisticMessage({content: '', stickers: [sticker.toJSON()]}, {hasAttachments: false});
		};
		return ComponentBus.subscribe('STICKER_SELECT', handleStickerSelect);
	}, [sendOptimisticMessage, enabled]);
	useEffect(() => {
		const handleFavoriteMemeSelect = (payload?: unknown) => {
			if (!enabled) return;
			const {meme, autoSend} = (payload ?? {}) as {
				meme?: FavoriteMeme;
				autoSend?: boolean;
			};
			if (!meme) return;
			const insertMemeUrl = () => {
				appendText(meme.url);
			};
			const providerShareUrl =
				meme.gifProvider && meme.gifSlug ? GifSlugUtils.buildShareUrl(meme.gifProvider, meme.gifSlug) : null;
			if (autoSend) {
				if (providerShareUrl) {
					sendOptimisticMessage({content: providerShareUrl}, {hasAttachments: false});
				} else if (canSendFavoriteMemeId) {
					const uploadingAttachment = UploadingAttachment.fromDescriptor({
						filename: meme.filename,
						title: meme.name,
						size: meme.size,
						contentType: meme.contentType,
					}).toJSON();
					sendOptimisticMessage(
						{content: '', attachments: [uploadingAttachment]},
						{hasAttachments: false, favoriteMemeId: meme.id},
					);
				} else {
					insertMemeUrl();
				}
			} else {
				if (providerShareUrl) {
					appendText(providerShareUrl);
				} else {
					insertMemeUrl();
				}
			}
		};
		return ComponentBus.subscribe('FAVORITE_MEME_SELECT', handleFavoriteMemeSelect);
	}, [appendText, canSendFavoriteMemeId, sendOptimisticMessage, enabled]);
	useEffect(() => {
		const handleInsertMention = (payload?: unknown) => {
			if (!enabled) return;
			const {userId} = (payload ?? {}) as {
				userId?: string;
			};
			if (!userId) return;
			const user = Users.getUser(userId);
			if (!user) {
				return;
			}
			const prevValue = textareaRef.current?.value ?? previousValueRef.current;
			const actualText = `<@${userId}>`;
			const displayText = `@${NicknameUtils.formatUserTagForStreamerMode(user)}`;
			const needsSpace = prevValue.length > 0 && !prevValue.endsWith(' ');
			const prefix = prevValue.length === 0 ? '' : needsSpace ? ' ' : '';
			const insertPosition = prevValue.length + prefix.length;
			const segmentManager = new TextareaSegmentManager();
			segmentManager.setSegments(segmentManagerRef.current.getSegmentsCopy());
			const {newText} = segmentManager.insertSegment(
				prevValue + prefix,
				insertPosition,
				displayText,
				actualText,
				'user',
				userId,
			);
			applyTextareaTextChange({
				textareaRef,
				setValue,
				segmentManagerRef,
				previousValueRef,
				prepareTextChange,
				nextValue: newText,
				nextSegments: segmentManager.getSegmentsCopy(),
				selectionStart: newText.length,
			});
		};
		return ComponentBus.subscribe('INSERT_MENTION', handleInsertMention);
	}, [insertSegment, previousValueRef, setValue, textareaRef, segmentManagerRef, prepareTextChange, enabled]);
};
