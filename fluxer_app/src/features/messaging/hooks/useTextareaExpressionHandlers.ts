// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Gif} from '@app/features/expressions/commands/GifCommands';
import type {FavoriteMeme} from '@app/features/expressions/models/FavoriteMeme';
import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import * as GifSlugUtils from '@app/features/expressions/utils/GifSlugUtils';
import {UploadingAttachment} from '@app/features/messaging/models/UploadingAttachment';
import {TextareaSegmentManager} from '@app/features/messaging/utils/TextareaSegmentManager';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import ActiveComposer from '@app/features/threads/state/ActiveComposer';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import type {MessageAttachment, MessageStickerItem} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {useCallback, useEffect} from 'react';

interface UseTextareaExpressionHandlersOptions {
	setValue: React.Dispatch<React.SetStateAction<string>>;
	previousValueRef: React.RefObject<string>;
	segmentManagerRef: React.RefObject<TextareaSegmentManager>;
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
	channelId?: string;
}

export const useTextareaExpressionHandlers = ({
	setValue,
	previousValueRef,
	segmentManagerRef,
	sendOptimisticMessage,
	enabled = true,
	channelId,
}: UseTextareaExpressionHandlersOptions) => {
	const appendText = useCallback(
		(text: string) => {
			if (!enabled || (channelId != null && !ActiveComposer.accepts(channelId))) return;
			const prevValue = previousValueRef.current;
			const prefix = prevValue.length === 0 ? '' : ' ';
			const nextValue = `${prevValue}${prefix}${text} `;
			segmentManagerRef.current.setSegments(segmentManagerRef.current.getSegmentsCopy());
			previousValueRef.current = nextValue;
			setValue(nextValue);
		},
		[enabled, channelId, previousValueRef, segmentManagerRef, setValue],
	);
	useEffect(() => {
		const handleGifSelect = (payload?: unknown) => {
			if (!enabled || (channelId != null && !ActiveComposer.accepts(channelId))) return;
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
	}, [appendText, sendOptimisticMessage, enabled, channelId]);
	useEffect(() => {
		const handleStickerSelect = (payload?: unknown) => {
			if (!enabled || (channelId != null && !ActiveComposer.accepts(channelId))) return;
			const {sticker} = (payload ?? {}) as {
				sticker?: GuildSticker;
			};
			if (!sticker) return;
			sendOptimisticMessage({content: '', stickers: [sticker.toJSON()]}, {hasAttachments: false});
		};
		return ComponentBus.subscribe('STICKER_SELECT', handleStickerSelect);
	}, [sendOptimisticMessage, enabled, channelId]);
	useEffect(() => {
		const handleFavoriteMemeSelect = (payload?: unknown) => {
			if (!enabled || (channelId != null && !ActiveComposer.accepts(channelId))) return;
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
				} else {
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
	}, [appendText, sendOptimisticMessage, enabled, channelId]);
	useEffect(() => {
		const handleInsertMention = (payload?: unknown) => {
			if (!enabled || (channelId != null && !ActiveComposer.accepts(channelId))) return;
			const {userId} = (payload ?? {}) as {
				userId?: string;
			};
			if (!userId) return;
			const user = Users.getUser(userId);
			if (!user) {
				return;
			}
			const prevValue = previousValueRef.current;
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
			segmentManagerRef.current.setSegments(segmentManager.getSegmentsCopy());
			previousValueRef.current = newText;
			setValue(newText);
		};
		return ComponentBus.subscribe('INSERT_MENTION', handleInsertMention);
	}, [previousValueRef, setValue, segmentManagerRef, enabled, channelId]);
};
