// SPDX-License-Identifier: AGPL-3.0-or-later

import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import {requestDeleteMessage} from '@app/features/channel/components/MessageActionUtils';
import {useMaybeMessageViewContext} from '@app/features/channel/components/MessageViewContext';
import {EmojiInfoBottomSheet} from '@app/features/emoji/components/bottomsheets/EmojiInfoBottomSheet';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import type {RendererProps} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import Messages from '@app/features/messaging/state/MessagingMessages';
import {getEmojiRenderData, getEmojiRenderUrl} from '@app/features/messaging/utils/markdown/EmojiDetector';
import {EmojiKind} from '@app/features/messaging/utils/markdown/parser/Enums';
import type {EmojiNode} from '@app/features/messaging/utils/markdown/parser/Nodes';
import {EmojiContextMenuItems, EmojiInlineMenuItems} from '@app/features/ui/action_menu/items/EmojiContextMenuItems';
import {MessageContextMenu} from '@app/features/ui/action_menu/MessageContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {EmojiWithTooltip} from '@app/features/ui/emoji_tooltip_content/EmojiWithTooltip';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {msg} from '@lingui/core/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useState} from 'react';

const EMOJI_FAILED_TO_LOAD_DESCRIPTOR = msg({
	message: '{emojiName} (failed to load)',
	comment: 'Error message in the messaging emoji renderer.',
});

const FAILED_EMOJI_STYLE: React.CSSProperties = {opacity: 0.5};

interface EmojiBottomSheetState {
	isOpen: boolean;
	emoji: {id?: string; name: string; animated?: boolean} | null;
}

export const EmojiRendererControl = observer(function EmojiRendererControl({
	node,
	id,
	options,
}: RendererProps<EmojiNode>): React.ReactElement {
	const {shouldJumboEmojis, messageId, channelId, disableAnimatedEmoji} = options;
	const i18n = options.i18n!;
	const emojiData = getEmojiRenderData(node, disableAnimatedEmoji);
	const isMobile = MobileLayout.enabled;
	const [bottomSheetState, setBottomSheetState] = useState<EmojiBottomSheetState>({
		isOpen: false,
		emoji: null,
	});
	const [failedUrl, setFailedUrl] = useState<string | null>(null);
	const messageView = useMaybeMessageViewContext();
	const isAnimatable = emojiData.isAnimatable;
	const shouldAnimate = useShouldAnimate({
		kind: 'emoji',
		isHovering: isAnimatable && messageView?.isHovering === true,
	});
	const animated = isAnimatable && shouldAnimate;
	const className = clsx('emoji', shouldJumboEmojis && 'jumboable');
	const emojiUrl = useMemo(
		() =>
			getEmojiRenderUrl({
				id: emojiData.id,
				surrogateUrl: emojiData.surrogateUrl,
				isAnimatable: emojiData.isAnimatable,
				animated,
				jumbo: shouldJumboEmojis,
			}),
		[emojiData.id, emojiData.surrogateUrl, emojiData.isAnimatable, animated, shouldJumboEmojis],
	);
	const isCustomEmoji = node.kind.kind === EmojiKind.Custom;
	const standardEmojiSurrogate = node.kind.kind === EmojiKind.Standard ? node.kind.raw : undefined;
	const emojiRecord: FlatEmoji | null = isCustomEmoji ? (emojiData.emoji ?? null) : null;
	const fallbackEmojiText = standardEmojiSurrogate ?? emojiData.name;
	const fallbackEmojiClassName = standardEmojiSurrogate ? className : undefined;
	const fallbackGuildId = emojiRecord?.guildId;
	const fallbackAnimated = emojiRecord?.animated ?? emojiData.isAnimated;
	const handleOpenBottomSheet = useCallback(() => {
		if (!isMobile) return;
		const emojiInfo =
			node.kind.kind === EmojiKind.Custom
				? {
						name: node.kind.name,
						id: node.kind.id,
						animated: node.kind.animated,
					}
				: {
						name: node.kind.raw,
						animated: false,
					};
		setBottomSheetState({isOpen: true, emoji: emojiInfo});
	}, [isMobile, node.kind]);
	const handleCloseBottomSheet = useCallback(() => {
		setBottomSheetState({isOpen: false, emoji: null});
	}, []);
	const handleKeyDown = useCallback(
		(e: React.KeyboardEvent) => {
			if (!isKeyboardActivationKey(e.key)) return;
			e.preventDefault();
			handleOpenBottomSheet();
		},
		[handleOpenBottomSheet],
	);
	const buildEmojiForSubtext = useCallback((): FlatEmoji => {
		if (emojiRecord) {
			return emojiRecord;
		}
		return {
			id: emojiData.id,
			guildId: fallbackGuildId,
			animated: fallbackAnimated,
			name: node.kind.name,
			allNamesString: `:${node.kind.name}:`,
			uniqueName: node.kind.name,
			surrogates: standardEmojiSurrogate,
			url: standardEmojiSurrogate ? (emojiData.surrogateUrl ?? undefined) : undefined,
		};
	}, [
		emojiData.id,
		emojiData.surrogateUrl,
		emojiRecord,
		fallbackAnimated,
		fallbackGuildId,
		node.kind.name,
		standardEmojiSurrogate,
	]);
	const handleImageError = useCallback((e: React.SyntheticEvent<HTMLImageElement>) => {
		setFailedUrl((e.target as HTMLImageElement).getAttribute('src'));
	}, []);
	const handleContextMenu = useCallback(
		(e: React.MouseEvent) => {
			if (!isCustomEmoji || !emojiData.id) return;
			e.preventDefault();
			e.stopPropagation();
			const emojiForMenu = emojiRecord ?? buildEmojiForSubtext();
			if (messageId && channelId) {
				const messageRecord = Messages.getMessage(channelId, messageId);
				if (messageRecord) {
					ContextMenuCommands.openFromEvent(e, ({onClose}) => (
						<MessageContextMenu
							message={messageRecord}
							onClose={onClose}
							onDelete={(bypassConfirm) => requestDeleteMessage(messageRecord, i18n, bypassConfirm)}
							inlineStickerOrEmojiItems={
								<EmojiInlineMenuItems
									emoji={emojiForMenu}
									onClose={onClose}
									data-flx="messaging.markdown.renderers.emoji-renderer-control.handle-context-menu.emoji-inline-menu-items"
								/>
							}
							data-flx="messaging.markdown.renderers.emoji-renderer-control.handle-context-menu.message-context-menu"
						/>
					));
					return;
				}
			}
			ContextMenuCommands.openFromEvent(e, ({onClose}) => (
				<EmojiContextMenuItems
					emoji={emojiForMenu}
					onClose={onClose}
					data-flx="messaging.markdown.renderers.emoji-renderer-control.handle-context-menu.emoji-context-menu-items"
				/>
			));
		},
		[buildEmojiForSubtext, emojiData.id, emojiRecord, isCustomEmoji, channelId, messageId, i18n],
	);
	const hasFailed = emojiUrl != null && failedUrl === emojiUrl;
	const renderEmojiElement = (contextMenu: boolean, dataFlx: string) =>
		emojiUrl ? (
			<img
				draggable={false}
				className={className}
				alt={hasFailed ? i18n._(EMOJI_FAILED_TO_LOAD_DESCRIPTOR, {emojiName: emojiData.name}) : emojiData.name}
				src={emojiUrl}
				style={hasFailed ? FAILED_EMOJI_STYLE : undefined}
				data-message-id={messageId}
				data-message-emoji="true"
				data-emoji-id={emojiData.id}
				data-animated={emojiData.isAnimated}
				onError={handleImageError}
				onContextMenu={contextMenu ? handleContextMenu : undefined}
				loading="eager"
				data-flx={dataFlx}
			/>
		) : (
			<span
				className={fallbackEmojiClassName}
				role="img"
				aria-label={emojiData.name}
				data-message-id={messageId}
				data-message-emoji="true"
				data-emoji-id={emojiData.id}
				data-animated={emojiData.isAnimated}
				onContextMenu={contextMenu ? handleContextMenu : undefined}
				data-flx={dataFlx}
			>
				{fallbackEmojiText}
			</span>
		);
	if (isMobile) {
		return (
			<>
				<span
					onClick={handleOpenBottomSheet}
					onContextMenu={handleContextMenu}
					onKeyDown={handleKeyDown}
					role="button"
					tabIndex={0}
					data-flx="messaging.markdown.renderers.emoji-renderer-control.button.open-bottom-sheet"
				>
					{renderEmojiElement(false, 'messaging.markdown.renderers.emoji-renderer-control.emoji')}
				</span>
				<EmojiInfoBottomSheet
					isOpen={bottomSheetState.isOpen}
					onClose={handleCloseBottomSheet}
					emoji={bottomSheetState.emoji}
					data-flx="messaging.markdown.renderers.emoji-renderer-control.emoji-info-bottom-sheet"
				/>
			</>
		);
	}
	return (
		<EmojiWithTooltip
			key={id}
			emojiUrl={emojiUrl}
			emojiName={emojiData.name}
			emojiForSubtext={buildEmojiForSubtext()}
			data-flx="messaging.markdown.renderers.emoji-renderer-control.emoji-with-tooltip"
		>
			{renderEmojiElement(true, 'messaging.markdown.renderers.emoji-renderer-control.emoji.context-menu')}
		</EmojiWithTooltip>
	);
});
