// SPDX-License-Identifier: AGPL-3.0-or-later

import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import {requestDeleteMessage} from '@app/features/channel/components/MessageActionUtils';
import {useMaybeMessageViewContext} from '@app/features/channel/components/MessageViewContext';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {ExpressionInfoBottomSheet} from '@app/features/expressions/components/bottomsheets/ExpressionInfoBottomSheet';
import {ExpressionHoverTooltipContent} from '@app/features/expressions/components/ExpressionHoverTooltipContent';
import {ExpressionInfoCard} from '@app/features/expressions/components/ExpressionInfoCard';
import {ExpressionInfoPopout} from '@app/features/expressions/components/ExpressionInfoPopout';
import ExpressionInfoCardRollout from '@app/features/expressions/state/ExpressionInfoCardRollout';
import {EXPRESSION_TOOLTIP_DELAY_MS} from '@app/features/expressions/utils/ExpressionPreviewConstants';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {EmojiRendererControl} from '@app/features/messaging/components/markdown/renderers/EmojiRendererControl';
import type {RendererProps} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import Messages from '@app/features/messaging/state/MessagingMessages';
import {getEmojiRenderData, getEmojiRenderUrl} from '@app/features/messaging/utils/markdown/EmojiDetector';
import {EmojiKind} from '@app/features/messaging/utils/markdown/parser/Enums';
import type {EmojiNode} from '@app/features/messaging/utils/markdown/parser/Nodes';
import {EmojiContextMenuItems, EmojiInlineMenuItems} from '@app/features/ui/action_menu/items/EmojiContextMenuItems';
import {MessageContextMenu} from '@app/features/ui/action_menu/MessageContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
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

const EmojiRendererTreatment = observer(function EmojiRendererTreatment({
	node,
	id,
	options,
}: RendererProps<EmojiNode>): React.ReactElement {
	const {shouldJumboEmojis, messageId, channelId, disableAnimatedEmoji} = options;
	const isPlainEmoji = options.disableInteractions === true || options.disableEmojiInteractions === true;
	const shouldDisableInfoCard = isPlainEmoji || options.disableEmojiInfoCard === true;
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
	const previewUrl = useMemo(
		() =>
			getEmojiRenderUrl({
				id: emojiData.id,
				surrogateUrl: emojiData.surrogateUrl,
				isAnimatable: emojiData.isAnimatable,
				animated,
				jumbo: false,
			}),
		[emojiData.id, emojiData.surrogateUrl, emojiData.isAnimatable, animated],
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
	const buildEmojiForMenu = useCallback((): FlatEmoji => {
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
			const emojiForMenu = emojiRecord ?? buildEmojiForMenu();
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
									data-flx="messaging.markdown.renderers.emoji-renderer.handle-context-menu.emoji-inline-menu-items"
								/>
							}
							data-flx="messaging.markdown.renderers.emoji-renderer.handle-context-menu.message-context-menu"
						/>
					));
					return;
				}
			}
			ContextMenuCommands.openFromEvent(e, ({onClose}) => (
				<EmojiContextMenuItems
					emoji={emojiForMenu}
					onClose={onClose}
					data-flx="messaging.markdown.renderers.emoji-renderer.handle-context-menu.emoji-context-menu-items"
				/>
			));
		},
		[buildEmojiForMenu, emojiData.id, emojiRecord, isCustomEmoji, channelId, messageId, i18n],
	);
	const hasFailed = emojiUrl != null && failedUrl === emojiUrl;
	const accessibleName = hasFailed
		? i18n._(EMOJI_FAILED_TO_LOAD_DESCRIPTOR, {emojiName: emojiData.name})
		: emojiData.name;
	const emojiIdentityAttributes = {
		'data-message-id': messageId,
		'data-message-emoji': 'true',
		'data-emoji-id': emojiData.id,
		'data-animated': emojiData.isAnimated,
	};
	const renderEmojiElement = (contextMenu: boolean, dataFlx: string) =>
		emojiUrl ? (
			<img
				draggable={false}
				className={className}
				alt={accessibleName}
				aria-label={accessibleName}
				src={emojiUrl}
				style={hasFailed ? FAILED_EMOJI_STYLE : undefined}
				{...emojiIdentityAttributes}
				onError={handleImageError}
				onContextMenu={contextMenu ? handleContextMenu : undefined}
				loading="eager"
				data-flx={dataFlx}
			/>
		) : (
			<span
				className={fallbackEmojiClassName}
				role="img"
				aria-label={accessibleName}
				{...emojiIdentityAttributes}
				onContextMenu={contextMenu ? handleContextMenu : undefined}
				data-flx={dataFlx}
			>
				{fallbackEmojiText}
			</span>
		);
	const renderHoverTooltip = () => (
		<ExpressionHoverTooltipContent
			displayName={emojiData.name}
			previewUrl={previewUrl}
			data-flx="messaging.markdown.renderers.emoji-renderer.emoji-renderer-treatment.expression-hover-tooltip-content"
		/>
	);
	const renderInfoCard = ({onClose}: {onClose: () => void}) =>
		emojiData.id != null ? (
			<ExpressionInfoCard
				kind="emoji"
				expressionId={emojiData.id}
				guildId={fallbackGuildId ?? null}
				displayName={emojiData.name}
				previewUrl={previewUrl}
				onClose={onClose}
				data-flx="messaging.markdown.renderers.emoji-renderer.emoji-renderer-treatment.expression-info-card.custom"
			/>
		) : (
			<ExpressionInfoCard
				kind="default_emoji"
				displayName={emojiData.name}
				previewUrl={previewUrl}
				onClose={onClose}
				data-flx="messaging.markdown.renderers.emoji-renderer.emoji-renderer-treatment.expression-info-card.default"
			/>
		);
	if (isPlainEmoji) {
		return renderEmojiElement(true, 'messaging.markdown.renderers.emoji-renderer.emoji-renderer-treatment.emoji.plain');
	}
	if (isMobile) {
		return (
			<>
				<span
					onClick={handleOpenBottomSheet}
					onContextMenu={handleContextMenu}
					onKeyDown={handleKeyDown}
					role="button"
					tabIndex={0}
					data-flx="messaging.markdown.renderers.emoji-renderer.emoji-renderer-treatment.button.open-bottom-sheet--2"
				>
					{renderEmojiElement(false, 'messaging.markdown.renderers.emoji-renderer.emoji-renderer-treatment.emoji')}
				</span>
				<ExpressionInfoBottomSheet
					kind="emoji"
					isOpen={bottomSheetState.isOpen}
					onClose={handleCloseBottomSheet}
					emoji={bottomSheetState.emoji}
					data-flx="messaging.markdown.renderers.emoji-renderer.emoji-renderer-treatment.expression-info-bottom-sheet--2"
				/>
			</>
		);
	}
	if (shouldDisableInfoCard) {
		return (
			<Tooltip
				key={id}
				text={emojiData.name}
				delay={EXPRESSION_TOOLTIP_DELAY_MS}
				data-flx="messaging.markdown.renderers.emoji-renderer.emoji-renderer-treatment.tooltip.plain"
			>
				{renderEmojiElement(true, 'messaging.markdown.renderers.emoji-renderer.emoji-renderer-treatment.emoji.plain')}
			</Tooltip>
		);
	}
	return (
		<ExpressionInfoPopout
			key={id}
			renderTooltip={renderHoverTooltip}
			renderCard={renderInfoCard}
			data-flx="messaging.markdown.renderers.emoji-renderer.emoji-renderer-treatment.expression-info-popout"
		>
			<span
				role="button"
				tabIndex={0}
				aria-label={accessibleName}
				data-emoji-interactive="true"
				data-flx="messaging.markdown.renderers.emoji-renderer.emoji-renderer-treatment.button.open-info-card"
			>
				{renderEmojiElement(
					true,
					'messaging.markdown.renderers.emoji-renderer.emoji-renderer-treatment.emoji.context-menu',
				)}
			</span>
		</ExpressionInfoPopout>
	);
});

export const EmojiRenderer = observer(function EmojiRenderer(props: RendererProps<EmojiNode>): React.ReactElement {
	if (!ExpressionInfoCardRollout.enabled) {
		return (
			<EmojiRendererControl {...props} data-flx="messaging.markdown.renderers.emoji-renderer.emoji-renderer-control" />
		);
	}
	return (
		<EmojiRendererTreatment
			{...props}
			data-flx="messaging.markdown.renderers.emoji-renderer.emoji-renderer-treatment"
		/>
	);
});
