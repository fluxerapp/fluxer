// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ReactionCommands from '@app/features/messaging/commands/ReactionCommands';
import {MessageReactionsModal} from '@app/features/messaging/components/modals/MessageReactionsModal';
import {useReactionUsers} from '@app/features/messaging/hooks/useMessageReactionStore';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {getReactionTooltip, useEmojiURL} from '@app/features/messaging/utils/ReactionUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {EmojiTooltipContent} from '@app/features/ui/emoji_tooltip_content/EmojiTooltipContent';
import {HoverFloatingTooltipSurface} from '@app/features/ui/tooltip/HoverFloatingTooltipSurface';
import {HoverFloatingTooltipTrigger} from '@app/features/ui/tooltip/HoverFloatingTooltipTrigger';
import {useHoverFloatingTooltip} from '@app/features/ui/tooltip/useHoverFloatingTooltip';
import type {MessageReaction} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useRef} from 'react';

const CLICK_TO_VIEW_ALL_REACTIONS_DESCRIPTOR = msg({
	message: 'Click to view all reactions',
	comment: 'Label in the reaction tooltip popout.',
});
export const ReactionTooltip = observer(
	({
		message,
		reaction,
		children,
		hoveredEmojiUrl,
		onTooltipHoverChange,
	}: {
		message: Message;
		reaction: MessageReaction;
		children: React.ReactElement<Record<string, unknown> & {ref?: React.Ref<HTMLElement>}>;
		hoveredEmojiUrl?: string | null;
		onTooltipHoverChange?: (hovering: boolean) => void;
	}) => {
		const {i18n} = useLingui();
		const tooltip = useHoverFloatingTooltip(500);
		useEffect(() => {
			onTooltipHoverChange?.(tooltip.state.isOpen);
		}, [tooltip.state.isOpen, onTooltipHoverChange]);
		const {fetchStatus} = useReactionUsers(message.id, reaction.emoji);
		const isLoading = fetchStatus === 'pending';
		const tooltipText = getReactionTooltip(message, reaction.emoji);
		const tooltipEmojiKey = reaction.emoji.id ?? reaction.emoji.name;
		const fallbackEmojiUrl = useEmojiURL({emoji: reaction.emoji});
		const emojiUrl = hoveredEmojiUrl ?? fallbackEmojiUrl;
		const errorRetryRef = useRef(false);
		useEffect(() => {
			if (!tooltip.state.isOpen) {
				errorRetryRef.current = false;
				return;
			}
			if (fetchStatus === 'pending') {
				return;
			}
			if (fetchStatus === 'success') {
				errorRetryRef.current = false;
				return;
			}
			if (fetchStatus === 'error' && errorRetryRef.current) {
				return;
			}
			if (fetchStatus === 'error') {
				errorRetryRef.current = true;
			}
			ReactionCommands.getReactions(message.channelId, message.id, reaction.emoji, {
				limit: 3,
				totalCount: reaction.count,
			}).catch((_error) => {});
		}, [tooltip.state.isOpen, message.channelId, message.id, reaction.emoji, reaction.count, fetchStatus]);
		const handleClick = () => {
			tooltip.hide();
			ModalCommands.push(
				modal(() => (
					<MessageReactionsModal
						channelId={message.channelId}
						messageId={message.id}
						openToReaction={reaction}
						data-flx="messaging.reaction-tooltip.handle-click.message-reactions-modal"
					/>
				)),
			);
		};
		return (
			<>
				<HoverFloatingTooltipTrigger
					tooltip={tooltip}
					data-flx="messaging.reaction-tooltip.hover-floating-tooltip-trigger"
				>
					{children}
				</HoverFloatingTooltipTrigger>
				<HoverFloatingTooltipSurface
					tooltip={tooltip}
					portalDataFlx="messaging.reaction-tooltip.floating-portal"
					presenceDataFlx="messaging.reaction-tooltip.animate-presence"
					data-flx="messaging.reaction-tooltip.div"
				>
					<EmojiTooltipContent
						emojiUrl={emojiUrl}
						emojiAlt={reaction.emoji.name}
						emojiKey={tooltipEmojiKey}
						primaryContent={tooltipText}
						subtext={i18n._(CLICK_TO_VIEW_ALL_REACTIONS_DESCRIPTOR)}
						isLoading={isLoading}
						interactive
						onClick={handleClick}
						data-flx="messaging.reaction-tooltip.emoji-tooltip-content.click"
					/>
				</HoverFloatingTooltipSurface>
			</>
		);
	},
);
