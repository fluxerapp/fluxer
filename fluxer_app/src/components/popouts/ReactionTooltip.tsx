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

import {FloatingPortal} from '@floating-ui/react';
import {useLingui} from '@lingui/react/macro';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ReactionActionCreators from '~/actions/ReactionActionCreators';
import {MessageReactionsModal} from '~/components/modals/MessageReactionsModal';
import {EmojiTooltipContent} from '~/components/uikit/EmojiTooltipContent/EmojiTooltipContent';
import {useTooltipPortalRoot} from '~/components/uikit/Tooltip';
import {useMergeRefs} from '~/hooks/useMergeRefs';
import {useReactionTooltip} from '~/hooks/useReactionTooltip';
import type {MessageReaction, MessageRecord} from '~/records/MessageRecord';
import MessageReactionsStore from '~/stores/MessageReactionsStore';
import {getReactionTooltip, useEmojiURL} from '~/utils/ReactionUtils';

export const ReactionTooltip = observer(
	({
		message,
		reaction,
		children,
		hoveredEmojiUrl,
		animationSyncKey,
		onRequestAnimationSync,
	}: {
		message: MessageRecord;
		reaction: MessageReaction;
		children: React.ReactElement<Record<string, unknown> & {ref?: React.Ref<HTMLElement>}>;
		hoveredEmojiUrl?: string | null;
		animationSyncKey?: number;
		onRequestAnimationSync?: () => void;
	}) => {
		const {t} = useLingui();
		const tooltipPortalRoot = useTooltipPortalRoot();
		const {targetRef, tooltipRef, state, updatePosition, handlers, tooltipHandlers, hide} = useReactionTooltip(500);

		const prevIsOpenRef = React.useRef(false);
		React.useEffect(() => {
			if (state.isOpen && !prevIsOpenRef.current) {
				onRequestAnimationSync?.();
			}
			prevIsOpenRef.current = state.isOpen;
		}, [state.isOpen, onRequestAnimationSync]);

		const fetchStatus = MessageReactionsStore.getFetchStatus(message.id, reaction.emoji);
		const isLoading = fetchStatus === 'pending';
		const tooltipText = getReactionTooltip(message, reaction.emoji);
		const emojiIdentifier = reaction.emoji.id ?? reaction.emoji.name;
		const tooltipEmojiKey = `${emojiIdentifier}-${animationSyncKey ?? 0}`;
		const fallbackEmojiUrl = useEmojiURL({
			emoji: reaction.emoji,
			isHovering: state.isOpen,
			forceAnimate: state.isOpen,
		});
		const emojiUrl = hoveredEmojiUrl ?? fallbackEmojiUrl;

		React.useEffect(() => {
			if (!state.isOpen) return;
			if (fetchStatus === 'idle') {
				ReactionActionCreators.getReactions(message.channelId, message.id, reaction.emoji, 3);
			}
		}, [state.isOpen, message.channelId, message.id, reaction.emoji, fetchStatus]);

		const handleClick = () => {
			hide();
			ModalActionCreators.push(
				modal(() => (
					<MessageReactionsModal channelId={message.channelId} messageId={message.id} openToReaction={reaction} />
				)),
			);
		};

		const child = React.Children.only(children);
		const childRef = child.props.ref ?? null;
		const mergedRef = useMergeRefs([targetRef, childRef]);

		return (
			<>
				{React.cloneElement(child, {
					ref: mergedRef,
					...handlers,
				})}
				{state.isOpen && (
					<FloatingPortal root={tooltipPortalRoot}>
						<AnimatePresence>
							<motion.div
								ref={(node) => {
									(tooltipRef as React.MutableRefObject<HTMLDivElement | null>).current = node;
									if (node && targetRef.current) {
										updatePosition();
									}
								}}
								style={{
									position: 'fixed',
									left: state.x,
									top: state.y,
									zIndex: 'var(--z-index-tooltip)',
									visibility: state.isReady ? 'visible' : 'hidden',
								}}
								initial={{opacity: 0, scale: 0.98}}
								animate={{opacity: 1, scale: 1}}
								exit={{opacity: 0, scale: 0.98}}
								transition={{
									opacity: {duration: 0.1},
									scale: {type: 'spring', damping: 25, stiffness: 500},
								}}
								{...tooltipHandlers}
							>
								<EmojiTooltipContent
									emojiUrl={emojiUrl}
									emojiAlt={reaction.emoji.name}
									emojiKey={tooltipEmojiKey}
									primaryContent={tooltipText}
									subtext={t`Click to view all reactions`}
									isLoading={isLoading}
									interactive
									onClick={handleClick}
								/>
							</motion.div>
						</AnimatePresence>
					</FloatingPortal>
				)}
			</>
		);
	},
);
