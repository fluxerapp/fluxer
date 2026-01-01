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

import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {autorun} from 'mobx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as MessageActionCreators from '~/actions/MessageActionCreators';
import * as ReadStateActionCreators from '~/actions/ReadStateActionCreators';
import {FLUXERBOT_ID, MessageEmbedTypes, MessagePreviewContext, MessageStates, MessageTypes} from '~/Constants';
import {MessageActionBar, MessageActionBarCore} from '~/components/channel/MessageActionBar';
import {MessageActionBottomSheet} from '~/components/channel/MessageActionBottomSheet';
import {MessageContextMenu} from '~/components/uikit/ContextMenu/MessageContextMenu';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {NodeType} from '~/lib/markdown/parser/types/enums';
import {MarkdownContext, parse} from '~/lib/markdown/renderers';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {MessageRecord} from '~/records/MessageRecord';
import AccessibilityStore from '~/stores/AccessibilityStore';
import ContextMenuStore, {isContextMenuNodeTarget} from '~/stores/ContextMenuStore';
import DeveloperOptionsStore from '~/stores/DeveloperOptionsStore';
import KeyboardModeStore from '~/stores/KeyboardModeStore';
import MessageEditStore from '~/stores/MessageEditStore';
import MessageReplyStore from '~/stores/MessageReplyStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import styles from '~/styles/Message.module.css';
import {getMessageComponent} from '~/utils/MessageComponentUtils';
import {MessageViewContextProvider} from './MessageViewContext';

const shouldApplyGroupedLayout = (message: MessageRecord, _prevMessage?: MessageRecord) => {
	if (message.type !== MessageTypes.DEFAULT && message.type !== MessageTypes.REPLY) {
		return false;
	}
	return true;
};

const isActivationKey = (key: string) => key === 'Enter' || key === ' ' || key === 'Spacebar' || key === 'Space';

const handleAltClickEvent = (event: React.MouseEvent, message: MessageRecord) => {
	if (!event.altKey) return;
	ReadStateActionCreators.markAsUnread(message.channelId, message.id);
};

const handleAltKeyboardEvent = (event: React.KeyboardEvent, message: MessageRecord) => {
	if (!event.altKey || !isActivationKey(event.key)) {
		return;
	}
	event.preventDefault();
	ReadStateActionCreators.markAsUnread(message.channelId, message.id);
};

const handleDeleteMessage = (i18n: any, bypassConfirm: boolean, message: MessageRecord) => {
	if (bypassConfirm) {
		MessageActionCreators.remove(message.channelId, message.id);
		return;
	}
	MessageActionCreators.showDeleteConfirmation(i18n, {message});
};

export type MessageBehaviorOverrides = Partial<{
	mobileLayoutEnabled: boolean;
	messageGroupSpacing: number;
	messageDisplayCompact: boolean;
	prefersReducedMotion: boolean;
	isEditing: boolean;
	isReplying: boolean;
	isHighlight: boolean;
	forceUnknownMessageType: boolean;
	contextMenuOpen: boolean;
	disableContextMenu: boolean;
	disableContextMenuTracking: boolean;
}>;

interface MessageProps {
	channel: ChannelRecord;
	message: MessageRecord;
	prevMessage?: MessageRecord;
	onEdit?: (targetNode: HTMLElement) => void;
	previewContext?: keyof typeof MessagePreviewContext;
	shouldGroup?: boolean;
	previewOverrides?: {
		usernameColor?: string;
		displayName?: string;
	};
	removeTopSpacing?: boolean;
	isJumpTarget?: boolean;
	previewMode?: boolean;
	behaviorOverrides?: MessageBehaviorOverrides;
	compact?: boolean;
	idPrefix?: string;
}

export const Message: React.FC<MessageProps> = observer((props) => {
	const {
		channel,
		message,
		prevMessage,
		onEdit,
		previewContext,
		shouldGroup = false,
		previewOverrides,
		removeTopSpacing = false,
		isJumpTarget = false,
		previewMode,
		behaviorOverrides,
		compact,
		idPrefix = 'message',
	} = props;

	const {i18n} = useLingui();

	const [showActionBar, setShowActionBar] = useState(false);
	const [isLongPressing, setIsLongPressing] = useState(false);
	const [contextMenuOpen, setContextMenuOpen] = useState(behaviorOverrides?.contextMenuOpen ?? false);
	const [isHoveringDesktop, setIsHoveringDesktop] = useState(false);
	const [isFocusedWithin, setIsFocusedWithin] = useState(false);
	const [isPopoutOpen, setIsPopoutOpen] = useState(false);

	const messageRef = useRef<HTMLDivElement | null>(null);
	const longPressTimerRef = useRef<NodeJS.Timeout | null>(null);
	const wasEditingInPreviousUpdateRef = useRef(false);

	const mobileLayoutEnabled = behaviorOverrides?.mobileLayoutEnabled ?? MobileLayoutStore.isEnabled();
	const messageDisplayCompact =
		compact ?? behaviorOverrides?.messageDisplayCompact ?? UserSettingsStore.getMessageDisplayCompact();
	const prefersReducedMotion = behaviorOverrides?.prefersReducedMotion ?? AccessibilityStore.useReducedMotion;
	const isEditing = behaviorOverrides?.isEditing ?? MessageEditStore.isEditing(message.channelId, message.id);
	const isReplying = behaviorOverrides?.isReplying ?? MessageReplyStore.isReplying(message.channelId, message.id);
	const isHighlight = behaviorOverrides?.isHighlight ?? MessageReplyStore.isHighlight(message.id);
	const forceUnknownMessageType =
		behaviorOverrides?.forceUnknownMessageType ?? DeveloperOptionsStore.forceUnknownMessageType;
	const messageGroupSpacing = behaviorOverrides?.messageGroupSpacing ?? AccessibilityStore.messageGroupSpacingValue;

	const handleContextMenuUpdate = useCallback(() => {
		const contextMenu = ContextMenuStore.contextMenu;
		const contextMenuTarget = contextMenu?.target?.target ?? null;
		const messageElement = messageRef.current;
		const isOpen =
			Boolean(contextMenu) &&
			isContextMenuNodeTarget(contextMenuTarget) &&
			Boolean(messageElement?.contains(contextMenuTarget));
		setContextMenuOpen(!!isOpen);
	}, []);

	const handleAltClick = useCallback(
		(event: React.MouseEvent) => {
			handleAltClickEvent(event, message);
		},
		[message],
	);
	const handleAltKeyDown = useCallback(
		(event: React.KeyboardEvent<HTMLDivElement>) => {
			handleAltKeyboardEvent(event, message);
		},
		[message],
	);

	const handleDelete = useCallback(
		(bypassConfirm = false) => {
			handleDeleteMessage(i18n, bypassConfirm, message);
		},
		[i18n, message],
	);

	const handleContextMenu = useCallback(
		(event: React.MouseEvent) => {
			if (behaviorOverrides?.disableContextMenu) {
				event.preventDefault();
				return;
			}
			if (
				(previewContext && previewContext !== MessagePreviewContext.LIST_POPOUT) ||
				message.state === MessageStates.SENDING ||
				isEditing
			) {
				return;
			}
			event.preventDefault();
			if (mobileLayoutEnabled) {
				return;
			}

			let linkUrl: string | undefined;
			const target = event.target as HTMLElement;
			const anchor = target.closest('a');
			if (anchor?.href) {
				linkUrl = anchor.href;
			}

			ContextMenuActionCreators.openFromEvent(event, (props) => (
				<MessageContextMenu message={message} onClose={props.onClose} onDelete={handleDelete} linkUrl={linkUrl} />
			));
		},
		[previewContext, message, isEditing, mobileLayoutEnabled, handleDelete, behaviorOverrides?.disableContextMenu],
	);

	const LONG_PRESS_DELAY = 500;
	const MOVEMENT_THRESHOLD = 10;
	const SWIPE_VELOCITY_THRESHOLD = 0.4;
	const HIGHLIGHT_DELAY = 100;

	const touchStartPos = useRef<{x: number; y: number} | null>(null);
	const velocitySamples = useRef<Array<{x: number; y: number; timestamp: number}>>([]);
	const highlightTimerRef = useRef<NodeJS.Timeout | null>(null);

	const clearLongPressState = useCallback(() => {
		if (longPressTimerRef.current) {
			clearTimeout(longPressTimerRef.current);
			longPressTimerRef.current = null;
		}
		if (highlightTimerRef.current) {
			clearTimeout(highlightTimerRef.current);
			highlightTimerRef.current = null;
		}
		touchStartPos.current = null;
		velocitySamples.current = [];
		setIsLongPressing(false);
	}, []);

	const calculateVelocity = useCallback((): number => {
		const samples = velocitySamples.current;
		if (samples.length < 2) return 0;

		const now = performance.now();
		const recentSamples = samples.filter((s) => now - s.timestamp < 100);
		if (recentSamples.length < 2) return 0;

		const first = recentSamples[0];
		const last = recentSamples[recentSamples.length - 1];
		const dt = last.timestamp - first.timestamp;
		if (dt === 0) return 0;

		const dx = last.x - first.x;
		const dy = last.y - first.y;
		return Math.sqrt(dx * dx + dy * dy) / dt;
	}, []);

	const handleLongPressStart = useCallback(
		(event: React.TouchEvent) => {
			if (!mobileLayoutEnabled || previewContext) {
				return;
			}
			const touch = event.touches[0];
			if (!touch) return;

			touchStartPos.current = {x: touch.clientX, y: touch.clientY};
			velocitySamples.current = [{x: touch.clientX, y: touch.clientY, timestamp: performance.now()}];

			highlightTimerRef.current = setTimeout(() => {
				if (touchStartPos.current) {
					setIsLongPressing(true);
				}
				highlightTimerRef.current = null;
			}, HIGHLIGHT_DELAY);

			longPressTimerRef.current = setTimeout(() => {
				if (touchStartPos.current) {
					setShowActionBar(true);
					setIsLongPressing(false);
				}
				clearLongPressState();
			}, LONG_PRESS_DELAY);
		},
		[mobileLayoutEnabled, previewContext, clearLongPressState],
	);

	const handleLongPressEnd = useCallback(() => {
		clearLongPressState();
	}, [clearLongPressState]);

	const handleLongPressMove = useCallback(
		(event: React.TouchEvent) => {
			if (!touchStartPos.current) return;
			const touch = event.touches[0];
			if (!touch) return;

			velocitySamples.current.push({x: touch.clientX, y: touch.clientY, timestamp: performance.now()});
			if (velocitySamples.current.length > 10) {
				velocitySamples.current = velocitySamples.current.slice(-10);
			}

			const deltaX = Math.abs(touch.clientX - touchStartPos.current.x);
			const deltaY = Math.abs(touch.clientY - touchStartPos.current.y);

			if (deltaX > MOVEMENT_THRESHOLD || deltaY > MOVEMENT_THRESHOLD) {
				clearLongPressState();
				return;
			}

			const velocity = calculateVelocity();
			if (velocity > SWIPE_VELOCITY_THRESHOLD) {
				clearLongPressState();
			}
		},
		[clearLongPressState, calculateVelocity],
	);

	useEffect(() => {
		if (behaviorOverrides?.disableContextMenuTracking) {
			return;
		}
		const disposer = autorun(() => {
			handleContextMenuUpdate();
		});
		return () => {
			disposer();
		};
	}, [handleContextMenuUpdate, behaviorOverrides?.disableContextMenuTracking]);

	useEffect(() => {
		if (!behaviorOverrides?.disableContextMenuTracking) {
			return;
		}
		if (behaviorOverrides.contextMenuOpen !== undefined) {
			setContextMenuOpen(behaviorOverrides.contextMenuOpen);
		}
	}, [behaviorOverrides?.contextMenuOpen, behaviorOverrides?.disableContextMenuTracking]);

	const keyboardModeEnabled = KeyboardModeStore.keyboardModeEnabled;

	const handleFocusWithin = useCallback(() => {
		if (!keyboardModeEnabled) {
			return;
		}
		setIsFocusedWithin(true);
	}, [keyboardModeEnabled]);

	const handleBlurWithin = useCallback(() => {
		setIsFocusedWithin(false);
	}, []);

	useEffect(() => {
		if (mobileLayoutEnabled || !messageRef.current) return;
		const element = messageRef.current;
		const syncHoverState = () => {
			setIsHoveringDesktop(element.matches(':hover'));
		};
		const handleMouseEnter = () => {
			setIsHoveringDesktop(true);
		};
		const handleMouseLeave = () => {
			setIsHoveringDesktop(false);
		};
		element.addEventListener('mouseenter', handleMouseEnter);
		element.addEventListener('mouseleave', handleMouseLeave);
		window.addEventListener('focus', syncHoverState);
		const rafId = requestAnimationFrame(syncHoverState);
		return () => {
			cancelAnimationFrame(rafId);
			element.removeEventListener('mouseenter', handleMouseEnter);
			element.removeEventListener('mouseleave', handleMouseLeave);
			window.removeEventListener('focus', syncHoverState);
		};
	}, [mobileLayoutEnabled, keyboardModeEnabled]);

	useLayoutEffect(() => {
		const wasEditing = wasEditingInPreviousUpdateRef.current;
		const justStartedEditing = !wasEditing && isEditing;

		if (justStartedEditing && onEdit && messageRef.current) {
			onEdit(messageRef.current);
		}

		wasEditingInPreviousUpdateRef.current = isEditing;
	}, [isEditing, onEdit]);

	useEffect(() => {
		if (!mobileLayoutEnabled) return;

		const handleScroll = () => {
			if (touchStartPos.current) {
				clearLongPressState();
			}
		};

		window.addEventListener('scroll', handleScroll, {capture: true, passive: true});
		return () => {
			window.removeEventListener('scroll', handleScroll, {capture: true});
			if (longPressTimerRef.current) {
				clearTimeout(longPressTimerRef.current);
			}
			if (highlightTimerRef.current) {
				clearTimeout(highlightTimerRef.current);
			}
		};
	}, [mobileLayoutEnabled, clearLongPressState]);

	const isHovering = mobileLayoutEnabled ? false : isHoveringDesktop;

	useEffect(() => {
		if (!keyboardModeEnabled) {
			setIsFocusedWithin(false);
			return;
		}
		const activeElement = messageRef.current?.ownerDocument?.activeElement ?? document.activeElement;
		if (messageRef.current && activeElement && messageRef.current.contains(activeElement)) {
			setIsFocusedWithin(true);
		}
	}, [keyboardModeEnabled]);
	const actionBarHoverState = previewMode
		? true
		: isHovering || (keyboardModeEnabled && isFocusedWithin) || isPopoutOpen;

	const messageContextValue = useMemo(
		() => ({
			channel,
			message,
			handleDelete,
			shouldGroup,
			isHovering,
			previewContext,
			previewOverrides,
			onPopoutToggle: setIsPopoutOpen,
		}),
		[channel, message, handleDelete, shouldGroup, isHovering, previewContext, previewOverrides, setIsPopoutOpen],
	);

	const messageComponent = (
		<MessageViewContextProvider value={messageContextValue}>
			{getMessageComponent(channel, message, forceUnknownMessageType)}
		</MessageViewContextProvider>
	);

	const {nodes: astNodes} = parse({
		content: message.content,
		context: MarkdownContext.STANDARD_WITH_JUMBO,
	});

	const shouldHideContent =
		UserSettingsStore.getRenderEmbeds() &&
		message.embeds.length > 0 &&
		message.embeds.every((embed) => embed.type === MessageEmbedTypes.IMAGE || embed.type === MessageEmbedTypes.GIFV) &&
		astNodes.length === 1 &&
		astNodes[0].type === NodeType.Link &&
		!message.suppressEmbeds;

	const shouldDisableHoverBackground = prefersReducedMotion && !isEditing;
	const isKeyboardFocused = keyboardModeEnabled && isFocusedWithin;
	const shouldApplySpacing = !shouldGroup && !removeTopSpacing && previewContext !== MessagePreviewContext.LIST_POPOUT;

	const messageClasses = clsx(
		messageDisplayCompact ? styles.messageCompact : styles.message,
		shouldDisableHoverBackground && styles.messageNoHover,
		isEditing && styles.messageEditing,
		!messageDisplayCompact && shouldGroup && shouldApplyGroupedLayout(message, prevMessage) && styles.messageGrouped,
		!previewContext && message.isMentioned() && styles.messageMentioned,
		!previewContext &&
			(isReplying || isHighlight || isJumpTarget) &&
			(isReplying ? styles.messageReplying : styles.messageHighlight),
		message.type === MessageTypes.CLIENT_SYSTEM && message.author.id === FLUXERBOT_ID && styles.messageClientSystem,
		isLongPressing && styles.messageLongPress,
		!previewContext && (contextMenuOpen || isPopoutOpen) && styles.contextMenuActive,
		previewContext && styles.messagePreview,
		MobileLayoutStore.isEnabled() && styles.mobileLayout,
		!messageDisplayCompact &&
			(!message.content || shouldHideContent) &&
			!isEditing &&
			message.isUserMessage() &&
			styles.messageNoText,
		isKeyboardFocused && styles.keyboardFocused,
		isKeyboardFocused && 'keyboard-focus-active',
		shouldApplySpacing && previewContext && styles.messagePreviewSpacing,
	);

	const shouldShowActionBar =
		!previewContext && message.state !== MessageStates.SENDING && !isEditing && !MobileLayoutStore.isEnabled();

	const shouldShowBottomSheet =
		MobileLayoutStore.isEnabled() &&
		showActionBar &&
		!previewContext &&
		message.state !== MessageStates.SENDING &&
		!isEditing;

	return (
		<>
			<FocusRing>
				<div
					role="article"
					id={`${idPrefix}-${channel.id}-${message.id}`}
					data-message-id={message.id}
					data-channel-id={channel.id}
					tabIndex={keyboardModeEnabled ? 0 : undefined}
					className={messageClasses}
					ref={messageRef}
					onClick={handleAltClick}
					onKeyDown={handleAltKeyDown}
					onFocus={handleFocusWithin}
					onBlur={handleBlurWithin}
					onContextMenu={handleContextMenu}
					onTouchStart={handleLongPressStart}
					onTouchEnd={handleLongPressEnd}
					onTouchMove={handleLongPressMove}
					style={{
						touchAction: 'pan-y',
						WebkitUserSelect: 'text',
						userSelect: 'text',
						marginTop: shouldApplySpacing && previewContext ? `${messageGroupSpacing}px` : undefined,
					}}
				>
					{messageComponent}
					{shouldShowActionBar &&
						(previewMode ? (
							<MessageActionBarCore
								message={message}
								handleDelete={handleDelete}
								permissions={{
									canSendMessages: true,
									canAddReactions: true,
									canEditMessage: true,
									canDeleteMessage: true,
									canPinMessage: true,
									shouldRenderSuppressEmbeds: true,
								}}
								isSaved={false}
								developerMode={false}
								isHovering={actionBarHoverState}
								onPopoutToggle={setIsPopoutOpen}
							/>
						) : (
							<MessageActionBar
								message={message}
								handleDelete={handleDelete}
								isHovering={actionBarHoverState}
								onPopoutToggle={setIsPopoutOpen}
							/>
						))}
				</div>
			</FocusRing>

			{shouldShowBottomSheet && (
				<MessageActionBottomSheet
					isOpen={shouldShowBottomSheet}
					onClose={() => setShowActionBar(false)}
					message={message}
					handleDelete={handleDelete}
				/>
			)}
		</>
	);
});
