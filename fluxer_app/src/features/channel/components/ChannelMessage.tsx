// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {useContextMenuHoverState} from '@app/features/app/hooks/useContextMenuHoverState';
import {isMediaOnlyEmbed} from '@app/features/channel/components/embeds/EmbedRenderUtils';
import {MessageActionBar, MessageActionBarCore} from '@app/features/channel/components/MessageActionBar';
import {MessageActionBottomSheet} from '@app/features/channel/components/MessageActionBottomSheet';
import {requestDeleteMessage} from '@app/features/channel/components/MessageActionUtils';
import {MessageViewContextProvider} from '@app/features/channel/components/MessageViewContext';
import type {Channel} from '@app/features/channel/models/Channel';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {parse} from '@app/features/messaging/components/markdown/renderers';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import type {Message as MessageModel} from '@app/features/messaging/models/MessagingMessage';
import MessageEdit from '@app/features/messaging/state/MessageEdit';
import MessageFocus from '@app/features/messaging/state/MessageFocus';
import MessageReply from '@app/features/messaging/state/MessageReply';
import {getMessageComponent} from '@app/features/messaging/utils/MessageComponentUtils';
import {getParserFlagsForContext} from '@app/features/messaging/utils/markdown/MarkdownParserFlags';
import {parseAndRenderToPlaintext} from '@app/features/messaging/utils/markdown/Plaintext';
import {NodeType} from '@app/features/messaging/utils/markdown/parser/Enums';
import {SystemMessageUtils} from '@app/features/messaging/utils/SystemMessageUtils';
import {subscribeWindowFocus} from '@app/features/platform/utils/WindowFocusBroadcast';
import * as ReadStateCommands from '@app/features/read_state/commands/ReadStateCommands';
import styles from '@app/features/theme/styles/Message.module.css';
import {MessageContextMenu} from '@app/features/ui/action_menu/MessageContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {FLUXERBOT_ID} from '@fluxer/constants/src/AppConstants';
import {MessagePreviewContext, MessageStates, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';

const ATTACHMENT_DESCRIPTOR = msg({
	message: 'attachment',
	comment:
		'Screen-reader fragment listing one attachment on a message. Lowercase because it appears inside a longer sentence.',
});
const ATTACHMENTS_DESCRIPTOR = msg({
	message: '{length} attachments',
	comment: 'Screen-reader fragment listing multiple attachments on a message. length is the count.',
});
const STICKER_DESCRIPTOR = msg({
	message: 'sticker',
	comment:
		'Screen-reader fragment listing one sticker on a message. Lowercase because it appears inside a longer sentence.',
});
const STICKERS_DESCRIPTOR = msg({
	message: '{length} stickers',
	comment: 'Screen-reader fragment listing multiple stickers on a message. length is the count.',
});
const EMBED_DESCRIPTOR = msg({
	message: 'embed',
	comment:
		'Screen-reader fragment listing one embed on a message. Lowercase because it appears inside a longer sentence.',
});
const EMBEDS_DESCRIPTOR = msg({
	message: '{length} embeds',
	comment: 'Screen-reader fragment listing multiple embeds on a message. length is the count.',
});
const NO_TEXT_CONTENT_DESCRIPTOR = msg({
	message: 'no text content',
	comment:
		'Screen-reader fragment indicating the message has no text body. Lowercase because it appears inside a longer sentence.',
});
const MESSAGE_DESCRIPTOR = msg({
	message: 'message',
	comment: 'Fallback role label used in screen-reader message summaries.',
});
const MESSAGE_TYPE_NAMES: Record<number, string> = Object.fromEntries(
	Object.entries(MessageTypes).map(([name, value]) => [value, name]),
);
const shouldApplyGroupedLayout = (message: MessageModel, _prevMessage?: MessageModel) => {
	if (message.type !== MessageTypes.DEFAULT && message.type !== MessageTypes.REPLY && message.type !== MessageTypes.THREAD_STARTER_MESSAGE) {
		return false;
	}
	return true;
};
const isDisplaySystemMessage = (message: MessageModel): boolean =>
	message.type !== MessageTypes.DEFAULT && message.type !== MessageTypes.REPLY && message.type !== MessageTypes.THREAD_STARTER_MESSAGE;
const isActivationKey = (key: string) => key === 'Enter' || key === ' ' || key === 'Spacebar' || key === 'Space';
const MESSAGE_ARIA_PARSER_FLAGS = getParserFlagsForContext(MarkdownContext.STANDARD_WITHOUT_JUMBO);
const MAX_ARIA_MESSAGE_TEXT_LENGTH = 220;
const LONG_PRESS_DELAY = 500;
const MOVEMENT_THRESHOLD = 10;
const SWIPE_VELOCITY_THRESHOLD = 0.4;
const HIGHLIGHT_DELAY = 100;
const SUPPRESS_POST_LONG_PRESS_CLICK_MS = 750;
const NESTED_LONG_PRESS_OWNER_SELECTOR = '[data-long-press-owner="true"]';
const mobileLongPressScrollCancelHandlers = new Set<() => void>();

let mobileLongPressScrollListenerAttached = false;

const handleMobileLongPressWindowScroll = () => {
	for (const handler of mobileLongPressScrollCancelHandlers) {
		handler();
	}
};
const subscribeMobileLongPressScrollCancel = (handler: () => void): (() => void) => {
	mobileLongPressScrollCancelHandlers.add(handler);
	if (!mobileLongPressScrollListenerAttached) {
		window.addEventListener('scroll', handleMobileLongPressWindowScroll, {capture: true, passive: true});
		mobileLongPressScrollListenerAttached = true;
	}
	return () => {
		mobileLongPressScrollCancelHandlers.delete(handler);
		if (mobileLongPressScrollListenerAttached && mobileLongPressScrollCancelHandlers.size === 0) {
			window.removeEventListener('scroll', handleMobileLongPressWindowScroll, {capture: true});
			mobileLongPressScrollListenerAttached = false;
		}
	};
};
const handleAltClickEvent = (event: React.MouseEvent, message: MessageModel) => {
	if (!event.altKey) return;
	ReadStateCommands.markAsUnread(message.channelId, message.id);
};
const handleAltKeyboardEvent = (event: React.KeyboardEvent, message: MessageModel) => {
	if (!event.altKey || !isActivationKey(event.key)) {
		return;
	}
	event.preventDefault();
	ReadStateCommands.markAsUnread(message.channelId, message.id);
};
const getContextMenuLinkUrl = (target: EventTarget | null): string | undefined => {
	if (!(target instanceof HTMLElement)) {
		return undefined;
	}
	const anchor = target.closest('a');
	if (anchor?.href) {
		return anchor.href;
	}
	const mediaTarget = target.closest('[data-message-emoji="true"], [data-message-sticker="true"]');
	if (!mediaTarget) {
		return undefined;
	}
	const imageElement =
		mediaTarget instanceof HTMLImageElement ? mediaTarget : mediaTarget.querySelector<HTMLImageElement>('img');
	if (!imageElement) {
		return undefined;
	}
	const imageUrl = imageElement.currentSrc || imageElement.src;
	return imageUrl || undefined;
};
const getElementFromEventTarget = (target: EventTarget | null): Element | null => {
	if (target instanceof Element) return target;
	if (target instanceof Node) return target.parentElement;
	return null;
};
const isNestedLongPressOwnerTarget = (target: EventTarget | null): boolean =>
	getElementFromEventTarget(target)?.closest(NESTED_LONG_PRESS_OWNER_SELECTOR) != null;
const normalizeAriaLabelText = (text: string): string => text.replace(/\s+/g, ' ').trim();
const truncateAriaLabelText = (text: string): string => {
	const normalized = normalizeAriaLabelText(text);
	if (normalized.length <= MAX_ARIA_MESSAGE_TEXT_LENGTH) {
		return normalized;
	}
	return `${normalized.slice(0, MAX_ARIA_MESSAGE_TEXT_LENGTH - 1).trimEnd()}...`;
};
const isPointInsideMessageTree = (messageElement: HTMLElement, point: {x: number; y: number}): boolean => {
	const target = messageElement.ownerDocument.elementFromPoint(point.x, point.y);
	return Boolean(target && messageElement.contains(target));
};
let lastPointerPosition: {x: number; y: number} | null = null;
let pointerPositionNotificationFrame: number | null = null;
let pointerPositionSubscriptionCount = 0;
const pointerPositionListeners = new Set<() => void>();
const notifyPointerPositionListeners = (): void => {
	for (const listener of Array.from(pointerPositionListeners)) {
		listener();
	}
};
const schedulePointerPositionNotification = (): void => {
	if (pointerPositionNotificationFrame != null) {
		return;
	}
	pointerPositionNotificationFrame = requestAnimationFrame(() => {
		pointerPositionNotificationFrame = null;
		notifyPointerPositionListeners();
	});
};
const notifyPointerPositionListenersOnLayoutChange = (): void => {
	schedulePointerPositionNotification();
};
const updateLastPointerPosition = (event: PointerEvent | MouseEvent): void => {
	if (lastPointerPosition?.x === event.clientX && lastPointerPosition.y === event.clientY) {
		return;
	}
	lastPointerPosition = {x: event.clientX, y: event.clientY};
	schedulePointerPositionNotification();
};
const clearLastPointerPosition = (): void => {
	if (!lastPointerPosition) {
		return;
	}
	lastPointerPosition = null;
	schedulePointerPositionNotification();
};
const clearLastPointerPositionOnWindowExit = (event: PointerEvent | MouseEvent): void => {
	if (event.relatedTarget == null) {
		clearLastPointerPosition();
	}
};
const supportsPointerPositionEvents = (): boolean => 'PointerEvent' in window;
const subscribePointerPosition = (listener: () => void): (() => void) => {
	if (pointerPositionSubscriptionCount === 0) {
		if (supportsPointerPositionEvents()) {
			window.addEventListener('pointermove', updateLastPointerPosition, true);
			window.addEventListener('pointerdown', updateLastPointerPosition, true);
			window.addEventListener('pointerout', clearLastPointerPositionOnWindowExit, true);
		} else {
			window.addEventListener('mousemove', updateLastPointerPosition, true);
			window.addEventListener('mousedown', updateLastPointerPosition, true);
			window.addEventListener('mouseout', clearLastPointerPositionOnWindowExit, true);
		}
		window.addEventListener('scroll', notifyPointerPositionListenersOnLayoutChange, true);
		window.addEventListener('resize', notifyPointerPositionListenersOnLayoutChange);
		window.addEventListener('blur', clearLastPointerPosition);
	}
	pointerPositionSubscriptionCount += 1;
	pointerPositionListeners.add(listener);
	return () => {
		pointerPositionListeners.delete(listener);
		pointerPositionSubscriptionCount = Math.max(0, pointerPositionSubscriptionCount - 1);
		if (pointerPositionSubscriptionCount !== 0) {
			return;
		}
		lastPointerPosition = null;
		if (pointerPositionNotificationFrame != null) {
			cancelAnimationFrame(pointerPositionNotificationFrame);
			pointerPositionNotificationFrame = null;
		}
		if (supportsPointerPositionEvents()) {
			window.removeEventListener('pointermove', updateLastPointerPosition, true);
			window.removeEventListener('pointerdown', updateLastPointerPosition, true);
			window.removeEventListener('pointerout', clearLastPointerPositionOnWindowExit, true);
		} else {
			window.removeEventListener('mousemove', updateLastPointerPosition, true);
			window.removeEventListener('mousedown', updateLastPointerPosition, true);
			window.removeEventListener('mouseout', clearLastPointerPositionOnWindowExit, true);
		}
		window.removeEventListener('scroll', notifyPointerPositionListenersOnLayoutChange, true);
		window.removeEventListener('resize', notifyPointerPositionListenersOnLayoutChange);
		window.removeEventListener('blur', clearLastPointerPosition);
	};
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
	channel: Channel;
	message: MessageModel;
	prevMessage?: MessageModel;
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
	readonlyPreview?: boolean;
	onHeadingActivate?: () => void;
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
		readonlyPreview,
		onHeadingActivate,
	} = props;
	const {i18n} = useLingui();
	const [showActionBar, setShowActionBar] = useState(false);
	const [isLongPressing, setIsLongPressing] = useState(false);
	const [isHoveringDesktop, setIsHoveringDesktop] = useState(false);
	const [isFocusedWithin, setIsFocusedWithin] = useState(false);
	const [isPopoutOpen, setIsPopoutOpen] = useState(false);
	const [mobileLongPressLinkUrl, setMobileLongPressLinkUrl] = useState<string | undefined>(undefined);
	const messageRef = useRef<HTMLDivElement | null>(null);
	const disableContextMenuTracking = behaviorOverrides?.disableContextMenuTracking ?? false;
	const trackedContextMenuOpen = useContextMenuHoverState(messageRef, !disableContextMenuTracking);
	const contextMenuOpen = disableContextMenuTracking
		? (behaviorOverrides?.contextMenuOpen ?? false)
		: trackedContextMenuOpen;
	const longPressTimerRef = useRef<NodeJS.Timeout | null>(null);
	const unsubscribeLongPressScrollCancelRef = useRef<(() => void) | null>(null);
	const wasEditingInPreviousUpdateRef = useRef(false);
	const mobileLayoutEnabled = behaviorOverrides?.mobileLayoutEnabled ?? MobileLayout.isEnabled();
	const messageDisplayCompact =
		compact ?? behaviorOverrides?.messageDisplayCompact ?? UserSettings.getMessageDisplayCompact();
	const prefersReducedMotion = behaviorOverrides?.prefersReducedMotion ?? Accessibility.useReducedMotion;
	const isEditing = behaviorOverrides?.isEditing ?? MessageEdit.isEditing(message.channelId, message.id);
	const isReplying = behaviorOverrides?.isReplying ?? MessageReply.isReplying(message.channelId, message.id);
	const isHighlight = behaviorOverrides?.isHighlight ?? MessageReply.isHighlight(message.id);
	const forceUnknownMessageType =
		behaviorOverrides?.forceUnknownMessageType ?? DeveloperOptions.forceUnknownMessageType;
	const messageGroupSpacing =
		behaviorOverrides?.messageGroupSpacing ?? Accessibility.getMessageGroupSpacingValue(messageDisplayCompact);
	const authorName =
		previewOverrides?.displayName || NicknameUtils.getNickname(message.author, channel.guildId, channel.id);
	const messageAriaLabel = useMemo(() => {
		const timeLabel = DateUtils.getFormattedDateTime(message.timestamp);
		const systemText = message.isSystemMessage() ? SystemMessageUtils.stringify(message, i18n) : null;
		let text =
			systemText ||
			parseAndRenderToPlaintext(message.content, MESSAGE_ARIA_PARSER_FLAGS, {
				channelId: channel.id,
				preserveMarkdown: false,
				includeEmojiNames: true,
				i18n,
			});
		if (!text && message.attachments.length > 0) {
			text =
				message.attachments.length === 1
					? i18n._(ATTACHMENT_DESCRIPTOR)
					: i18n._(ATTACHMENTS_DESCRIPTOR, {length: message.attachments.length});
		}
		if (!text && message.stickerItems.length > 0) {
			text =
				message.stickerItems.length === 1
					? i18n._(STICKER_DESCRIPTOR)
					: i18n._(STICKERS_DESCRIPTOR, {length: message.stickerItems.length});
		}
		if (!text && message.embeds.length > 0) {
			text =
				message.embeds.length === 1
					? i18n._(EMBED_DESCRIPTOR)
					: i18n._(EMBEDS_DESCRIPTOR, {length: message.embeds.length});
		}
		if (!text) {
			text = i18n._(NO_TEXT_CONTENT_DESCRIPTOR);
		}
		return `${authorName}, ${truncateAriaLabelText(text)}, ${timeLabel}, ${i18n._(MESSAGE_DESCRIPTOR)}`;
	}, [
		authorName,
		channel.id,
		i18n.locale,
		message,
		message.attachments.length,
		message.content,
		message.embeds.length,
		message.stickerItems.length,
		message.timestamp,
	]);
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
			requestDeleteMessage(message, i18n, bypassConfirm);
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
			MessageFocus.holdContextFocus(channel.id, message.id, message, channel);
			const linkUrl = getContextMenuLinkUrl(event.target);
			ContextMenuCommands.openFromEvent(event, (props) => (
				<MessageContextMenu
					message={message}
					sourceChannel={channel}
					onClose={props.onClose}
					onDelete={handleDelete}
					linkUrl={linkUrl}
					data-flx="channel.message.handle-context-menu.message-context-menu"
				/>
			));
		},
		[
			previewContext,
			message,
			channel,
			isEditing,
			mobileLayoutEnabled,
			handleDelete,
			behaviorOverrides?.disableContextMenu,
		],
	);
	const touchStartPos = useRef<{x: number; y: number} | null>(null);
	const velocitySamples = useRef<Array<{x: number; y: number; timestamp: number}>>([]);
	const highlightTimerRef = useRef<NodeJS.Timeout | null>(null);
	const suppressClickUntilRef = useRef(0);
	const popoutCloseRafRef = useRef<number | null>(null);
	const isHoveringDesktopRef = useRef(false);
	const setDesktopHoverState = useCallback((isHovered: boolean) => {
		if (isHoveringDesktopRef.current === isHovered) {
			return;
		}
		isHoveringDesktopRef.current = isHovered;
		setIsHoveringDesktop(isHovered);
	}, []);
	const unsubscribeLongPressScrollCancel = useCallback(() => {
		unsubscribeLongPressScrollCancelRef.current?.();
		unsubscribeLongPressScrollCancelRef.current = null;
	}, []);
	const clearLongPressState = useCallback(
		(options?: {preserveLinkUrl?: boolean}) => {
			unsubscribeLongPressScrollCancel();
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
			if (!options?.preserveLinkUrl) {
				setMobileLongPressLinkUrl(undefined);
			}
		},
		[unsubscribeLongPressScrollCancel],
	);
	const calculateVelocity = useCallback((): number => {
		const samples = velocitySamples.current;
		if (samples.length < 2) return 0;
		const now = performance.now();
		let firstRecentIndex = samples.length - 1;
		for (let i = samples.length - 1; i >= 0; i--) {
			if (now - samples[i].timestamp >= 100) {
				break;
			}
			firstRecentIndex = i;
		}
		if (samples.length - firstRecentIndex < 2) return 0;
		const first = samples[firstRecentIndex];
		const last = samples[samples.length - 1];
		const dt = last.timestamp - first.timestamp;
		if (dt === 0) return 0;
		const dx = last.x - first.x;
		const dy = last.y - first.y;
		return Math.sqrt(dx * dx + dy * dy) / dt;
	}, []);
	const handleLongPressStart = useCallback(
		(event: React.TouchEvent) => {
			if (!mobileLayoutEnabled || previewContext || isNestedLongPressOwnerTarget(event.target)) {
				return;
			}
			const touch = event.touches[0];
			if (!touch) return;
			unsubscribeLongPressScrollCancel();
			setMobileLongPressLinkUrl(getContextMenuLinkUrl(event.target));
			unsubscribeLongPressScrollCancelRef.current = subscribeMobileLongPressScrollCancel(() => {
				if (touchStartPos.current) {
					clearLongPressState();
				}
			});
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
					suppressClickUntilRef.current = performance.now() + SUPPRESS_POST_LONG_PRESS_CLICK_MS;
					setShowActionBar(true);
					setIsLongPressing(false);
				}
				clearLongPressState({preserveLinkUrl: true});
			}, LONG_PRESS_DELAY);
		},
		[mobileLayoutEnabled, previewContext, clearLongPressState, unsubscribeLongPressScrollCancel],
	);
	const handleLongPressEnd = useCallback(() => {
		if (touchStartPos.current) {
			clearLongPressState();
		}
	}, [clearLongPressState]);
	const handleLongPressMove = useCallback(
		(event: React.TouchEvent) => {
			if (!touchStartPos.current) return;
			const touch = event.touches[0];
			if (!touch) return;
			velocitySamples.current.push({x: touch.clientX, y: touch.clientY, timestamp: performance.now()});
			if (velocitySamples.current.length > 10) {
				velocitySamples.current.shift();
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
	const handleClickCapture = useCallback(
		(event: React.MouseEvent<HTMLDivElement>) => {
			if (!mobileLayoutEnabled || performance.now() > suppressClickUntilRef.current) {
				return;
			}
			event.preventDefault();
			event.stopPropagation();
		},
		[mobileLayoutEnabled],
	);
	const handleBottomSheetClose = useCallback(() => {
		setShowActionBar(false);
		setMobileLongPressLinkUrl(undefined);
	}, []);
	const keyboardModeEnabled = KeyboardMode.keyboardModeEnabled;
	const isPointerInsideMessage = useCallback((): boolean => {
		if (mobileLayoutEnabled) {
			return false;
		}
		const element = messageRef.current;
		if (!element) {
			return false;
		}
		if (!lastPointerPosition) {
			return element.matches(':hover');
		}
		return isPointInsideMessageTree(element, lastPointerPosition);
	}, [mobileLayoutEnabled]);
	const syncPointerHoverState = useCallback((): boolean => {
		const isHovered = isPointerInsideMessage();
		setDesktopHoverState(isHovered);
		return isHovered;
	}, [isPointerInsideMessage, setDesktopHoverState]);
	const cancelScheduledPopoutClose = useCallback(() => {
		if (popoutCloseRafRef.current == null) {
			return;
		}
		cancelAnimationFrame(popoutCloseRafRef.current);
		popoutCloseRafRef.current = null;
	}, []);
	const handleMessagePopoutToggle = useCallback(
		(isOpen: boolean) => {
			if (isOpen) {
				cancelScheduledPopoutClose();
				setIsPopoutOpen(true);
				return;
			}
			cancelScheduledPopoutClose();
			popoutCloseRafRef.current = requestAnimationFrame(() => {
				popoutCloseRafRef.current = null;
				syncPointerHoverState();
				setIsPopoutOpen(false);
			});
		},
		[cancelScheduledPopoutClose, syncPointerHoverState],
	);
	const handleFocusWithin = useCallback(() => {
		if (!keyboardModeEnabled) {
			return;
		}
		setIsFocusedWithin(true);
		MessageFocus.focusMessage(channel.id, message.id, message, channel);
	}, [channel, message, keyboardModeEnabled]);
	const handleBlurWithin = useCallback(
		(event: React.FocusEvent<HTMLDivElement>) => {
			const nextTarget = event.relatedTarget;
			if (nextTarget instanceof Node && event.currentTarget.contains(nextTarget)) {
				return;
			}
			setIsFocusedWithin(false);
			MessageFocus.blurMessage(channel.id, message.id);
		},
		[channel.id, message.id],
	);
	useEffect(() => {
		if (mobileLayoutEnabled || !messageRef.current) return;
		const element = messageRef.current;
		const handleMouseEnter = (event: MouseEvent) => {
			updateLastPointerPosition(event);
			setDesktopHoverState(true);
		};
		const handleMouseLeave = (event: MouseEvent) => {
			updateLastPointerPosition(event);
			setDesktopHoverState(false);
		};
		element.addEventListener('mouseenter', handleMouseEnter);
		element.addEventListener('mouseleave', handleMouseLeave);
		const unsubscribeFocus = subscribeWindowFocus(syncPointerHoverState);
		const rafId = requestAnimationFrame(syncPointerHoverState);
		return () => {
			cancelAnimationFrame(rafId);
			element.removeEventListener('mouseenter', handleMouseEnter);
			element.removeEventListener('mouseleave', handleMouseLeave);
			unsubscribeFocus();
		};
	}, [mobileLayoutEnabled, keyboardModeEnabled, syncPointerHoverState]);
	const shouldTrackActivePointer = !mobileLayoutEnabled && (isHoveringDesktop || isPopoutOpen || contextMenuOpen);
	useEffect(() => {
		if (!shouldTrackActivePointer) {
			return;
		}
		const rafId = requestAnimationFrame(syncPointerHoverState);
		const unsubscribePointerPosition = subscribePointerPosition(syncPointerHoverState);
		return () => {
			cancelAnimationFrame(rafId);
			unsubscribePointerPosition();
		};
	}, [shouldTrackActivePointer, syncPointerHoverState]);
	const wasContextMenuOpenRef = useRef(false);
	useLayoutEffect(() => {
		const wasOpen = wasContextMenuOpenRef.current;
		wasContextMenuOpenRef.current = contextMenuOpen;
		if (wasOpen && !contextMenuOpen) {
			syncPointerHoverState();
		}
	}, [contextMenuOpen, syncPointerHoverState]);
	useEffect(() => {
		return () => {
			cancelScheduledPopoutClose();
		};
	}, [cancelScheduledPopoutClose]);
	useEffect(() => {
		if (!keyboardModeEnabled) return;
		if (contextMenuOpen) {
			MessageFocus.holdContextFocus(channel.id, message.id, message, channel);
			return;
		}
		MessageFocus.releaseContextFocus(channel.id, message.id);
		if (!isFocusedWithin) {
			MessageFocus.clearFocusedMessageIfMatches(channel.id, message.id);
		}
	}, [channel, contextMenuOpen, isFocusedWithin, keyboardModeEnabled, message, message.id]);
	useEffect(() => {
		const wasEditing = wasEditingInPreviousUpdateRef.current;
		const justStartedEditing = !wasEditing && isEditing;
		if (justStartedEditing && onEdit && messageRef.current) {
			onEdit(messageRef.current);
		}
		wasEditingInPreviousUpdateRef.current = isEditing;
	}, [isEditing, onEdit]);
	useEffect(() => {
		return () => {
			unsubscribeLongPressScrollCancel();
			if (longPressTimerRef.current) {
				clearTimeout(longPressTimerRef.current);
			}
			if (highlightTimerRef.current) {
				clearTimeout(highlightTimerRef.current);
			}
		};
	}, [unsubscribeLongPressScrollCancel]);
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
	const messageContextValue = useMemo(
		() => ({
			channel,
			message,
			handleDelete,
			shouldGroup,
			isHovering,
			messageDisplayCompact,
			previewContext,
			previewOverrides,
			previewPermissions: previewMode
				? {
						isDM: false,
						canSendMessages: true,
						canAddReactions: true,
						canEditMessage: true,
						canDeleteMessage: true,
						canDeleteAttachment: true,
						canPinMessage: true,
						canForwardMessage: true,
						canCreateThread: false,
						canSuppressEmbeds: true,
						shouldRenderSuppressEmbeds: false,
					}
				: undefined,
			onPopoutToggle: handleMessagePopoutToggle,
			readonlyPreview,
			onHeadingActivate,
		}),
		[
			channel,
			message,
			handleDelete,
			shouldGroup,
			isHovering,
			messageDisplayCompact,
			previewContext,
			previewOverrides,
			previewMode,
			handleMessagePopoutToggle,
			readonlyPreview,
			onHeadingActivate,
		],
	);
	const messageComponent = (
		<MessageViewContextProvider value={messageContextValue} data-flx="channel.message.message-view-context-provider">
			{getMessageComponent(message, channel, forceUnknownMessageType)}
		</MessageViewContextProvider>
	);
	const {nodes: astNodes} = useMemo(
		() =>
			parse({
				content: message.content,
				context: MarkdownContext.STANDARD_WITH_JUMBO,
			}),
		[message.content],
	);
	const shouldHideContent =
		UserSettings.getRenderEmbeds() &&
		message.embeds.length > 0 &&
		message.embeds.every(isMediaOnlyEmbed) &&
		astNodes.length === 1 &&
		astNodes[0].type === NodeType.Link &&
		!message.suppressEmbeds;
	const shouldDisableHoverBackground = (prefersReducedMotion && !isEditing) || readonlyPreview;
	const isKeyboardFocused = keyboardModeEnabled && isFocusedWithin;
	const shouldApplySpacing = !shouldGroup && !removeTopSpacing && previewContext !== MessagePreviewContext.LIST_POPOUT;
	const systemFollowsSystem = Boolean(
		shouldGroup && prevMessage && isDisplaySystemMessage(prevMessage) && isDisplaySystemMessage(message),
	);
	const messageClasses = useMemo(
		() =>
			clsx(
				messageDisplayCompact ? styles.messageCompact : styles.message,
				isHovering && styles.messageHovered,
				shouldDisableHoverBackground && styles.messageNoHover,
				isEditing && styles.messageEditing,
				!messageDisplayCompact &&
					shouldGroup &&
					shouldApplyGroupedLayout(message, prevMessage) &&
					styles.messageGrouped,
				systemFollowsSystem && styles.systemMessageFollowsSystem,
				!previewContext && message.isMentioned() && styles.messageMentioned,
				!previewContext &&
					(isReplying || isHighlight || isJumpTarget) &&
					(isReplying ? styles.messageReplying : styles.messageHighlight),
				message.type === MessageTypes.CLIENT_SYSTEM && message.author.id === FLUXERBOT_ID && styles.messageClientSystem,
				isLongPressing && styles.messageLongPress,
				!previewContext && (contextMenuOpen || isPopoutOpen) && styles.contextMenuActive,
				previewContext && styles.messagePreview,
				mobileLayoutEnabled && styles.mobileLayout,
				!messageDisplayCompact &&
					(!message.content || shouldHideContent) &&
					!isEditing &&
					message.isUserMessage() &&
					styles.messageNoText,
				isKeyboardFocused && styles.keyboardFocused,
				isKeyboardFocused && 'keyboard-focus-active',
				shouldApplySpacing && previewContext && styles.messagePreviewSpacing,
			),
		[
			messageDisplayCompact,
			isHovering,
			shouldDisableHoverBackground,
			isEditing,
			systemFollowsSystem,
			shouldGroup,
			message,
			prevMessage,
			previewContext,
			isReplying,
			isHighlight,
			isJumpTarget,
			isLongPressing,
			contextMenuOpen,
			isPopoutOpen,
			mobileLayoutEnabled,
			shouldHideContent,
			isKeyboardFocused,
			shouldApplySpacing,
		],
	);
	const shouldShowActionBar = useMemo(
		() =>
			!previewContext &&
			!readonlyPreview &&
			message.state !== MessageStates.SENDING &&
			!isEditing &&
			!mobileLayoutEnabled,
		[previewContext, readonlyPreview, message.state, isEditing, mobileLayoutEnabled],
	);
	const shouldRenderInlineActionBar = useMemo(
		() => shouldShowActionBar && (previewMode || isHovering || isKeyboardFocused || contextMenuOpen || isPopoutOpen),
		[shouldShowActionBar, previewMode, isHovering, isKeyboardFocused, contextMenuOpen, isPopoutOpen],
	);
	const shouldShowBottomSheet = useMemo(
		() =>
			mobileLayoutEnabled && showActionBar && !previewContext && message.state !== MessageStates.SENDING && !isEditing,
		[mobileLayoutEnabled, showActionBar, previewContext, message.state, isEditing],
	);
	const articleStyle = useMemo<React.CSSProperties>(
		() => ({
			touchAction: 'pan-y',
			WebkitUserSelect: 'text',
			userSelect: 'text',
			marginTop: shouldApplySpacing && previewContext ? `${messageGroupSpacing}px` : undefined,
		}),
		[shouldApplySpacing, previewContext, messageGroupSpacing],
	);
	return (
		<>
			<FocusRing data-flx="channel.message.focus-ring">
				<div
					role="article"
					aria-label={messageAriaLabel}
					id={`${idPrefix}-${channel.id}-${message.id}`}
					data-message-id={message.id}
					data-channel-id={channel.id}
					data-flx-message-id={message.id}
					data-flx-message-type={MESSAGE_TYPE_NAMES[message.type] ?? String(message.type)}
					data-flx-channel-id={channel.id}
					data-flx-guild-id={channel.guildId ?? undefined}
					data-flx-author-id={message.author.id}
					data-flx-author-username={NicknameUtils.formatNameForStreamerMode(message.author.username)}
					data-flx-author-name={authorName}
					data-flx-author-self={message.author.id === Users.currentUserId ? 'true' : undefined}
					data-flx-author-bot={message.author.bot ? 'true' : undefined}
					data-flx-author-webhook={message.webhookId != null ? 'true' : undefined}
					data-flx-reply={message.type === MessageTypes.REPLY || message.messageReference != null ? 'true' : undefined}
					data-flx-blocked={message.blocked ? 'true' : undefined}
					data-flx-pinned={message.pinned ? 'true' : undefined}
					data-flx-call={message.type === MessageTypes.CALL ? 'true' : undefined}
					data-flx-system={message.isSystemMessage() ? 'true' : undefined}
					data-flx-mentioned={message.isMentioned() ? 'true' : undefined}
					data-flx-edited={message.editedTimestamp != null ? 'true' : undefined}
					data-flx-compact={messageDisplayCompact ? 'true' : undefined}
					data-flx-grouped={shouldGroup && shouldApplyGroupedLayout(message, prevMessage) ? 'true' : undefined}
					tabIndex={keyboardModeEnabled ? -1 : undefined}
					className={messageClasses}
					ref={messageRef}
					onClickCapture={handleClickCapture}
					onClick={handleAltClick}
					onKeyDown={handleAltKeyDown}
					onFocus={handleFocusWithin}
					onBlur={handleBlurWithin}
					onContextMenu={handleContextMenu}
					onTouchStart={handleLongPressStart}
					onTouchEnd={handleLongPressEnd}
					onTouchMove={handleLongPressMove}
					onTouchCancel={handleLongPressEnd}
					style={articleStyle}
					data-flx="channel.message.article.alt-click"
				>
					{messageComponent}
					{shouldRenderInlineActionBar &&
						(previewMode ? (
							<MessageActionBarCore
								message={message}
								handleDelete={handleDelete}
								permissions={{
									channel,
									canSendMessages: true,
									canAddReactions: true,
									canEditMessage: true,
									canDeleteMessage: true,
									canPinMessage: true,
									canForwardMessage: true,
									shouldRenderSuppressEmbeds: true,
									canCreateThread: false,
								}}
								isSaved={false}
								developerMode={false}
								onPopoutToggle={handleMessagePopoutToggle}
								data-flx="channel.message.message-action-bar-core"
							/>
						) : (
							<MessageActionBar
								message={message}
								handleDelete={handleDelete}
								sourceChannel={channel}
								onPopoutToggle={handleMessagePopoutToggle}
								data-flx="channel.message.message-action-bar"
							/>
						))}
				</div>
			</FocusRing>
			{shouldShowBottomSheet && (
				<MessageActionBottomSheet
					isOpen={shouldShowBottomSheet}
					onClose={handleBottomSheetClose}
					message={message}
					sourceChannel={channel}
					handleDelete={handleDelete}
					linkUrl={mobileLongPressLinkUrl}
					data-flx="channel.message.message-action-bottom-sheet"
				/>
			)}
		</>
	);
});
