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

import * as MessageActionCreators from '@app/actions/MessageActionCreators';
import * as ReadStateActionCreators from '@app/actions/ReadStateActionCreators';
import {renderChannelStream} from '@app/components/channel/ChannelMessageStream';
import {ChannelWelcomeSection} from '@app/components/channel/ChannelWelcomeSection';
import styles from '@app/components/channel/Messages.module.css';
import {NewMessagesBar} from '@app/components/channel/NewMessagesBar';
import ScrollFillerSkeleton from '@app/components/channel/ScrollFillerSkeleton';
import {UploadManager} from '@app/components/channel/UploadManager';
import {Scroller} from '@app/components/uikit/Scroller';
import {Spinner} from '@app/components/uikit/Spinner';
import {useMessageListKeyboardNavigation} from '@app/hooks/useMessageListKeyboardNavigation';
import {ChannelMessages} from '@app/lib/ChannelMessages';
import {ComponentDispatch} from '@app/lib/ComponentDispatch';
import {parseAndRenderToPlaintext} from '@app/lib/markdown/Plaintext';
import {getParserFlagsForContext} from '@app/lib/markdown/renderers';
import {MarkdownContext} from '@app/lib/markdown/renderers/RendererTypes';
import {usePlaceholderSpecs} from '@app/lib/PlaceholderSpecs';
import {useScrollManager} from '@app/lib/ScrollManager';
import type {ChannelRecord} from '@app/records/ChannelRecord';
import type {UserRecord} from '@app/records/UserRecord';
import AccessibilityStore from '@app/stores/AccessibilityStore';
import GuildVerificationStore from '@app/stores/GuildVerificationStore';
import GatewayConnectionStore from '@app/stores/gateway/GatewayConnectionStore';
import KeyboardModeStore from '@app/stores/KeyboardModeStore';
import MessageEditStore from '@app/stores/MessageEditStore';
import MessageFocusStore from '@app/stores/MessageFocusStore';
import MessageStore from '@app/stores/MessageStore';
import ModalStore from '@app/stores/ModalStore';
import PermissionStore from '@app/stores/PermissionStore';
import ReadStateStore from '@app/stores/ReadStateStore';
import SelectedChannelStore from '@app/stores/SelectedChannelStore';
import UserSettingsStore from '@app/stores/UserSettingsStore';
import UserStore from '@app/stores/UserStore';
import WindowStore from '@app/stores/WindowStore';
import {type ChannelStreamItem, createChannelStream} from '@app/utils/MessageGroupingUtils';
import {buildMessageSelectionCopyText} from '@app/utils/MessageSelectionCopyUtils';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MAX_MESSAGES_PER_CHANNEL} from '@fluxer/constants/src/LimitConstants';
import {extractTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';
import {useLingui} from '@lingui/react/macro';
import {runInAction} from 'mobx';
import {observer, useLocalObservable} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useMemo, useRef} from 'react';

const MESSAGE_COPY_PARSER_FLAGS = getParserFlagsForContext(MarkdownContext.STANDARD_WITHOUT_JUMBO);

function checkPermissions(channel: ChannelRecord) {
	const canSendMessages = PermissionStore.can(Permissions.SEND_MESSAGES, channel);
	const passesVerification = channel.isPrivate() || GuildVerificationStore.canAccessGuild(channel.guildId || '');
	const canChat = channel.isPrivate() || (canSendMessages && passesVerification);
	const canAttachFiles = channel.isPrivate()
		? canChat
		: canChat && PermissionStore.can(Permissions.ATTACH_FILES, channel);
	const canManageMessages = PermissionStore.can(Permissions.MANAGE_MESSAGES, channel);
	return {canSendMessages, canChat, canAttachFiles, canManageMessages};
}

interface MessagesStoreSnapshot {
	unreadCount: number;
	oldestUnreadMessageId: string | null;
	visualUnreadMessageId: string | null;
	ackMessageId: string | null;
	lastReadStateMessageId: string | null;
	messages: ChannelMessages;
	messageVersion: number;
	revealedMessageId: string | null;
	permissionVersion: number;
	messageGroupSpacing: number;
	fontSize: number;
	messageDisplayCompact: boolean;
	editingMessageId: string | null;
	currentUser: UserRecord | undefined;
	isEstimated: boolean;
	isManualAck: boolean;
}

interface MessagesProps {
	channel: ChannelRecord;
	onBottomBarVisibilityChange?: (visible: boolean) => void;
}

const readFromStores = (channelId: string): MessagesStoreSnapshot => {
	const messages = MessageStore.getMessages(channelId);

	return {
		unreadCount: ReadStateStore.getUnreadCount(channelId),
		oldestUnreadMessageId: ReadStateStore.getOldestUnreadMessageId(channelId),
		visualUnreadMessageId: ReadStateStore.getVisualUnreadMessageId(channelId),
		ackMessageId: ReadStateStore.ackMessageId(channelId),
		lastReadStateMessageId: ReadStateStore.lastMessageId(channelId),
		messages,
		messageVersion: messages.version,
		revealedMessageId: messages.revealedMessageId,
		permissionVersion: PermissionStore.version,
		messageGroupSpacing: AccessibilityStore.messageGroupSpacingValue,
		fontSize: AccessibilityStore.fontSize,
		messageDisplayCompact: UserSettingsStore.getMessageDisplayCompact(),
		editingMessageId: MessageEditStore.getEditingMessageId(channelId),
		currentUser: UserStore.currentUser ?? undefined,
		isEstimated: ReadStateStore.getIfExists(channelId)?.estimated ?? false,
		isManualAck: ReadStateStore.getIfExists(channelId)?.isManualAck ?? false,
	};
};

function shallowEqual<T extends object>(a: T, b: T): boolean {
	const aKeys = Object.keys(a);
	if (aKeys.length !== Object.keys(b).length) return false;
	for (const key of aKeys) {
		if ((a as Record<string, unknown>)[key] !== (b as Record<string, unknown>)[key]) return false;
	}
	return true;
}

export const Messages = observer(function Messages({channel, onBottomBarVisibilityChange}: MessagesProps) {
	const {i18n} = useLingui();
	const messagesWrapperRef = useRef<HTMLDivElement | null>(null);
	const scrollerInnerRef = useRef<HTMLDivElement | null>(null);
	const lastStoreSnapshotRef = useRef<MessagesStoreSnapshot | null>(null);
	const recoveryFetchChannelIdRef = useRef<string | null>(null);

	interface MessageState extends MessagesStoreSnapshot {
		highlightedMessageId: string | null;
		isAtBottom: boolean;
	}

	const state = useLocalObservable<MessageState>(() => {
		const initial = readFromStores(channel.id);
		lastStoreSnapshotRef.current = initial;
		return {
			...initial,
			highlightedMessageId: null,
			isAtBottom: false,
		};
	});

	const windowId = WindowStore.windowId;
	const isWindowFocused = WindowStore.isFocused();
	const isModalOpen = ModalStore.hasModalOpen();
	const isGatewayConnected = GatewayConnectionStore.isConnected;
	const selectedChannelId = SelectedChannelStore.currentChannelId;
	const placeholderSpecs = usePlaceholderSpecs(
		state.messageDisplayCompact,
		state.messageGroupSpacing,
		state.fontSize,
		channel.id,
	);
	const safeMessages = state.messages ?? MessageStore.getMessages(channel.id);

	const canAutoAck = !state.isManualAck && isWindowFocused && !isModalOpen;

	const scrollManager = useScrollManager({
		messages: safeMessages,
		channel,
		compact: state.messageDisplayCompact,
		hasUnreads: state.unreadCount > 0,
		focusId: null,
		placeholderHeight: placeholderSpecs.totalHeight,
		canLoadMore: true,
		windowId,
		handleScrollToBottom: () => {
			runInAction(() => {
				state.isAtBottom = true;
			});
		},
		handleScrollFromBottom: () => {
			runInAction(() => {
				state.isAtBottom = false;
			});
		},
		additionalMessagePadding: 48,
		canAutoAck,
	});

	useLayoutEffect(() => {
		const node = messagesWrapperRef.current;
		if (node) {
			node.style.setProperty('--message-group-spacing', `${state.messageGroupSpacing}px`);
		}
	}, [state.messageGroupSpacing]);

	useEffect(() => {
		ChannelMessages.retainChannel(channel.id);
		return () => {
			ChannelMessages.releaseRetainedChannel(channel.id);
		};
	}, [channel.id]);

	const jumpHighlightTimeoutRef = useRef<number | null>(null);
	const lastJumpSequenceIdRef = useRef<number | null>(null);

	const updateFromStores = useCallback(() => {
		const snapshot = readFromStores(channel.id);
		const previous = lastStoreSnapshotRef.current;
		if (previous && shallowEqual(previous, snapshot)) return;

		runInAction(() => {
			Object.assign(state, snapshot);
		});
		lastStoreSnapshotRef.current = snapshot;
	}, [channel.id, state]);

	const onMessageEdit = useCallback(
		(targetNode: HTMLElement) => {
			const scrollerNode = scrollManager.ref.current?.getScrollerNode();
			if (!scrollerNode) return;

			if (scrollManager.isPinned()) {
				return;
			}

			if (KeyboardModeStore.keyboardModeEnabled) {
				const focusedMessageId = MessageFocusStore.focusedMessageId;
				const focusedChannelId = MessageFocusStore.focusedChannelId;
				const editedMessageId = targetNode.getAttribute('data-message-id');
				if (focusedChannelId === channel.id && focusedMessageId && editedMessageId === focusedMessageId) {
					return;
				}
			}

			const targetRect = targetNode.getBoundingClientRect();
			const scrollerRect = scrollerNode.getBoundingClientRect();
			const isAbove = targetRect.top < scrollerRect.top;
			const isBelow = targetRect.bottom > scrollerRect.bottom;

			if (isAbove || isBelow) {
				scrollManager.ref.current?.scrollIntoViewNode({
					node: targetNode,
					padding: 80,
					animate: false,
				});
				scrollManager.handleScroll();
			}
		},
		[scrollManager, channel.id],
	);

	const onReveal = useCallback(
		(messageId: string | null) => {
			MessageActionCreators.revealMessage(channel.id, messageId);
		},
		[channel.id],
	);

	const onScrollToPresent = useCallback(() => {
		if (state.messages?.hasMoreAfter) {
			MessageActionCreators.jumpToPresent(channel.id, MAX_MESSAGES_PER_CHANNEL);
		} else {
			scrollManager.setScrollToBottom(false);
		}
	}, [channel.id, state.messages?.hasMoreAfter, scrollManager]);

	const onScrollToPresentAndAck = useCallback(() => {
		if (state.messages?.hasMoreAfter) {
			MessageActionCreators.jumpToPresent(channel.id, MAX_MESSAGES_PER_CHANNEL);
		} else {
			scrollManager.setScrollToBottom(false);
		}
		if (state.visualUnreadMessageId != null) {
			ReadStateActionCreators.clearStickyUnread(channel.id);
		}
		if (ReadStateStore.hasUnread(channel.id)) {
			ReadStateActionCreators.ack(channel.id, true, false);
		}
	}, [channel.id, state.messages?.hasMoreAfter, state.visualUnreadMessageId, scrollManager]);

	const onRetryLoadMessages = useCallback(() => {
		void MessageActionCreators.fetchMessages(channel.id, null, null, MAX_MESSAGES_PER_CHANNEL);
	}, [channel.id]);

	const onCopySelectedMessages = useCallback(
		(event: React.ClipboardEvent<HTMLDivElement>) => {
			if (state.messageDisplayCompact) {
				return;
			}

			const scrollerInnerNode = scrollerInnerRef.current;
			if (!scrollerInnerNode || !event.clipboardData) {
				return;
			}

			const selection = scrollerInnerNode.ownerDocument.defaultView?.getSelection() ?? null;
			const clipboardText = buildMessageSelectionCopyText({
				rootElement: scrollerInnerNode,
				selection,
				getMessagePlaintext: (messageId: string) => {
					const message = MessageStore.getMessage(channel.id, messageId);
					if (!message || !message.isUserMessage()) {
						return null;
					}

					return parseAndRenderToPlaintext(message.content, MESSAGE_COPY_PARSER_FLAGS, {
						channelId: channel.id,
						preserveMarkdown: false,
						includeEmojiNames: true,
						i18n,
					});
				},
			});

			if (!clipboardText) {
				return;
			}

			event.preventDefault();
			event.clipboardData.setData('text/plain', clipboardText);
		},
		[state.messageDisplayCompact, channel.id, i18n],
	);

	useEffect(() => {
		const storeUnsubs = [
			MessageStore.subscribe(updateFromStores),
			ReadStateStore.subscribe(updateFromStores),
			UserStore.subscribe(updateFromStores),
			PermissionStore.subscribe(updateFromStores),
			AccessibilityStore.subscribe(updateFromStores),
			UserSettingsStore.subscribe(updateFromStores),
			MessageEditStore.subscribe(updateFromStores),
		];

		const onForceJumpToPresent = () => {
			MessageActionCreators.jumpToPresent(channel.id, MAX_MESSAGES_PER_CHANNEL);
		};

		const onScrollPageUp = () => scrollManager.scrollPageUp(true);
		const onScrollPageDown = () => scrollManager.scrollPageDown(true);

		const onLayoutResized = (payload?: unknown) => {
			const data = payload as {channelId?: string; heightDelta?: number} | undefined;
			if (data?.channelId && data.channelId !== channel.id) return;
			scrollManager.handleLayoutResized(data?.heightDelta);
		};

		const onFocusBottommostMessage = (payload?: unknown) => {
			const data = (payload ?? {}) as {channelId?: string};
			if (!data.channelId || data.channelId !== channel.id) return;

			const scroller = scrollManager.ref.current?.getScrollerNode();
			if (!scroller) return;

			const doc = scroller.ownerDocument ?? document;
			const messageElements = Array.from(
				doc.querySelectorAll<HTMLElement>(`[data-channel-id="${channel.id}"][data-message-id]`),
			);
			if (!messageElements.length) return;

			const scrollerRect = scroller.getBoundingClientRect();

			let bottomMostVisibleMessage: HTMLElement | null = null;
			let bottomMostVisibleY = -Infinity;

			for (const messageEl of messageElements) {
				const rect = messageEl.getBoundingClientRect();
				const messageHeight = rect.height;

				if (messageHeight === 0) continue;

				const visibleTop = Math.max(rect.top, scrollerRect.top);
				const visibleBottom = Math.min(rect.bottom, scrollerRect.bottom);
				const visibleHeight = Math.max(0, visibleBottom - visibleTop);
				const visibilityRatio = visibleHeight / messageHeight;

				if (visibilityRatio >= 0.75) {
					if (rect.bottom > bottomMostVisibleY) {
						bottomMostVisibleY = rect.bottom;
						bottomMostVisibleMessage = messageEl;
					}
				}
			}

			if (bottomMostVisibleMessage) {
				const messageId = bottomMostVisibleMessage.dataset.messageId;
				if (messageId) {
					scrollManager.focusMessage(messageId);
				}
			}
		};

		const dispatchUnsubs = [
			ComponentDispatch.subscribe('SCROLLTO_PRESENT', onScrollToPresent),
			ComponentDispatch.subscribe('FORCE_JUMP_TO_PRESENT', onForceJumpToPresent),
			ComponentDispatch.subscribe('ESCAPE_PRESSED', onScrollToPresentAndAck),
			ComponentDispatch.subscribe('SCROLL_PAGE_UP', onScrollPageUp),
			ComponentDispatch.subscribe('SCROLL_PAGE_DOWN', onScrollPageDown),
			ComponentDispatch.subscribe('LAYOUT_RESIZED', onLayoutResized),
			ComponentDispatch.subscribe('FOCUS_BOTTOMMOST_MESSAGE', onFocusBottommostMessage),
		];

		updateFromStores();

		return () => {
			storeUnsubs.forEach((u) => u());
			dispatchUnsubs.forEach((u) => u());
		};
	}, [channel.id, updateFromStores, onScrollToPresent, onScrollToPresentAndAck, scrollManager]);

	useEffect(() => {
		const editingMessageId = state.editingMessageId;

		if (editingMessageId) {
			scrollManager.enterEditMode();
		} else {
			scrollManager.exitEditMode();
		}
	}, [state.editingMessageId, scrollManager]);

	useEffect(() => {
		const messages = state.messages;
		if (
			!messages ||
			messages.ready ||
			messages.loadingMore ||
			messages.error ||
			messages.length > 0 ||
			!isGatewayConnected ||
			selectedChannelId !== channel.id
		) {
			if (recoveryFetchChannelIdRef.current === channel.id) {
				recoveryFetchChannelIdRef.current = null;
			}
			return;
		}

		if (recoveryFetchChannelIdRef.current === channel.id) {
			return;
		}

		recoveryFetchChannelIdRef.current = channel.id;
		void MessageActionCreators.fetchMessages(channel.id, null, null, MAX_MESSAGES_PER_CHANNEL).finally(() => {
			if (recoveryFetchChannelIdRef.current === channel.id) {
				recoveryFetchChannelIdRef.current = null;
			}
		});
	}, [
		channel.id,
		isGatewayConnected,
		selectedChannelId,
		state.messages?.ready,
		state.messages?.loadingMore,
		state.messages?.error,
		state.messageVersion,
	]);

	useMessageListKeyboardNavigation({
		containerRef: scrollManager.ref,
		channelId: channel.id,
		onFocusMessage: (messageId) => {
			scrollManager.focusMessage(messageId);
		},
		onLoadMoreBefore: () => {
			scrollManager.loadMoreForKeyboardNavigation(false);
		},
		onLoadMoreAfter: () => {
			scrollManager.loadMoreForKeyboardNavigation(true);
		},
		hasMoreBefore: state.messages?.hasMoreBefore ?? false,
		hasMoreAfter: state.messages?.hasMoreAfter ?? false,
		isLoadingMore: state.messages?.loadingMore ?? false,
		onEscape: () => {
			ComponentDispatch.dispatch('FOCUS_TEXTAREA', {channelId: channel.id});
		},
		allowWhenInactive: true,
	});

	useLayoutEffect(() => {
		const messages = state.messages;
		if (!messages || !messages.ready || !messages.jumped || !messages.jumpTargetId) return;

		const jsid = messages.jumpSequenceId;
		if (jsid === lastJumpSequenceIdRef.current) return;
		lastJumpSequenceIdRef.current = jsid;

		if (jumpHighlightTimeoutRef.current != null) {
			clearTimeout(jumpHighlightTimeoutRef.current);
			jumpHighlightTimeoutRef.current = null;
		}

		if (messages.jumpFlash && messages.jumpTargetId) {
			runInAction(() => {
				state.highlightedMessageId = messages.jumpTargetId;
			});

			jumpHighlightTimeoutRef.current = window.setTimeout(() => {
				runInAction(() => {
					if (state.highlightedMessageId === messages.jumpTargetId) {
						state.highlightedMessageId = null;
					}
				});
				jumpHighlightTimeoutRef.current = null;
			}, 2000);
		}
	}, [state.messages?.ready, state.messages?.jumpSequenceId, state.messages?.jumpTargetId, state]);

	useEffect(() => {
		if (!canAutoAck || !state.isAtBottom || !state.messages?.ready) return;
		if (ReadStateStore.hasUnread(channel.id)) {
			ReadStateActionCreators.ackWithStickyUnread(channel.id);
		}
	}, [canAutoAck, state.isAtBottom, state.messages?.ready, channel.id]);

	useEffect(() => {
		return () => {
			const readState = ReadStateStore.getIfExists(channel.id);
			if (readState?.isManualAck) {
				ReadStateActionCreators.clearManualAck(channel.id);
			}
			ReadStateActionCreators.clearStickyUnread(channel.id);
		};
	}, [channel.id]);

	const channelStream = useMemo<Array<ChannelStreamItem>>(() => {
		if (!state.messages?.ready) return [];

		return createChannelStream({
			channel,
			messages: state.messages,
			oldestUnreadMessageId: state.visualUnreadMessageId,
			treatSpam: false,
		});
	}, [channel, state.messages?.ready, state.messageVersion, state.visualUnreadMessageId]);

	const {canAttachFiles} = useMemo(
		() => checkPermissions(channel),
		[channel.id, channel.guildId, state.permissionVersion],
	);

	const streamMarkup = useMemo(() => {
		if (!state.messages?.ready) return null;

		return renderChannelStream({
			channelStream,
			messages: state.messages,
			channel,
			highlightedMessageId: state.highlightedMessageId,
			messageDisplayCompact: state.messageDisplayCompact,
			messageGroupSpacing: state.messageGroupSpacing,
			revealedMessageId: state.revealedMessageId,
			onMessageEdit,
			onReveal,
		});
	}, [
		channelStream,
		state.messages?.ready,
		channel,
		state.highlightedMessageId,
		state.messageDisplayCompact,
		state.messageGroupSpacing,
		state.revealedMessageId,
		onMessageEdit,
		onReveal,
	]);

	const hasJumpToPresentBar = Boolean(state.messages?.ready && state.messages.hasMoreAfter);
	const hasLoadErrorBar = Boolean(state.messages?.error);
	const hasBottomBar = hasJumpToPresentBar || hasLoadErrorBar;

	useEffect(() => {
		onBottomBarVisibilityChange?.(hasBottomBar);
	}, [hasBottomBar, onBottomBarVisibilityChange]);

	useEffect(() => {
		return () => {
			onBottomBarVisibilityChange?.(false);
		};
	}, [onBottomBarVisibilityChange]);

	const jumpToPresentBar = hasJumpToPresentBar ? (
		<JumpToPresentBar
			loadingMore={state.messages.loadingMore}
			jumpedToPresent={state.messages.jumpedToPresent}
			onJumpToPresent={onScrollToPresent}
		/>
	) : null;

	const loadErrorBar = hasLoadErrorBar ? (
		<LoadErrorBar loading={state.messages.loadingMore} onRetry={onRetryLoadMessages} />
	) : null;

	const showNewMessagesBar = state.messages?.ready && state.unreadCount > 0;

	const topBar = showNewMessagesBar ? (
		<NewMessagesBar
			unreadCount={state.unreadCount}
			oldestUnreadTimestamp={state.oldestUnreadMessageId ? extractTimestamp(state.oldestUnreadMessageId) : 0}
			isEstimated={state.isEstimated}
			onJumpToNewMessages={onScrollToPresentAndAck}
		/>
	) : null;

	const readyMessages = state.messages?.ready ? state.messages : null;

	const scrollerInner = readyMessages ? (
		<>
			{!readyMessages.hasMoreBefore && <ChannelWelcomeSection channel={channel} />}
			{readyMessages.hasMoreBefore && (
				<>
					<div className={styles.placeholderSpacer} />
					<ScrollFillerSkeleton {...placeholderSpecs} />
				</>
			)}
			{streamMarkup}
			{readyMessages.hasMoreAfter && <ScrollFillerSkeleton {...placeholderSpecs} />}
			<div className={styles.scrollerSpacer} />
		</>
	) : null;

	return (
		<div className={styles.messagesWrapper} ref={messagesWrapperRef}>
			{canAttachFiles && <UploadManager channel={channel} />}
			{topBar}
			<div className={styles.scrollerContainer}>
				<Scroller
					fade={false}
					scrollbar="regular"
					hideThumbWhenWindowBlurred
					ref={scrollManager.ref}
					onScroll={scrollManager.handleScroll}
					onResize={scrollManager.handleResize}
					key={`scroller-${channel.id}`}
				>
					<div className={styles.scrollerContent}>
						<div className={styles.scrollerInner} ref={scrollerInnerRef} onCopy={onCopySelectedMessages}>
							{scrollerInner}
						</div>
					</div>
				</Scroller>
			</div>
			{loadErrorBar ?? jumpToPresentBar}
		</div>
	);
});

const getBottomBarStyle = (background: string): React.CSSProperties => ({
	borderRadius: '0.5rem 0.5rem 0 0',
	bottom: '-6px',
	background,
	paddingBottom: '6px',
	paddingTop: 0,
	top: 'auto',
});

const JumpToPresentBar = observer(function JumpToPresentBar({
	loadingMore,
	jumpedToPresent,
	onJumpToPresent,
}: {
	loadingMore: boolean;
	jumpedToPresent: boolean;
	onJumpToPresent: () => void;
}) {
	const {t} = useLingui();
	const isJumping = loadingMore && jumpedToPresent;

	return (
		<button
			type="button"
			className={styles.newMessagesBar}
			style={{
				...getBottomBarStyle('var(--background-secondary-alt)'),
				cursor: isJumping ? 'wait' : 'pointer',
			}}
			onClick={onJumpToPresent}
			disabled={isJumping}
			aria-busy={isJumping}
		>
			<span className={styles.newMessagesBarText}>{t`You're viewing older messages`}</span>
			<span className={styles.newMessagesBarAction}>{isJumping ? <Spinner size="small" /> : t`Jump to Present`}</span>
		</button>
	);
});

function LoadErrorBar({loading, onRetry}: {loading: boolean; onRetry: () => void}) {
	const {t} = useLingui();

	return (
		<button
			type="button"
			aria-busy={loading}
			className={styles.newMessagesBar}
			disabled={loading}
			onClick={onRetry}
			style={{
				...getBottomBarStyle('var(--status-danger)'),
				cursor: loading ? 'wait' : 'pointer',
			}}
		>
			<span className={styles.newMessagesBarText}>{t`Messages failed to load`}</span>
			<span className={styles.newMessagesBarAction}>{loading ? <Spinner size="small" /> : t`Try again`}</span>
		</button>
	);
}
