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
import {runInAction} from 'mobx';
import {observer, useLocalObservable} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useMemo, useRef} from 'react';
import * as ChannelPinsActionCreators from '~/actions/ChannelPinsActionCreators';
import * as MessageActionCreators from '~/actions/MessageActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ReadStateActionCreators from '~/actions/ReadStateActionCreators';
import * as SavedMessageActionCreators from '~/actions/SavedMessageActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import {MAX_MESSAGES_PER_CHANNEL, MessageTypes, Permissions} from '~/Constants';
import {BlockedMessageGroups} from '~/components/channel/BlockedMessageGroups';
import {ChannelWelcomeSection} from '~/components/channel/ChannelWelcomeSection';
import {Divider} from '~/components/channel/Divider';
import {MessageGroup} from '~/components/channel/MessageGroup';
import {triggerAddReaction} from '~/components/channel/messageActionUtils';
import ScrollFillerSkeleton from '~/components/channel/ScrollFillerSkeleton';
import {UploadManager} from '~/components/channel/UploadManager';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {ForwardModal} from '~/components/modals/ForwardModal';
import {Button} from '~/components/uikit/Button/Button';
import {Scroller} from '~/components/uikit/Scroller';
import {Spinner} from '~/components/uikit/Spinner';
import type {ChannelMessages} from '~/lib/ChannelMessages';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import {usePlaceholderSpecs} from '~/lib/PlaceholderSpecs';
import {useScrollManager} from '~/lib/ScrollManager';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {MessageRecord} from '~/records/MessageRecord';
import AccessibilityStore from '~/stores/AccessibilityStore';
import GuildVerificationStore from '~/stores/GuildVerificationStore';
import KeyboardModeStore from '~/stores/KeyboardModeStore';
import MessageEditStore from '~/stores/MessageEditStore';
import MessageStore from '~/stores/MessageStore';
import ModalStore from '~/stores/ModalStore';
import PermissionStore from '~/stores/PermissionStore';
import ReadStateStore from '~/stores/ReadStateStore';
import SavedMessagesStore from '~/stores/SavedMessagesStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import UserStore from '~/stores/UserStore';
import WindowStore from '~/stores/WindowStore';
import {type ChannelStreamItem, ChannelStreamType, createChannelStream} from '~/utils/MessageGroupingUtils';
import {buildMessageJumpLink} from '~/utils/messageLinkUtils';
import SnowflakeUtils from '~/utils/SnowflakeUtil';
import styles from './Messages.module.css';
import {NewMessagesBar} from './NewMessagesBar';

const isSystemMessage = (message: MessageRecord | undefined): boolean => {
	if (!message) return false;
	return message.type !== MessageTypes.DEFAULT && message.type !== MessageTypes.REPLY;
};

type MessageGroupKind = 'system' | 'regular';

const getMessageGroupKind = (message: MessageRecord | undefined): MessageGroupKind => {
	return isSystemMessage(message) ? 'system' : 'regular';
};

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

const readFromStores = (channelId: string) => {
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
		showMessageDividers: AccessibilityStore.showMessageDividers,
		messageGroupSpacing: AccessibilityStore.messageGroupSpacingValue,
		fontSize: AccessibilityStore.fontSize,
		messageDisplayCompact: UserSettingsStore.getMessageDisplayCompact(),
		editingMessageId: MessageEditStore.getEditingMessageId(channelId),
		currentUser: UserStore.currentUser ?? undefined,
		isEstimated: ReadStateStore.getIfExists(channelId)?.estimated ?? false,
		isManualAck: ReadStateStore.getIfExists(channelId)?.isManualAck ?? false,
	};
};

function shallowEqual<T extends Record<string, unknown>>(a: T, b: T): boolean {
	const aKeys = Object.keys(a);
	if (aKeys.length !== Object.keys(b).length) return false;
	for (const key of aKeys) {
		if (a[key] !== b[key]) return false;
	}
	return true;
}

export const Messages = observer(function Messages({channel}: {channel: ChannelRecord}) {
	const {t, i18n} = useLingui();

	const messagesWrapperRef = useRef<HTMLDivElement | null>(null);
	const lastStoreSnapshotRef = useRef<ReturnType<typeof readFromStores> | null>(null);

	const state = useLocalObservable(() => {
		const initial = readFromStores(channel.id);
		lastStoreSnapshotRef.current = initial;
		return {
			...initial,
			highlightedMessageId: null as string | null,
			isAtBottom: false,
		};
	});

	const windowId = WindowStore.windowId;
	const isWindowFocused = WindowStore.isFocused();
	const isModalOpen = ModalStore.hasModalOpen();
	const keyboardModeEnabled = KeyboardModeStore.keyboardModeEnabled;
	const placeholderSpecs = usePlaceholderSpecs(state.messageDisplayCompact, state.messageGroupSpacing, state.fontSize);
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

	useEffect(() => {
		const node = messagesWrapperRef.current;
		if (node) {
			node.style.setProperty('--message-group-spacing', `${state.messageGroupSpacing}px`);
		}
	}, [state.messageGroupSpacing]);

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
				const focusedMessageId = (document.activeElement as HTMLElement)
					?.closest?.('[data-message-id]')
					?.getAttribute('data-message-id');
				const editedMessageId = targetNode.getAttribute('data-message-id');
				if (focusedMessageId && focusedMessageId === editedMessageId) {
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
			}
		},
		[scrollManager],
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

			if (scrollManager.isPinned()) {
				scrollManager.handleScroll();
			}
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
		if (!keyboardModeEnabled) return;

		const messageNodesCache = {nodes: [] as Array<HTMLElement>, ts: 0};

		const getMessageElements = (): Array<HTMLElement> => {
			const now = Date.now();
			if (messageNodesCache.nodes.length && now - messageNodesCache.ts < 120) {
				return messageNodesCache.nodes;
			}
			const doc = scrollManager.ref.current?.getScrollerNode()?.ownerDocument ?? document;
			messageNodesCache.nodes = Array.from(
				doc?.querySelectorAll<HTMLElement>(`[data-channel-id="${channel.id}"][data-message-id]`) ?? [],
			);
			messageNodesCache.ts = now;
			return messageNodesCache.nodes;
		};

		const getFocusedMessageId = (target: EventTarget | null): string | null => {
			const el = (target as HTMLElement | null)?.closest?.('[data-message-id][data-channel-id]');
			return (el as HTMLElement | null)?.dataset?.messageId ?? null;
		};

		const focusByDelta = (delta: number) => {
			const nodes = getMessageElements();
			if (!nodes.length) return;
			const activeId = getFocusedMessageId(document.activeElement);
			let idx = nodes.findIndex((n) => n.dataset.messageId === activeId);
			if (idx === -1) {
				idx = delta > 0 ? -1 : nodes.length;
			}
			const nextIdx = idx + delta;

			if (nextIdx < 0) {
				if (state.messages?.hasMoreBefore && !state.messages.loadingMore) {
					scrollManager.loadMoreForKeyboardNavigation(false);
				}
				return;
			}
			if (nextIdx >= nodes.length) {
				if (state.messages?.hasMoreAfter && !state.messages.loadingMore) {
					scrollManager.loadMoreForKeyboardNavigation(true);
				}
				return;
			}

			const nextId = nodes[nextIdx]?.dataset?.messageId;
			if (nextId) {
				scrollManager.focusMessage(nextId);
			}
		};

		const handleMessageAction = (event: KeyboardEvent) => {
			if (!keyboardModeEnabled) return;

			const activeElement = document.activeElement;
			if (activeElement instanceof HTMLTextAreaElement || activeElement instanceof HTMLInputElement) {
				return;
			}

			const targetMessageId = getFocusedMessageId(event.target);
			if (!targetMessageId) return;

			const message = MessageStore.getMessage(channel.id, targetMessageId);
			const key = event.key;

			switch (true) {
				case key === 'ArrowUp': {
					event.preventDefault();
					focusByDelta(-1);
					return;
				}
				case key === 'ArrowDown': {
					event.preventDefault();
					focusByDelta(1);
					return;
				}
				case key === 'e': {
					if (message?.isCurrentUserAuthor()) {
						MessageActionCreators.startEdit(channel.id, targetMessageId, message.content ?? '');
						event.preventDefault();
					}
					return;
				}
				case key === 'Backspace' || key === 'Delete': {
					if (message) {
						event.preventDefault();
						ModalActionCreators.push(
							modal(() => (
								<ConfirmModal
									title={t`Delete Message`}
									description={t`This will create a rift in the space-time continuum and cannot be undone.`}
									message={message}
									primaryText={t`Delete`}
									onPrimary={() => MessageActionCreators.remove(channel.id, targetMessageId)}
								/>
							)),
						);
					}
					return;
				}
				case key === 'p': {
					if (message) {
						if (message.pinned) {
							void ChannelPinsActionCreators.unpin(channel.id, targetMessageId);
						} else {
							void ChannelPinsActionCreators.pin(channel.id, targetMessageId);
						}
						event.preventDefault();
					}
					return;
				}
				case key === '+': {
					triggerAddReaction(targetMessageId);
					event.preventDefault();
					return;
				}
				case key === 'r' && !event.shiftKey: {
					MessageActionCreators.startReply(channel.id, targetMessageId, true);
					event.preventDefault();
					return;
				}
				case key === 'R' && event.shiftKey: {
					MessageActionCreators.startReply(channel.id, targetMessageId, false);
					event.preventDefault();
					return;
				}
				case key === 'f': {
					event.preventDefault();
					if (!message) return;
					ModalActionCreators.push(modal(() => <ForwardModal message={message} />));
					return;
				}
				case (key === 'c' || key === 'C') && (event.metaKey || event.ctrlKey): {
					if (message?.content) {
						void TextCopyActionCreators.copy(i18n, message.content, false);
						event.preventDefault();
					}
					return;
				}
				case key === 'Enter' && event.altKey: {
					ReadStateActionCreators.markAsUnread(channel.id, targetMessageId).catch(() => {});
					event.preventDefault();
					return;
				}
				case key === 'c' && !event.metaKey && !event.ctrlKey: {
					if (message?.content) {
						void TextCopyActionCreators.copy(i18n, message.content, false);
						event.preventDefault();
					}
					return;
				}
				case key === 'b': {
					if (message) {
						const isSaved = SavedMessagesStore.isSaved(message.id);
						if (isSaved) {
							void SavedMessageActionCreators.remove(i18n, message.id);
						} else {
							void SavedMessageActionCreators.create(i18n, channel.id, message.id);
						}
						event.preventDefault();
					}
					return;
				}
				case key === 'd': {
					if (message) {
						event.preventDefault();
						ModalActionCreators.push(
							modal(() => (
								<ConfirmModal
									title={t`Delete Message`}
									description={t`This will create a rift in the space-time continuum and cannot be undone.`}
									message={message}
									primaryText={t`Delete`}
									onPrimary={() => MessageActionCreators.remove(channel.id, targetMessageId)}
								/>
							)),
						);
					}
					return;
				}
				case key === 'm': {
					ReadStateActionCreators.markAsUnread(channel.id, targetMessageId).catch(() => {});
					event.preventDefault();
					return;
				}
				case key === 'l': {
					if (message) {
						const jumpLink = buildMessageJumpLink({
							guildId: channel.guildId,
							channelId: channel.id,
							messageId: message.id,
						});
						void TextCopyActionCreators.copy(i18n, jumpLink, false);
						event.preventDefault();
					}
					return;
				}
				case key === 's': {
					if (message) {
						void MessageActionCreators.toggleSuppressEmbeds(channel.id, message.id, message.flags);
						event.preventDefault();
					}
					return;
				}
				case key === 'Escape': {
					ComponentDispatch.dispatch('FOCUS_TEXTAREA', {channelId: channel.id});
					event.preventDefault();
					return;
				}
				default:
					return;
			}
		};

		window.addEventListener('keydown', handleMessageAction, true);
		return () => {
			window.removeEventListener('keydown', handleMessageAction, true);
		};
	}, [
		channel.id,
		keyboardModeEnabled,
		scrollManager,
		state.messages?.hasMoreAfter,
		state.messages?.hasMoreBefore,
		state.messages?.loadingMore,
	]);

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
			showMessageDividers: state.showMessageDividers,
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
		state.showMessageDividers,
		state.messageGroupSpacing,
		state.revealedMessageId,
		onMessageEdit,
		onReveal,
	]);

	const jumpToPresentBar =
		state.messages?.ready && state.messages.hasMoreAfter ? (
			<JumpToPresentBar
				loadingMore={state.messages.loadingMore}
				jumpedToPresent={state.messages.jumpedToPresent}
				onJumpToPresent={onScrollToPresent}
			/>
		) : null;

	const loadErrorBar = state.messages?.error ? (
		<LoadErrorBar loading={state.messages.loadingMore} onRetry={onRetryLoadMessages} />
	) : null;

	const showNewMessagesBar = state.messages?.ready && state.unreadCount > 0;

	const topBar = showNewMessagesBar ? (
		<NewMessagesBar
			unreadCount={state.unreadCount}
			oldestUnreadTimestamp={
				state.oldestUnreadMessageId ? SnowflakeUtils.extractTimestamp(state.oldestUnreadMessageId) : 0
			}
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
						<div className={styles.scrollerInner}>{scrollerInner}</div>
					</div>
				</Scroller>
			</div>
			{loadErrorBar ?? jumpToPresentBar}
		</div>
	);
});

function renderChannelStream(props: {
	channelStream: Array<ChannelStreamItem>;
	messages: ChannelMessages;
	channel: ChannelRecord;
	highlightedMessageId: string | null;
	messageDisplayCompact: boolean;
	showMessageDividers: boolean;
	messageGroupSpacing: number;
	revealedMessageId: string | null;
	onMessageEdit: (target: HTMLElement) => void;
	onReveal: (messageId: string | null) => void;
}): Array<React.ReactNode> {
	const {
		channelStream,
		channel,
		highlightedMessageId,
		messageDisplayCompact,
		showMessageDividers,
		messageGroupSpacing,
		revealedMessageId,
		onMessageEdit,
		onReveal,
	} = props;

	const nodes: Array<React.ReactNode> = [];

	let pendingMessages: Array<MessageRecord> = [];
	let pendingStreamItems: Array<ChannelStreamItem> = [];
	let pendingGroupId: string | undefined;
	let pendingFlashKey: number | undefined;
	let lastRenderedGroupKind: MessageGroupKind | null = null;
	let spacerCounter = 0;

	const pushSpacerIfNeeded = (nextKind: MessageGroupKind, keyBase: string, nextMessageHasUnreadDivider = false) => {
		if (showMessageDividers || messageGroupSpacing <= 0 || lastRenderedGroupKind == null) return;
		if (nextMessageHasUnreadDivider) return;

		const bothSystem = lastRenderedGroupKind === 'system' && nextKind === 'system';
		const spacerClass = bothSystem ? styles.groupSpacerHalf : styles.groupSpacer;

		nodes.push(<div key={`group-spacer-${keyBase}-${spacerCounter++}`} className={spacerClass} aria-hidden="true" />);
	};

	const flushPendingGroup = () => {
		if (pendingMessages.length === 0) return;

		const groupKey = pendingGroupId ?? pendingMessages[0].id;
		const groupKind = getMessageGroupKind(pendingMessages[0]);
		const streamItemsMap = new Map(pendingStreamItems.map((item) => [(item.content as MessageRecord).id, item]));
		const firstMessageHasUnreadDivider = streamItemsMap.get(pendingMessages[0].id)?.showUnreadDividerBefore ?? false;
		pushSpacerIfNeeded(groupKind, groupKey, firstMessageHasUnreadDivider);

		const getUnreadDividerVisibility = (messageId: string, position: 'before' | 'after') => {
			if (position === 'before') {
				const streamItem = streamItemsMap.get(messageId);
				const visible = streamItem?.showUnreadDividerBefore ?? false;
				return visible;
			}
			return false;
		};

		nodes.push(
			<MessageGroup
				key={groupKey}
				messages={pendingMessages}
				channel={channel}
				onEdit={onMessageEdit}
				highlightedMessageId={highlightedMessageId}
				messageDisplayCompact={messageDisplayCompact}
				flashKey={pendingFlashKey}
				getUnreadDividerVisibility={getUnreadDividerVisibility}
				idPrefix="chat-messages"
			/>,
		);

		lastRenderedGroupKind = groupKind;
		pendingMessages = [];
		pendingStreamItems = [];
		pendingGroupId = undefined;
		pendingFlashKey = undefined;
	};

	for (let i = 0; i < channelStream.length; i++) {
		const item = channelStream[i];

		if (item.type !== ChannelStreamType.MESSAGE) {
			flushPendingGroup();

			if (item.type === ChannelStreamType.DIVIDER) {
				const isUnread = item.unreadId != null;
				const isDateDivider = !!item.content;
				const dividerSpacing = isDateDivider ? 16 : 0;
				nodes.push(
					<Divider
						key={item.contentKey || `divider-${i}`}
						spacing={dividerSpacing}
						red={isUnread}
						isDate={isDateDivider}
						id={isUnread ? 'new-messages-bar' : undefined}
					>
						{item.content as string}
					</Divider>,
				);
				lastRenderedGroupKind = null;
				continue;
			}

			if (item.type === ChannelStreamType.MESSAGE_GROUP_BLOCKED) {
				pushSpacerIfNeeded('regular', item.key ?? `blocked-${i}`);
				nodes.push(
					<BlockedMessageGroups
						key={item.key}
						revealed={item.key === revealedMessageId}
						messageGroups={item.content as Array<ChannelStreamItem>}
						onReveal={onReveal}
						compact={messageDisplayCompact}
						channel={channel}
						messageGroupSpacing={messageGroupSpacing}
					/>,
				);
				lastRenderedGroupKind = 'regular';
				continue;
			}

			continue;
		}

		const message = item.content as MessageRecord;
		const itemGroupId = item.groupId ?? message.id;

		if (pendingGroupId && pendingGroupId !== itemGroupId) {
			flushPendingGroup();
		}

		if (!pendingGroupId) {
			pendingGroupId = itemGroupId;
		}

		pendingMessages.push(message);
		pendingStreamItems.push(item);

		if (item.flashKey != null) {
			pendingFlashKey = item.flashKey;
		}
	}

	flushPendingGroup();

	return nodes;
}

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

	return (
		<div className={styles.newMessagesBar} style={getBottomBarStyle('var(--background-secondary-alt)')}>
			<span className={styles.newMessagesBarText}>{t`You're viewing older messages`}</span>
			<Button
				variant="primary"
				superCompact
				fitContent
				onClick={onJumpToPresent}
				submitting={loadingMore && jumpedToPresent}
			>
				{t`Jump to Present`}
			</Button>
		</div>
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
