// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {usePlaceholderSpecs} from '@app/features/app/components/skeleton/PlaceholderSpecs';
import {ScrollFillerSkeleton} from '@app/features/app/components/skeleton/ScrollFillerSkeleton';
import {
	reportSkeletonMessagePresentation,
	resolveDefaultSkeletonChatViewportHeightPx,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {measureSkeletonHeightPx, useSkeletonLayoutReport} from '@app/features/app/hooks/useSkeletonLayoutMemoryCapture';
import {renderChannelStream} from '@app/features/channel/components/ChannelMessageStream';
import styles from '@app/features/channel/components/ChannelMessages.module.css';
import {ChannelWelcomeSection} from '@app/features/channel/components/ChannelWelcomeSection';
import {CollapsedMessageVisibilityProvider} from '@app/features/channel/components/CollapsedMessageVisibilityContext';
import {NewMessagesBar} from '@app/features/channel/components/NewMessagesBar';
import {UploadManager} from '@app/features/channel/components/UploadManager';
import type {Channel} from '@app/features/channel/models/Channel';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import GuildVerification from '@app/features/guild/state/GuildVerification';
import {TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {useMessageListKeyboardNavigation} from '@app/features/messaging/hooks/useMessageListKeyboardNavigation';
import {useMessageSelectionCopyForMessageGetter} from '@app/features/messaging/hooks/useMessageSelectionCopy';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {ChannelMessages} from '@app/features/messaging/state/ChannelMessages';
import MessageEdit from '@app/features/messaging/state/MessageEdit';
import MessageFocus from '@app/features/messaging/state/MessageFocus';
import MessagesState from '@app/features/messaging/state/MessagingMessages';
import {
	type ChannelStreamItem,
	createChannelStream,
	getCollapsedMessageGroupKey,
} from '@app/features/messaging/utils/MessageGroupingUtils';
import LocalUserSpamOverride from '@app/features/moderation/state/LocalUserSpamOverride';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import Permission from '@app/features/permissions/state/Permission';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import {useScrollManager} from '@app/features/platform/utils/ScrollManager';
import * as ReadStateCommands from '@app/features/read_state/commands/ReadStateCommands';
import ReadStates from '@app/features/read_state/state/ReadStates';
import {shouldAutoAck} from '@app/features/read_state/utils/AutoAckPredicate';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {Scroller} from '@app/features/ui/components/Scroller';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import MediaViewer from '@app/features/ui/state/MediaViewer';
import Modal from '@app/features/ui/state/Modal';
import type {User} from '@app/features/user/models/User';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import Window from '@app/features/window/state/Window';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MAX_MESSAGES_PER_CHANNEL} from '@fluxer/constants/src/LimitConstants';
import {extractTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {runInAction} from 'mobx';
import {observer, useLocalObservable} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef} from 'react';

const MESSAGE_LIST_FOR_DESCRIPTOR = msg({
	message: 'Message list for {channelName}',
	comment: 'Label in the channel and chat messages. Preserve {channelName}; it is inserted by code.',
});
const MESSAGE_LIST_DESCRIPTOR = msg({
	message: 'Message list',
	comment: 'Short label in the channel and chat messages. Keep it concise.',
});
const YOU_RE_VIEWING_OLDER_MESSAGES_DESCRIPTOR = msg({
	message: "You're viewing older messages",
	comment: 'Label in the channel and chat messages.',
});
const JUMP_TO_PRESENT_DESCRIPTOR = msg({
	message: 'Jump to present',
	comment: 'Short label in the channel and chat messages. Keep it concise.',
});
const MESSAGES_FAILED_TO_LOAD_DESCRIPTOR = msg({
	message: 'Messages failed to load',
	comment: 'Error message in the channel and chat messages.',
});

function checkPermissions(channel: Channel) {
	const canSendMessages = Permission.can(Permissions.SEND_MESSAGES, channel);
	const passesVerification = channel.isPrivate() || GuildVerification.canAccessGuild(channel.guildId || '');
	const canChat = channel.isPrivate() || (canSendMessages && passesVerification);
	const canAttachFiles = channel.isPrivate() ? canChat : canChat && Permission.can(Permissions.ATTACH_FILES, channel);
	const canManageMessages = Permission.can(Permissions.MANAGE_MESSAGES, channel);
	return {canSendMessages, canChat, canAttachFiles, canManageMessages};
}

interface MessagesStateSnapshot {
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
	currentUser: User | undefined;
	isEstimated: boolean;
	isManualAck: boolean;
}

interface MessagesProps {
	channel: Channel;
	onBottomBarVisibilityChange?: (visible: boolean) => void;
	allowAutoAck?: boolean;
}

type MessagesWrapperStyle = React.CSSProperties & {'--message-group-spacing': string};

const readFromState = (channelId: string): MessagesStateSnapshot => {
	const messages = MessagesState.getMessages(channelId);
	const messageDisplayCompact = UserSettings.getMessageDisplayCompact();
	return {
		unreadCount: ReadStates.getUnreadCount(channelId),
		oldestUnreadMessageId: ReadStates.getOldestUnreadMessageId(channelId),
		visualUnreadMessageId: ReadStates.getVisualUnreadMessageId(channelId),
		ackMessageId: ReadStates.ackMessageId(channelId),
		lastReadStateMessageId: ReadStates.lastMessageId(channelId),
		messages,
		messageVersion: messages.version,
		revealedMessageId: messages.revealedMessageId,
		permissionVersion: Permission.version,
		messageGroupSpacing: Accessibility.getMessageGroupSpacingValue(messageDisplayCompact),
		fontSize: Accessibility.fontSize,
		messageDisplayCompact,
		editingMessageId: MessageEdit.getEditingMessageId(channelId),
		currentUser: Users.currentUser ?? undefined,
		isEstimated: ReadStates.getIfExists(channelId)?.estimated ?? false,
		isManualAck: ReadStates.getIfExists(channelId)?.isManualAck ?? false,
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

export const Messages = observer(function Messages({
	channel,
	onBottomBarVisibilityChange,
	allowAutoAck = true,
}: MessagesProps) {
	const {i18n} = useLingui();
	const scrollerInnerRef = useRef<HTMLDivElement | null>(null);
	const scrollerContainerRef = useRef<HTMLDivElement | null>(null);
	const lastStateSnapshotRef = useRef<MessagesStateSnapshot | null>(null);
	const recoveryFetchChannelIdRef = useRef<string | null>(null);
	interface MessageState extends MessagesStateSnapshot {
		highlightedMessageId: string | null;
		isAtBottom: boolean;
	}
	const state = useLocalObservable<MessageState>(() => {
		const initial = readFromState(channel.id);
		lastStateSnapshotRef.current = initial;
		return {
			...initial,
			highlightedMessageId: null,
			isAtBottom: false,
		};
	});
	const windowId = Window.windowId;
	const isWindowFocused = Window.isFocused();
	const isModalOpen = Modal.hasModalOpen();
	const isGatewayConnected = GatewayConnection.isConnected;
	const selectedChannelId = SelectedChannel.currentChannelId;
	const placeholderSpecs = usePlaceholderSpecs({
		compact: state.messageDisplayCompact,
		compactAvatarsVisible: Accessibility.showUserAvatarsInCompactMode,
		groupSpacing: state.messageGroupSpacing,
		viewportHeightPx: resolveDefaultSkeletonChatViewportHeightPx(),
		seedKey: channel.id,
	});
	const safeMessages = state.messages ?? MessagesState.getMessages(channel.id);
	const canAutoAck = shouldAutoAck({
		channelActive: allowAutoAck,
		windowFocused: isWindowFocused,
		atBottom: true,
		textChatVisible: true,
		manualAck: state.isManualAck,
		blockingModalOpen: isModalOpen || MediaViewer.isOpen,
	});
	const jumpHighlightTimeoutRef = useRef<number | null>(null);
	const lastJumpSequenceKeyRef = useRef<string | null>(null);
	const handleJumpHighlight = useCallback(
		(targetMessageId: string | null, jumpSequenceId: number) => {
			const jumpSequenceKey = `${channel.id}:${jumpSequenceId}`;
			if (jumpSequenceKey === lastJumpSequenceKeyRef.current) return;
			lastJumpSequenceKeyRef.current = jumpSequenceKey;
			if (jumpHighlightTimeoutRef.current != null) {
				clearTimeout(jumpHighlightTimeoutRef.current);
				jumpHighlightTimeoutRef.current = null;
			}
			runInAction(() => {
				state.highlightedMessageId = targetMessageId;
			});
			if (!targetMessageId) return;
			jumpHighlightTimeoutRef.current = window.setTimeout(() => {
				runInAction(() => {
					if (state.highlightedMessageId === targetMessageId) {
						state.highlightedMessageId = null;
					}
				});
				jumpHighlightTimeoutRef.current = null;
			}, 2000);
		},
		[channel.id, state],
	);
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
		handleJumpHighlight,
	});
	useEffect(() => {
		ChannelMessages.retainChannel(channel.id);
		return () => {
			ChannelMessages.releaseRetainedChannel(channel.id);
		};
	}, [channel.id]);
	const updateFromState = useCallback(() => {
		const snapshot = readFromState(channel.id);
		const previous = lastStateSnapshotRef.current;
		if (previous && shallowEqual(previous, snapshot)) return;
		runInAction(() => {
			Object.assign(state, snapshot);
		});
		lastStateSnapshotRef.current = snapshot;
	}, [channel.id, state]);
	const onMessageEdit = useCallback(
		(targetNode: HTMLElement) => {
			const scrollerNode = scrollManager.ref.current?.getScrollerNode();
			if (!scrollerNode) return;
			if (scrollManager.pinIsAtBottomNow()) {
				return;
			}
			if (KeyboardMode.keyboardModeEnabled) {
				const focusedMessageId = MessageFocus.focusedMessageId;
				const focusedChannelId = MessageFocus.focusedChannelId;
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
				scrollManager.scrollHandle();
			}
		},
		[scrollManager, channel.id],
	);
	const onReveal = useCallback(
		(messageId: string | null) => {
			MessageCommands.revealMessage(channel.id, messageId);
		},
		[channel.id],
	);
	const onScrollToPresent = useCallback(() => {
		if (state.messages?.hasMoreAfter) {
			MessageCommands.jumpToPresent(channel.id, MAX_MESSAGES_PER_CHANNEL);
		} else {
			scrollManager.scrollSetToBottom(false);
		}
	}, [channel.id, state.messages?.hasMoreAfter, scrollManager]);
	const onMessageSent = useCallback(
		(payload?: unknown) => {
			const data = payload as {channelId?: string} | undefined;
			if (data?.channelId && data.channelId !== channel.id) return;
			if (!Accessibility.scrollToBottomOnMessageSend) return;
			if (state.messages?.hasMoreAfter) return;
			window.requestAnimationFrame(() => {
				scrollManager.scrollSetToBottom(false);
			});
		},
		[channel.id, state.messages?.hasMoreAfter, scrollManager],
	);
	const onJumpToOldestUnread = useCallback(() => {
		const messageId = state.oldestUnreadMessageId ?? state.ackMessageId;
		if (messageId == null) {
			return;
		}
		MessageCommands.jumpToMessage({channelId: channel.id, messageId, flash: true});
	}, [channel.id, state.ackMessageId, state.oldestUnreadMessageId]);
	const onScrollToPresentAndAck = useCallback(() => {
		if (state.messages?.hasMoreAfter) {
			MessageCommands.jumpToPresent(channel.id, MAX_MESSAGES_PER_CHANNEL);
		} else {
			scrollManager.scrollSetToBottom(false);
		}
		if (state.visualUnreadMessageId != null) {
			ReadStateCommands.clearStickyUnread(channel.id);
		}
		if (ReadStates.hasUnread(channel.id)) {
			ReadStateCommands.ack(channel.id, true, false);
		}
	}, [channel.id, state.messages?.hasMoreAfter, state.visualUnreadMessageId, scrollManager]);
	const onEscapePressed = useCallback(
		(payload?: unknown) => {
			const data = payload as {channelId?: string} | undefined;
			if (data?.channelId && data.channelId !== channel.id) return;
			if (scrollManager.jumpReturnToOrigin()) {
				return;
			}
			onScrollToPresentAndAck();
		},
		[channel.id, onScrollToPresentAndAck, scrollManager],
	);
	const onRetryLoadMessages = useCallback(() => {
		void MessageCommands.fetchMessages(channel.id, null, null, MAX_MESSAGES_PER_CHANNEL);
	}, [channel.id]);
	const getSelectionCopyMessage = useCallback(
		(messageId: string) => MessagesState.getMessage(channel.id, messageId),
		[channel.id],
	);
	const onCopySelectedMessages = useMessageSelectionCopyForMessageGetter<HTMLDivElement>(getSelectionCopyMessage);
	useEffect(() => {
		const storeUnsubs = [
			MessagesState.subscribe(updateFromState),
			ReadStates.subscribe(updateFromState),
			Users.subscribe(updateFromState),
			Permission.subscribe(updateFromState),
			Accessibility.subscribe(updateFromState),
			UserSettings.subscribe(updateFromState),
			MessageEdit.subscribe(updateFromState),
		];
		const onForceJumpToPresent = () => {
			MessageCommands.jumpToPresent(channel.id, MAX_MESSAGES_PER_CHANNEL);
		};
		const onScrollPageUp = () => scrollManager.scrollPageUp(true);
		const onScrollPageDown = () => scrollManager.scrollPageDown(true);
		const onLayoutResized = (payload?: unknown) => {
			const data = payload as {channelId?: string} | undefined;
			if (data?.channelId && data.channelId !== channel.id) return;
			scrollManager.layoutHandleResized();
		};
		const onFocusBottommostMessage = (payload?: unknown) => {
			const data = (payload ?? {}) as {channelId?: string};
			if (!data.channelId || data.channelId !== channel.id) return;
			const scroller = scrollManager.ref.current?.getScrollerNode();
			const scrollerInner = scrollerInnerRef.current;
			if (!scroller || !scrollerInner) return;
			const messageElements = scrollerInner.querySelectorAll<HTMLElement>('[data-message-id]');
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
					scrollManager.focusOnMessage(messageId);
				}
			}
		};
		const dispatchUnsubs = [
			ComponentDispatch.subscribe('SCROLLTO_PRESENT', onScrollToPresent),
			ComponentDispatch.subscribe('MESSAGE_SENT', onMessageSent),
			ComponentDispatch.subscribe('FORCE_JUMP_TO_PRESENT', onForceJumpToPresent),
			ComponentDispatch.subscribe('ESCAPE_PRESSED', onEscapePressed),
			ComponentDispatch.subscribe('SCROLL_PAGE_UP', onScrollPageUp),
			ComponentDispatch.subscribe('SCROLL_PAGE_DOWN', onScrollPageDown),
			ComponentDispatch.subscribe('LAYOUT_RESIZED', onLayoutResized),
			ComponentDispatch.subscribe('FOCUS_BOTTOMMOST_MESSAGE', onFocusBottommostMessage),
		];
		updateFromState();
		return () => {
			storeUnsubs.forEach((u) => u());
			dispatchUnsubs.forEach((u) => u());
		};
	}, [channel.id, updateFromState, onScrollToPresent, onMessageSent, onEscapePressed, scrollManager]);
	useEffect(() => {
		const editingMessageId = state.editingMessageId;
		if (editingMessageId) {
			scrollManager.editEnter();
		} else {
			scrollManager.editExit();
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
		void MessageCommands.fetchMessages(channel.id, null, null, MAX_MESSAGES_PER_CHANNEL).finally(() => {
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
			scrollManager.focusOnMessage(messageId);
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
			if (scrollManager.jumpReturnToOrigin()) {
				return;
			}
			scrollManager.jumpCancel();
			ComponentDispatch.dispatch('FOCUS_TEXTAREA', {channelId: channel.id});
		},
		allowWhenInactive: true,
	});
	useEffect(() => {
		return () => {
			if (jumpHighlightTimeoutRef.current != null) {
				clearTimeout(jumpHighlightTimeoutRef.current);
				jumpHighlightTimeoutRef.current = null;
			}
		};
	}, []);
	useEffect(() => {
		if (!canAutoAck || !state.isAtBottom || !state.messages?.ready) return;
		if (ReadStates.hasUnread(channel.id)) {
			ReadStateCommands.ackWithStickyUnread(channel.id);
		}
	}, [canAutoAck, state.isAtBottom, state.messages?.ready, channel.id]);
	useEffect(() => {
		return () => {
			const readState = ReadStates.getIfExists(channel.id);
			if (readState?.isManualAck) {
				ReadStateCommands.clearManualAck(channel.id);
			}
			ReadStateCommands.clearStickyUnread(channel.id);
		};
	}, [channel.id]);
	const spammerOverrideVersion = LocalUserSpamOverride.version;
	const channelStream = useMemo<Array<ChannelStreamItem>>(() => {
		if (!state.messages?.ready) return [];
		return createChannelStream({
			channel,
			messages: state.messages,
			oldestUnreadMessageId: state.visualUnreadMessageId,
			treatSpam: true,
		});
	}, [channel, state.messages?.ready, state.messageVersion, state.visualUnreadMessageId, spammerOverrideVersion]);
	useEffect(() => {
		const messages = state.messages;
		if (!messages?.ready) {
			return;
		}
		void MessageCommands.ensureMembersForMessages(messages.toArray());
	}, [channel.id, state.messages?.ready, state.messageVersion]);
	const immediateHighlightedMessageId = (() => {
		const messages = state.messages;
		if (!messages?.ready || !messages.jumped || !messages.jumpFlash || !messages.jumpTargetId) {
			return null;
		}
		return messages.jumpTargetId !== channel.id ? messages.jumpTargetId : null;
	})();
	const highlightedMessageId = immediateHighlightedMessageId ?? state.highlightedMessageId;
	const collapsedMessageVisibility = useMemo(
		() => ({
			isMessageRevealed: (message: Message) => {
				if (!state.revealedMessageId || !state.messages || message.channelId !== channel.id) {
					return false;
				}
				return (
					getCollapsedMessageGroupKey({
						channel,
						messages: state.messages,
						messageId: message.id,
						treatSpam: true,
					}) === state.revealedMessageId
				);
			},
		}),
		[channel, state.messages, state.revealedMessageId, state.messageVersion, spammerOverrideVersion],
	);
	const {canChat, canAttachFiles} = useMemo(
		() => checkPermissions(channel),
		[channel.id, channel.guildId, state.permissionVersion],
	);
	const streamMarkup = useMemo(() => {
		if (!state.messages?.ready) return null;
		return renderChannelStream({
			channelStream,
			messages: state.messages,
			channel,
			highlightedMessageId,
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
		highlightedMessageId,
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
			data-flx="channel.messages.jump-to-present-bar"
		/>
	) : null;
	const loadErrorBar = hasLoadErrorBar ? (
		<LoadErrorBar
			loading={state.messages.loadingMore}
			onRetry={onRetryLoadMessages}
			data-flx="channel.messages.load-error-bar"
		/>
	) : null;
	const showNewMessagesBar = Boolean(state.messages?.ready && state.unreadCount > 0);
	const unreadTimestampMessageId = state.oldestUnreadMessageId ?? state.ackMessageId;
	const topBar = showNewMessagesBar ? (
		<NewMessagesBar
			unreadCount={state.unreadCount}
			oldestUnreadTimestamp={unreadTimestampMessageId ? extractTimestamp(unreadTimestampMessageId) : 0}
			isEstimated={state.isEstimated}
			onJumpToOldestUnread={onJumpToOldestUnread}
			onJumpToNewMessages={onScrollToPresentAndAck}
			data-flx="channel.messages.new-messages-bar"
		/>
	) : null;
	const readyMessages = state.messages?.ready ? state.messages : null;
	const messagesWrapperStyle = useMemo<MessagesWrapperStyle>(
		() => ({
			'--message-group-spacing': remFromPx(state.messageGroupSpacing),
		}),
		[state.messageGroupSpacing],
	);
	const compactAvatarsVisible = Accessibility.showUserAvatarsInCompactMode;
	const messageGutter = Accessibility.messageGutter;
	useSkeletonLayoutReport(
		() =>
			reportSkeletonMessagePresentation({
				compact: state.messageDisplayCompact,
				messageGutterPx: messageGutter,
				fontSizePx: state.fontSize,
				groupSpacingPx: state.messageGroupSpacing,
				compactAvatarsVisible,
				viewportHeightPx: measureSkeletonHeightPx(scrollerContainerRef.current),
			}),
		`${state.messageDisplayCompact}|${messageGutter}|${state.fontSize}|${state.messageGroupSpacing}|${compactAvatarsVisible}|${channel.id}|${state.messageVersion}`,
	);
	const messageListLabel = channel.name
		? i18n._(MESSAGE_LIST_FOR_DESCRIPTOR, {channelName: channel.name})
		: i18n._(MESSAGE_LIST_DESCRIPTOR);
	const messageListLiveMode = Accessibility.screenReaderAnnounceNewMessages && state.isAtBottom ? 'polite' : 'off';
	const scrollerInner = readyMessages ? (
		<>
			{!readyMessages.hasMoreBefore && (
				<ChannelWelcomeSection channel={channel} data-flx="channel.messages.channel-welcome-section" />
			)}
			{readyMessages.hasMoreBefore && (
				<>
					<div className={styles.placeholderSpacer} data-flx="channel.messages.placeholder-spacer" />
					<ScrollFillerSkeleton data-flx="channel.messages.scroll-filler-skeleton" {...placeholderSpecs} />
				</>
			)}
			{streamMarkup}
			{readyMessages.hasMoreAfter && (
				<ScrollFillerSkeleton data-flx="channel.messages.scroll-filler-skeleton--2" {...placeholderSpecs} />
			)}
			<div className={styles.scrollerSpacer} data-flx="channel.messages.scroller-spacer" />
		</>
	) : null;
	return (
		<div className={styles.messagesWrapper} style={messagesWrapperStyle} data-flx="channel.messages.messages-wrapper">
			<UploadManager
				channel={channel}
				canAttachFiles={canAttachFiles}
				canSendMessages={canChat}
				data-flx="channel.messages.upload-manager"
			/>
			{topBar}
			<div
				className={styles.scrollerContainer}
				ref={scrollerContainerRef}
				data-flx="channel.messages.scroller-container"
			>
				<Scroller
					fade={false}
					scrollbar="regular"
					hideThumbWhenWindowBlurred
					ref={scrollManager.ref}
					onScroll={scrollManager.scrollHandle}
					onScrollIntent={scrollManager.scrollHandleUserIntent}
					onResize={scrollManager.scrollHandleResize}
					key={`scroller-${channel.id}`}
					data-flx="channel.messages.scroller"
				>
					<div className={styles.scrollerContent} data-flx="channel.messages.scroller-content">
						<div
							className={styles.scrollerInner}
							ref={scrollerInnerRef}
							onCopy={onCopySelectedMessages}
							role="log"
							data-message-selection-root="true"
							aria-label={messageListLabel}
							aria-live={messageListLiveMode}
							aria-relevant="additions text"
							aria-atomic="false"
							aria-busy={state.messages?.loadingMore ? true : undefined}
							data-flx="channel.messages.scroller-inner"
						>
							<CollapsedMessageVisibilityProvider
								value={collapsedMessageVisibility}
								data-flx="channel.messages.collapsed-message-visibility-provider"
							>
								{scrollerInner}
							</CollapsedMessageVisibilityProvider>
						</div>
					</div>
				</Scroller>
			</div>
			{loadErrorBar !== null ? loadErrorBar : jumpToPresentBar}
		</div>
	);
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
	const {i18n} = useLingui();
	const jumpIsActiveNow = loadingMore && jumpedToPresent;
	return (
		<div
			className={[styles.newMessagesBar, styles.messageBottomPill, styles.jumpToPresentBar].join(' ')}
			aria-busy={jumpIsActiveNow}
			data-flx="channel.messages.jump-to-present-bar"
		>
			<span className={styles.newMessagesBarText} data-flx="channel.messages.jump-to-present-bar.new-messages-bar-text">
				{i18n._(YOU_RE_VIEWING_OLDER_MESSAGES_DESCRIPTOR)}
			</span>
			<Button
				variant="primary"
				compact
				fitContent
				className={styles.newMessagesBarAction}
				onClick={onJumpToPresent}
				submitting={jumpIsActiveNow}
				disabled={jumpIsActiveNow}
				data-flx="channel.messages.jump-to-present-bar.new-messages-bar-action"
			>
				{i18n._(JUMP_TO_PRESENT_DESCRIPTOR)}
			</Button>
		</div>
	);
});

function LoadErrorBar({loading, onRetry}: {loading: boolean; onRetry: () => void}) {
	const {i18n} = useLingui();
	return (
		<div
			aria-busy={loading}
			className={[styles.newMessagesBar, styles.messageBottomPill, styles.loadErrorBar].join(' ')}
			data-flx="channel.messages.load-error-bar"
		>
			<span className={styles.newMessagesBarText} data-flx="channel.messages.load-error-bar.new-messages-bar-text">
				{i18n._(MESSAGES_FAILED_TO_LOAD_DESCRIPTOR)}
			</span>
			<Button
				variant="primary"
				compact
				fitContent
				className={styles.newMessagesBarAction}
				disabled={loading}
				submitting={loading}
				onClick={onRetry}
				data-flx="channel.messages.load-error-bar.new-messages-bar-action"
			>
				{i18n._(TRY_AGAIN_DESCRIPTOR)}
			</Button>
		</div>
	);
}
