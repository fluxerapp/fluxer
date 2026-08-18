// SPDX-License-Identifier: AGPL-3.0-or-later

import MessageFocus from '@app/features/messaging/state/MessageFocus';
import type {ScrollerHandle} from '@app/features/ui/components/Scroller';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import {type RefObject, useEffect} from 'react';

interface MessageListKeyboardNavigationOptions {
	containerRef?: RefObject<ScrollerHandle | HTMLElement | null>;
	channelId?: string;
	onFocusMessage?: (messageId: string) => void;
	onLoadMoreBefore?: () => void;
	onLoadMoreAfter?: () => void;
	hasMoreBefore?: boolean;
	hasMoreAfter?: boolean;
	isLoadingMore?: boolean;
	onEscape?: () => void;
	allowWhenInactive?: boolean;
}

const getScrollerNode = (value: ScrollerHandle | HTMLElement | null | undefined): HTMLElement | null => {
	if (!value) return null;
	if ('getScrollerNode' in value && typeof value.getScrollerNode === 'function') {
		return value.getScrollerNode();
	}
	if (value instanceof HTMLElement) {
		return value;
	}
	return null;
};
const isEditableTarget = (target: Element | null): boolean => {
	if (!target) return false;
	if (target instanceof HTMLTextAreaElement || target instanceof HTMLInputElement) return true;
	return target instanceof HTMLElement && target.isContentEditable;
};
const hasShortcutModifier = (event: KeyboardEvent): boolean =>
	event.altKey || event.ctrlKey || event.metaKey || event.shiftKey;
const MESSAGE_NODE_CACHE_TTL_MS = 120;

interface MessageNodesSnapshot {
	nodes: Array<HTMLElement>;
	indexById: Map<string, number>;
	root: ParentNode | null;
	selector: string;
	ts: number;
}

const EMPTY_MESSAGE_NODES_SNAPSHOT: MessageNodesSnapshot = {
	nodes: [],
	indexById: new Map(),
	root: null,
	selector: '',
	ts: 0,
};
const escapeSelectorValue = (value: string): string => {
	if (typeof CSS !== 'undefined' && typeof CSS.escape === 'function') {
		return CSS.escape(value);
	}
	return value.replace(/\\/gu, '\\\\').replace(/"/gu, '\\"');
};
const getMessageSelector = (channelId?: string, messageId?: string): string => {
	const channelSelector = channelId ? `[data-channel-id="${escapeSelectorValue(channelId)}"]` : '[data-channel-id]';
	const messageSelector = messageId ? `[data-message-id="${escapeSelectorValue(messageId)}"]` : '[data-message-id]';
	return `${channelSelector}${messageSelector}`;
};

export function useMessageListKeyboardNavigation(options: MessageListKeyboardNavigationOptions): void {
	const {
		containerRef,
		channelId,
		onFocusMessage,
		onLoadMoreBefore,
		onLoadMoreAfter,
		hasMoreBefore = false,
		hasMoreAfter = false,
		isLoadingMore = false,
		onEscape,
		allowWhenInactive = false,
	} = options;
	const keyboardModeEnabled = KeyboardMode.keyboardModeEnabled;
	useEffect(() => {
		if (!keyboardModeEnabled) return;
		let messageNodesCache: MessageNodesSnapshot = EMPTY_MESSAGE_NODES_SNAPSHOT;
		let observedRoot: ParentNode | null = null;
		let observer: MutationObserver | null = null;
		const invalidateMessageNodesCache = () => {
			messageNodesCache = {
				...messageNodesCache,
				nodes: [],
				indexById: new Map(),
				ts: 0,
			};
		};
		const observeRoot = (root: ParentNode) => {
			if (observedRoot === root || typeof MutationObserver === 'undefined') {
				return;
			}
			observer?.disconnect();
			observedRoot = root;
			const target = root instanceof Document ? (root.body ?? root.documentElement) : root;
			observer = new MutationObserver(invalidateMessageNodesCache);
			observer.observe(target, {
				childList: true,
				subtree: true,
				attributes: true,
				attributeFilter: ['data-message-id', 'data-channel-id'],
			});
		};
		const getMessageElementsSnapshot = (): MessageNodesSnapshot => {
			const now = Date.now();
			const cache = messageNodesCache;
			const container = getScrollerNode(containerRef?.current ?? null);
			if (!container && containerRef) {
				messageNodesCache = {
					...EMPTY_MESSAGE_NODES_SNAPSHOT,
					ts: now,
				};
				return messageNodesCache;
			}
			const root = container ?? document;
			const selector = getMessageSelector(channelId);
			if (cache.root === root && cache.selector === selector && now - cache.ts < MESSAGE_NODE_CACHE_TTL_MS) {
				return cache;
			}
			observeRoot(root);
			const nodes = Array.from(root.querySelectorAll<HTMLElement>(selector));
			const indexById = new Map<string, number>();
			for (let i = 0; i < nodes.length; i++) {
				const messageId = nodes[i]?.dataset.messageId;
				if (messageId) {
					indexById.set(messageId, i);
				}
			}
			messageNodesCache = {
				nodes,
				indexById,
				root,
				selector,
				ts: now,
			};
			return messageNodesCache;
		};
		const focusNode = (node: HTMLElement, messageId: string) => {
			if (onFocusMessage) {
				onFocusMessage(messageId);
				return;
			}
			node.focus({preventScroll: true});
			node.scrollIntoView({block: 'nearest', inline: 'nearest'});
		};
		const focusByDelta = (delta: number) => {
			const {nodes, indexById} = getMessageElementsSnapshot();
			if (!nodes.length) return;
			const activeId = MessageFocus.focusedMessageId ?? null;
			let idx = activeId ? (indexById.get(activeId) ?? -1) : -1;
			if (idx === -1) {
				idx = delta > 0 ? -1 : nodes.length;
			}
			const nextIdx = idx + delta;
			if (nextIdx < 0) {
				if (hasMoreBefore && onLoadMoreBefore && !isLoadingMore) {
					onLoadMoreBefore();
				}
				return;
			}
			if (nextIdx >= nodes.length) {
				if (hasMoreAfter && onLoadMoreAfter && !isLoadingMore) {
					onLoadMoreAfter();
				}
				return;
			}
			const nextNode = nodes[nextIdx];
			const nextId = nextNode?.dataset?.messageId;
			if (nextId) {
				focusNode(nextNode, nextId);
			}
		};
		const isFocusedMessageInContainer = (container: HTMLElement): boolean => {
			const focusedId = MessageFocus.focusedMessageId;
			if (!focusedId) {
				return false;
			}
			return container.querySelector(getMessageSelector(channelId, focusedId)) != null;
		};
		const canHandleInsideContainer = (container: HTMLElement): boolean => {
			const activeElement = container.ownerDocument?.activeElement ?? document.activeElement;
			if (activeElement && container.contains(activeElement)) {
				return true;
			}
			if (isFocusedMessageInContainer(container)) {
				return true;
			}
			return (
				allowWhenInactive &&
				(activeElement === document.body || activeElement === document.documentElement || !activeElement)
			);
		};
		const handleKeyDown = (event: KeyboardEvent) => {
			if (!keyboardModeEnabled) return;
			if (isEditableTarget(document.activeElement)) return;
			const delta = event.key === 'ArrowUp' ? -1 : event.key === 'ArrowDown' ? 1 : 0;
			const isNavigationKey = delta !== 0;
			if (isNavigationKey && hasShortcutModifier(event)) return;
			if (!isNavigationKey && event.key !== 'Escape') return;
			const container = getScrollerNode(containerRef?.current ?? null);
			if (container && !canHandleInsideContainer(container)) {
				return;
			}
			if (isNavigationKey) {
				event.preventDefault();
				focusByDelta(delta);
				return;
			}
			if (onEscape) {
				event.preventDefault();
				onEscape();
			}
		};
		window.addEventListener('keydown', handleKeyDown, true);
		return () => {
			window.removeEventListener('keydown', handleKeyDown, true);
			observer?.disconnect();
			observer = null;
			observedRoot = null;
			messageNodesCache = EMPTY_MESSAGE_NODES_SNAPSHOT;
		};
	}, [
		keyboardModeEnabled,
		containerRef,
		channelId,
		onFocusMessage,
		onLoadMoreBefore,
		onLoadMoreAfter,
		hasMoreBefore,
		hasMoreAfter,
		isLoadingMore,
		onEscape,
		allowWhenInactive,
	]);
}
