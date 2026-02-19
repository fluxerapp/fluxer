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
import type {ScrollerHandle} from '@app/components/uikit/Scroller';
import type {ChannelMessages} from '@app/lib/ChannelMessages';
import {evaluateScrollPinning, type ScrollPinResult} from '@app/lib/scroll/ScrollPosition';
import type {ChannelRecord} from '@app/records/ChannelRecord';
import AccessibilityStore from '@app/stores/AccessibilityStore';
import DimensionStore from '@app/stores/DimensionStore';
import KeyboardModeStore from '@app/stores/KeyboardModeStore';
import MessageStore from '@app/stores/MessageStore';
import {MAX_MESSAGES_PER_CHANNEL, NEW_MESSAGES_BAR_BUFFER} from '@fluxer/constants/src/LimitConstants';
import {compare, extractTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';
import debounce from 'lodash/debounce';
import {createRef, useLayoutEffect, useState} from 'react';

type ScrollerRef = React.RefObject<ScrollerHandle | null> | React.RefObject<ScrollerHandle>;

type DebouncedFunction<T> = T extends (...args: infer P) => infer R
	? {
			(...args: P): R;
			cancel(): void;
			flush(): void;
		}
	: never;

interface AnchorData {
	id: string;
	offsetFromTop: number;
	offsetTop: number;
	offsetHeight: number;
	clamped: boolean;
}

interface ScrollerState {
	scrollTop: number;
	scrollHeight: number;
	offsetHeight: number;
}

export interface ScrollManagerProps {
	messages: ChannelMessages;
	channel: ChannelRecord;
	compact: boolean;
	hasUnreads: boolean;
	focusId: string | null;
	placeholderHeight: number;
	canLoadMore: boolean;
	windowId: string;
	handleScrollToBottom: () => void;
	handleScrollFromBottom: () => void;
	additionalMessagePadding: number;
	canAutoAck: boolean;
}

const DEFAULT_SCROLLER_STATE: ScrollerState = {
	scrollTop: 0,
	scrollHeight: 0,
	offsetHeight: 0,
};

const BOTTOM_LOCK_TOLERANCE = 8;

enum ScrollRegion {
	None = 0,
	Top = 1,
	Bottom = 2,
}

function resolveJumpTargetId(messages: ChannelMessages): string | null {
	const {jumpTargetId, jumpTargetOffset} = messages;

	if (!jumpTargetId || !messages.ready) return null;

	if (messages.has(jumpTargetId) || (!messages.hasMoreBefore && jumpTargetId === messages.channelId)) {
		if (jumpTargetOffset === 0) {
			return jumpTargetId;
		}
		const index = messages.indexOf(jumpTargetId);
		const targetMessage = messages.getByIndex(index + jumpTargetOffset);
		return targetMessage?.id ?? jumpTargetId;
	}

	const allIds = [jumpTargetId, ...messages.map((m) => m.id)].sort(compare);
	const jumpIndex = allIds.indexOf(jumpTargetId);
	const offset = Math.abs(jumpTargetOffset) > 0 ? jumpTargetOffset : 1;
	const closestId = allIds[jumpIndex + offset] ?? allIds[jumpIndex - 1];

	return closestId ?? null;
}

export class ScrollManager {
	ref: ScrollerRef = createRef();
	props: ScrollManagerProps;

	private automaticAnchor: AnchorData | null = null;
	private messageFetchAnchor: AnchorData | null = null;
	private focusAnchor: AnchorData | null = null;
	private bottomAnchor: AnchorData | null = null;

	private isLoadingMoreMessages: boolean;
	private isJumpingToMessage = false;
	private isPinnedToBottom!: boolean;
	private isUserDragging = false;
	private hadSavedScrollPosition = false;
	private isCurrentlyAtBottom = false;
	private isProgrammaticallyScrollingToBottom = false;
	private isDisposed = false;
	private isInEditMode = false;

	private scrollAnchorTimeoutId: number | null = null;
	private pendingInitialScrollTop: number | null | undefined = null;
	private cachedOffsetHeight = 0;
	private cachedScrollHeight = 0;
	private cachedScrollTop = -1;
	private previousScrollTop: number | null = null;

	private prependScrollSnapshot: {scrollTop: number; scrollHeight: number} | null = null;
	private lastMessageLoadDirection: 'before' | 'after' | null = null;

	private preUpdatePinState: ScrollPinResult | null = null;

	private automaticAnchorListeners: Array<(anchor: AnchorData | null, bottom: AnchorData | null) => void> = [];
	private scrollCompleteListeners: Array<() => void> = [];

	private updateStoreDimensionsDebounced: DebouncedFunction<() => void>;

	constructor(props: ScrollManagerProps) {
		this.props = props;
		this.isLoadingMoreMessages = props.messages.loadingMore;

		if (props.messages.jumpTargetId != null) {
			this.isPinnedToBottom = false;
		} else {
			const stored = DimensionStore.getChannelDimensions(props.channel.id);
			this.hadSavedScrollPosition = stored != null;
			const isAtBottom = DimensionStore.isAtBottom(props.channel.id);
			this.isPinnedToBottom = isAtBottom ?? true;
			this.pendingInitialScrollTop = this.isPinnedToBottom ? null : (stored?.scrollTop ?? null);
		}

		this.updateStoreDimensionsDebounced = debounce(this.updateStoreDimensions.bind(this), 200);
	}

	private takePreUpdatePinState(): ScrollPinResult | null {
		const snapshot = this.preUpdatePinState;
		this.preUpdatePinState = null;
		return snapshot;
	}

	isReady(): boolean {
		return this.props.messages.ready;
	}

	isLoading(): boolean {
		return this.isLoadingMoreMessages || this.props.messages.loadingMore;
	}

	isPinned(): boolean {
		return this.isPinnedToBottom;
	}

	isJumping(): boolean {
		return this.isJumpingToMessage;
	}

	isDragging(): boolean {
		return this.isUserDragging;
	}

	isInitialized(): boolean {
		return this.pendingInitialScrollTop === undefined;
	}

	isScrollLoadingDisabled(): boolean {
		return (
			this.isLoading() || !this.isInitialized() || this.isJumping() || this.isDragging() || !this.props.canLoadMore
		);
	}

	private computePinState(state: ScrollerState): ScrollPinResult {
		return evaluateScrollPinning(state, {
			tolerance: BOTTOM_LOCK_TOLERANCE,
			wasPinned: this.isPinnedToBottom,
			hasMoreAfter: this.props.messages.hasMoreAfter,
			allowPinWhenHasMoreAfter: true,
		});
	}

	getDocument(): Document | undefined {
		const node = this.ref.current?.getScrollerNode();
		return node?.ownerDocument;
	}

	getScrollerState(): ScrollerState {
		return this.ref.current?.getScrollerState() ?? DEFAULT_SCROLLER_STATE;
	}

	isScrolledToBottom(state: ScrollerState = this.getScrollerState()): boolean {
		const pinState = this.computePinState(state);
		return pinState.isPinned;
	}

	getElementFromMessageId(messageId: string): HTMLElement | null {
		const doc = this.getDocument();
		const {channel} = this.props;
		if (!doc) return null;
		const elementId = `chat-messages-${channel.id}-${messageId}`;
		return doc.getElementById(elementId) as HTMLElement | null;
	}

	private getOffsetTop(element: HTMLElement, container: HTMLElement): number {
		const elRect = element.getBoundingClientRect();
		const containerRect = container.getBoundingClientRect();
		return container.scrollTop + (elRect.top - containerRect.top);
	}

	private getJumpBreathingRoom(): number {
		return NEW_MESSAGES_BAR_BUFFER;
	}

	private lockNodeTopToPadding(node: HTMLElement, padding: number, animate: boolean, callback?: () => void): void {
		const scrollerNode = this.ref.current?.getScrollerNode();
		if (!scrollerNode) {
			callback?.();
			return;
		}

		const doScroll = (targetScrollTop: number, animated: boolean, cb?: () => void) => {
			this.scrollTo(targetScrollTop, animated, cb);
		};

		const computeTargetScrollTop = (): number => {
			const state = this.getScrollerState();
			const containerRect = scrollerNode.getBoundingClientRect();
			const nodeRect = node.getBoundingClientRect();
			const delta = nodeRect.top - containerRect.top;
			const nodeOffsetTop = state.scrollTop + delta;
			return nodeOffsetTop - padding;
		};

		const correctOnce = () => {
			const containerRect = scrollerNode.getBoundingClientRect();
			const nodeRect = node.getBoundingClientRect();
			const currentY = nodeRect.top - containerRect.top;
			const diff = currentY - padding;

			if (Math.abs(diff) <= 0.5) return;

			this.mergeTo(scrollerNode.scrollTop + diff);
		};

		const target = computeTargetScrollTop();
		doScroll(target, animate, () => {
			correctOnce();
			requestAnimationFrame(() => {
				if (this.isDisposed) return;
				correctOnce();
				callback?.();
			});
		});
	}

	getAnchorData(messageId: string, scrollTop: number, clampTo?: number): AnchorData | null {
		const element = this.getElementFromMessageId(messageId);
		const scrollerNode = this.ref.current?.getScrollerNode();
		if (!element || !scrollerNode) return null;

		const offsetHeight = element.offsetHeight;
		const offsetTop = this.getOffsetTop(element, scrollerNode);
		let offsetFromTop = offsetTop - scrollTop;

		if (clampTo != null) {
			offsetFromTop = Math.max(-offsetHeight, Math.min(clampTo, offsetFromTop));
		}

		return {
			id: messageId,
			offsetFromTop,
			offsetTop,
			offsetHeight,
			clamped: clampTo != null,
		};
	}

	getNewMessageBarBuffer(): number {
		return NEW_MESSAGES_BAR_BUFFER;
	}

	setAutomaticAnchor(anchor: AnchorData | null): void {
		this.automaticAnchor = anchor;

		for (const cb of this.automaticAnchorListeners) {
			cb(this.automaticAnchor, this.bottomAnchor);
		}
	}

	clearAutomaticAnchor(): void {
		this.setAutomaticAnchor(null);
	}

	findTopVisibleAnchor(): AnchorData | null {
		const {messages, hasUnreads, channel} = this.props;
		const state = this.getScrollerState();
		const {scrollTop, offsetHeight} = state;

		const buffer = hasUnreads && scrollTop >= this.getNewMessageBarBuffer() ? this.getNewMessageBarBuffer() : 0;

		let anchor: AnchorData | null = null;
		let index = -1;
		let foundAnchor = false;

		const getMessageId = (idx: number): string | undefined => {
			if (idx === -1) {
				return channel.id;
			}
			return messages.getByIndex(idx)?.id;
		};

		while (true) {
			const messageId = getMessageId(index);
			if (!messageId) break;

			const anchorData = this.getAnchorData(messageId, scrollTop);
			this.bottomAnchor = anchorData;

			if (foundAnchor && anchorData != null && anchorData.offsetTop > scrollTop + buffer + offsetHeight) {
				break;
			}

			if (foundAnchor) {
				index++;
				continue;
			}

			if (anchorData != null && (anchorData.offsetTop >= scrollTop + buffer || index === messages.length - 1)) {
				anchor = anchorData;
				foundAnchor = true;
			}

			index++;
		}

		return anchor;
	}

	findLoadMoreAnchor(isBefore: boolean): AnchorData | null {
		const {messages} = this.props;
		const {scrollTop} = this.getScrollerState();

		const direction = isBefore ? -1 : 1;
		const startIndex = isBefore ? messages.length - 1 : 0;
		let anchor: AnchorData | null = null;

		for (let i = startIndex; messages.getByIndex(i) != null; i += direction) {
			const msg = messages.getByIndex(i)!;
			const data = this.getAnchorData(msg.id, scrollTop);
			if (data) {
				anchor = data;
				break;
			}
		}

		return anchor;
	}

	getAnchorFixData(): {node: HTMLElement; fixedScrollTop: number} | null {
		const candidates = [this.focusAnchor, this.isLoading() ? null : this.messageFetchAnchor, this.automaticAnchor];
		const scrollerNode = this.ref.current?.getScrollerNode();
		if (!scrollerNode) return null;

		for (const anchor of candidates) {
			if (!anchor) continue;
			const element = this.getElementFromMessageId(anchor.id);
			if (!element) continue;

			const currentOffsetTop = this.getOffsetTop(element, scrollerNode);
			const fixedScrollTop = Math.max(0, currentOffsetTop - anchor.offsetFromTop);

			return {
				node: element,
				fixedScrollTop,
			};
		}

		return null;
	}

	fixAnchorScrollPosition(): void {
		const anchorData = this.getAnchorFixData();

		if (!anchorData) {
			this.handleScroll();
			return;
		}

		const {node, fixedScrollTop} = anchorData;

		if (this.focusAnchor) {
			if (this.isPinned()) {
				this.scrollTo(Number.MAX_SAFE_INTEGER, false, this.handleScroll);
			} else {
				this.mergeTo(fixedScrollTop, this.handleScroll);
			}

			this.ref.current?.scrollIntoViewNode({
				node,
				padding: 16 + this.props.additionalMessagePadding,
				callback: this.handleScroll,
			});

			if (KeyboardModeStore.keyboardModeEnabled && this.focusAnchor) {
				const elementToFocus = this.getElementFromMessageId(this.focusAnchor.id);
				if (elementToFocus) {
					elementToFocus.focus({preventScroll: true});
				}
			}

			if (!this.isLoading()) {
				this.focusAnchor = null;
			}
		} else {
			this.mergeTo(fixedScrollTop, this.handleScroll);
		}

		if (!this.isLoading()) {
			this.messageFetchAnchor = null;
		}
	}

	hasAnchor(): boolean {
		return !!this.focusAnchor || !!this.messageFetchAnchor || !!this.automaticAnchor;
	}

	updateFocusAnchor(messageId: string | null | undefined, scrollTop: number, offsetHeight: number): void {
		if (messageId) {
			this.focusAnchor = this.getAnchorData(messageId, scrollTop);
		}

		const anchor = this.focusAnchor;
		if (!anchor) return;

		if (anchor.offsetFromTop >= offsetHeight || scrollTop > anchor.offsetTop + anchor.offsetHeight) {
			this.focusAnchor = null;
		}
	}

	handleFocusAnchorScroll(scrollTop: number, offsetHeight: number): void {
		this.updateFocusAnchor(this.focusAnchor?.id ?? null, scrollTop, offsetHeight);
	}

	updateFetchAnchor(scrollTop: number, offsetHeight: number, scrollHeight: number): void {
		const scrollerNode = this.ref.current?.getScrollerNode();
		if (!this.messageFetchAnchor || !scrollerNode) return;

		const region = this.isInPlaceholderRegion({scrollTop, offsetHeight, scrollHeight});
		const clampTo = region !== ScrollRegion.None ? offsetHeight : undefined;

		this.messageFetchAnchor = this.getAnchorData(this.messageFetchAnchor.id, scrollTop, clampTo);
	}

	updateAutomaticAnchor(scrollTop: number): void {
		const scrollerNode = this.ref.current?.getScrollerNode();
		if (!this.automaticAnchor || !scrollerNode) return;

		const anchorData = this.getAnchorData(this.automaticAnchor.id, scrollTop);
		if (!anchorData) {
			this.setAutomaticAnchor(null);
			return;
		}

		this.setAutomaticAnchor(anchorData);
	}

	isHeightChange(offsetHeight: number, scrollHeight: number): boolean {
		return offsetHeight !== this.cachedOffsetHeight || scrollHeight !== this.cachedScrollHeight;
	}

	isInPlaceholderRegion(state: ScrollerState): ScrollRegion {
		const {scrollTop, offsetHeight, scrollHeight} = state;
		const {messages, placeholderHeight} = this.props;

		if (messages.hasMoreBefore && scrollTop < placeholderHeight && scrollHeight > offsetHeight) {
			return ScrollRegion.Top;
		}

		if (messages.hasMoreAfter && scrollTop >= scrollHeight - offsetHeight - placeholderHeight) {
			return ScrollRegion.Bottom;
		}

		return ScrollRegion.None;
	}

	getOffsetToTriggerLoading(edge: 'top' | 'bottom', state: ScrollerState): number {
		const {scrollHeight, offsetHeight} = state;
		const {messages, hasUnreads, placeholderHeight} = this.props;

		if (edge === 'top') {
			if (!messages.hasMoreBefore) {
				return 0;
			}
			return hasUnreads ? placeholderHeight - NEW_MESSAGES_BAR_BUFFER - 2 : placeholderHeight + 500;
		}

		return messages.hasMoreAfter ? scrollHeight - offsetHeight - placeholderHeight - 500 : scrollHeight - offsetHeight;
	}

	getOffsetToPreventLoading(edge: 'top' | 'bottom'): number {
		const {messages} = this.props;
		let delta = 0;

		if (edge === 'top' && messages.hasMoreBefore) {
			delta = 2;
		} else if (edge === 'bottom' && messages.hasMoreAfter) {
			delta = -2;
		}

		return this.getOffsetToTriggerLoading(edge, this.getScrollerState()) + delta;
	}

	isInScrollTriggerLoadingRegion(state: ScrollerState): ScrollRegion {
		const {scrollTop, offsetHeight, scrollHeight} = state;
		const {messages} = this.props;

		if (
			messages.hasMoreBefore &&
			scrollTop <= this.getOffsetToTriggerLoading('top', state) &&
			scrollHeight > offsetHeight
		) {
			return ScrollRegion.Top;
		}

		if (messages.hasMoreAfter && scrollTop >= this.getOffsetToTriggerLoading('bottom', state)) {
			return ScrollRegion.Bottom;
		}

		return ScrollRegion.None;
	}

	handleScrollSpeed(state: ScrollerState): void {
		if (this.isJumping() || this.isDragging() || !this.props.canLoadMore) {
			return;
		}

		const {scrollTop, offsetHeight, scrollHeight} = state;
		const prev = this.previousScrollTop;
		const {placeholderHeight} = this.props;

		this.previousScrollTop = scrollTop;
		if (prev == null) return;

		const region = this.isInPlaceholderRegion(state);
		const delta = scrollTop - prev;

		if (region === ScrollRegion.None || delta === 0) return;

		if (region === ScrollRegion.Top && scrollTop + delta <= 0) {
			const newTop = placeholderHeight - offsetHeight;
			this.mergeTo(newTop);
			this.previousScrollTop = newTop;
		} else if (region === ScrollRegion.Bottom && scrollTop + delta >= scrollHeight - offsetHeight) {
			const newTop = scrollHeight - placeholderHeight;
			this.mergeTo(newTop);
			this.previousScrollTop = newTop;
		}
	}

	private loadMore = (loadAfter = false): void => {
		const {messages, channel} = this.props;

		let beforeId: string | undefined;
		let afterId: string | undefined;

		if (loadAfter) {
			const last = messages.last();
			if (last) afterId = last.id;
		} else {
			const first = messages.first();
			if (first) beforeId = first.id;
		}

		const {scrollTop, scrollHeight} = this.getScrollerState();
		if (!loadAfter) {
			this.prependScrollSnapshot = {
				scrollTop,
				scrollHeight,
			};
			this.lastMessageLoadDirection = 'before';
		} else {
			this.prependScrollSnapshot = null;
			this.lastMessageLoadDirection = 'after';
		}

		if (KeyboardModeStore.keyboardModeEnabled) {
			const focusedElement = document.activeElement;
			const scrollerNode = this.ref.current?.getScrollerNode();
			if (focusedElement && scrollerNode?.contains(focusedElement)) {
				const messageId = (focusedElement as HTMLElement).dataset?.messageId;
				if (messageId) {
					const {scrollTop} = this.getScrollerState();
					const anchor = this.getAnchorData(messageId, scrollTop);
					if (anchor) {
						this.focusAnchor = anchor;
					}
				}
			}
		}

		this.messageFetchAnchor = this.findLoadMoreAnchor(loadAfter);
		this.isLoadingMoreMessages = true;

		MessageActionCreators.fetchMessages(channel.id, beforeId ?? null, afterId ?? null, MAX_MESSAGES_PER_CHANNEL);
	};

	loadMoreForKeyboardNavigation(loadAfter: boolean): void {
		if (this.isLoadingMoreMessages || this.props.messages.loadingMore) return;

		const focusedElement = document.activeElement;
		const scrollerNode = this.ref.current?.getScrollerNode();
		if (focusedElement && scrollerNode?.contains(focusedElement)) {
			const messageId = (focusedElement as HTMLElement).dataset?.messageId;
			if (messageId) {
				const {scrollTop} = this.getScrollerState();
				const anchor = this.getAnchorData(messageId, scrollTop);
				if (anchor) {
					this.focusAnchor = anchor;
				}
			}
		}

		this.loadMore(loadAfter);
	}

	handleScroll = (event?: React.UIEvent<HTMLDivElement> | Event): void => {
		if (this.isDisposed) return;
		if (!this.isInitialized()) return;

		const state = this.getScrollerState();
		const pinState = this.computePinState(state);
		const heightChanged =
			state.offsetHeight !== this.cachedOffsetHeight || state.scrollHeight !== this.cachedScrollHeight;

		const isAtBottom =
			(heightChanged && this.isPinnedToBottom) || this.isProgrammaticallyScrollingToBottom ? true : pinState.isPinned;

		if (isAtBottom !== this.isCurrentlyAtBottom) {
			this.isCurrentlyAtBottom = isAtBottom;
			if (isAtBottom) {
				this.props.handleScrollToBottom();
			} else {
				this.props.handleScrollFromBottom();
			}
		}

		if (heightChanged) {
			if (this.scrollAnchorTimeoutId != null) {
				clearTimeout(this.scrollAnchorTimeoutId);
				this.scrollAnchorTimeoutId = null;
			}

			if (!this.isPinned()) {
				if (!this.automaticAnchor) {
					this.setAutomaticAnchor(this.findTopVisibleAnchor());
				} else {
					this.updateAutomaticAnchor(state.scrollTop);
				}
			}

			this.cachedScrollTop = state.scrollTop;
			this.fixScrollPosition(state.offsetHeight, state.scrollHeight, isAtBottom);
		} else {
			if (event && event.target !== this.ref.current?.getScrollerNode()) {
				return;
			}

			if (this.cachedScrollTop !== state.scrollTop) {
				this.isPinnedToBottom = isAtBottom;

				if (this.isPinnedToBottom) {
					this.clearAutomaticAnchor();
				} else if (this.automaticAnchor) {
					this.updateAutomaticAnchor(state.scrollTop);
				} else {
					this.setAutomaticAnchor(this.findTopVisibleAnchor());
				}

				this.cachedScrollTop = state.scrollTop;

				if (this.scrollAnchorTimeoutId != null) {
					clearTimeout(this.scrollAnchorTimeoutId);
				}
				this.scrollAnchorTimeoutId = window.setTimeout(() => {
					this.scrollAnchorTimeoutId = null;
					this.previousScrollTop = null;

					const {scrollHeight, offsetHeight} = this.getScrollerState();
					if (this.isHeightChange(offsetHeight, scrollHeight)) {
						this.handleScroll();
					} else if (!this.isPinned() && !this.automaticAnchor) {
						this.setAutomaticAnchor(this.findTopVisibleAnchor());
					}
				}, 35);
			}
		}

		this.handleFocusAnchorScroll(state.scrollTop, state.offsetHeight);
		this.updateStoreDimensionsDebounced();

		if (this.isScrollLoadingDisabled()) {
			this.handleScrollSpeed(state);
			return;
		}

		const loadingRegion = this.isInScrollTriggerLoadingRegion(state);
		if (loadingRegion === ScrollRegion.Top) {
			this.loadMore();
		} else if (loadingRegion === ScrollRegion.Bottom) {
			this.loadMore(true);
		}

		this.handleScrollSpeed(state);
	};

	handleResize = (_entry: ResizeObserverEntry, _type: 'container' | 'content'): void => {
		if (this.isDisposed) return;
		this.handleLayoutResized();
	};

	handleLayoutResized(heightDelta?: number): void {
		if (this.isDisposed || !this.isInitialized()) return;

		if (typeof heightDelta === 'number' && this.applyLayoutShift(heightDelta)) {
			return;
		}

		const state = this.getScrollerState();
		const hasResized = this.isHeightChange(state.offsetHeight, state.scrollHeight);
		if (!hasResized) {
			this.handleScroll();
			return;
		}

		const shouldStickToBottom =
			this.isPinned() || this.computePinState(state).isPinned || this.isProgrammaticallyScrollingToBottom;
		if (!shouldStickToBottom && this.automaticAnchor) {
			this.updateAutomaticAnchor(state.scrollTop);
		}
		this.fixScrollPosition(state.offsetHeight, state.scrollHeight, shouldStickToBottom);
	}

	handleMouseDown = (event: React.MouseEvent): void => {
		if (this.isDisposed) return;

		if (event.target === event.currentTarget) {
			this.isUserDragging = true;
		}
	};

	handleMouseUp = (): void => {
		if (this.isDisposed) return;

		this.isUserDragging = false;
		this.handleScroll();
	};

	fixScrollPosition(offsetHeight: number, scrollHeight: number, forceAtBottom = false): void {
		this.cachedOffsetHeight = offsetHeight;
		this.cachedScrollHeight = scrollHeight;

		if (this.prependScrollSnapshot && !this.isLoading() && this.lastMessageLoadDirection === 'before') {
			const {scrollTop: prevScrollTop, scrollHeight: prevScrollHeight} = this.prependScrollSnapshot;
			this.prependScrollSnapshot = null;
			this.lastMessageLoadDirection = null;

			const addedHeight = scrollHeight - prevScrollHeight;

			if (addedHeight !== 0) {
				const currentState = this.getScrollerState();
				const maxScroll = Math.max(0, scrollHeight - currentState.offsetHeight);
				const targetScrollTop = Math.max(0, Math.min(prevScrollTop + addedHeight, maxScroll));

				this.isPinnedToBottom = false;
				this.mergeTo(targetScrollTop, this.handleScroll);
				return;
			}
		}

		if (this.isJumping()) {
			this.fixJumpTarget();
			return;
		}

		const currentState = this.getScrollerState();
		const currentPinState = this.computePinState(currentState);
		const atBottom = forceAtBottom || this.isPinned() || currentPinState.isPinned;

		if (atBottom) {
			this.isProgrammaticallyScrollingToBottom = true;
			this.scrollTo(Number.MAX_SAFE_INTEGER, false, () => {
				this.isProgrammaticallyScrollingToBottom = false;
				this.isPinnedToBottom = true;
				this.handleScroll();
			});
			return;
		}

		this.fixAnchorScrollPosition();
	}

	private fixJumpTarget(): void {
		const {messages} = this.props;

		const targetId = messages.jumpTargetId ? resolveJumpTargetId(messages) : null;

		if (targetId) {
			const element = this.getElementFromMessageId(targetId);
			if (element) {
				const padding = this.getJumpBreathingRoom();
				const scrollerNode = this.ref.current?.getScrollerNode();
				if (!scrollerNode) return;

				const containerRect = scrollerNode.getBoundingClientRect();
				const nodeRect = element.getBoundingClientRect();
				const delta = nodeRect.top - containerRect.top;
				const targetScrollTop = scrollerNode.scrollTop + delta - padding;

				this.mergeTo(targetScrollTop);
				return;
			}

			this.scrollToNewMessages('top', undefined, false);
			return;
		}

		this.scrollTo(Number.MAX_SAFE_INTEGER, false);
	}

	scrollToNewMessages(
		orientation: 'top' | 'middle' = 'top',
		callback?: () => void,
		animate = true,
		suppressPadding = false,
	): void {
		const doc = this.getDocument();
		const newMessagesBar = doc?.getElementById('new-messages-bar');

		const onComplete = () => {
			this.isJumpingToMessage = false;
			this.setAutomaticAnchor(this.findTopVisibleAnchor());
			callback?.();
			this.handleScroll();

			for (const cb of this.scrollCompleteListeners) {
				cb();
			}
		};

		this.isPinnedToBottom = false;
		this.isJumpingToMessage = true;

		const padding = suppressPadding ? 0 : this.getJumpBreathingRoom();

		if (newMessagesBar) {
			if (orientation === 'middle') {
				const scrollerNode = this.ref.current?.getScrollerNode();
				if (!scrollerNode) {
					this.scrollTo(Number.MAX_SAFE_INTEGER, animate, onComplete);
					return;
				}

				const {offsetHeight} = this.getScrollerState();
				const containerRect = scrollerNode.getBoundingClientRect();
				const nodeRect = newMessagesBar.getBoundingClientRect();
				const delta = nodeRect.top - containerRect.top;
				const nodeOffsetTop = scrollerNode.scrollTop + delta;

				const middleTarget = nodeOffsetTop - 0.5 * offsetHeight + 0.5 * nodeRect.height;
				const target = Math.min(middleTarget, nodeOffsetTop - padding);

				this.scrollTo(target, animate, onComplete);
				return;
			}

			this.lockNodeTopToPadding(newMessagesBar, padding, animate, onComplete);
		} else {
			this.scrollTo(Number.MAX_SAFE_INTEGER, animate, onComplete);
		}
	}

	scrollToBelowUnreadDivider(): void {
		const doc = this.getDocument();
		const scrollerNode = this.ref.current?.getScrollerNode();
		const newMessagesBar = doc?.getElementById('new-messages-bar');

		if (!scrollerNode || !newMessagesBar) {
			this.setScrollToBottom();
			return;
		}

		const offsetTop = this.getOffsetTop(newMessagesBar, scrollerNode);
		const elementHeight = newMessagesBar.offsetHeight;
		const targetScrollTop = offsetTop + elementHeight + 2;

		this.isPinnedToBottom = false;
		this.scrollTo(targetScrollTop, false, () => {
			this.setAutomaticAnchor(this.findTopVisibleAnchor());
			this.handleScroll();
		});
	}

	restoreScroll(): void {
		if (this.isInitialized()) return;

		const {pendingInitialScrollTop} = this;
		this.pendingInitialScrollTop = undefined;

		const targetId = resolveJumpTargetId(this.props.messages);

		if (targetId != null) {
			this.scrollToMessage(targetId, false);
		} else if (pendingInitialScrollTop != null) {
			const targetScroll = pendingInitialScrollTop + this.props.placeholderHeight;
			this.scrollTo(targetScroll, false, this.handleScroll);
		} else if (!this.hadSavedScrollPosition && this.props.hasUnreads) {
			this.scrollToBelowUnreadDivider();
		} else {
			this.setScrollToBottom();
		}
	}

	scrollTo(position: number, animate = false, callback?: () => void): void {
		if (this.isDisposed) return;

		this.ref.current?.scrollTo({
			to: position,
			animate: !AccessibilityStore.useReducedMotion && animate,
			callback,
		});

		if (this.isPinned()) {
			this.updateStoreDimensions();
		} else {
			this.updateStoreDimensionsDebounced();
		}
	}

	mergeTo(position: number, callback?: () => void): void {
		if (this.isDisposed) return;

		this.ref.current?.mergeTo({
			to: position,
			callback,
		});

		if (this.isPinned()) {
			this.updateStoreDimensions();
		} else {
			this.updateStoreDimensionsDebounced();
		}
	}

	setScrollToBottom(animate = false): void {
		if (this.isDisposed) return;

		const {messages, channel} = this.props;

		DimensionStore.updateChannelDimensions(channel.id, 1, 1, 0);

		if (messages.hasMoreAfter) {
			MessageActionCreators.jumpToPresent(channel.id, MAX_MESSAGES_PER_CHANNEL);
		} else {
			this.isProgrammaticallyScrollingToBottom = true;
			this.isPinnedToBottom = true;
			this.scrollTo(Number.MAX_SAFE_INTEGER, animate, () => {
				this.isJumpingToMessage = false;
				this.isProgrammaticallyScrollingToBottom = false;
				this.handleScroll();
			});
		}
	}

	applyLayoutShift(heightDelta: number): boolean {
		if (this.isDisposed || !this.isInitialized()) return false;
		if (heightDelta === 0) return false;

		const scrollerNode = this.ref.current?.getScrollerNode();
		if (!scrollerNode) return false;

		if (this.isPinned()) {
			this.scrollTo(Number.MAX_SAFE_INTEGER, false, this.handleScroll);
			return true;
		}

		if (this.isInEditMode) {
			return false;
		}

		const state = this.getScrollerState();
		const distanceFromBottom = Math.max(state.scrollHeight - state.offsetHeight - state.scrollTop, 0);
		const stickThreshold = Math.max(Math.abs(heightDelta) + BOTTOM_LOCK_TOLERANCE, 64);
		const shouldStick = distanceFromBottom <= stickThreshold;

		if (!shouldStick) return false;

		const maxScrollTop = Math.max(0, scrollerNode.scrollHeight - scrollerNode.offsetHeight);
		const targetScrollTop = Math.max(0, Math.min(state.scrollTop + heightDelta, maxScrollTop));

		this.mergeTo(targetScrollTop, this.handleScroll);
		return true;
	}

	enterEditMode(): void {
		this.isInEditMode = true;
	}

	exitEditMode(): void {
		if (!this.isInEditMode) return;
		this.isInEditMode = false;
	}

	private updateStoreDimensions(callback?: () => void): void {
		if (this.isDisposed) return;
		if (this.isJumping() || !this.isInitialized()) return;

		const {channel, placeholderHeight} = this.props;

		if (this.isPinned()) {
			DimensionStore.updateChannelDimensions(channel.id, 1, 1, 0, callback);
		} else {
			const {scrollTop, scrollHeight, offsetHeight} = this.getScrollerState();
			const adjustedScrollTop = scrollTop - placeholderHeight;
			const adjustedScrollHeight = scrollHeight - placeholderHeight;

			DimensionStore.updateChannelDimensions(
				channel.id,
				adjustedScrollTop,
				adjustedScrollHeight,
				offsetHeight,
				callback,
			);
		}
	}

	focusMessage(messageId: string): void {
		const element = this.getElementFromMessageId(messageId);
		if (!element) return;

		const scrollerNode = this.ref.current?.getScrollerNode();
		if (!scrollerNode) return;

		const elementOffset = this.getOffsetTop(element, scrollerNode);
		const elementHeight = element.offsetHeight;
		const {scrollTop, offsetHeight} = this.getScrollerState();

		const topPadding = 80;
		const bottomPadding = 120;

		const elementTop = elementOffset;
		const elementBottom = elementOffset + elementHeight;
		const viewportTop = scrollTop + topPadding;
		const viewportBottom = scrollTop + offsetHeight - bottomPadding;

		let targetScrollTop: number | null = null;

		if (elementTop < viewportTop) {
			targetScrollTop = elementOffset + elementHeight - offsetHeight + bottomPadding;
		} else if (elementBottom > viewportBottom) {
			targetScrollTop = elementOffset - topPadding;
		}

		if (targetScrollTop !== null) {
			const maxScroll = scrollerNode.scrollHeight - offsetHeight;
			targetScrollTop = Math.max(0, Math.min(targetScrollTop, maxScroll));
			scrollerNode.scrollTop = targetScrollTop;
		}

		const newScrollTop = targetScrollTop ?? scrollTop;
		const anchor = this.getAnchorData(messageId, newScrollTop);
		if (anchor) {
			this.focusAnchor = anchor;
		}

		if (KeyboardModeStore.keyboardModeEnabled) {
			if (element.tabIndex < 0) {
				element.tabIndex = -1;
			}
			element.focus({preventScroll: true});
		}
	}

	scrollPageUp(animate = false): void {
		this.ref.current?.scrollPageUp({animate});
	}

	scrollPageDown(animate = false): void {
		this.ref.current?.scrollPageDown({animate});
	}

	scrollToMessage(messageId: string, animate = true, previousTimestamp?: number): void {
		if (!this.ref.current) return;

		if (messageId === this.props.channel.id) {
			this.scrollTo(0);
			return;
		}

		const element = this.getElementFromMessageId(messageId);

		if (!this.isJumping() && animate && previousTimestamp != null && !AccessibilityStore.useReducedMotion) {
			const ts = extractTimestamp(messageId);
			if (ts > previousTimestamp) {
				this.scrollTo(0, false);
			} else {
				this.scrollTo(Number.MAX_SAFE_INTEGER, false);
			}
		}

		this.isPinnedToBottom = false;
		this.isJumpingToMessage = true;

		const onComplete = () => {
			this.isJumpingToMessage = false;

			MessageStore.handleClearJumpTarget({channelId: this.props.channel.id});

			const state = this.getScrollerState();
			const pinState = this.computePinState(state);
			if (pinState.isPinned) {
				this.isPinnedToBottom = true;
			}

			if (element && KeyboardModeStore.keyboardModeEnabled) {
				if (element.tabIndex < 0) {
					element.tabIndex = -1;
				}
				element.focus({preventScroll: true});
			}

			this.handleScroll();

			for (const cb of this.scrollCompleteListeners) {
				cb();
			}
		};

		if (element) {
			const padding = this.getJumpBreathingRoom();
			this.lockNodeTopToPadding(element, padding, animate, onComplete);
		} else {
			this.scrollToNewMessages('middle', onComplete, animate);
		}
	}

	getSnapshotBeforeUpdate(focusId: string | null): void {
		const {scrollTop, offsetHeight, scrollHeight} = this.getScrollerState();
		this.preUpdatePinState = this.computePinState({scrollTop, offsetHeight, scrollHeight});

		if (!this.hasAnchor() && focusId == null) return;

		this.updateFocusAnchor(focusId, scrollTop, offsetHeight);
		this.updateFetchAnchor(scrollTop, offsetHeight, scrollHeight);
		this.updateAutomaticAnchor(scrollTop);
	}

	mergePropsAndUpdate(nextProps: ScrollManagerProps): void {
		if (this.isDisposed) return;

		this.applyPropsUpdate(nextProps);
	}

	private applyPropsUpdate(nextProps: ScrollManagerProps): void {
		const prevMessages = this.props.messages;
		const prevFocusId = this.props.focusId;
		const preUpdatePinState = this.takePreUpdatePinState();

		this.props = {...nextProps};

		const {scrollTop, offsetHeight, scrollHeight} = this.getScrollerState();
		const heightChanged = this.isHeightChange(offsetHeight, scrollHeight);
		const shouldForceBottom = preUpdatePinState?.isPinned ?? false;

		this.cachedOffsetHeight = offsetHeight;
		this.cachedScrollHeight = scrollHeight;
		this.isLoadingMoreMessages = nextProps.messages.loadingMore;

		if (this.isInitialized() || this.isReady()) {
			if (!this.isInitialized()) {
				this.restoreScroll();
				return;
			}
		} else {
			if (nextProps.messages.jumpTargetId == null) {
				this.scrollTo(Number.MAX_SAFE_INTEGER);
			}
			return;
		}

		if (nextProps.messages.jumpTargetId != null) {
			if (this.isLoading()) {
				return;
			}

			const targetId = resolveJumpTargetId(nextProps.messages);

			if (targetId == null || this.isJumping() || nextProps.messages.jumpSequenceId === prevMessages.jumpSequenceId) {
				if (this.isJumping()) {
					if (targetId != null) {
						this.scrollToMessage(targetId, true);
					} else {
						this.isJumpingToMessage = false;
					}
				}
				return;
			}

			let previousTimestamp: number | undefined;
			const prevFirst = prevMessages.first();

			if (
				prevFirst != null &&
				nextProps.messages.last() !== prevMessages.last() &&
				nextProps.messages.first() !== prevMessages.first()
			) {
				previousTimestamp = extractTimestamp(prevFirst.id);
			}

			this.scrollToMessage(targetId, true, previousTimestamp);
			return;
		}

		if (nextProps.messages.jumpedToPresent && prevMessages.jumpSequenceId !== nextProps.messages.jumpSequenceId) {
			this.isJumpingToMessage = true;
			this.scrollTo(0);
			this.setScrollToBottom();
			return;
		}

		const lastMessage = nextProps.messages.last();
		const prevLastMessage = prevMessages.last();

		if (lastMessage != null && lastMessage.state === 'SENDING' && prevLastMessage?.id !== lastMessage.id) {
			if (this.isPinned()) {
				this.setScrollToBottom();
			}
			return;
		}

		const {focusId} = this.props;
		if (focusId != null && prevFocusId !== focusId) {
			const el = this.getElementFromMessageId(focusId);
			if (el) {
				this.ref.current?.scrollIntoViewNode({
					node: el,
					padding: 16 + this.props.additionalMessagePadding,
					callback: this.handleScroll,
				});
				return;
			}
		}

		if (heightChanged) {
			if (!shouldForceBottom && this.automaticAnchor) {
				this.updateAutomaticAnchor(scrollTop);
			}
			this.fixScrollPosition(offsetHeight, scrollHeight, shouldForceBottom);
		}
	}

	addAutomaticAnchorCallback(
		callback: (anchor: AnchorData | null, bottom: AnchorData | null) => void,
		immediate = true,
	): void {
		if (!this.automaticAnchorListeners.includes(callback)) {
			this.automaticAnchorListeners.push(callback);
		}
		if (immediate) {
			this.setAutomaticAnchor(this.findTopVisibleAnchor());
		}
	}

	removeAutomaticAnchorCallback(callback: (anchor: AnchorData | null, bottom: AnchorData | null) => void): void {
		this.automaticAnchorListeners = this.automaticAnchorListeners.filter((cb) => cb !== callback);
	}

	addScrollCompleteCallback(callback: () => void): void {
		if (!this.scrollCompleteListeners.includes(callback)) {
			this.scrollCompleteListeners.push(callback);
		}
	}

	removeScrollCompleteCallback(callback: () => void): void {
		this.scrollCompleteListeners = this.scrollCompleteListeners.filter((cb) => cb !== callback);
	}

	cleanup(): void {
		this.isDisposed = true;
		this.updateStoreDimensionsDebounced.cancel();

		if (this.scrollAnchorTimeoutId != null) {
			clearTimeout(this.scrollAnchorTimeoutId);
			this.scrollAnchorTimeoutId = null;
		}

		this.prependScrollSnapshot = null;
		this.lastMessageLoadDirection = null;
		this.preUpdatePinState = null;

		for (const cb of this.automaticAnchorListeners) {
			this.removeAutomaticAnchorCallback(cb);
		}
	}
}

export function useScrollManager(props: ScrollManagerProps): ScrollManager {
	const [manager] = useState(() => new ScrollManager(props));

	manager.getSnapshotBeforeUpdate(props.focusId);

	useLayoutEffect(() => {
		manager.mergePropsAndUpdate(props);
	});

	useLayoutEffect(() => {
		return () => manager.cleanup();
	}, [manager]);

	return manager;
}
