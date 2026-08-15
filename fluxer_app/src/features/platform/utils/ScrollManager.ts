// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import type {Channel} from '@app/features/channel/models/Channel';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import type {ChannelMessages} from '@app/features/messaging/state/ChannelMessages';
import Messages from '@app/features/messaging/state/MessagingMessages';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import Navigation from '@app/features/navigation/state/Navigation';
import {evaluateScrollPinning, type ScrollPinResult} from '@app/features/platform/utils/ScrollPosition';
import {
	type AnchorData,
	BOTTOM_LOCK_TOLERANCE,
	DEFAULT_SCROLLER_STATE,
	type DebouncedFunction,
	resolveContainerResizeShift,
	resolveJumpTargetId,
	type ScrollerRef,
	type ScrollerState,
	ScrollRegion,
	shouldAnimateMessageJump,
} from '@app/features/platform/utils/scroll_manager/shared';
import {getRemScaleForDocument} from '@app/features/theme/layout/RemFromPx';
import Dimension from '@app/features/ui/state/Dimension';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import {JumpTypes} from '@fluxer/constants/src/JumpConstants';
import {MAX_MESSAGES_PER_CHANNEL, NEW_MESSAGES_BAR_BUFFER} from '@fluxer/constants/src/LimitConstants';
import debounce from 'lodash/debounce';
import {createRef, useLayoutEffect, useState} from 'react';

const ANCHOR_SCROLL_IDLE_MS = 35;

interface ContainerLayout {
	rectTop: number;
	scrollTop: number;
}

export interface ScrollManagerProps {
	messages: ChannelMessages;
	channel: Channel;
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
	handleJumpHighlight: (messageId: string | null, jumpSequenceId: number) => void;
}

export class ScrollManager {
	ref: ScrollerRef = createRef();
	props: ScrollManagerProps;
	private anchorAutomatic: AnchorData | null = null;
	private anchorFetch: AnchorData | null = null;
	private anchorFetchFallback: AnchorData | null = null;
	private anchorFocus: AnchorData | null = null;
	private anchorBottom: AnchorData | null = null;
	private loadIsActive: boolean;
	private jumpIsActive = false;
	private pinIsAtBottom!: boolean;
	private dragIsActive = false;
	private restoreHadSavedPosition = false;
	private pinIsCurrentlyAtBottom = false;
	private pinIsScrollingProgrammatically = false;
	private lifecycleIsDisposed = false;
	private editIsActive = false;
	private anchorTimeoutId: number | null = null;
	private anchorTimeoutDeadline = 0;
	private resizeReentryGuard = false;
	private resizeContainerHeight = -1;
	private restorePendingInitialScrollTop: number | null | undefined = null;
	private cacheOffsetHeight = 0;
	private cacheScrollHeight = 0;
	private cacheScrollTop = -1;
	private cachePreviousScrollTop: number | null = null;
	private loadPrependSnapshot: {
		scrollTop: number;
		scrollHeight: number;
	} | null = null;
	private loadLastDirection: 'before' | 'after' | null = null;
	private pinPreUpdateState: ScrollPinResult | null = null;
	private jumpSequenceId: number | null = null;
	private jumpCallbackToken = 0;
	private anchorAutomaticListeners: Array<(anchor: AnchorData | null, bottom: AnchorData | null) => void> = [];
	private dimensionPersistDebounced: DebouncedFunction<() => void>;

	constructor(props: ScrollManagerProps) {
		this.props = props;
		this.loadIsActive = props.messages.loadingMore;
		if (props.messages.jumpTargetId != null) {
			this.pinIsAtBottom = false;
		} else {
			const stored = Dimension.getChannelDimensions(props.channel.id);
			this.restoreHadSavedPosition = stored != null;
			const isAtBottom = Dimension.isAtBottom(props.channel.id);
			this.pinIsAtBottom = isAtBottom ?? true;
			this.restorePendingInitialScrollTop = this.pinIsAtBottom ? null : (stored?.scrollTop ?? null);
		}
		this.dimensionPersistDebounced = debounce(this.dimensionPersist.bind(this), 200);
	}

	private pinTakePreUpdateState(): ScrollPinResult | null {
		const snapshot = this.pinPreUpdateState;
		this.pinPreUpdateState = null;
		return snapshot;
	}

	lifecycleIsReady(): boolean {
		return this.props.messages.ready;
	}

	loadIsInProgress(): boolean {
		return this.loadIsActive || this.props.messages.loadingMore;
	}

	pinIsAtBottomNow(): boolean {
		return this.pinIsAtBottom;
	}

	jumpIsActiveNow(): boolean {
		return this.jumpIsActive;
	}

	dragIsActiveNow(): boolean {
		return this.dragIsActive;
	}

	lifecycleIsInitialized(): boolean {
		return this.restorePendingInitialScrollTop === undefined;
	}

	loadIsDisabled(): boolean {
		return (
			this.loadIsInProgress() ||
			!this.lifecycleIsInitialized() ||
			this.jumpIsActiveNow() ||
			this.dragIsActiveNow() ||
			!this.props.canLoadMore
		);
	}

	private jumpBegin(jumpSequenceId: number | null): number {
		this.jumpCallbackToken += 1;
		this.jumpIsActive = true;
		this.jumpSequenceId = jumpSequenceId;
		return this.jumpCallbackToken;
	}

	private jumpIsCurrent(token: number): boolean {
		return !this.lifecycleIsDisposed && this.jumpCallbackToken === token;
	}

	private jumpStop(cancelScroll = false): void {
		this.jumpCallbackToken += 1;
		if (cancelScroll) {
			this.ref.current?.cancelScroll();
		}
		this.jumpIsActive = false;
		this.jumpSequenceId = null;
	}

	private pinComputeState(state: ScrollerState): ScrollPinResult {
		return evaluateScrollPinning(state, {
			tolerance: BOTTOM_LOCK_TOLERANCE,
			wasPinned: this.pinIsAtBottom,
			hasMoreAfter: this.props.messages.hasMoreAfter,
			allowPinWhenHasMoreAfter: false,
		});
	}

	scrollGetDocument(): Document | undefined {
		const node = this.ref.current?.getScrollerNode();
		return node?.ownerDocument;
	}

	scrollGetState(): ScrollerState {
		return this.ref.current?.getScrollerState() ?? DEFAULT_SCROLLER_STATE;
	}

	private placeholderHeightGet(): number {
		const scrollerHandle = this.ref.current;
		if (scrollerHandle == null) {
			return this.props.placeholderHeight;
		}
		const scrollerNode = scrollerHandle.getScrollerNode();
		if (scrollerNode == null) {
			return this.props.placeholderHeight;
		}
		return this.props.placeholderHeight * getRemScaleForDocument(scrollerNode.ownerDocument);
	}

	pinIsAtBottomFor(state: ScrollerState = this.scrollGetState()): boolean {
		const pinState = this.pinComputeState(state);
		return pinState.isPinned;
	}

	layoutGetElementFromMessageId(messageId: string): HTMLElement | null {
		const doc = this.scrollGetDocument();
		const {channel} = this.props;
		if (!doc) return null;
		const elementId = `chat-messages-${channel.id}-${messageId}`;
		return doc.getElementById(elementId) as HTMLElement | null;
	}

	private layoutGetContainerLayout(container: HTMLElement): ContainerLayout {
		return {
			rectTop: container.getBoundingClientRect().top,
			scrollTop: container.scrollTop,
		};
	}

	private layoutGetOffsetTop(element: HTMLElement, container: HTMLElement, layout?: ContainerLayout): number {
		const elRect = element.getBoundingClientRect();
		const containerLayout = layout ?? this.layoutGetContainerLayout(container);
		return containerLayout.scrollTop + (elRect.top - containerLayout.rectTop);
	}

	private jumpGetBreathingRoom(): number {
		return NEW_MESSAGES_BAR_BUFFER;
	}

	private layoutGetNodeAlignedScrollTop(
		node: HTMLElement,
		alignment: 'start' | 'center',
		padding: number,
		scrollerNode: HTMLElement,
	): number {
		const state = this.scrollGetState();
		const containerRect = scrollerNode.getBoundingClientRect();
		const nodeRect = node.getBoundingClientRect();
		const delta = nodeRect.top - containerRect.top;
		const nodeOffsetTop = state.scrollTop + delta;
		if (alignment === 'center') {
			const centered = nodeOffsetTop - (state.offsetHeight - nodeRect.height) / 2;
			return Math.min(centered, nodeOffsetTop - padding);
		}
		return nodeOffsetTop - padding;
	}

	private layoutLockNodeToAlignment(
		node: HTMLElement,
		alignment: 'start' | 'center',
		padding: number,
		animate: boolean,
		callback?: () => void,
		shouldContinue?: () => boolean,
	): void {
		const scrollerNode = this.ref.current?.getScrollerNode();
		if (!scrollerNode) {
			callback?.();
			return;
		}
		const target = this.layoutGetNodeAlignedScrollTop(node, alignment, padding, scrollerNode);
		this.scrollTo(target, animate, () => {
			if (this.lifecycleIsDisposed || shouldContinue?.() === false) return;
			callback?.();
		});
	}

	anchorGetData(messageId: string, scrollTop: number, clampTo?: number, layout?: ContainerLayout): AnchorData | null {
		const element = this.layoutGetElementFromMessageId(messageId);
		const scrollerNode = this.ref.current?.getScrollerNode();
		if (!element || !scrollerNode) return null;
		const offsetHeight = element.offsetHeight;
		const offsetTop = this.layoutGetOffsetTop(element, scrollerNode, layout);
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

	layoutGetNewMessageBarBuffer(): number {
		return NEW_MESSAGES_BAR_BUFFER;
	}

	anchorSetAutomatic(anchor: AnchorData | null): void {
		this.anchorAutomatic = anchor;
		for (const cb of this.anchorAutomaticListeners) {
			cb(this.anchorAutomatic, this.anchorBottom);
		}
	}

	anchorClearAutomatic(): void {
		this.anchorSetAutomatic(null);
	}

	anchorFindTopVisible(): AnchorData | null {
		const {messages, hasUnreads, channel} = this.props;
		const state = this.scrollGetState();
		const {scrollTop, offsetHeight} = state;
		const scrollerNode = this.ref.current?.getScrollerNode();
		const layout = scrollerNode ? this.layoutGetContainerLayout(scrollerNode) : undefined;
		const buffer =
			hasUnreads && scrollTop >= this.layoutGetNewMessageBarBuffer() ? this.layoutGetNewMessageBarBuffer() : 0;
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
			const anchorData = this.anchorGetData(messageId, scrollTop, undefined, layout);
			this.anchorBottom = anchorData;
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

	anchorFindLoadMore(isBefore: boolean): AnchorData | null {
		const {messages} = this.props;
		const {scrollTop} = this.scrollGetState();
		const scrollerNode = this.ref.current?.getScrollerNode();
		const layout = scrollerNode ? this.layoutGetContainerLayout(scrollerNode) : undefined;
		const direction = isBefore ? 1 : -1;
		const startIndex = isBefore ? 0 : messages.length - 1;
		let anchor: AnchorData | null = null;
		for (let i = startIndex; messages.getByIndex(i) != null; i += direction) {
			const msg = messages.getByIndex(i)!;
			const data = this.anchorGetData(msg.id, scrollTop, undefined, layout);
			if (data) {
				anchor = data;
				break;
			}
		}
		return anchor;
	}

	private anchorFindViewportPreservation(): AnchorData | null {
		return this.anchorFindTopVisible();
	}

	anchorGetFixData(): {
		node: HTMLElement;
		fixedScrollTop: number;
	} | null {
		const candidates = [
			this.anchorFocus,
			this.loadIsInProgress() ? null : this.anchorFetch,
			this.loadIsInProgress() ? null : this.anchorFetchFallback,
			this.anchorAutomatic,
		];
		const scrollerNode = this.ref.current?.getScrollerNode();
		if (!scrollerNode) return null;
		const layout = this.layoutGetContainerLayout(scrollerNode);
		for (const anchor of candidates) {
			if (!anchor) continue;
			const element = this.layoutGetElementFromMessageId(anchor.id);
			if (!element) continue;
			const currentOffsetTop = this.layoutGetOffsetTop(element, scrollerNode, layout);
			const fixedScrollTop = Math.max(0, currentOffsetTop - anchor.offsetFromTop);
			return {
				node: element,
				fixedScrollTop,
			};
		}
		return null;
	}

	private jumpComplete(focusElement?: HTMLElement | null, jumpToken?: number): void {
		if (jumpToken != null && !this.jumpIsCurrent(jumpToken)) {
			return;
		}
		this.jumpIsActive = false;
		this.jumpSequenceId = null;
		this.jumpCallbackToken += 1;
		const state = this.scrollGetState();
		const pinState = this.pinComputeState(state);
		if (pinState.isPinned) {
			this.pinIsAtBottom = true;
		}
		if (focusElement) {
			if (focusElement.tabIndex < 0) {
				focusElement.tabIndex = -1;
			}
			focusElement.focus({preventScroll: true});
		}
		this.scrollHandle();
		Messages.handleClearJumpTarget({channelId: this.props.channel.id});
	}

	anchorFixScrollPosition(): boolean {
		const anchorData = this.anchorGetFixData();
		if (!anchorData) {
			this.scrollHandle();
			return false;
		}
		const {node, fixedScrollTop} = anchorData;
		if (this.anchorFocus) {
			if (this.pinIsAtBottomNow()) {
				this.scrollTo(Number.MAX_SAFE_INTEGER, false, this.scrollHandle);
			} else {
				this.scrollMergeTo(fixedScrollTop, this.scrollHandle);
			}
			this.ref.current?.scrollIntoViewNode({
				node,
				padding: 16 + this.props.additionalMessagePadding,
				callback: this.scrollHandle,
			});
			if (KeyboardMode.keyboardModeEnabled && this.anchorFocus) {
				const elementToFocus = this.layoutGetElementFromMessageId(this.anchorFocus.id);
				if (elementToFocus) {
					elementToFocus.focus({preventScroll: true});
				}
			}
			if (!this.loadIsInProgress()) {
				this.anchorFocus = null;
			}
		} else {
			this.scrollMergeTo(fixedScrollTop, this.scrollHandle);
		}
		if (!this.loadIsInProgress()) {
			this.anchorFetch = null;
			this.anchorFetchFallback = null;
		}
		return true;
	}

	anchorHasAny(): boolean {
		return !!this.anchorFocus || !!this.anchorFetch || !!this.anchorFetchFallback || !!this.anchorAutomatic;
	}

	anchorUpdateFocus(
		messageId: string | null | undefined,
		scrollTop: number,
		offsetHeight: number,
		layout?: ContainerLayout,
	): void {
		if (messageId) {
			this.anchorFocus = this.anchorGetData(messageId, scrollTop, undefined, layout);
		}
		const anchor = this.anchorFocus;
		if (!anchor) return;
		if (anchor.offsetFromTop >= offsetHeight || scrollTop > anchor.offsetTop + anchor.offsetHeight) {
			this.anchorFocus = null;
		}
	}

	anchorHandleFocusScroll(scrollTop: number, offsetHeight: number): void {
		this.anchorUpdateFocus(this.anchorFocus?.id ?? null, scrollTop, offsetHeight);
	}

	anchorUpdateFetch(scrollTop: number, offsetHeight: number, scrollHeight: number, passLayout?: ContainerLayout): void {
		const scrollerNode = this.ref.current?.getScrollerNode();
		if ((!this.anchorFetch && !this.anchorFetchFallback) || !scrollerNode) return;
		const region = this.scrollIsInPlaceholderRegion({scrollTop, offsetHeight, scrollHeight});
		const clampTo = region !== ScrollRegion.None ? offsetHeight : undefined;
		const layout = passLayout ?? this.layoutGetContainerLayout(scrollerNode);
		if (this.anchorFetch) {
			this.anchorFetch = this.anchorGetData(this.anchorFetch.id, scrollTop, clampTo, layout);
		}
		if (this.anchorFetchFallback) {
			this.anchorFetchFallback = this.anchorGetData(this.anchorFetchFallback.id, scrollTop, clampTo, layout);
		}
	}

	anchorUpdateAutomatic(scrollTop: number, layout?: ContainerLayout): void {
		const scrollerNode = this.ref.current?.getScrollerNode();
		if (!this.anchorAutomatic || !scrollerNode) return;
		const anchorData = this.anchorGetData(this.anchorAutomatic.id, scrollTop, undefined, layout);
		if (!anchorData) {
			this.anchorSetAutomatic(null);
			return;
		}
		this.anchorSetAutomatic(anchorData);
	}

	layoutIsHeightChange(offsetHeight: number, scrollHeight: number): boolean {
		return offsetHeight !== this.cacheOffsetHeight || scrollHeight !== this.cacheScrollHeight;
	}

	scrollIsInPlaceholderRegion(state: ScrollerState): ScrollRegion {
		const {scrollTop, offsetHeight, scrollHeight} = state;
		const {messages} = this.props;
		const placeholderHeight = this.placeholderHeightGet();
		if (messages.hasMoreBefore && scrollTop < placeholderHeight && scrollHeight > offsetHeight) {
			return ScrollRegion.Top;
		}
		if (messages.hasMoreAfter && scrollTop >= scrollHeight - offsetHeight - placeholderHeight) {
			return ScrollRegion.Bottom;
		}
		return ScrollRegion.None;
	}

	loadGetOffsetToTrigger(edge: 'top' | 'bottom', state: ScrollerState): number {
		const {scrollHeight, offsetHeight} = state;
		const {messages, hasUnreads} = this.props;
		const placeholderHeight = this.placeholderHeightGet();
		if (edge === 'top') {
			if (!messages.hasMoreBefore) {
				return 0;
			}
			return hasUnreads ? placeholderHeight - NEW_MESSAGES_BAR_BUFFER - 2 : placeholderHeight + 500;
		}
		return messages.hasMoreAfter ? scrollHeight - offsetHeight - placeholderHeight - 500 : scrollHeight - offsetHeight;
	}

	loadGetOffsetToPrevent(edge: 'top' | 'bottom'): number {
		const {messages} = this.props;
		let delta = 0;
		if (edge === 'top' && messages.hasMoreBefore) {
			delta = 2;
		} else if (edge === 'bottom' && messages.hasMoreAfter) {
			delta = -2;
		}
		return this.loadGetOffsetToTrigger(edge, this.scrollGetState()) + delta;
	}

	loadIsInTriggerRegion(state: ScrollerState): ScrollRegion {
		const {scrollTop, offsetHeight, scrollHeight} = state;
		const {messages} = this.props;
		if (
			messages.hasMoreBefore &&
			scrollTop <= this.loadGetOffsetToTrigger('top', state) &&
			scrollHeight > offsetHeight
		) {
			return ScrollRegion.Top;
		}
		if (messages.hasMoreAfter && scrollTop >= this.loadGetOffsetToTrigger('bottom', state)) {
			return ScrollRegion.Bottom;
		}
		return ScrollRegion.None;
	}

	scrollHandleSpeed(state: ScrollerState): void {
		if (this.jumpIsActiveNow() || this.dragIsActiveNow() || this.loadIsInProgress() || !this.props.canLoadMore) {
			return;
		}
		const {scrollTop, offsetHeight, scrollHeight} = state;
		const prev = this.cachePreviousScrollTop;
		const placeholderHeight = this.placeholderHeightGet();
		this.cachePreviousScrollTop = scrollTop;
		if (prev == null) return;
		const region = this.scrollIsInPlaceholderRegion(state);
		const delta = scrollTop - prev;
		if (region === ScrollRegion.None || delta === 0) return;
		if (region === ScrollRegion.Top && scrollTop + delta <= 0) {
			const newTop = placeholderHeight - offsetHeight;
			this.scrollMergeTo(newTop);
			this.cachePreviousScrollTop = newTop;
		} else if (region === ScrollRegion.Bottom && scrollTop + delta >= scrollHeight - offsetHeight) {
			const newTop = scrollHeight - placeholderHeight;
			this.scrollMergeTo(newTop);
			this.cachePreviousScrollTop = newTop;
		}
	}

	private focusGetMessageIdInScroller(): string | null {
		const focusedElement = document.activeElement;
		const scrollerNode = this.ref.current?.getScrollerNode();
		if (!focusedElement || !scrollerNode?.contains(focusedElement)) {
			return null;
		}
		return (focusedElement as HTMLElement).dataset?.messageId ?? null;
	}

	private anchorCaptureFocused(): boolean {
		const messageId = this.focusGetMessageIdInScroller();
		if (!messageId) {
			return false;
		}
		const {scrollTop} = this.scrollGetState();
		const anchor = this.anchorGetData(messageId, scrollTop);
		if (!anchor) {
			return false;
		}
		this.anchorFocus = anchor;
		return true;
	}

	private loadUpdateSnapshot(loadAfter: boolean, scrollTop: number, scrollHeight: number): void {
		if (!loadAfter) {
			this.loadPrependSnapshot = {
				scrollTop,
				scrollHeight,
			};
			this.loadLastDirection = 'before';
			return;
		}
		this.loadPrependSnapshot = null;
		this.loadLastDirection = 'after';
	}

	private anchorUpdateFetchForLoad(loadAfter: boolean): void {
		const viewportAnchor = this.anchorFindViewportPreservation();
		this.anchorFetch = viewportAnchor ?? this.anchorFindLoadMore(loadAfter);
		this.anchorFetchFallback = viewportAnchor ? this.anchorFindLoadMore(loadAfter) : null;
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
		const {scrollTop, scrollHeight} = this.scrollGetState();
		this.loadUpdateSnapshot(loadAfter, scrollTop, scrollHeight);
		if (KeyboardMode.keyboardModeEnabled) {
			this.anchorCaptureFocused();
		}
		this.anchorUpdateFetchForLoad(loadAfter);
		this.cachePreviousScrollTop = null;
		this.loadIsActive = true;
		MessageCommands.fetchMessages(channel.id, beforeId ?? null, afterId ?? null, MAX_MESSAGES_PER_CHANNEL);
	};

	private anchorScheduleScrollIdleCheck(): void {
		this.anchorTimeoutDeadline = performance.now() + ANCHOR_SCROLL_IDLE_MS;
		if (this.anchorTimeoutId != null) {
			return;
		}
		const run = () => {
			if (this.lifecycleIsDisposed) {
				this.anchorTimeoutId = null;
				return;
			}
			const remainingMs = this.anchorTimeoutDeadline - performance.now();
			if (remainingMs > 0) {
				this.anchorTimeoutId = window.setTimeout(run, remainingMs);
				return;
			}
			this.anchorTimeoutId = null;
			this.cachePreviousScrollTop = null;
			const {scrollHeight, offsetHeight} = this.scrollGetState();
			if (this.layoutIsHeightChange(offsetHeight, scrollHeight)) {
				this.scrollHandle();
			} else if (!this.pinIsAtBottomNow() && !this.anchorAutomatic) {
				this.anchorSetAutomatic(this.anchorFindTopVisible());
			}
		};
		this.anchorTimeoutId = window.setTimeout(run, ANCHOR_SCROLL_IDLE_MS);
	}

	loadMoreForKeyboardNavigation(loadAfter: boolean): void {
		if (this.loadIsActive || this.props.messages.loadingMore) return;
		this.anchorCaptureFocused();
		this.loadMore(loadAfter);
	}

	scrollHandle = (event?: React.UIEvent<HTMLDivElement> | Event): void => {
		if (this.lifecycleIsDisposed) return;
		if (!this.lifecycleIsInitialized()) return;
		const state = this.scrollGetState();
		const pinState = this.pinComputeState(state);
		const heightChanged =
			state.offsetHeight !== this.cacheOffsetHeight || state.scrollHeight !== this.cacheScrollHeight;
		const isAtBottom =
			(heightChanged && this.pinIsAtBottom) || this.pinIsScrollingProgrammatically ? true : pinState.isPinned;
		if (isAtBottom !== this.pinIsCurrentlyAtBottom) {
			this.pinIsCurrentlyAtBottom = isAtBottom;
			if (isAtBottom) {
				this.props.handleScrollToBottom();
			} else {
				this.props.handleScrollFromBottom();
			}
		}
		if (heightChanged) {
			if (this.anchorTimeoutId != null) {
				clearTimeout(this.anchorTimeoutId);
				this.anchorTimeoutId = null;
				this.anchorTimeoutDeadline = 0;
			}
			if (!this.pinIsAtBottomNow() && !this.anchorAutomatic && !this.jumpIsActiveNow()) {
				this.anchorSetAutomatic(this.anchorFindTopVisible());
			}
			this.cacheScrollTop = state.scrollTop;
			this.scrollFixPosition(state.offsetHeight, state.scrollHeight, isAtBottom);
		} else {
			if (event && event.target !== this.ref.current?.getScrollerNode()) {
				return;
			}
			if (this.cacheScrollTop !== state.scrollTop) {
				this.pinIsAtBottom = isAtBottom;
				if (this.pinIsAtBottom) {
					this.anchorClearAutomatic();
				} else if (this.anchorAutomatic) {
					this.anchorUpdateAutomatic(state.scrollTop);
				} else {
					this.anchorSetAutomatic(this.anchorFindTopVisible());
				}
				this.cacheScrollTop = state.scrollTop;
				this.anchorScheduleScrollIdleCheck();
			}
		}
		this.anchorHandleFocusScroll(state.scrollTop, state.offsetHeight);
		this.dimensionPersistDebounced();
		if (this.loadIsDisabled()) {
			this.scrollHandleSpeed(state);
			return;
		}
		const loadingRegion = this.loadIsInTriggerRegion(state);
		if (loadingRegion === ScrollRegion.Top) {
			this.loadMore();
		} else if (loadingRegion === ScrollRegion.Bottom) {
			this.loadMore(true);
		}
		this.scrollHandleSpeed(state);
	};
	scrollHandleResize = (_entry: ResizeObserverEntry, type: 'container' | 'content'): void => {
		if (this.lifecycleIsDisposed) return;
		if (this.resizeReentryGuard) return;
		this.resizeReentryGuard = true;
		try {
			if (type === 'container') {
				const {offsetHeight} = this.scrollGetState();
				const previousHeight = this.resizeContainerHeight;
				this.resizeContainerHeight = offsetHeight;
				const heightDelta = previousHeight === -1 ? 0 : previousHeight - offsetHeight;
				this.layoutHandleResized(heightDelta === 0 ? undefined : heightDelta);
				return;
			}
			this.layoutHandleResized();
		} finally {
			this.resizeReentryGuard = false;
		}
	};

	layoutHandleResized(heightDelta?: number): void {
		if (this.lifecycleIsDisposed || !this.lifecycleIsInitialized()) return;
		if (typeof heightDelta === 'number' && this.layoutApplyShift(heightDelta)) {
			return;
		}
		const state = this.scrollGetState();
		const hasResized = this.layoutIsHeightChange(state.offsetHeight, state.scrollHeight);
		if (!hasResized) {
			this.scrollHandle();
			return;
		}
		const shouldStickToBottom =
			this.pinIsAtBottomNow() || this.pinComputeState(state).isPinned || this.pinIsScrollingProgrammatically;
		if (!shouldStickToBottom && !this.anchorAutomatic && !this.jumpIsActiveNow()) {
			this.anchorSetAutomatic(this.anchorFindTopVisible());
		}
		this.scrollFixPosition(state.offsetHeight, state.scrollHeight, shouldStickToBottom);
	}

	dragHandleMouseDown = (event: React.MouseEvent): void => {
		if (this.lifecycleIsDisposed) return;
		if (event.target === event.currentTarget) {
			this.dragIsActive = true;
		}
	};
	dragHandleMouseUp = (): void => {
		if (this.lifecycleIsDisposed) return;
		this.dragIsActive = false;
		this.scrollHandle();
	};

	scrollFixPosition(offsetHeight: number, scrollHeight: number, forceAtBottom = false): void {
		this.cacheOffsetHeight = offsetHeight;
		this.cacheScrollHeight = scrollHeight;
		if (this.loadPrependSnapshot && !this.loadIsInProgress() && this.loadLastDirection === 'before') {
			const {scrollTop: prevScrollTop, scrollHeight: prevScrollHeight} = this.loadPrependSnapshot;
			this.loadPrependSnapshot = null;
			this.loadLastDirection = null;
			if (this.anchorGetFixData()) {
				this.anchorFixScrollPosition();
				return;
			}
			const addedHeight = scrollHeight - prevScrollHeight;
			if (addedHeight !== 0) {
				const currentState = this.scrollGetState();
				const maxScroll = Math.max(0, scrollHeight - currentState.offsetHeight);
				const targetScrollTop = Math.max(0, Math.min(prevScrollTop + addedHeight, maxScroll));
				this.pinIsAtBottom = false;
				this.scrollMergeTo(targetScrollTop, this.scrollHandle);
				return;
			}
		}
		if (this.loadLastDirection === 'after' && !this.loadIsInProgress()) {
			this.loadLastDirection = null;
			if (!forceAtBottom && !this.pinIsAtBottom) {
				this.anchorFixScrollPosition();
				return;
			}
		}
		if (this.jumpIsActiveNow()) {
			this.jumpFixTarget();
			return;
		}
		const currentState = this.scrollGetState();
		const currentPinState = this.pinComputeState(currentState);
		const hasMoreAfter = this.props.messages.hasMoreAfter;
		const atBottom = !hasMoreAfter && (forceAtBottom || this.pinIsAtBottomNow() || currentPinState.isPinned);
		if (atBottom) {
			this.pinIsScrollingProgrammatically = true;
			this.scrollTo(Number.MAX_SAFE_INTEGER, false, () => {
				this.pinIsScrollingProgrammatically = false;
				this.pinIsAtBottom = true;
				this.scrollHandle();
			});
			return;
		}
		this.anchorFixScrollPosition();
	}

	private jumpFixTarget(): void {
		const {messages} = this.props;
		const jumpToken = this.jumpCallbackToken;
		const targetId = messages.jumpTargetId ? resolveJumpTargetId(messages) : null;
		if (targetId) {
			const element = this.layoutGetElementFromMessageId(targetId);
			if (element) {
				const padding = this.jumpGetBreathingRoom();
				const scrollerNode = this.ref.current?.getScrollerNode();
				if (!scrollerNode) return;
				const targetScrollTop = this.layoutGetNodeAlignedScrollTop(element, 'center', padding, scrollerNode);
				this.scrollMergeTo(targetScrollTop, () => this.jumpComplete(element, jumpToken));
				return;
			}
			this.scrollToNewMessages('top', () => this.jumpComplete(null, jumpToken), false, false, jumpToken);
			return;
		}
		if (!messages.hasMoreAfter) {
			this.scrollTo(Number.MAX_SAFE_INTEGER, false);
		}
	}

	scrollToNewMessages(
		orientation: 'top' | 'middle' = 'top',
		callback?: () => void,
		animate = true,
		suppressPadding = false,
		jumpToken = this.jumpBegin(this.jumpSequenceId),
	): void {
		const doc = this.scrollGetDocument();
		const newMessagesBar = doc?.getElementById('new-messages-bar');
		const shouldContinue = () => this.jumpIsCurrent(jumpToken);
		const onComplete = () => {
			if (!shouldContinue()) {
				return;
			}
			this.jumpIsActive = false;
			this.jumpSequenceId = null;
			this.anchorSetAutomatic(this.anchorFindTopVisible());
			callback?.();
			if (this.jumpIsCurrent(jumpToken)) {
				this.jumpCallbackToken += 1;
			}
			this.scrollHandle();
		};
		this.pinIsAtBottom = false;
		const padding = suppressPadding ? 0 : this.jumpGetBreathingRoom();
		if (newMessagesBar) {
			if (orientation === 'middle') {
				const scrollerNode = this.ref.current?.getScrollerNode();
				if (!scrollerNode) {
					this.scrollTo(Number.MAX_SAFE_INTEGER, animate, onComplete);
					return;
				}
				const {offsetHeight} = this.scrollGetState();
				const containerRect = scrollerNode.getBoundingClientRect();
				const nodeRect = newMessagesBar.getBoundingClientRect();
				const delta = nodeRect.top - containerRect.top;
				const nodeOffsetTop = scrollerNode.scrollTop + delta;
				const middleTarget = nodeOffsetTop - 0.5 * offsetHeight + 0.5 * nodeRect.height;
				const target = Math.min(middleTarget, nodeOffsetTop - padding);
				this.scrollTo(target, animate, onComplete);
				return;
			}
			this.layoutLockNodeToAlignment(newMessagesBar, 'start', padding, animate, onComplete, shouldContinue);
		} else if (!this.props.messages.hasMoreAfter) {
			this.scrollTo(Number.MAX_SAFE_INTEGER, animate, onComplete);
		} else {
			onComplete();
		}
	}

	scrollToBelowUnreadDivider(): void {
		const doc = this.scrollGetDocument();
		const scrollerNode = this.ref.current?.getScrollerNode();
		const newMessagesBar = doc?.getElementById('new-messages-bar');
		if (!scrollerNode || !newMessagesBar) {
			this.scrollSetToBottom();
			return;
		}
		const offsetTop = this.layoutGetOffsetTop(newMessagesBar, scrollerNode);
		const elementHeight = newMessagesBar.offsetHeight;
		const targetScrollTop = offsetTop + elementHeight + 2;
		this.pinIsAtBottom = false;
		this.scrollTo(targetScrollTop, false, () => {
			this.anchorSetAutomatic(this.anchorFindTopVisible());
			this.scrollHandle();
		});
	}

	restoreInitial(): void {
		if (this.lifecycleIsInitialized()) return;
		const {restorePendingInitialScrollTop} = this;
		this.restorePendingInitialScrollTop = undefined;
		const targetId = resolveJumpTargetId(this.props.messages);
		if (targetId != null) {
			this.scrollToMessage(targetId, false);
		} else if (restorePendingInitialScrollTop != null) {
			const targetScroll = restorePendingInitialScrollTop + this.placeholderHeightGet();
			this.scrollTo(targetScroll, false, this.scrollHandle);
		} else if (!this.restoreHadSavedPosition && this.props.hasUnreads) {
			this.scrollToBelowUnreadDivider();
		} else {
			this.scrollSetToBottom();
		}
	}

	scrollTo(position: number, animate = false, callback?: () => void): void {
		if (this.lifecycleIsDisposed) return;
		this.ref.current?.scrollTo({
			to: position,
			animate: Accessibility.useSmoothScrolling && animate,
			callback,
		});
		if (this.pinIsAtBottomNow()) {
			this.dimensionPersist();
		} else {
			this.dimensionPersistDebounced();
		}
	}

	scrollMergeTo(position: number, callback?: () => void): void {
		if (this.lifecycleIsDisposed) return;
		this.ref.current?.mergeTo({
			to: position,
			callback,
		});
		if (this.pinIsAtBottomNow()) {
			this.dimensionPersist();
		} else {
			this.dimensionPersistDebounced();
		}
	}

	scrollSetToBottom(animate = false): void {
		if (this.lifecycleIsDisposed) return;
		const {messages, channel} = this.props;
		this.jumpStop(true);
		Dimension.updateChannelDimensions(channel.id, 1, 1, 0);
		if (messages.hasMoreAfter) {
			MessageCommands.jumpToPresent(channel.id, MAX_MESSAGES_PER_CHANNEL);
		} else {
			this.pinIsScrollingProgrammatically = true;
			this.pinIsAtBottom = true;
			this.scrollTo(Number.MAX_SAFE_INTEGER, animate, () => {
				this.jumpIsActive = false;
				this.jumpSequenceId = null;
				this.pinIsScrollingProgrammatically = false;
				this.scrollHandle();
			});
		}
	}

	scrollHandleUserIntent = (): void => {
		if (this.lifecycleIsDisposed || !this.jumpIsActive) {
			return;
		}
		this.jumpStop();
		this.pinIsScrollingProgrammatically = false;
		Messages.handleClearJumpTarget({channelId: this.props.channel.id});
		const state = this.scrollGetState();
		const pinState = this.pinComputeState(state);
		this.pinIsAtBottom = pinState.isPinned;
		if (pinState.isPinned) {
			this.anchorClearAutomatic();
		} else {
			this.anchorSetAutomatic(this.anchorFindTopVisible());
		}
		this.scrollHandle();
	};

	jumpReturnToOrigin(): boolean {
		if (this.lifecycleIsDisposed) {
			return false;
		}
		const {channel, messages} = this.props;
		const returnTargetId = messages.jumpReturnTargetId;
		if (!returnTargetId) {
			return false;
		}
		const returnChannelId = messages.jumpReturnChannelId ?? channel.id;
		if (returnChannelId !== channel.id) {
			Messages.handleClearJumpTarget({channelId: channel.id, clearReturnTarget: true});
			return false;
		}
		const fallbackReturnGuildId = Navigation.guildId ?? channel.guildId;
		const returnGuildId = messages.jumpReturnGuildId ?? fallbackReturnGuildId;
		this.jumpStop(true);
		this.pinIsScrollingProgrammatically = false;
		Messages.handleClearJumpTarget({channelId: channel.id, clearReturnTarget: true});
		const dispatch = {
			messageId: returnTargetId,
			flash: false,
			jumpType: JumpTypes.INSTANT,
		};
		if (Navigation.channelId === returnChannelId && Navigation.messageId === returnTargetId) {
			MessageCommands.jumpToMessage({
				channelId: returnChannelId,
				...dispatch,
			});
			return true;
		}
		Messages.setPendingJumpDispatch(returnChannelId, dispatch);
		NavigationCommands.navigateToMessage(returnGuildId, returnChannelId, returnTargetId, 'replace');
		return true;
	}

	jumpCancel(): boolean {
		if (this.lifecycleIsDisposed || (!this.jumpIsActive && this.props.messages.jumpTargetId == null)) {
			return false;
		}
		this.jumpStop(true);
		this.pinIsScrollingProgrammatically = false;
		Messages.handleClearJumpTarget({channelId: this.props.channel.id});
		this.scrollHandle();
		return true;
	}

	layoutApplyShift(heightDelta: number): boolean {
		if (this.lifecycleIsDisposed || !this.lifecycleIsInitialized()) return false;
		const scrollerNode = this.ref.current?.getScrollerNode();
		if (!scrollerNode) return false;
		const action = resolveContainerResizeShift({
			heightDelta,
			isPinned: this.pinIsAtBottomNow(),
			editIsActive: this.editIsActive,
			state: this.scrollGetState(),
		});
		if (action.kind === 'pin') {
			this.scrollTo(Number.MAX_SAFE_INTEGER, false, this.scrollHandle);
			return true;
		}
		if (action.kind === 'shift') {
			this.scrollMergeTo(action.targetScrollTop, this.scrollHandle);
			return true;
		}
		return false;
	}

	editEnter(): void {
		this.editIsActive = true;
	}

	editExit(): void {
		if (!this.editIsActive) return;
		this.editIsActive = false;
	}

	private dimensionPersist(callback?: () => void): void {
		if (this.lifecycleIsDisposed) return;
		if (this.jumpIsActiveNow() || !this.lifecycleIsInitialized()) return;
		const {channel} = this.props;
		const placeholderHeight = this.placeholderHeightGet();
		if (this.pinIsAtBottomNow()) {
			Dimension.updateChannelDimensions(channel.id, 1, 1, 0, callback);
		} else {
			const {scrollTop, scrollHeight, offsetHeight} = this.scrollGetState();
			const adjustedScrollTop = scrollTop - placeholderHeight;
			const adjustedScrollHeight = scrollHeight - placeholderHeight;
			Dimension.updateChannelDimensions(channel.id, adjustedScrollTop, adjustedScrollHeight, offsetHeight, callback);
		}
	}

	focusOnMessage(messageId: string): void {
		const element = this.layoutGetElementFromMessageId(messageId);
		if (!element) return;
		const scrollerNode = this.ref.current?.getScrollerNode();
		if (!scrollerNode) return;
		const elementOffset = this.layoutGetOffsetTop(element, scrollerNode);
		const elementHeight = element.offsetHeight;
		const {scrollTop, offsetHeight} = this.scrollGetState();
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
		const anchor = this.anchorGetData(messageId, newScrollTop);
		if (anchor) {
			this.anchorFocus = anchor;
		}
		if (element.tabIndex < 0) {
			element.tabIndex = -1;
		}
		element.focus({preventScroll: true});
	}

	scrollPageUp(animate = false): void {
		this.ref.current?.scrollPageUp({animate});
	}

	scrollPageDown(animate = false): void {
		this.ref.current?.scrollPageDown({animate});
	}

	scrollToMessage(messageId: string, animate = true): void {
		const scrollerHandle = this.ref.current;
		if (!scrollerHandle) return;
		const {jumpFlash, jumpSequenceId, jumpTargetId} = this.props.messages;
		const highlightTargetId = jumpFlash && jumpTargetId !== this.props.channel.id ? jumpTargetId : null;
		this.props.handleJumpHighlight(highlightTargetId, jumpSequenceId);
		if (messageId === this.props.channel.id) {
			this.scrollTo(0);
			return;
		}
		const element = this.layoutGetElementFromMessageId(messageId);
		const scrollerNode = scrollerHandle.getScrollerNode();
		this.pinIsAtBottom = false;
		const jumpToken = this.jumpBegin(this.props.messages.jumpSequenceId);
		const shouldContinue = () => this.jumpIsCurrent(jumpToken);
		const onComplete = () => this.jumpComplete(element, jumpToken);
		if (!element || !scrollerNode) {
			this.scrollToNewMessages('middle', onComplete, animate, false, jumpToken);
			return;
		}
		const padding = this.jumpGetBreathingRoom();
		const target = this.layoutGetNodeAlignedScrollTop(element, 'center', padding, scrollerNode);
		this.anchorFocus = this.anchorGetData(messageId, target);
		this.scrollTo(target, animate, () => {
			if (this.lifecycleIsDisposed || !shouldContinue()) return;
			onComplete();
		});
	}

	lifecycleGetSnapshotBeforeUpdate(focusId: string | null): void {
		if (!this.anchorHasAny() && focusId == null) {
			this.pinPreUpdateState =
				this.pinIsAtBottomNow() || this.pinIsCurrentlyAtBottom || this.pinIsScrollingProgrammatically
					? {
							distanceFromBottom: 0,
							isAtBottom: true,
							isPinned: true,
						}
					: null;
			return;
		}
		const {scrollTop, offsetHeight, scrollHeight} = this.scrollGetState();
		const scrollerNode = this.ref.current?.getScrollerNode();
		const layout = scrollerNode ? this.layoutGetContainerLayout(scrollerNode) : undefined;
		this.pinPreUpdateState = this.pinComputeState({scrollTop, offsetHeight, scrollHeight});
		this.anchorUpdateFocus(focusId, scrollTop, offsetHeight, layout);
		this.anchorUpdateFetch(scrollTop, offsetHeight, scrollHeight, layout);
		this.anchorUpdateAutomatic(scrollTop, layout);
	}

	lifecycleMergeProps(nextProps: ScrollManagerProps): void {
		if (this.lifecycleIsDisposed) return;
		this.propsApplyUpdate(nextProps);
	}

	private propsApplyUpdate(nextProps: ScrollManagerProps): void {
		const prevMessages = this.props.messages;
		const prevFocusId = this.props.focusId;
		const pinPreUpdateState = this.pinTakePreUpdateState();
		this.props = {...nextProps};
		const {offsetHeight, scrollHeight} = this.scrollGetState();
		const heightChanged = this.layoutIsHeightChange(offsetHeight, scrollHeight);
		const shouldForceBottom = pinPreUpdateState?.isPinned ?? false;
		this.cacheOffsetHeight = offsetHeight;
		this.cacheScrollHeight = scrollHeight;
		this.loadIsActive = nextProps.messages.loadingMore;
		if (this.lifecycleIsInitialized() || this.lifecycleIsReady()) {
			if (!this.lifecycleIsInitialized()) {
				this.restoreInitial();
				return;
			}
		} else {
			if (nextProps.messages.jumpTargetId == null) {
				this.scrollTo(Number.MAX_SAFE_INTEGER);
			}
			return;
		}
		if (nextProps.messages.jumpTargetId != null) {
			if (this.loadIsInProgress()) {
				return;
			}
			const targetId = resolveJumpTargetId(nextProps.messages);
			if (targetId == null) {
				nextProps.handleJumpHighlight(null, nextProps.messages.jumpSequenceId);
				this.jumpStop();
				return;
			}
			if (this.jumpSequenceId === nextProps.messages.jumpSequenceId) {
				return;
			}
			if (nextProps.messages.jumpSequenceId === prevMessages.jumpSequenceId && this.jumpIsActiveNow()) {
				return;
			}
			this.scrollToMessage(targetId, shouldAnimateMessageJump(nextProps.messages.jumpType));
			return;
		}
		if (nextProps.messages.jumpedToPresent && prevMessages.jumpSequenceId !== nextProps.messages.jumpSequenceId) {
			nextProps.handleJumpHighlight(null, nextProps.messages.jumpSequenceId);
			this.jumpBegin(nextProps.messages.jumpSequenceId);
			this.scrollTo(0);
			this.scrollSetToBottom();
			return;
		}
		const lastMessage = nextProps.messages.last();
		const prevLastMessage = prevMessages.last();
		if (lastMessage != null && lastMessage.state === 'SENDING' && prevLastMessage?.id !== lastMessage.id) {
			if (this.pinIsAtBottomNow() || (Accessibility.scrollToBottomOnMessageSend && !nextProps.messages.hasMoreAfter)) {
				this.scrollSetToBottom();
			}
			return;
		}
		const {focusId} = this.props;
		if (focusId != null && prevFocusId !== focusId) {
			const el = this.layoutGetElementFromMessageId(focusId);
			if (el) {
				this.ref.current?.scrollIntoViewNode({
					node: el,
					padding: 16 + this.props.additionalMessagePadding,
					callback: this.scrollHandle,
				});
				return;
			}
		}
		if (heightChanged) {
			this.scrollFixPosition(offsetHeight, scrollHeight, shouldForceBottom);
		}
	}

	anchorAddAutomaticListener(
		callback: (anchor: AnchorData | null, bottom: AnchorData | null) => void,
		immediate = true,
	): void {
		if (!this.anchorAutomaticListeners.includes(callback)) {
			this.anchorAutomaticListeners.push(callback);
		}
		if (immediate) {
			this.anchorSetAutomatic(this.anchorFindTopVisible());
		}
	}

	anchorRemoveAutomaticListener(callback: (anchor: AnchorData | null, bottom: AnchorData | null) => void): void {
		this.anchorAutomaticListeners = this.anchorAutomaticListeners.filter((cb) => cb !== callback);
	}

	cleanup(): void {
		this.lifecycleIsDisposed = true;
		this.dimensionPersistDebounced.cancel();
		if (this.anchorTimeoutId != null) {
			clearTimeout(this.anchorTimeoutId);
			this.anchorTimeoutId = null;
		}
		this.anchorTimeoutDeadline = 0;
		this.resizeReentryGuard = false;
		this.loadPrependSnapshot = null;
		this.loadLastDirection = null;
		this.anchorFetch = null;
		this.anchorFetchFallback = null;
		this.pinPreUpdateState = null;
		this.jumpSequenceId = null;
		this.jumpCallbackToken += 1;
		for (const cb of this.anchorAutomaticListeners) {
			this.anchorRemoveAutomaticListener(cb);
		}
	}
}

export function useScrollManager(props: ScrollManagerProps): ScrollManager {
	const [manager] = useState(() => new ScrollManager(props));
	manager.lifecycleGetSnapshotBeforeUpdate(props.focusId);
	useLayoutEffect(() => {
		manager.lifecycleMergeProps(props);
	});
	useLayoutEffect(() => {
		return () => manager.cleanup();
	}, [manager]);
	return manager;
}
