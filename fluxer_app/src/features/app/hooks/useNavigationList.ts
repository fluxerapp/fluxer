// SPDX-License-Identifier: AGPL-3.0-or-later

import {TextEntryElementPolicy} from '@app/features/input/TextEntryElementPolicy';
import {Edge, type VerticalEdge} from '@app/features/ui/AxisOrientation';
import type {ScrollerHandle} from '@app/features/ui/components/Scroller';
import type React from 'react';
import {useCallback, useLayoutEffect, useMemo, useRef, useState} from 'react';

const NAVIGATION_INDEX_ATTRIBUTE = 'data-navigation-index';

export const NavigationAlignment = Object.freeze({
	AUTO: 'auto',
	END: 'end',
	START: 'start',
} as const);

export type NavigationAlignment = (typeof NavigationAlignment)[keyof typeof NavigationAlignment];

export type NavigationEdgeAlignment = typeof NavigationAlignment.END | typeof NavigationAlignment.START;

type NavigationScrollerAlignment = 'start' | 'end' | 'nearest';

const NavigationFocusableEdge = Object.freeze({
	FIRST: 'first',
	LAST: 'last',
} as const);

type NavigationFocusableEdge = (typeof NavigationFocusableEdge)[keyof typeof NavigationFocusableEdge];

export interface NavigationRow {
	readonly key: string;
	readonly focusable: boolean;
	readonly focusTargetIdentity: string | object;
}

export interface NavigationRowBounds {
	readonly top: number;
	readonly bottom: number;
}

export interface NavigationListPosition {
	readonly position: number;
	readonly setSize: number;
}

export interface ResolveMissingNavigationActiveKeyRequest {
	readonly missingActiveKey: string;
}

export type MissingNavigationActiveKeyResolver = (request: ResolveMissingNavigationActiveKeyRequest) => string | null;

interface NavigationListOptions {
	readonly scrollerRef: React.RefObject<ScrollerHandle | null>;
	readonly rows: ReadonlyArray<NavigationRow>;
	readonly focusableSelector: string;
	readonly keyboardNavigationEnabled: boolean;
	readonly preferredActiveKey: string | null;
	readonly resolveMissingActiveKey: MissingNavigationActiveKeyResolver | null;
}

interface NavigationListResult {
	readonly layoutRevision: number;
	readonly navigationRef: React.RefCallback<HTMLElement>;
	readonly onKeyDownCapture: (event: React.KeyboardEvent<HTMLElement>) => void;
	readonly onFocusCapture: (event: React.FocusEvent<HTMLElement>) => void;
	readonly onBlurCapture: (event: React.FocusEvent<HTMLElement>) => void;
	readonly getRowBounds: (keys: ReadonlyArray<string>) => ReadonlyMap<string, NavigationRowBounds>;
	readonly getRowListPosition: (index: number) => NavigationListPosition | null;
	readonly scrollToKey: (key: string, align: NavigationAlignment) => boolean;
}

interface NavigationLayoutState {
	readonly rows: ReadonlyArray<NavigationRow>;
	readonly rowIndexByKey: ReadonlyMap<string, number>;
	readonly focusableRowIndexes: ReadonlyArray<number>;
	readonly focusPositionByRowIndex: ReadonlyMap<number, number>;
}

interface NavigationLayoutOwner {
	readonly layout: NavigationLayout;
	readonly revision: number;
	readonly sourceRows: ReadonlyArray<NavigationRow>;
}

class NavigationLayout {
	readonly rows: ReadonlyArray<NavigationRow>;
	readonly rowIndexByKey: ReadonlyMap<string, number>;
	readonly focusableRowIndexes: ReadonlyArray<number>;
	readonly focusPositionByRowIndex: ReadonlyMap<number, number>;

	private constructor({rows, rowIndexByKey, focusableRowIndexes, focusPositionByRowIndex}: NavigationLayoutState) {
		this.rows = rows;
		this.rowIndexByKey = rowIndexByKey;
		this.focusableRowIndexes = focusableRowIndexes;
		this.focusPositionByRowIndex = focusPositionByRowIndex;
	}

	static fromRows(sourceRows: ReadonlyArray<NavigationRow>): NavigationLayout {
		const rows: Array<NavigationRow> = [];
		const rowIndexByKey = new Map<string, number>();
		const focusableRowIndexes: Array<number> = [];
		const focusPositionByRowIndex = new Map<number, number>();
		for (let index = 0; index < sourceRows.length; index += 1) {
			const sourceRow = sourceRows[index];
			if (sourceRow == null) {
				throw new Error(`Navigation source row ${index} is missing`);
			}
			const row = Object.freeze({
				key: sourceRow.key,
				focusable: sourceRow.focusable,
				focusTargetIdentity: sourceRow.focusTargetIdentity,
			});
			if (rowIndexByKey.has(row.key)) {
				throw new Error(`Navigation row key ${row.key} is duplicated`);
			}
			rows.push(row);
			rowIndexByKey.set(row.key, index);
			if (!row.focusable) {
				continue;
			}
			focusPositionByRowIndex.set(index, focusableRowIndexes.length);
			focusableRowIndexes.push(index);
		}
		return new NavigationLayout({
			rows: Object.freeze(rows),
			rowIndexByKey,
			focusableRowIndexes: Object.freeze(focusableRowIndexes),
			focusPositionByRowIndex,
		});
	}

	matches(sourceRows: ReadonlyArray<NavigationRow>): boolean {
		if (this.rows.length !== sourceRows.length) {
			return false;
		}
		for (let index = 0; index < sourceRows.length; index += 1) {
			const current = this.rows[index];
			if (current == null) {
				throw new Error(`Navigation layout row ${index} is missing`);
			}
			const source = sourceRows[index];
			if (source == null) {
				throw new Error(`Navigation source row ${index} is missing`);
			}
			if (current.key !== source.key) {
				return false;
			}
			if (current.focusable !== source.focusable) {
				return false;
			}
		}
		return true;
	}

	isFocusableRow(index: number): boolean {
		const row = this.rows[index];
		if (row == null) {
			return false;
		}
		return row.focusable;
	}

	resolveListPosition(index: number): NavigationListPosition | null {
		const position = this.focusPositionByRowIndex.get(index);
		if (position == null) {
			return null;
		}
		return {
			position: position + 1,
			setSize: this.focusableRowIndexes.length,
		};
	}

	resolveActiveKey(preferredActiveKey: string | null): string | null {
		if (preferredActiveKey == null) {
			return this.resolveFirstFocusableKey();
		}
		const preferredIndex = this.rowIndexByKey.get(preferredActiveKey);
		if (preferredIndex == null) {
			return this.resolveFirstFocusableKey();
		}
		if (!this.isFocusableRow(preferredIndex)) {
			return this.resolveFirstFocusableKey();
		}
		return preferredActiveKey;
	}

	private resolveFirstFocusableKey(): string | null {
		const firstFocusableIndex = this.focusableRowIndexes[0];
		if (firstFocusableIndex == null) {
			return null;
		}
		const firstFocusableRow = this.rows[firstFocusableIndex];
		if (firstFocusableRow == null) {
			throw new Error(`Navigation focusable row ${firstFocusableIndex} is missing`);
		}
		return firstFocusableRow.key;
	}
}

function nextNavigationLayoutRevision(currentRevision: number): number {
	if (!Number.isSafeInteger(currentRevision)) {
		throw new Error('Navigation layout revision is not a safe integer');
	}
	if (currentRevision < 0) {
		throw new Error('Navigation layout revision is negative');
	}
	if (currentRevision >= Number.MAX_SAFE_INTEGER) {
		throw new Error('Navigation layout revision exhausted the safe integer range');
	}
	return currentRevision + 1;
}

function useNavigationLayout(rows: ReadonlyArray<NavigationRow>): NavigationLayoutOwner {
	const ownerRef = useRef<NavigationLayoutOwner | null>(null);
	const current = ownerRef.current;
	if (current == null) {
		const initial = NavigationLayout.fromRows(rows);
		const initialOwner = {layout: initial, revision: 0, sourceRows: rows};
		ownerRef.current = initialOwner;
		return initialOwner;
	}
	if (current.sourceRows === rows) {
		return current;
	}
	if (current.layout.matches(rows)) {
		const matchingOwner = {layout: current.layout, revision: current.revision, sourceRows: rows};
		ownerRef.current = matchingOwner;
		return matchingOwner;
	}
	const nextOwner = {
		layout: NavigationLayout.fromRows(rows),
		revision: nextNavigationLayoutRevision(current.revision),
		sourceRows: rows,
	};
	ownerRef.current = nextOwner;
	return nextOwner;
}

function resolveInitialActiveKey(preferredActiveKey: string | null): string | null {
	if (preferredActiveKey == null) {
		return null;
	}
	return preferredActiveKey;
}

interface ResolveOptionalRowIndexQuery {
	readonly key: string | null;
	readonly rowIndexByKey: ReadonlyMap<string, number>;
}

function resolveOptionalRowIndex({key, rowIndexByKey}: ResolveOptionalRowIndexQuery): number | null {
	if (key == null) {
		return null;
	}
	const rowIndex = rowIndexByKey.get(key);
	if (rowIndex == null) {
		return null;
	}
	return rowIndex;
}

function resolveScrollNode(scrollerRef: React.RefObject<ScrollerHandle | null>): HTMLElement | null {
	const scroller = scrollerRef.current;
	if (scroller == null) {
		return null;
	}
	return scroller.getScrollerNode();
}

interface ResolveNavigationRowNodeQuery {
	readonly index: number;
	readonly navigationNode: HTMLElement | null;
}

function resolveNavigationRowNode({index, navigationNode}: ResolveNavigationRowNodeQuery): HTMLElement | null {
	if (navigationNode == null) {
		return null;
	}
	return navigationNode.querySelector<HTMLElement>(`[${NAVIGATION_INDEX_ATTRIBUTE}="${index}"]`);
}

function resolveNavigationScrollerAlignment(align: NavigationAlignment): NavigationScrollerAlignment {
	if (align === NavigationAlignment.START) {
		return 'start';
	}
	if (align === NavigationAlignment.END) {
		return 'end';
	}
	return 'nearest';
}

interface ScrollNavigationRowIntoViewRequest {
	readonly align: NavigationAlignment;
	readonly index: number;
	readonly navigationNode: HTMLElement | null;
	readonly scrollerRef: React.RefObject<ScrollerHandle | null>;
}

function scrollNavigationRowIntoView({
	align,
	index,
	navigationNode,
	scrollerRef,
}: ScrollNavigationRowIntoViewRequest): boolean {
	const scroller = scrollerRef.current;
	if (scroller == null) {
		return false;
	}
	const node = resolveNavigationRowNode({index, navigationNode});
	if (node == null) {
		return false;
	}
	scroller.scrollIntoViewNode({node, alignment: resolveNavigationScrollerAlignment(align)});
	return true;
}

interface ResolveNavigationRowBoundsRequest {
	readonly keys: ReadonlyArray<string>;
	readonly navigationNode: HTMLElement | null;
	readonly rowIndexByKey: ReadonlyMap<string, number>;
	readonly scrollerRef: React.RefObject<ScrollerHandle | null>;
}

function resolveNavigationRowBounds({
	keys,
	navigationNode,
	rowIndexByKey,
	scrollerRef,
}: ResolveNavigationRowBoundsRequest): ReadonlyMap<string, NavigationRowBounds> {
	const bounds = new Map<string, NavigationRowBounds>();
	if (keys.length === 0) {
		return bounds;
	}
	if (navigationNode == null) {
		return bounds;
	}
	const scrollNode = resolveScrollNode(scrollerRef);
	if (scrollNode == null) {
		return bounds;
	}
	const containerRect = scrollNode.getBoundingClientRect();
	const scrollTop = scrollNode.scrollTop;
	for (const key of keys) {
		const index = rowIndexByKey.get(key);
		if (index == null) {
			continue;
		}
		const node = resolveNavigationRowNode({index, navigationNode});
		if (node == null) {
			continue;
		}
		if (!node.isConnected) {
			continue;
		}
		if (!scrollNode.contains(node)) {
			continue;
		}
		const rect = node.getBoundingClientRect();
		bounds.set(key, {
			top: scrollTop + rect.top - containerRect.top,
			bottom: scrollTop + rect.bottom - containerRect.top,
		});
	}
	return bounds;
}

interface ResolveFocusableElementQuery {
	readonly edge: NavigationFocusableEdge;
	readonly focusableSelector: string;
	readonly row: Element | null;
}

function resolveFocusableElement({row, focusableSelector, edge}: ResolveFocusableElementQuery): HTMLElement | null {
	if (row == null) {
		return null;
	}
	const focusableElements = row.querySelectorAll<HTMLElement>(focusableSelector);
	if (focusableElements.length === 0) {
		return null;
	}
	if (edge === NavigationFocusableEdge.LAST) {
		return focusableElements.item(focusableElements.length - 1);
	}
	return focusableElements.item(0);
}

interface ResolveCurrentNavigationRowRequest {
	readonly eventTarget: EventTarget;
	readonly navigationNode: HTMLElement;
}

function resolveCurrentNavigationRow({
	eventTarget,
	navigationNode,
}: ResolveCurrentNavigationRowRequest): Element | null {
	const ownerWindow = navigationNode.ownerDocument.defaultView;
	if (ownerWindow == null) {
		return null;
	}
	if (!(eventTarget instanceof ownerWindow.Element)) {
		return null;
	}
	return eventTarget.closest(`[${NAVIGATION_INDEX_ATTRIBUTE}]`);
}

function resolveCurrentNavigationIndex(currentRow: Element | null, activeRowIndex: number | null): number | null {
	if (currentRow == null) {
		return activeRowIndex;
	}
	const parsedIndex = Number(currentRow.getAttribute(NAVIGATION_INDEX_ATTRIBUTE));
	if (!Number.isInteger(parsedIndex)) {
		return activeRowIndex;
	}
	return parsedIndex;
}

function resolveFocusablePosition(
	rowIndex: number | null,
	focusPositionByRowIndex: ReadonlyMap<number, number>,
): number | null {
	if (rowIndex == null) {
		return null;
	}
	const focusablePosition = focusPositionByRowIndex.get(rowIndex);
	if (focusablePosition == null) {
		return null;
	}
	return focusablePosition;
}

function resolveArrowDelta(key: string): number {
	if (key === 'ArrowDown') {
		return 1;
	}
	return -1;
}

interface ResolveNextFocusablePositionArgs {
	readonly key: string;
	readonly currentPosition: number | null;
	readonly focusableRowCount: number;
}

function resolveNextFocusablePosition({
	key,
	currentPosition,
	focusableRowCount,
}: ResolveNextFocusablePositionArgs): number {
	if (key === 'Home') {
		return 0;
	}
	if (key === 'End') {
		return focusableRowCount - 1;
	}
	if (currentPosition == null) {
		if (key === 'ArrowDown') {
			return 0;
		}
		return focusableRowCount - 1;
	}
	const delta = resolveArrowDelta(key);
	return (currentPosition + delta + focusableRowCount) % focusableRowCount;
}

function resolveFocusableEdge(key: string): NavigationFocusableEdge {
	if (key === 'ArrowUp') {
		return NavigationFocusableEdge.LAST;
	}
	if (key === 'End') {
		return NavigationFocusableEdge.LAST;
	}
	return NavigationFocusableEdge.FIRST;
}

interface FocusRenderedNavigationRowRequest {
	readonly edge: NavigationFocusableEdge;
	readonly focusableSelector: string;
	readonly index: number;
	readonly navigationNode: HTMLElement | null;
	readonly shouldFocus: () => boolean;
}

interface OwnedAnimationFrame {
	readonly id: number;
	readonly ownerWindow: Window;
}

function focusRenderedNavigationRow({
	edge,
	focusableSelector,
	index,
	navigationNode,
	shouldFocus,
}: FocusRenderedNavigationRowRequest): boolean {
	if (navigationNode == null) return false;
	if (!shouldFocus()) return false;
	const row = resolveNavigationRowNode({index, navigationNode});
	const target = resolveFocusableElement({row, focusableSelector, edge});
	if (target == null) return false;
	target.focus();
	return target.ownerDocument.activeElement === target;
}

function cancelOwnedAnimationFrame(frame: OwnedAnimationFrame | null): void {
	if (frame == null) return;
	frame.ownerWindow.cancelAnimationFrame(frame.id);
}

function shouldAlwaysFocusNavigationRow(): boolean {
	return true;
}

type FocusNavigationRow = (request: FocusRenderedNavigationRowRequest) => boolean;

function useNavigationFocusFrame(navigationNode: HTMLElement | null): FocusNavigationRow {
	const frameRef = useRef<OwnedAnimationFrame | null>(null);
	const cancelPendingFocus = useCallback(() => {
		cancelOwnedAnimationFrame(frameRef.current);
		frameRef.current = null;
	}, []);
	const focusRow = useCallback(
		(request: FocusRenderedNavigationRowRequest): boolean => {
			cancelPendingFocus();
			if (focusRenderedNavigationRow(request)) return true;
			if (navigationNode == null) return false;
			if (!request.shouldFocus()) return false;
			const ownerWindow = navigationNode.ownerDocument.defaultView;
			if (ownerWindow == null) return false;
			let frame: OwnedAnimationFrame | null = null;
			const id = ownerWindow.requestAnimationFrame(() => {
				if (frameRef.current !== frame) return;
				frameRef.current = null;
				focusRenderedNavigationRow(request);
			});
			frame = {id, ownerWindow};
			frameRef.current = frame;
			return false;
		},
		[cancelPendingFocus, navigationNode],
	);
	useLayoutEffect(() => cancelPendingFocus, [cancelPendingFocus, navigationNode]);
	return focusRow;
}

interface HandleNavigationKeyRequest {
	readonly activeRowIndex: number | null;
	readonly event: React.KeyboardEvent<HTMLElement>;
	readonly focusPositionByRowIndex: ReadonlyMap<number, number>;
	readonly focusableRowIndexes: ReadonlyArray<number>;
	readonly focusableSelector: string;
	readonly keyboardNavigationEnabled: boolean;
	readonly moveFocus: (rowIndex: number, edge: NavigationFocusableEdge) => void;
}

interface FocusWithinNavigationRowRequest {
	readonly currentRow: Element | null;
	readonly event: React.KeyboardEvent<HTMLElement>;
	readonly focusableSelector: string;
}

function isNavigationKey(key: string): boolean {
	switch (key) {
		case 'ArrowDown':
		case 'ArrowUp':
		case 'Home':
		case 'End':
			return true;
		default:
			return false;
	}
}

function focusWithinNavigationRow({currentRow, event, focusableSelector}: FocusWithinNavigationRowRequest): boolean {
	if (currentRow == null) return false;
	if (event.key !== 'ArrowDown' && event.key !== 'ArrowUp') return false;
	const ownerWindow = event.currentTarget.ownerDocument.defaultView;
	if (ownerWindow == null) return false;
	if (!(event.target instanceof ownerWindow.Element)) return false;
	const focusableElements = Array.from(currentRow.querySelectorAll<HTMLElement>(focusableSelector));
	const currentFocusable = event.target.closest<HTMLElement>(focusableSelector);
	if (currentFocusable == null) return false;
	const currentFocusableIndex = focusableElements.indexOf(currentFocusable);
	if (currentFocusableIndex < 0) return false;
	const nextFocusableIndex = currentFocusableIndex + resolveArrowDelta(event.key);
	const nextFocusable = focusableElements[nextFocusableIndex];
	if (nextFocusable == null) return false;
	event.preventDefault();
	event.stopPropagation();
	nextFocusable.focus();
	return true;
}

function handleNavigationKey({
	activeRowIndex,
	event,
	focusPositionByRowIndex,
	focusableRowIndexes,
	focusableSelector,
	keyboardNavigationEnabled,
	moveFocus,
}: HandleNavigationKeyRequest): void {
	if (!keyboardNavigationEnabled) return;
	if (!isNavigationKey(event.key)) return;
	const ownerWindow = event.currentTarget.ownerDocument.defaultView;
	if (ownerWindow == null) return;
	if (event.target instanceof ownerWindow.HTMLElement && TextEntryElementPolicy.isTextEntry(event.target)) return;
	if (focusableRowIndexes.length === 0) return;
	const currentRow = resolveCurrentNavigationRow({eventTarget: event.target, navigationNode: event.currentTarget});
	if (focusWithinNavigationRow({currentRow, event, focusableSelector})) return;
	const currentIndex = resolveCurrentNavigationIndex(currentRow, activeRowIndex);
	const currentPosition = resolveFocusablePosition(currentIndex, focusPositionByRowIndex);
	const nextPosition = resolveNextFocusablePosition({
		key: event.key,
		currentPosition,
		focusableRowCount: focusableRowIndexes.length,
	});
	const nextRowIndex = focusableRowIndexes[nextPosition];
	if (nextRowIndex == null) return;
	event.preventDefault();
	event.stopPropagation();
	moveFocus(nextRowIndex, resolveFocusableEdge(event.key));
}

export function resolveNavigationAlignment(direction: VerticalEdge): NavigationEdgeAlignment {
	if (direction === Edge.TOP) {
		return NavigationAlignment.START;
	}
	return NavigationAlignment.END;
}

interface MutableFocusReference<Value> {
	current: Value;
}

interface NavigationFocusReferences {
	readonly focusedElement: MutableFocusReference<HTMLElement | null>;
	readonly focusedRowKey: MutableFocusReference<string | null>;
}

interface NavigationFocusOwnership extends NavigationFocusReferences {
	readonly onFocusCapture: (event: React.FocusEvent<HTMLElement>) => void;
	readonly onBlurCapture: (event: React.FocusEvent<HTMLElement>) => void;
}

function clearNavigationFocus(references: NavigationFocusReferences): void {
	references.focusedElement.current = null;
	references.focusedRowKey.current = null;
}

function useNavigationFocusOwnership(navigationRows: ReadonlyArray<NavigationRow>): NavigationFocusOwnership {
	const focusedElement = useRef<HTMLElement | null>(null);
	const focusedRowKey = useRef<string | null>(null);
	const references = useMemo(() => ({focusedElement, focusedRowKey}), [focusedElement, focusedRowKey]);
	const onFocusCapture = useCallback(
		(event: React.FocusEvent<HTMLElement>) => {
			const ownerWindow = event.currentTarget.ownerDocument.defaultView;
			if (ownerWindow == null) return;
			if (!(event.target instanceof ownerWindow.HTMLElement)) return;
			const row = event.target.closest(`[${NAVIGATION_INDEX_ATTRIBUTE}]`);
			if (row == null) return;
			const index = Number(row.getAttribute(NAVIGATION_INDEX_ATTRIBUTE));
			if (!Number.isInteger(index)) return;
			const navigationRow = navigationRows[index];
			if (navigationRow == null) return;
			focusedElement.current = event.target;
			focusedRowKey.current = navigationRow.key;
		},
		[navigationRows],
	);
	const onBlurCapture = useCallback((event: React.FocusEvent<HTMLElement>) => {
		const ownerWindow = event.currentTarget.ownerDocument.defaultView;
		if (ownerWindow == null) {
			clearNavigationFocus(references);
			return;
		}
		const nextFocusedTarget = event.relatedTarget;
		if (nextFocusedTarget instanceof ownerWindow.Node && event.currentTarget.contains(nextFocusedTarget)) return;
		clearNavigationFocus(references);
	}, []);
	return useMemo(
		() => ({focusedElement, focusedRowKey, onFocusCapture, onBlurCapture}),
		[focusedElement, focusedRowKey, onBlurCapture, onFocusCapture],
	);
}

interface RestoreOwnedNavigationFocusRequest {
	readonly focusReferences: NavigationFocusReferences;
	readonly focusRow: FocusNavigationRow;
	readonly focusableSelector: string;
	readonly navigationNode: HTMLElement | null;
	readonly ownedRowKey: string;
	readonly scrollerRef: React.RefObject<ScrollerHandle | null>;
	readonly targetRowIndex: number;
	readonly targetRowKey: string;
}

function restoreOwnedNavigationFocus({
	focusReferences,
	focusRow,
	focusableSelector,
	navigationNode,
	ownedRowKey,
	scrollerRef,
	targetRowIndex,
	targetRowKey,
}: RestoreOwnedNavigationFocusRequest): boolean {
	if (focusReferences.focusedRowKey.current !== ownedRowKey) return false;
	const focusedElement = focusReferences.focusedElement.current;
	if (focusedElement == null) return false;
	if (focusedElement.isConnected) return false;
	if (navigationNode == null) return false;
	const focusDocument = navigationNode.ownerDocument;
	if (focusDocument.activeElement !== focusDocument.body) {
		clearNavigationFocus(focusReferences);
		return false;
	}
	focusReferences.focusedRowKey.current = targetRowKey;
	const shouldRestoreFocus = (): boolean => {
		if (focusReferences.focusedRowKey.current !== targetRowKey) return false;
		if (focusDocument.activeElement === focusDocument.body) return true;
		clearNavigationFocus(focusReferences);
		return false;
	};
	const focusedImmediately = focusRow({
		edge: NavigationFocusableEdge.FIRST,
		focusableSelector,
		index: targetRowIndex,
		navigationNode,
		shouldFocus: shouldRestoreFocus,
	});
	if (!focusedImmediately) {
		scrollNavigationRowIntoView({
			align: NavigationAlignment.AUTO,
			index: targetRowIndex,
			navigationNode,
			scrollerRef,
		});
	}
	return true;
}

interface ResolveReplacementActiveKeyRequest {
	readonly activeKey: string | null;
	readonly layout: NavigationLayout;
	readonly preferredActiveKey: string | null;
	readonly resolveMissingActiveKey: MissingNavigationActiveKeyResolver | null;
}

function resolveReplacementActiveKey({
	activeKey,
	layout,
	preferredActiveKey,
	resolveMissingActiveKey,
}: ResolveReplacementActiveKeyRequest): string | null {
	let candidate: string | null = null;
	if (activeKey != null && resolveMissingActiveKey != null) {
		candidate = resolveMissingActiveKey({missingActiveKey: activeKey});
	}
	if (candidate == null) {
		candidate = preferredActiveKey;
	}
	return layout.resolveActiveKey(candidate);
}

interface NavigationFocusReconciliationOptions {
	readonly activeFocusTargetIdentity: string | object | null;
	readonly activeKey: string | null;
	readonly activeRowIndex: number | null;
	readonly focusReferences: NavigationFocusReferences;
	readonly focusRow: FocusNavigationRow;
	readonly focusableSelector: string;
	readonly layout: NavigationLayout;
	readonly navigationNode: HTMLElement | null;
	readonly preferredActiveKey: string | null;
	readonly resolveMissingActiveKey: MissingNavigationActiveKeyResolver | null;
	readonly rowIndexByKey: ReadonlyMap<string, number>;
	readonly scrollerRef: React.RefObject<ScrollerHandle | null>;
	readonly setActiveKey: (key: string | null) => void;
}

function useNavigationFocusReconciliation({
	activeFocusTargetIdentity,
	activeKey,
	activeRowIndex,
	focusReferences,
	focusRow,
	focusableSelector,
	layout,
	navigationNode,
	preferredActiveKey,
	resolveMissingActiveKey,
	rowIndexByKey,
	scrollerRef,
	setActiveKey,
}: NavigationFocusReconciliationOptions): void {
	useLayoutEffect(() => {
		if (activeKey != null && activeRowIndex != null && layout.isFocusableRow(activeRowIndex)) {
			restoreOwnedNavigationFocus({
				focusReferences,
				focusRow,
				focusableSelector,
				navigationNode,
				ownedRowKey: activeKey,
				scrollerRef,
				targetRowIndex: activeRowIndex,
				targetRowKey: activeKey,
			});
			return;
		}
		const nextActiveKey = resolveReplacementActiveKey({
			activeKey,
			layout,
			preferredActiveKey,
			resolveMissingActiveKey,
		});
		if (nextActiveKey === activeKey) return;
		if (activeKey != null && nextActiveKey != null) {
			const nextActiveRowIndex = rowIndexByKey.get(nextActiveKey);
			if (nextActiveRowIndex != null) {
				setActiveKey(nextActiveKey);
				if (
					restoreOwnedNavigationFocus({
						focusReferences,
						focusRow,
						focusableSelector,
						navigationNode,
						ownedRowKey: activeKey,
						scrollerRef,
						targetRowIndex: nextActiveRowIndex,
						targetRowKey: nextActiveKey,
					})
				) {
					return;
				}
			}
		}
		if (focusReferences.focusedRowKey.current === activeKey) {
			clearNavigationFocus(focusReferences);
		}
		setActiveKey(nextActiveKey);
	}, [
		activeFocusTargetIdentity,
		activeKey,
		activeRowIndex,
		focusReferences,
		focusRow,
		focusableSelector,
		layout,
		navigationNode,
		preferredActiveKey,
		resolveMissingActiveKey,
		rowIndexByKey,
		scrollerRef,
		setActiveKey,
	]);
}

function resolveActiveFocusTargetIdentity(
	rows: ReadonlyArray<NavigationRow>,
	activeRowIndex: number | null,
): string | object | null {
	if (activeRowIndex == null) return null;
	const row = rows[activeRowIndex];
	if (row == null) {
		throw new Error(`Navigation active source row ${activeRowIndex} is missing`);
	}
	return row.focusTargetIdentity;
}

export function useNavigationList({
	scrollerRef,
	rows,
	focusableSelector,
	keyboardNavigationEnabled,
	preferredActiveKey,
	resolveMissingActiveKey,
}: NavigationListOptions): NavigationListResult {
	const [navigationNode, setNavigationNode] = useState<HTMLElement | null>(null);
	const [activeKey, setActiveKey] = useState<string | null>(() => resolveInitialActiveKey(preferredActiveKey));
	const layoutOwner = useNavigationLayout(rows);
	const layout = layoutOwner.layout;
	const navigationRows = layout.rows;
	const rowIndexByKey = layout.rowIndexByKey;
	const focusableRowIndexes = layout.focusableRowIndexes;
	const focusPositionByRowIndex = layout.focusPositionByRowIndex;
	const activeRowIndex = resolveOptionalRowIndex({key: activeKey, rowIndexByKey});
	const activeFocusTargetIdentity = resolveActiveFocusTargetIdentity(rows, activeRowIndex);
	const focusOwnership = useNavigationFocusOwnership(navigationRows);
	const focusRow = useNavigationFocusFrame(navigationNode);
	const moveFocus = useCallback(
		(rowIndex: number, edge: NavigationFocusableEdge) => {
			const row = navigationRows[rowIndex];
			if (row == null) return;
			setActiveKey(row.key);
			scrollNavigationRowIntoView({
				align: NavigationAlignment.AUTO,
				index: rowIndex,
				navigationNode,
				scrollerRef,
			});
			focusRow({
				edge,
				focusableSelector,
				index: rowIndex,
				navigationNode,
				shouldFocus: shouldAlwaysFocusNavigationRow,
			});
		},
		[focusRow, focusableSelector, navigationNode, navigationRows, scrollerRef],
	);
	const onKeyDownCapture = useCallback(
		(event: React.KeyboardEvent<HTMLElement>) => {
			handleNavigationKey({
				activeRowIndex,
				event,
				focusPositionByRowIndex,
				focusableRowIndexes,
				focusableSelector,
				keyboardNavigationEnabled,
				moveFocus,
			});
		},
		[
			activeRowIndex,
			focusPositionByRowIndex,
			focusableRowIndexes,
			focusableSelector,
			keyboardNavigationEnabled,
			moveFocus,
		],
	);
	const onFocusCapture = useCallback(
		(event: React.FocusEvent<HTMLElement>) => {
			focusOwnership.onFocusCapture(event);
			const rowKey = focusOwnership.focusedRowKey.current;
			if (rowKey != null) setActiveKey(rowKey);
		},
		[focusOwnership],
	);
	const getRowBounds = useCallback(
		(keys: ReadonlyArray<string>): ReadonlyMap<string, NavigationRowBounds> =>
			resolveNavigationRowBounds({keys, navigationNode, rowIndexByKey, scrollerRef}),
		[navigationNode, rowIndexByKey, scrollerRef],
	);
	const scrollToKey = useCallback(
		(key: string, align: NavigationAlignment): boolean => {
			const index = rowIndexByKey.get(key);
			if (index == null) return false;
			return scrollNavigationRowIntoView({align, index, navigationNode, scrollerRef});
		},
		[navigationNode, rowIndexByKey, scrollerRef],
	);
	const getRowListPosition = useCallback((index: number) => layout.resolveListPosition(index), [layout]);
	useNavigationFocusReconciliation({
		activeFocusTargetIdentity,
		activeKey,
		activeRowIndex,
		focusReferences: focusOwnership,
		focusRow,
		focusableSelector,
		layout,
		navigationNode,
		preferredActiveKey,
		resolveMissingActiveKey,
		rowIndexByKey,
		scrollerRef,
		setActiveKey,
	});
	return {
		layoutRevision: layoutOwner.revision,
		navigationRef: setNavigationNode,
		onKeyDownCapture,
		onFocusCapture,
		onBlurCapture: focusOwnership.onBlurCapture,
		getRowBounds,
		getRowListPosition,
		scrollToKey,
	};
}
