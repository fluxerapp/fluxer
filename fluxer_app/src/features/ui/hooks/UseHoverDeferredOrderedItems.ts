// SPDX-License-Identifier: AGPL-3.0-or-later

import type React from 'react';
import {useLayoutEffect, useMemo, useRef, useState} from 'react';

type HoverDeferredOrderReleaseToken = string | number | boolean | null;

interface UseHoverDeferredOrderedItemsOptions<T> {
	readonly items: ReadonlyArray<T>;
	readonly getKey: (item: T) => string;
	readonly isHoveringDynamicItem: boolean;
	readonly releaseToken: HoverDeferredOrderReleaseToken;
}

interface HoverDeferredOrderSource {
	readonly keys: ReadonlyArray<string>;
	readonly releaseToken: HoverDeferredOrderReleaseToken;
}

function getOrderKeys<T>(items: ReadonlyArray<T>, getKey: (item: T) => string): ReadonlyArray<string> {
	return items.map(getKey);
}

function orderKeysEqual(currentKeys: ReadonlyArray<string>, nextKeys: ReadonlyArray<string>): boolean {
	if (currentKeys.length !== nextKeys.length) return false;
	for (let index = 0; index < currentKeys.length; index++) {
		if (currentKeys[index] !== nextKeys[index]) return false;
	}
	return true;
}

function mergeItemsInCurrentOrder<T>(
	currentItems: ReadonlyArray<T>,
	nextItems: ReadonlyArray<T>,
	getKey: (item: T) => string,
): ReadonlyArray<T> {
	const nextByKey = new Map<string, T>();
	for (let index = 0; index < nextItems.length; index++) {
		nextByKey.set(getKey(nextItems[index]), nextItems[index]);
	}
	const merged: Array<T> = [];
	const seenKeys = new Set<string>();
	for (let index = 0; index < currentItems.length; index++) {
		const key = getKey(currentItems[index]);
		const nextItem = nextByKey.get(key);
		if (nextItem == null) continue;
		merged.push(nextItem);
		seenKeys.add(key);
	}
	for (let index = 0; index < nextItems.length; index++) {
		const item = nextItems[index];
		const key = getKey(item);
		if (seenKeys.has(key)) continue;
		merged.push(item);
	}
	return merged;
}

function keepCurrentArrayIfEqual<T>(currentItems: ReadonlyArray<T>, nextItems: ReadonlyArray<T>): ReadonlyArray<T> {
	if (currentItems.length !== nextItems.length) return nextItems;
	for (let index = 0; index < currentItems.length; index++) {
		if (currentItems[index] !== nextItems[index]) return nextItems;
	}
	return currentItems;
}

interface ResolveHoverDeferredItemsRequest<T> {
	readonly currentItems: ReadonlyArray<T>;
	readonly getKey: (item: T) => string;
	readonly hasDeferredOrderRef: React.MutableRefObject<boolean>;
	readonly isHoveringDynamicItem: boolean;
	readonly items: ReadonlyArray<T>;
	readonly sourceChanged: boolean;
}

function resolveHoverDeferredItems<T>({
	currentItems,
	getKey,
	hasDeferredOrderRef,
	isHoveringDynamicItem,
	items,
	sourceChanged,
}: ResolveHoverDeferredItemsRequest<T>): ReadonlyArray<T> {
	if (isHoveringDynamicItem) {
		hasDeferredOrderRef.current = true;
		return keepCurrentArrayIfEqual(currentItems, mergeItemsInCurrentOrder(currentItems, items, getKey));
	}
	if (hasDeferredOrderRef.current && !sourceChanged) {
		return keepCurrentArrayIfEqual(currentItems, mergeItemsInCurrentOrder(currentItems, items, getKey));
	}
	hasDeferredOrderRef.current = false;
	return keepCurrentArrayIfEqual(currentItems, items);
}

export function useHoverDeferredOrderedItems<T>({
	items,
	getKey,
	isHoveringDynamicItem,
	releaseToken,
}: UseHoverDeferredOrderedItemsOptions<T>): ReadonlyArray<T> {
	const [visibleItems, setVisibleItems] = useState<ReadonlyArray<T>>(() => items);
	const orderKeys = useMemo(() => getOrderKeys(items, getKey), [items, getKey]);
	const lastSourceRef = useRef<HoverDeferredOrderSource>({keys: orderKeys, releaseToken});
	const hasDeferredOrderRef = useRef(false);
	useLayoutEffect(() => {
		const previousSource = lastSourceRef.current;
		const sourceChanged =
			!Object.is(previousSource.releaseToken, releaseToken) || !orderKeysEqual(previousSource.keys, orderKeys);
		lastSourceRef.current = {keys: orderKeys, releaseToken};
		setVisibleItems((currentItems) =>
			resolveHoverDeferredItems({
				currentItems,
				getKey,
				hasDeferredOrderRef,
				isHoveringDynamicItem,
				items,
				sourceChanged,
			}),
		);
	}, [getKey, isHoveringDynamicItem, items, orderKeys, releaseToken]);
	return visibleItems;
}
