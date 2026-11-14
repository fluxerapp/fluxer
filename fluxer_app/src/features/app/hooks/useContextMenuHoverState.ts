// SPDX-License-Identifier: AGPL-3.0-or-later

import ContextMenu, {isContextMenuNodeTarget} from '@app/features/ui/state/ContextMenu';
import {autorun} from 'mobx';
import {type RefObject, useEffect, useRef, useState} from 'react';

interface ContextMenuHoverSubscriber {
	readonly elementRef: RefObject<HTMLElement | null>;
	readonly setContextMenuOpen: (contextMenuOpen: boolean) => void;
}

const contextMenuHoverSubscribers = new Set<ContextMenuHoverSubscriber>();
let contextMenuHoverDisposer: (() => void) | null = null;

function resolveContextMenuTargetChain(): ReadonlySet<Node> | null {
	const contextMenu = ContextMenu.contextMenu;
	const target = contextMenu?.target?.target ?? null;
	if (contextMenu == null || !isContextMenuNodeTarget(target)) return null;
	const chain = new Set<Node>();
	for (let node: Node | null = target; node != null; node = node.parentNode) {
		chain.add(node);
	}
	return chain;
}

function syncContextMenuHoverSubscribers(): void {
	const chain = resolveContextMenuTargetChain();
	for (const subscriber of Array.from(contextMenuHoverSubscribers)) {
		const element = subscriber.elementRef.current;
		subscriber.setContextMenuOpen(chain != null && element != null && chain.has(element));
	}
}

function subscribeContextMenuHover(subscriber: ContextMenuHoverSubscriber): () => void {
	contextMenuHoverSubscribers.add(subscriber);
	if (contextMenuHoverDisposer == null) {
		contextMenuHoverDisposer = autorun(syncContextMenuHoverSubscribers);
	} else {
		syncContextMenuHoverSubscribers();
	}
	return () => {
		contextMenuHoverSubscribers.delete(subscriber);
		if (contextMenuHoverSubscribers.size > 0 || contextMenuHoverDisposer == null) return;
		contextMenuHoverDisposer();
		contextMenuHoverDisposer = null;
	};
}

export function useContextMenuHoverState(elementRef: RefObject<HTMLElement | null>, enabled: boolean = true): boolean {
	const [contextMenuOpen, setContextMenuOpen] = useState(false);
	const publishedContextMenuOpenRef = useRef(false);
	useEffect(() => {
		const publishContextMenuOpen = (nextContextMenuOpen: boolean) => {
			if (publishedContextMenuOpenRef.current === nextContextMenuOpen) return;
			publishedContextMenuOpenRef.current = nextContextMenuOpen;
			setContextMenuOpen(nextContextMenuOpen);
		};
		if (!enabled) {
			publishContextMenuOpen(false);
			return;
		}
		const unsubscribe = subscribeContextMenuHover({elementRef, setContextMenuOpen: publishContextMenuOpen});
		return () => {
			unsubscribe();
			publishContextMenuOpen(false);
		};
	}, [elementRef, enabled]);
	return contextMenuOpen;
}
