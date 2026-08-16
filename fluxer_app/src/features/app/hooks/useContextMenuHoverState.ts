// SPDX-License-Identifier: AGPL-3.0-or-later

import ContextMenu, {isContextMenuNodeTarget} from '@app/features/ui/state/ContextMenu';
import {autorun} from 'mobx';
import {type RefObject, useEffect, useState} from 'react';

interface ContextMenuHoverSubscriber {
	readonly elementRef: RefObject<HTMLElement | null>;
	readonly setContextMenuOpen: (contextMenuOpen: boolean) => void;
	contextMenuOpen: boolean;
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
		const contextMenuOpen = chain != null && element != null && chain.has(element);
		if (subscriber.contextMenuOpen === contextMenuOpen) continue;
		subscriber.contextMenuOpen = contextMenuOpen;
		subscriber.setContextMenuOpen(contextMenuOpen);
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
	useEffect(() => {
		if (!enabled) {
			setContextMenuOpen(false);
			return;
		}
		return subscribeContextMenuHover({elementRef, setContextMenuOpen, contextMenuOpen: false});
	}, [elementRef, enabled]);
	return contextMenuOpen;
}
