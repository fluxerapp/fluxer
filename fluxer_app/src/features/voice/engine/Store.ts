// SPDX-License-Identifier: AGPL-3.0-or-later

import {makeObservable, observableRef, runInAction} from 'mobx';
import {useSyncExternalStore} from 'react';

export type StoreListener = () => void;

export interface StoreLike {
	subscribe(listener: StoreListener): () => void;
	getSnapshot(): number;
	getServerSnapshot(): number;
	getMobxSnapshot?(): number;
}

export class Store implements StoreLike {
	private listeners = new Set<StoreListener>();
	private storeVersion = 0;
	private updateDepth = 0;
	private pendingChange = false;

	constructor() {
		makeObservable<this, 'storeVersion'>(this, {
			storeVersion: observableRef,
		});
	}

	subscribe = (listener: StoreListener): (() => void) => {
		this.listeners.add(listener);
		return () => {
			this.listeners.delete(listener);
		};
	};

	getSnapshot = (): number => this.storeVersion;

	getServerSnapshot = (): number => this.storeVersion;

	getMobxSnapshot = (): number => this.storeVersion;

	protected emitChange(): void {
		if (this.updateDepth > 0) {
			this.pendingChange = true;
			return;
		}
		this.publishChange();
	}

	protected update<T>(fn: () => T): T {
		this.updateDepth += 1;
		try {
			const result = runInAction(fn);
			this.pendingChange = true;
			return result;
		} finally {
			this.updateDepth -= 1;
			if (this.updateDepth === 0 && this.pendingChange) {
				this.pendingChange = false;
				this.publishChange();
			}
		}
	}

	private publishChange(): void {
		runInAction(() => {
			this.storeVersion += 1;
		});
		for (const listener of [...this.listeners]) {
			listener();
		}
	}
}

export function useStoreVersion(store: StoreLike): number {
	const externalVersion = useSyncExternalStore(store.subscribe, store.getSnapshot, store.getServerSnapshot);
	return store.getMobxSnapshot?.() ?? externalVersion;
}
