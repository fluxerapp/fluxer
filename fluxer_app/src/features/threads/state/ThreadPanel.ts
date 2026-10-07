// SPDX-License-Identifier: AGPL-3.0-or-later

import Navigation from '@app/features/navigation/state/Navigation';
import {makeAutoObservable, observable} from 'mobx';

export interface ThreadCreateTarget {
	readonly parentId: string;
	readonly messageId: string | null;
}

class ThreadPanel {
	createTarget: ThreadCreateTarget | null = null;
	private readonly createCooldowns = observable.map<string, number>();

	constructor() {
		makeAutoObservable<this, 'createCooldowns'>(this, {createCooldowns: false}, {autoBind: true});
	}

	getCreateCooldownUntil(parentId: string): number {
		return this.createCooldowns.get(parentId) ?? 0;
	}

	startCreateCooldown(parentId: string, seconds: number): void {
		if (seconds <= 0) return;
		this.createCooldowns.set(parentId, Date.now() + seconds * 1000);
	}

	get openThreadId(): string | null {
		return Navigation.threadId;
	}

	getCreateTarget(parentId: string): ThreadCreateTarget | null {
		return this.createTarget?.parentId === parentId ? this.createTarget : null;
	}

	openCreate(parentId: string, messageId: string | null): void {
		this.createTarget = {parentId, messageId};
	}

	closeCreate(): void {
		this.createTarget = null;
	}
}

export default new ThreadPanel();
