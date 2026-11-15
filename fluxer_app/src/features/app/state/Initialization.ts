// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import {makeAutoObservable} from 'mobx';

const InitializationState = {
	LOADING: 'LOADING',
	CONNECTING: 'CONNECTING',
	STALE: 'STALE',
	READY: 'READY',
	ERROR: 'ERROR',
} as const;

type InitializationState = ValueOf<typeof InitializationState>;

class Initialization {
	state: InitializationState = InitializationState.LOADING;
	hasCompletedInitialLoad = false;
	showsRestoredView = false;
	error: string | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get isReady(): boolean {
		return this.state === InitializationState.READY;
	}

	get isStale(): boolean {
		return this.showsRestoredView;
	}

	get hasError(): boolean {
		return this.state === InitializationState.ERROR;
	}

	get canNavigateToProtectedRoutes(): boolean {
		return this.state === InitializationState.READY || this.state === InitializationState.STALE;
	}

	setLoading(): void {
		this.state = InitializationState.LOADING;
		this.showsRestoredView = false;
		this.error = null;
	}

	setConnecting(): void {
		this.state = this.showsRestoredView ? InitializationState.STALE : InitializationState.CONNECTING;
		this.error = null;
	}

	setStale(): boolean {
		if (this.state !== InitializationState.LOADING || this.hasCompletedInitialLoad) {
			return false;
		}
		this.state = InitializationState.STALE;
		this.showsRestoredView = true;
		this.error = null;
		return true;
	}

	setReady(): void {
		this.state = InitializationState.READY;
		this.hasCompletedInitialLoad = true;
		this.showsRestoredView = false;
		this.error = null;
	}

	setError(error: string): void {
		this.state = InitializationState.ERROR;
		this.error = error;
	}

	reset(): void {
		this.state = InitializationState.LOADING;
		this.hasCompletedInitialLoad = false;
		this.showsRestoredView = false;
		this.error = null;
	}
}

export default new Initialization();
