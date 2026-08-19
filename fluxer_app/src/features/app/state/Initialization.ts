// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import {action, makeAutoObservable} from 'mobx';

const InitializationState = {
	LOADING: 'LOADING',
	CONNECTING: 'CONNECTING',
	READY: 'READY',
	ERROR: 'ERROR',
} as const;

type InitializationState = ValueOf<typeof InitializationState>;

class Initialization {
	state: InitializationState = InitializationState.LOADING;
	hasCompletedInitialLoad = false;
	error: string | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get isLoading(): boolean {
		return this.state === InitializationState.LOADING;
	}

	get isConnecting(): boolean {
		return this.state === InitializationState.CONNECTING;
	}

	get isReady(): boolean {
		return this.state === InitializationState.READY;
	}

	get hasError(): boolean {
		return this.state === InitializationState.ERROR;
	}

	get canNavigateToProtectedRoutes(): boolean {
		return this.state === InitializationState.READY;
	}

	@action
	setLoading(): void {
		this.state = InitializationState.LOADING;
		this.error = null;
	}

	@action
	setConnecting(): void {
		this.state = InitializationState.CONNECTING;
		this.error = null;
	}

	@action
	setReady(): void {
		this.state = InitializationState.READY;
		this.hasCompletedInitialLoad = true;
		this.error = null;
	}

	@action
	setError(error: string): void {
		this.state = InitializationState.ERROR;
		this.error = error;
	}

	@action
	reset(): void {
		this.state = InitializationState.LOADING;
		this.hasCompletedInitialLoad = false;
		this.error = null;
	}
}

export default new Initialization();
