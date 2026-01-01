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

import {action, makeAutoObservable} from 'mobx';

const InitializationState = {
	LOADING: 'LOADING',
	CONNECTING: 'CONNECTING',
	READY: 'READY',
	ERROR: 'ERROR',
} as const;

type InitializationState = (typeof InitializationState)[keyof typeof InitializationState];

class InitializationStore {
	state: InitializationState = InitializationState.LOADING;
	error: string | null = null;
	readyPayload: unknown = null;

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
		this.readyPayload = null;
	}

	@action
	setConnecting(): void {
		this.state = InitializationState.CONNECTING;
		this.error = null;
		this.readyPayload = null;
	}

	@action
	setReady(payload: unknown): void {
		this.state = InitializationState.READY;
		this.error = null;
		this.readyPayload = payload;
	}

	@action
	setError(error: string): void {
		this.state = InitializationState.ERROR;
		this.error = error;
		this.readyPayload = null;
	}

	@action
	reset(): void {
		this.state = InitializationState.LOADING;
		this.error = null;
		this.readyPayload = null;
	}
}

export default new InitializationStore();
