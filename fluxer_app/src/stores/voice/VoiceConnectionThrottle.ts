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

import {makeAutoObservable, runInAction} from 'mobx';

const CONNECT_THROTTLE_MS = 1000;

export interface ConnectionThrottleState {
	lastConnectRequestAt: number | null;
	connectAttemptId: number;
	inFlightConnect: boolean;
}

const initialThrottleState: ConnectionThrottleState = {
	lastConnectRequestAt: null,
	connectAttemptId: 0,
	inFlightConnect: false,
};

export class VoiceConnectionThrottle {
	throttleState: ConnectionThrottleState = initialThrottleState;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get connectAttemptId(): number {
		return this.throttleState.connectAttemptId;
	}

	get inFlightConnect(): boolean {
		return this.throttleState.inFlightConnect;
	}

	shouldThrottle(): boolean {
		const now = Date.now();
		const last = this.throttleState.lastConnectRequestAt ?? 0;
		return now - last < CONNECT_THROTTLE_MS;
	}

	isLatestAttempt(id: number): boolean {
		return id === this.throttleState.connectAttemptId;
	}

	recordConnectRequest(): void {
		runInAction(() => {
			this.throttleState = {
				...this.throttleState,
				lastConnectRequestAt: Date.now(),
			};
		});
	}

	incrementAttemptId(): void {
		runInAction(() => {
			this.throttleState = {
				...this.throttleState,
				connectAttemptId: this.throttleState.connectAttemptId + 1,
			};
		});
	}

	setInFlightConnect(value: boolean): void {
		runInAction(() => {
			this.throttleState = {
				...this.throttleState,
				inFlightConnect: value,
			};
		});
	}

	reset(): void {
		runInAction(() => {
			this.throttleState = initialThrottleState;
		});
	}
}
