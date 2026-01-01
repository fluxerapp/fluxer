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
import type {Subscription} from 'rxjs';
import {timer} from 'rxjs';
import {Logger} from '~/lib/Logger';

const logger = new Logger('VoiceReconnectManager');

const RECONNECT_WINDOW_MS = 30000;
const MAX_RECONNECT_ATTEMPTS = 5;
const RECONNECT_BASE_DELAY_MS = 1000;
const RECONNECT_MAX_DELAY_MS = 30000;

export interface ReconnectState {
	lastConnectedGuildId: string | null;
	lastConnectedChannelId: string | null;
	shouldReconnect: boolean;
	disconnectReason: 'user' | 'error' | 'server' | null;
	lastDisconnectTime: number | null;
	reconnectAttempts: number;
	nextReconnectDelay: number;
}

const initialReconnectState: ReconnectState = {
	lastConnectedGuildId: null,
	lastConnectedChannelId: null,
	shouldReconnect: false,
	disconnectReason: null,
	lastDisconnectTime: null,
	reconnectAttempts: 0,
	nextReconnectDelay: RECONNECT_BASE_DELAY_MS,
};

export class VoiceReconnectManager {
	reconnectState: ReconnectState = initialReconnectState;
	private reconnectTimerSub: Subscription | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get shouldAutoReconnect(): boolean {
		const r = this.reconnectState;
		if (r.disconnectReason === 'user') return false;
		if (!r.shouldReconnect) return false;
		if (r.lastDisconnectTime && Date.now() - r.lastDisconnectTime > RECONNECT_WINDOW_MS) return false;
		if (r.reconnectAttempts >= MAX_RECONNECT_ATTEMPTS) return false;
		return true;
	}

	get reconnectAttempts(): number {
		return this.reconnectState.reconnectAttempts;
	}

	get lastConnectedChannel(): {guildId: string; channelId: string} | null {
		const r = this.reconnectState;
		if (r.lastConnectedGuildId && r.lastConnectedChannelId) {
			return {
				guildId: r.lastConnectedGuildId,
				channelId: r.lastConnectedChannelId,
			};
		}
		return null;
	}

	setLastConnectedChannel(guildId: string | null, channelId: string): void {
		runInAction(() => {
			this.reconnectState = {
				...this.reconnectState,
				lastConnectedGuildId: guildId,
				lastConnectedChannelId: channelId,
			};
		});
	}

	setReconnectState(reason: 'user' | 'error' | 'server'): void {
		const shouldReconnect = reason === 'error';
		runInAction(() => {
			this.reconnectState = {
				...this.reconnectState,
				shouldReconnect,
				disconnectReason: reason,
				lastDisconnectTime: Date.now(),
			};
		});
		logger.debug('Reconnect state updated', {reason, shouldReconnect});
	}

	scheduleReconnect(callback: () => void): boolean {
		if (!this.shouldAutoReconnect) {
			logger.debug('Auto-reconnect not allowed');
			return false;
		}

		const delay = this.reconnectState.nextReconnectDelay;
		logger.info('Scheduling reconnect', {
			delay,
			attempt: this.reconnectState.reconnectAttempts + 1,
			maxAttempts: MAX_RECONNECT_ATTEMPTS,
		});

		this.clearReconnectTimer();
		this.reconnectTimerSub = timer(delay).subscribe(() => {
			if (!this.shouldAutoReconnect) {
				logger.debug('Reconnect cancelled');
				return;
			}

			runInAction(() => {
				this.reconnectState = {
					...this.reconnectState,
					reconnectAttempts: this.reconnectState.reconnectAttempts + 1,
					nextReconnectDelay: Math.min(this.reconnectState.nextReconnectDelay * 2, RECONNECT_MAX_DELAY_MS),
				};
			});

			logger.info('Executing reconnect', {attempt: this.reconnectState.reconnectAttempts});
			callback();
		});

		return true;
	}

	resetOnConnection(): void {
		runInAction(() => {
			this.reconnectState = {
				...this.reconnectState,
				reconnectAttempts: 0,
				nextReconnectDelay: RECONNECT_BASE_DELAY_MS,
				shouldReconnect: false,
			};
		});
		this.clearReconnectTimer();
	}

	markAttempted(): void {
		runInAction(() => {
			this.reconnectState = {
				...this.reconnectState,
				shouldReconnect: false,
			};
		});
	}

	reset(): void {
		this.clearReconnectTimer();
		runInAction(() => {
			this.reconnectState = initialReconnectState;
		});
	}

	private clearReconnectTimer(): void {
		this.reconnectTimerSub?.unsubscribe();
		this.reconnectTimerSub = null;
	}

	cleanup(): void {
		this.clearReconnectTimer();
	}
}
