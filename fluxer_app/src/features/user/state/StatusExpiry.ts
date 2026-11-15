// SPDX-License-Identifier: AGPL-3.0-or-later

import Accounts from '@app/features/auth/state/Accounts';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {deferUntilModulesLoaded} from '@app/features/platform/utils/DeferUntilModulesLoaded';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import UserSettings from '@app/features/user/state/UserSettings';
import type {StatusType} from '@fluxer/constants/src/StatusConstants';
import {StatusTypes} from '@fluxer/constants/src/StatusConstants';
import {makeAutoObservable, reaction} from 'mobx';

class StatusExpiry {
	private logger = new Logger('StatusExpiry');
	activeUserId: string | null = null;
	private timer: NodeJS.Timeout | null = null;

	constructor() {
		makeAutoObservable(this);
		deferUntilModulesLoaded(() => {
			reaction(
				() => Accounts.currentUserId,
				(userId) => {
					this.handleActiveUserChange(userId);
				},
				{fireImmediately: true},
			);
			reaction(
				() => ({
					statusResetsAt: UserSettings.statusResetsAt,
					statusResetsTo: UserSettings.statusResetsTo,
					status: UserSettings.status,
				}),
				() => {
					this.scheduleTimer();
				},
			);
		});
	}

	get activeExpiresAt(): number | null {
		const resetsAt = UserSettings.statusResetsAt;
		if (!resetsAt) {
			return null;
		}
		return new Date(resetsAt).getTime();
	}

	get activeFallbackStatus(): StatusType {
		const resetsTo = UserSettings.statusResetsTo;
		if (!resetsTo) {
			return StatusTypes.ONLINE;
		}
		switch (resetsTo) {
			case 'online':
				return StatusTypes.ONLINE;
			case 'idle':
				return StatusTypes.IDLE;
			case 'dnd':
				return StatusTypes.DND;
			case 'invisible':
				return StatusTypes.INVISIBLE;
			default:
				return StatusTypes.ONLINE;
		}
	}

	setActiveStatusExpiry({
		status,
		durationMs,
		fallbackStatus = StatusTypes.ONLINE,
	}: {
		status: StatusType;
		durationMs: number | null;
		fallbackStatus?: StatusType;
	}): void {
		if (durationMs === null) {
			this.clearStatusExpiry();
			UserSettingsCommands.update({
				status,
				statusResetsAt: null,
				statusResetsTo: null,
			}).catch((error) => {
				this.logger.error('Failed to update status:', error);
			});
			return;
		}
		const expiresAt = new Date(Date.now() + durationMs);
		UserSettingsCommands.update({
			status,
			statusResetsAt: expiresAt.toISOString(),
			statusResetsTo: fallbackStatus,
		}).catch((error) => {
			this.logger.error('Failed to update status with expiry:', error);
		});
	}

	clearStatusExpiry(): void {
		if (this.timer) {
			clearTimeout(this.timer);
			this.timer = null;
		}
	}

	handleActiveUserChange(userId: string | null): void {
		this.activeUserId = userId;
		this.scheduleTimer();
	}

	private scheduleTimer(): void {
		if (this.timer) {
			clearTimeout(this.timer);
			this.timer = null;
		}
		const expiresAt = this.activeExpiresAt;
		if (!expiresAt) {
			return;
		}
		const delay = expiresAt - Date.now();
		if (delay <= 0) {
			void this.handleExpiry();
			return;
		}
		this.timer = setTimeout(() => {
			void this.handleExpiry();
		}, delay);
	}

	private async handleExpiry(): Promise<void> {
		const fallbackStatus = this.activeFallbackStatus;
		try {
			await UserSettingsCommands.update({
				status: fallbackStatus,
				statusResetsAt: null,
				statusResetsTo: null,
			});
		} catch (error) {
			this.logger.error('Failed to revert status after expiry:', error);
		}
	}
}

export default new StatusExpiry();
