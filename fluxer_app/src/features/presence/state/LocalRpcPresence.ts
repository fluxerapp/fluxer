// SPDX-License-Identifier: AGPL-3.0-or-later

import {sanitizeActivityAssetsForGateway} from '@app/features/presence/utils/sanitizeActivityAssetsForGateway';
import type {UserActivity} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {action, makeAutoObservable} from 'mobx';

const DEBOUNCE_MS = 1000;

class LocalRpcPresence {
	activity: UserActivity | null = null;
	gatewayActivity: UserActivity | null = null;
	activityVersion = 0;
	private debounceTimer: ReturnType<typeof setTimeout> | null = null;
	private pendingActivity: UserActivity | null | undefined = undefined;

	constructor() {
		makeAutoObservable<this, 'debounceTimer' | 'pendingActivity' | 'commitPendingActivity' | 'clearActivityState'>(
			this,
			{
				debounceTimer: false,
				pendingActivity: false,
				setActivity: action,
				applyActivityImmediate: action,
				commitPendingActivity: action,
				clearActivityState: action,
			},
			{autoBind: true},
		);
	}

	private commitPendingActivity(): void {
		this.debounceTimer = null;
		if (this.pendingActivity === undefined) return;
		this.activity = this.pendingActivity;
		this.gatewayActivity = this.pendingActivity;
		this.pendingActivity = undefined;
		this.activityVersion++;
	}

	private clearActivityState(): void {
		this.pendingActivity = undefined;
		this.activity = null;
		this.gatewayActivity = null;
		this.activityVersion++;
	}

	get activityKey(): string {
		return `${this.activityVersion}:${this.activity?.application_id ?? 'none'}:${this.activity?.name ?? 'none'}`;
	}

	applyActivityImmediate(activity: UserActivity, gatewayActivity?: UserActivity): void {
		if (this.debounceTimer) {
			clearTimeout(this.debounceTimer);
			this.debounceTimer = null;
		}
		this.pendingActivity = undefined;
		this.activity = activity;
		this.gatewayActivity = gatewayActivity ?? activity;
		this.activityVersion++;
	}

	setActivity(activity: UserActivity | null): void {
		this.pendingActivity = activity;
		if (this.debounceTimer) clearTimeout(this.debounceTimer);
		this.debounceTimer = setTimeout(this.commitPendingActivity, DEBOUNCE_MS);
	}

	getGatewayActivities(): Array<UserActivity> | null {
		const activity = this.gatewayActivity ?? this.activity;
		return activity ? [sanitizeActivityAssetsForGateway(activity)] : null;
	}

	clearImmediately(): void {
		if (this.debounceTimer) {
			clearTimeout(this.debounceTimer);
			this.debounceTimer = null;
		}
		this.clearActivityState();
	}
}

export default new LocalRpcPresence();
