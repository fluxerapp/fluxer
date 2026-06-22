// SPDX-License-Identifier: AGPL-3.0-or-later

import {ActivityEmitter} from '@app/features/presence/state/ActivityEmitter';
import {sanitizeActivityAssetsForGateway} from '@app/features/presence/utils/sanitizeActivityAssetsForGateway';
import type {UserActivity} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {action, makeAutoObservable} from 'mobx';

const DEBOUNCE_MS = 1000;

function buildActivityIdentity(activity: UserActivity | null): string {
	if (!activity) return 'none';
	return JSON.stringify({
		application_id: activity.application_id ?? null,
		name: activity.name ?? null,
		type: activity.type ?? null,
		details: activity.details ?? null,
		state: activity.state ?? null,
		large_image: activity.assets?.large_image ?? null,
		small_image: activity.assets?.small_image ?? null,
		large_text: activity.assets?.large_text ?? null,
		small_text: activity.assets?.small_text ?? null,
		start: activity.timestamps?.start ?? null,
		end: activity.timestamps?.end ?? null,
		buttons: activity.buttons?.map((button) => `${button.label}:${button.url}`) ?? [],
	});
}

class LocalRpcPresence {
	activity: UserActivity | null = null;
	gatewayActivity: UserActivity | null = null;
	private debounceTimer: ReturnType<typeof setTimeout> | null = null;
	private pendingActivity: UserActivity | null | undefined = undefined;
	activityVersion = 0;

	constructor() {
		makeAutoObservable<
			this,
			'debounceTimer' | 'pendingActivity' | 'setActivity' | 'applyActivityImmediate' | 'commitPendingActivity' | 'clearActivityState'
		>(
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
		return `${this.activityVersion}:${buildActivityIdentity(this.activity)}`;
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
		ActivityEmitter.emitPresenceChange('local');
	}

	setActivity(activity: UserActivity | null): void {
		this.pendingActivity = activity;
		if (this.debounceTimer) clearTimeout(this.debounceTimer);
		this.debounceTimer = setTimeout(() => {
			this.commitPendingActivity();
			ActivityEmitter.emitPresenceChange('local');
		}, DEBOUNCE_MS);
	}

	getGatewayActivities(): Array<UserActivity> {
		const activity = this.gatewayActivity ?? this.activity;
		return activity ? [sanitizeActivityAssetsForGateway(activity)] : [];
	}

	clearImmediately(): void {
		if (this.debounceTimer) {
			clearTimeout(this.debounceTimer);
			this.debounceTimer = null;
		}
		this.clearActivityState();
		ActivityEmitter.emitPresenceChange('local');
	}
}

export default new LocalRpcPresence();
