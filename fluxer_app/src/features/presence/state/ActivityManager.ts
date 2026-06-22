// SPDX-License-Identifier: AGPL-3.0-or-later

import {ActivityEmitter} from '@app/features/presence/state/ActivityEmitter';
import {resolveActivityImageUrl} from '@app/features/presence/utils/resolveActivityImageUrl';
import {sanitizeActivityAssetsForGateway} from '@app/features/presence/utils/sanitizeActivityAssetsForGateway';
import type {UserActivity} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {action, makeAutoObservable} from 'mobx';

export type ActivitySource = 'rpc' | 'detected';

interface ActivityEntry {
	key: string;
	activity: UserActivity | null;
	gatewayActivity: UserActivity | null;
	updatedAt: number;
}

const SOURCE_PRIORITY: ReadonlyArray<ActivitySource> = ['rpc', 'detected'];

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

function hasResolvablePrimaryImage(activity: UserActivity | null): boolean {
	const image = activity?.assets?.large_image ?? activity?.assets?.small_image;
	return Boolean(resolveActivityImageUrl(image, activity?.application_id));
}

function mergeRpcEntryWithDetectedFallback(rpcEntry: ActivityEntry, detectedEntry: ActivityEntry | undefined): ActivityEntry {
	if (!detectedEntry) {
		return rpcEntry;
	}
	const nextActivity = hasResolvablePrimaryImage(rpcEntry.activity)
		? rpcEntry.activity
		: rpcEntry.activity && detectedEntry.activity?.assets
			? {...rpcEntry.activity, assets: detectedEntry.activity.assets}
			: rpcEntry.activity;
	const nextGatewayActivity = hasResolvablePrimaryImage(rpcEntry.gatewayActivity)
		? rpcEntry.gatewayActivity
		: rpcEntry.gatewayActivity && detectedEntry.gatewayActivity?.assets
			? {...rpcEntry.gatewayActivity, assets: detectedEntry.gatewayActivity.assets}
			: rpcEntry.gatewayActivity;
	if (nextActivity === rpcEntry.activity && nextGatewayActivity === rpcEntry.gatewayActivity) {
		return rpcEntry;
	}
	return {
		...rpcEntry,
		activity: nextActivity,
		gatewayActivity: nextGatewayActivity,
	};
}

export class ActivityManager {
	activities: Array<UserActivity> = [];
	activity: UserActivity | null = null;
	gatewayActivities: Array<UserActivity> = [];
	gatewayActivity: UserActivity | null = null;
	currentSource: ActivitySource | null = null;
	activityVersion = 0;
	private readonly sourceActivities = new Map<ActivitySource, Map<string, ActivityEntry>>();

	constructor() {
		makeAutoObservable<this, 'sourceActivities' | 'recomputeActiveActivity'>(
			this,
			{
				sourceActivities: false,
				setSourceActivity: action,
				clearSourceActivity: action,
				clearAll: action,
				recomputeActiveActivity: action,
			},
			{autoBind: true},
		);
	}

	get activityKey(): string {
		return `${this.activityVersion}:${this.currentSource ?? 'none'}:${buildActivityIdentity(this.activity)}`;
	}

	getActivities(): Array<UserActivity> {
		return this.activities;
	}

	setSourceActivity(source: ActivitySource, key: string, activity: UserActivity, gatewayActivity?: UserActivity | null): void {
		let entries = this.sourceActivities.get(source);
		if (!entries) {
			entries = new Map<string, ActivityEntry>();
			this.sourceActivities.set(source, entries);
		}
		entries.set(key, {
			key,
			activity,
			gatewayActivity: gatewayActivity ?? activity,
			updatedAt: Date.now(),
		});
		this.recomputeActiveActivity();
	}

	clearSourceActivity(source: ActivitySource, key?: string): void {
		const entries = this.sourceActivities.get(source);
		if (!entries) {
			return;
		}
		if (key === undefined) {
			this.sourceActivities.delete(source);
			this.recomputeActiveActivity();
			return;
		}
		if (!entries.delete(key)) {
			return;
		}
		if (entries.size === 0) {
			this.sourceActivities.delete(source);
		}
		this.recomputeActiveActivity();
	}

	clearAll(): void {
		if (
			this.sourceActivities.size === 0 &&
			this.activities.length === 0 &&
			this.activity === null &&
			this.gatewayActivities.length === 0 &&
			this.gatewayActivity === null &&
			this.currentSource === null
		) {
			return;
		}
		this.sourceActivities.clear();
		this.recomputeActiveActivity();
	}

	getGatewayActivities(): Array<UserActivity> {
		return this.gatewayActivities.map(sanitizeActivityAssetsForGateway);
	}

	private recomputeActiveActivity(): void {
		const orderedEntries: Array<ActivityEntry & {source: ActivitySource}> = [];
		let nextSource: ActivitySource | null = null;
		let nextActivity: UserActivity | null = null;
		let nextGatewayActivity: UserActivity | null = null;
		const rpcKeys = new Set<string>();
		const detectedEntries = this.sourceActivities.get('detected');

		for (const source of SOURCE_PRIORITY) {
			const entries = this.sourceActivities.get(source);
			if (!entries?.size) {
				continue;
			}
			const sortedEntries = Array.from(entries.values())
				.filter((entry) => entry.activity)
				.sort((left, right) => right.updatedAt - left.updatedAt);
			if (sortedEntries.length === 0) {
				continue;
			}
			const normalizedEntries = sortedEntries
				.map((entry) =>
					source === 'rpc' ? mergeRpcEntryWithDetectedFallback(entry, detectedEntries?.get(entry.key)) : entry,
				)
				.filter((entry) => {
					if (source !== 'detected') {
						return true;
					}
					return !rpcKeys.has(entry.key);
				});
			for (const entry of normalizedEntries) {
				if (source === 'rpc') {
					rpcKeys.add(entry.key);
				}
				orderedEntries.push({...entry, source});
			}
			if (nextActivity === null) {
				nextSource = source;
				nextActivity = normalizedEntries[0]?.activity ?? null;
				nextGatewayActivity = normalizedEntries[0]?.gatewayActivity ?? normalizedEntries[0]?.activity ?? null;
			}
		}
		const nextActivities = orderedEntries.map((entry) => entry.activity!).filter(Boolean);
		const nextGatewayActivities = orderedEntries
			.map((entry) => entry.gatewayActivity ?? entry.activity)
			.filter((activity): activity is UserActivity => Boolean(activity));
		const activitiesChanged =
			this.activities.length !== nextActivities.length || this.activities.some((activity, index) => activity !== nextActivities[index]);
		const gatewayActivitiesChanged =
			this.gatewayActivities.length !== nextGatewayActivities.length ||
			this.gatewayActivities.some((activity, index) => activity !== nextGatewayActivities[index]);

		if (
			!activitiesChanged &&
			!gatewayActivitiesChanged &&
			this.currentSource === nextSource &&
			this.activity === nextActivity &&
			this.gatewayActivity === nextGatewayActivity
		) {
			return;
		}

		this.activities = nextActivities;
		this.gatewayActivities = nextGatewayActivities;
		this.currentSource = nextSource;
		this.activity = nextActivity;
		this.gatewayActivity = nextGatewayActivity;
		this.activityVersion++;
		ActivityEmitter.emitPresenceChange('local');
	}
}

export default new ActivityManager();
