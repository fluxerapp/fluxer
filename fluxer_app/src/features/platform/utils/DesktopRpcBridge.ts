// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import {Logger} from '@app/features/platform/utils/AppLogger';
import ActivityManager, {type ActivitySource} from '@app/features/presence/state/ActivityManager';
import {getElectronAPI, isDesktop} from '@app/features/ui/utils/NativeUtils';
import type {UserActivity} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

const logger = new Logger('DesktopRpcBridge');

function getActivityKey(payload: {source: 'ipc' | 'process-scan'; pid?: number; activity: UserActivity | null}): string | null {
	if (typeof payload.pid === 'number' && Number.isFinite(payload.pid)) {
		return `pid:${payload.pid}`;
	}
	if (payload.activity?.application_id) {
		return `app:${payload.activity.application_id}`;
	}
	return payload.activity ? `${payload.source}:singleton` : null;
}

function toUserActivity(desktopActivity: {
	type: number;
	status_display_type?: number;
	application_id: string;
	name: string;
	details?: string;
	details_url?: string;
	state?: string;
	state_url?: string;
	timestamps?: {start?: number; end?: number};
	assets?: UserActivity['assets'];
	buttons?: UserActivity['buttons'];
	party?: UserActivity['party'];
	secrets?: UserActivity['secrets'];
	metadata?: UserActivity['metadata'];
}): UserActivity {
	return {
		type: desktopActivity.type,
		status_display_type: desktopActivity.status_display_type,
		name: desktopActivity.name,
		application_id: desktopActivity.application_id,
		details: desktopActivity.details,
		details_url: desktopActivity.details_url,
		state: desktopActivity.state,
		state_url: desktopActivity.state_url,
		timestamps: desktopActivity.timestamps,
		assets: desktopActivity.assets,
		buttons: desktopActivity.buttons,
		party: desktopActivity.party,
		secrets: desktopActivity.secrets,
		metadata: desktopActivity.metadata,
	};
}

function applyDesktopActivityPayload(payload: {
	activity: UserActivity | null;
	gatewayActivity?: UserActivity | null;
	pid?: number;
	source: 'ipc' | 'process-scan';
}): void {
	if (!Authentication.isAuthenticated) return;
	const activitySource: ActivitySource = payload.source === 'process-scan' ? 'detected' : 'rpc';
	const activityKey = getActivityKey(payload);
	if (payload.activity) {
		ActivityManager.setSourceActivity(activitySource, activityKey ?? `${activitySource}:singleton`, payload.activity, payload.gatewayActivity ?? payload.activity);
	} else {
		ActivityManager.clearSourceActivity(activitySource, activityKey ?? undefined);
	}
	logger.debug('RPC activity updated', {name: payload.activity?.name ?? null, source: payload.source});
}

export function initializeDesktopRpcBridge(): (() => void) | null {
	if (!isDesktop()) return null;
	const electronApi = getElectronAPI();
	if (!electronApi?.onRpcActivityUpdate) return null;
	const unsubscribe = electronApi.onRpcActivityUpdate((payload) => {
		const displayActivity = payload.activity ? toUserActivity(payload.activity) : null;
		const gatewayActivity = payload.gatewayActivity ? toUserActivity(payload.gatewayActivity) : displayActivity;
		applyDesktopActivityPayload({
			activity: displayActivity,
			gatewayActivity,
			pid: payload.pid,
			source: payload.source,
		});
	});
	void electronApi
		.getLatestRpcActivityUpdate?.()
		.then((payload) => {
			if (!payload) return;
			const displayActivity = payload.activity ? toUserActivity(payload.activity) : null;
			const gatewayActivity = payload.gatewayActivity ? toUserActivity(payload.gatewayActivity) : displayActivity;
			applyDesktopActivityPayload({
				activity: displayActivity,
				gatewayActivity,
				pid: payload.pid,
				source: payload.source,
			});
		})
		.catch((error: unknown) => {
			logger.warn('Failed to hydrate latest RPC activity', error);
		});
	return unsubscribe;
}
