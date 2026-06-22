// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import {Logger} from '@app/features/platform/utils/AppLogger';
import LocalRpcPresence from '@app/features/presence/state/LocalRpcPresence';
import {getElectronAPI, isDesktop} from '@app/features/ui/utils/NativeUtils';
import type {UserActivity} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

const logger = new Logger('DesktopRpcBridge');
let lastRpcActivityReceivedAt = 0;

function toUserActivity(desktopActivity: {
	type: number;
	application_id?: string;
	name: string;
	details?: string;
	state?: string;
	timestamps?: {start?: number; end?: number};
	assets?: UserActivity['assets'];
}): UserActivity {
	return {
		type: desktopActivity.type,
		name: desktopActivity.name,
		application_id: desktopActivity.application_id,
		details: desktopActivity.details,
		state: desktopActivity.state,
		timestamps: desktopActivity.timestamps,
		assets: desktopActivity.assets,
	};
}

export function initializeDesktopRpcBridge(): (() => void) | null {
	if (!isDesktop()) return null;
	const electronApi = getElectronAPI();
	if (!electronApi?.onRpcActivityUpdate) return null;
	return electronApi.onRpcActivityUpdate((payload) => {
		if (!Authentication.isAuthenticated) return;
		const receivedAt = payload.receivedAt ?? Date.now();
		if (receivedAt < lastRpcActivityReceivedAt) {
			logger.debug('Ignored stale RPC activity update', {receivedAt, lastReceivedAt: lastRpcActivityReceivedAt});
			return;
		}
		lastRpcActivityReceivedAt = receivedAt;
		if (payload.activity) {
			const displayActivity = toUserActivity(payload.activity);
			const gatewayActivity = payload.gatewayActivity ? toUserActivity(payload.gatewayActivity) : displayActivity;
			LocalRpcPresence.applyActivityImmediate(displayActivity, gatewayActivity);
		} else {
			LocalRpcPresence.clearImmediately();
		}
		logger.debug('RPC activity updated', {name: payload.activity?.name ?? null, source: payload.source});
	});
}
