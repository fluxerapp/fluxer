// SPDX-License-Identifier: AGPL-3.0-or-later

import {getMainWindow} from '@electron/main/Window';
import {getConnectedIpcClientCount, onRpcActivity} from '@electron/main/ArRpcServer';
import {loadDetectableApplications, resolveByClientId} from '@electron/main/DetectableApplications';
import {startLinuxProcessScanner, stopLinuxProcessScanner} from '@electron/main/LinuxProcessScanner';
import {resolveRpcActivityAssetsForDisplay, sanitizeRpcActivityAssetsForGateway} from '@electron/main/RpcCoverArt';
import type {RpcActivityUpdatePayload} from '@electron/common/RpcActivityTypes';
import type {RpcActivityPayload} from '@electron/main/rpc/RpcTypes';
import {normalizeTimestamps} from '@electron/main/rpc/RpcUtils';
import log from 'electron-log';

let unsubscribe: (() => void) | null = null;

function hasConnectedIpcClients(): boolean {
	return getConnectedIpcClientCount() > 0;
}

function toUpdatePayload(
	activity: RpcActivityPayload | null,
	pid?: number,
	source: RpcActivityUpdatePayload['source'] = 'ipc',
	gatewayActivity?: RpcActivityPayload | null,
): RpcActivityUpdatePayload {
	if (!activity) {
		return {activity: null, pid, source};
	}
	const timestamps = activity.timestamps ? {...activity.timestamps} : undefined;
	if (timestamps) {
		normalizeTimestamps(timestamps);
	}
	const resolved = resolveByClientId(activity.application_id);
	return {
		activity: {
			...activity,
			name: activity.name || resolved?.name || 'Unknown',
			timestamps,
		},
		gatewayActivity: gatewayActivity
			? {
					...gatewayActivity,
					name: gatewayActivity.name || resolved?.name || 'Unknown',
					timestamps: gatewayActivity.timestamps ? {...gatewayActivity.timestamps} : undefined,
				}
			: undefined,
		pid,
		source,
	};
}

function forwardActivity(payload: RpcActivityUpdatePayload): void {
	const window = getMainWindow();
	if (!window || window.isDestroyed()) return;
	window.webContents.send('rpc-activity-update', payload);
}

export function startRpcActivityBridge(): void {
	if (unsubscribe) return;
	loadDetectableApplications();
	if (process.env.FLUXER_RPC_PROCESS_SCAN === '1') {
		startLinuxProcessScanner();
		log.info('[RPC] Process scanner enabled (FLUXER_RPC_PROCESS_SCAN=1)');
	}
	unsubscribe = onRpcActivity((activity, pid, source = 'ipc') => {
		if (source !== 'ipc') {
			return;
		}
		if (activity === null && hasConnectedIpcClients()) {
			return;
		}
		void (async () => {
			let displayActivity = activity;
			let gatewayActivity = activity;
			if (activity?.assets) {
				const displayAssets = await resolveRpcActivityAssetsForDisplay(activity.assets);
				const gatewayAssets = sanitizeRpcActivityAssetsForGateway(activity.assets);
				displayActivity =
					displayAssets !== activity.assets ? {...activity, assets: displayAssets} : activity;
				gatewayActivity =
					gatewayAssets !== activity.assets ? {...activity, assets: gatewayAssets} : activity;
			}
			const payload = toUpdatePayload(displayActivity, pid, source, gatewayActivity);
			log.info('[RPC] Forwarding activity update', {
				name: payload.activity?.name ?? null,
				source: payload.source,
				coverArt: payload.activity?.assets?.large_image?.slice(0, 32) ?? null,
			});
			forwardActivity(payload);
		})();
	});
}

export function stopRpcActivityBridge(): void {
	unsubscribe?.();
	unsubscribe = null;
	stopLinuxProcessScanner();
}
