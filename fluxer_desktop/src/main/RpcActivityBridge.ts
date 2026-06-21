// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RpcActivityUpdatePayload} from '@electron/common/RpcActivityTypes';
import {getConnectedIpcClientCount, onRpcActivity} from '@electron/main/ArRpcServer';
import {loadDetectableApplications, resolveByClientId} from '@electron/main/DetectableApplications';
import {resolveMprisCoverArtUrl} from '@electron/main/MprisCoverArt';
import {resolveRpcActivityAssetsForDisplay, sanitizeRpcActivityAssetsForGateway} from '@electron/main/RpcCoverArt';
import type {RpcActivityPayload} from '@electron/main/rpc/RpcTypes';
import {normalizeTimestamps} from '@electron/main/rpc/RpcUtils';
import {getMainWindow} from '@electron/main/Window';
import log from 'electron-log';

let unsubscribe: (() => void) | null = null;
let latestActivityPayload: RpcActivityUpdatePayload | null = null;

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
	latestActivityPayload = payload;
	const window = getMainWindow();
	if (!window || window.isDestroyed()) return;
	window.webContents.send('rpc-activity-update', payload);
}

async function buildPayload(
	activity: RpcActivityPayload | null,
	pid?: number,
	source: RpcActivityUpdatePayload['source'] = 'ipc',
): Promise<RpcActivityUpdatePayload> {
	if (!activity) {
		return {activity: null, pid, source};
	}
	let displayActivity = activity;
	let gatewayActivity = activity;
	if (activity.assets) {
		const mprisCoverArt = await resolveMprisCoverArtUrl(activity);
		const displaySourceAssets =
			mprisCoverArt && mprisCoverArt !== activity.assets.large_image
				? {...activity.assets, large_image: mprisCoverArt}
				: activity.assets;
		const displayAssets = await resolveRpcActivityAssetsForDisplay(displaySourceAssets);
		const gatewayAssets = sanitizeRpcActivityAssetsForGateway(activity.assets);
		displayActivity = displayAssets !== activity.assets ? {...activity, assets: displayAssets} : activity;
		gatewayActivity = gatewayAssets !== activity.assets ? {...activity, assets: gatewayAssets} : activity;
	}
	return toUpdatePayload(displayActivity, pid, source, gatewayActivity);
}

export function startRpcActivityBridge(): void {
	if (unsubscribe) return;
	loadDetectableApplications();
	unsubscribe = onRpcActivity((activity, pid, source = 'ipc') => {
		if (source === 'process-scan') return;
		if (activity === null && source === 'ipc-disconnect' && hasConnectedIpcClients()) {
			return;
		}
		void (async () => {
			const payload = await buildPayload(activity, pid, 'ipc');
			log.info('[RPC] Forwarding activity update', {
				name: payload.activity?.name ?? null,
				source: payload.source,
				coverArt: payload.activity?.assets?.large_image?.slice(0, 32) ?? null,
			});
			forwardActivity(payload);
		})();
	});
}

export async function getLatestRpcActivityUpdate(): Promise<RpcActivityUpdatePayload | null> {
	if (latestActivityPayload !== null) return latestActivityPayload;
	return null;
}

export function stopRpcActivityBridge(): void {
	unsubscribe?.();
	unsubscribe = null;
	latestActivityPayload = null;
}
