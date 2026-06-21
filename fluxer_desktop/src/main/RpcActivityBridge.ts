// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RpcActivityUpdatePayload} from '@electron/common/RpcActivityTypes';
import {getConnectedIpcClientCount, onRpcActivity} from '@electron/main/ArRpcServer';
import {loadDetectableApplications, resolveByClientId, resolveMappedRpcImage} from '@electron/main/DetectableApplications';
import {getScannedGameIdByPid} from '@electron/main/LinuxProcessScanner';
import {resolveMprisCoverArtUrl} from '@electron/main/MprisCoverArt';
import {resolveRpcActivityAssetsForDisplay, sanitizeRpcActivityAssetsForGateway} from '@electron/main/RpcCoverArt';
import type {RpcActivityPayload} from '@electron/main/rpc/RpcTypes';
import {normalizeTimestamps} from '@electron/main/rpc/RpcUtils';
import {getMainWindow} from '@electron/main/Window';
import log from 'electron-log';

let unsubscribe: (() => void) | null = null;
let latestActivityPayload: RpcActivityUpdatePayload | null = null;

function resolveApplication(applicationId: string, pid?: number) {
	const resolved = resolveByClientId(applicationId);
	if (resolved || pid === undefined) {
		return resolved;
	}
	const scannedGameId = getScannedGameIdByPid(pid);
	return scannedGameId ? resolveByClientId(scannedGameId) : null;
}

function resolveMappedAssets(
	applicationId: string,
	pid: number | undefined,
	assets: RpcActivityPayload['assets'] | undefined,
): RpcActivityPayload['assets'] | undefined {
	if (!assets) return assets;
	const mappedApplicationId = resolveApplication(applicationId, pid)?.id ?? applicationId;
	const largeImage = resolveMappedRpcImage(mappedApplicationId, assets.large_image);
	const smallImage = resolveMappedRpcImage(mappedApplicationId, assets.small_image);
	if (largeImage === assets.large_image && smallImage === assets.small_image) {
		return assets;
	}
	return {
		...assets,
		large_image: largeImage,
		small_image: smallImage,
	};
}

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
	const resolved = resolveApplication(activity.application_id, pid);
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
	const resolved = resolveApplication(activity.application_id, pid);
	let displayActivity = activity;
	let gatewayActivity = activity;
	if (activity.assets) {
		const mprisCoverArt = await resolveMprisCoverArtUrl(activity);
		const resolvedAssets = resolveMappedAssets(activity.application_id, pid, activity.assets);
		const displaySourceAssets =
			mprisCoverArt && mprisCoverArt !== activity.assets.large_image
				? {...resolvedAssets, large_image: mprisCoverArt}
				: resolvedAssets;
		const displayAssets = await resolveRpcActivityAssetsForDisplay(displaySourceAssets);
		const gatewayAssets = sanitizeRpcActivityAssetsForGateway(resolvedAssets);
		displayActivity = displayAssets !== activity.assets ? {...activity, assets: displayAssets} : activity;
		gatewayActivity = gatewayAssets !== activity.assets ? {...activity, assets: gatewayAssets} : activity;
	} else if (resolved?.iconUrl) {
		const fallbackAssets = await resolveRpcActivityAssetsForDisplay({
			large_image: resolved.iconUrl,
			large_text: resolved.name,
		});
		const gatewayFallbackAssets = sanitizeRpcActivityAssetsForGateway({
			large_image: resolved.iconUrl,
			large_text: resolved.name,
		});
		displayActivity = {...activity, assets: fallbackAssets};
		gatewayActivity = {...activity, assets: gatewayFallbackAssets};
	}
	return toUpdatePayload(displayActivity, pid, source, gatewayActivity);
}

export function startRpcActivityBridge(): void {
	if (unsubscribe) return;
	loadDetectableApplications();
	unsubscribe = onRpcActivity((activity, pid, source = 'ipc') => {
		if (activity === null && source === 'ipc-disconnect' && hasConnectedIpcClients()) {
			return;
		}
		void (async () => {
			const payloadSource: RpcActivityUpdatePayload['source'] = source === 'process-scan' ? 'process-scan' : 'ipc';
			const payload = await buildPayload(activity, pid, payloadSource);
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
