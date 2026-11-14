// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {signHarvestDownloadToken} from '@app/api/user/services/HarvestDownloadToken';

const HARVEST_DOWNLOAD_PATH = '/harvest-downloads';

export async function buildHarvestDownloadUrl(params: {
	userId: bigint;
	harvestId: bigint;
	storageKey: string;
	expiresInSeconds: number;
	storageService: IStorageService;
}): Promise<string> {
	const {userId, harvestId, storageKey, expiresInSeconds, storageService} = params;
	if (Config.presignedHarvestDownloadsEnabled) {
		return storageService.getPresignedDownloadURL({
			bucket: Config.s3.buckets.harvests,
			key: storageKey,
			expiresIn: expiresInSeconds,
		});
	}
	const token = signHarvestDownloadToken(
		{
			userId: userId.toString(),
			harvestId: harvestId.toString(),
			storageKey,
			expiresAt: Date.now() + expiresInSeconds * 1000,
		},
		Config.auth.connectionInitiationSecret,
	);
	const base = Config.endpoints.apiPublic.replace(/\/+$/u, '');
	return `${base}${HARVEST_DOWNLOAD_PATH}/${harvestId}?token=${encodeURIComponent(token)}`;
}
