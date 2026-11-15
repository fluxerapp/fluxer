// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac} from 'node:crypto';
import {Config} from '@app/api/Config';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {isJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import {signExpiringToken, verifyExpiringToken} from '@app/api/utils/SignedExpiringToken';

const KEY_DERIVATION_LABEL = 'fluxer.admin-download.v1';
export const ADMIN_DOWNLOAD_PATH = '/signed-downloads';

type AdminDownloadBucket = 'harvests' | 'reports';

interface AdminDownloadTokenPayload {
	bucket: AdminDownloadBucket;
	key: string;
	expiresAt: number;
}

function deriveKey(): Buffer {
	return createHmac('sha256', Config.auth.connectionInitiationSecret).update(KEY_DERIVATION_LABEL).digest();
}

function isAdminDownloadTokenPayload(value: unknown): value is AdminDownloadTokenPayload {
	return (
		isJsonRecord(value) &&
		(value['bucket'] === 'harvests' || value['bucket'] === 'reports') &&
		typeof value['key'] === 'string' &&
		typeof value['expiresAt'] === 'number' &&
		Number.isFinite(value['expiresAt'])
	);
}

export function verifyAdminDownloadToken(token: string): AdminDownloadTokenPayload | null {
	return verifyExpiringToken(token, deriveKey(), isAdminDownloadTokenPayload);
}

export async function buildAdminDownloadUrl(
	storageService: Pick<IStorageService, 'getPresignedDownloadURL'>,
	params: {bucket: AdminDownloadBucket; key: string; filename: string; expiresInSeconds: number},
): Promise<string> {
	if (Config.presignedHarvestDownloadsEnabled) {
		return storageService.getPresignedDownloadURL({
			bucket: Config.s3.buckets[params.bucket],
			key: params.key,
			expiresIn: params.expiresInSeconds,
		});
	}
	const token = signExpiringToken(
		{bucket: params.bucket, key: params.key, expiresAt: Date.now() + params.expiresInSeconds * 1000},
		deriveKey(),
	);
	const base = Config.endpoints.apiPublic.replace(/\/+$/u, '');
	return `${base}${ADMIN_DOWNLOAD_PATH}/${encodeURIComponent(params.filename)}?token=${encodeURIComponent(token)}`;
}
