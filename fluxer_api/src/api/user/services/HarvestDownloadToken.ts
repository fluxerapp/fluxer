// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac} from 'node:crypto';
import {isJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import {signExpiringToken, verifyExpiringToken} from '@app/api/utils/SignedExpiringToken';

const KEY_DERIVATION_LABEL = 'fluxer.harvest-download.v1';

interface HarvestDownloadTokenPayload {
	userId: string;
	harvestId: string;
	storageKey: string;
	expiresAt: number;
}

function deriveKey(secret: string): Buffer {
	return createHmac('sha256', secret).update(KEY_DERIVATION_LABEL).digest();
}

function isHarvestDownloadTokenPayload(value: unknown): value is HarvestDownloadTokenPayload {
	if (!isJsonRecord(value)) return false;
	return (
		typeof value['userId'] === 'string' &&
		typeof value['harvestId'] === 'string' &&
		typeof value['storageKey'] === 'string' &&
		typeof value['expiresAt'] === 'number' &&
		Number.isFinite(value['expiresAt'])
	);
}

export function signHarvestDownloadToken(payload: HarvestDownloadTokenPayload, secret: string): string {
	return signExpiringToken(payload, deriveKey(secret));
}

export function verifyHarvestDownloadToken(token: string, secret: string): HarvestDownloadTokenPayload | null {
	return verifyExpiringToken(token, deriveKey(secret), isHarvestDownloadTokenPayload);
}
