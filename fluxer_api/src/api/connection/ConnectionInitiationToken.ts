// SPDX-License-Identifier: AGPL-3.0-or-later

import {isJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import {signExpiringToken, verifyExpiringToken} from '@app/api/utils/SignedExpiringToken';
import {type ConnectionType, ConnectionTypes} from '@fluxer/constants/src/ConnectionConstants';

interface ConnectionInitiationTokenPayload {
	userId: string;
	type: ConnectionType;
	identifier: string;
	verificationCode: string;
	expiresAt: number;
}

function isConnectionType(value: unknown): value is ConnectionType {
	return value === ConnectionTypes.BLUESKY || value === ConnectionTypes.DOMAIN;
}

function isConnectionInitiationTokenPayload(value: unknown): value is ConnectionInitiationTokenPayload {
	if (!isJsonRecord(value)) return false;
	return (
		typeof value['userId'] === 'string' &&
		isConnectionType(value['type']) &&
		typeof value['identifier'] === 'string' &&
		typeof value['verificationCode'] === 'string' &&
		typeof value['expiresAt'] === 'number' &&
		Number.isFinite(value['expiresAt'])
	);
}

export function signInitiationToken(payload: ConnectionInitiationTokenPayload, secret: string): string {
	return signExpiringToken(payload, secret);
}

export function verifyInitiationToken(token: string, secret: string): ConnectionInitiationTokenPayload | null {
	return verifyExpiringToken(token, secret, isConnectionInitiationTokenPayload);
}
