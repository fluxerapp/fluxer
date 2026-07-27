import type {ActivityPayload} from '@app/features/gateway/types/GatewayPresenceTypes';
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayCustomStatusPayload} from '@app/features/user/state/CustomStatus';
import {StatusTypes} from '@fluxer/constants/src/StatusConstants';

export interface GatewayPresenceRetirementSocket {
	updatePresence(
		status: string,
		afk?: boolean,
		mobile?: boolean,
		customStatus?: GatewayCustomStatusPayload | null,
		activities?: readonly ActivityPayload[] | null,
	): void;
}

export interface GatewayPresenceRetirementLogger {
	warn(message: string, error?: unknown): void;
}

export type GatewaySessionRetirementReason = 'logout' | 'account-switch';

export function sendInvisiblePresenceForLocalSession(
	socket: GatewayPresenceRetirementSocket | null,
	isMobile: boolean,
	reason: GatewaySessionRetirementReason,
	logger: GatewayPresenceRetirementLogger,
): boolean {
	if (!socket) {
		return false;
	}
	try {
		socket.updatePresence(StatusTypes.INVISIBLE, false, isMobile, null, null);
		return true;
	} catch (err) {
		logger.warn(`Failed to send invisible presence before ${reason}`, err);
		return false;
	}
}
