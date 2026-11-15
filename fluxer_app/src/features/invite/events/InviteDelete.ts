// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import Invites from '@app/features/invite/state/Invites';
import {currentInstanceTarget} from '@app/features/platform/transport/InstanceHTTP';

interface InviteDeletePayload {
	code: string;
	guild_id: string;
}

export function handleInviteDelete(data: InviteDeletePayload, _context: GatewayHandlerContext): void {
	Invites.handleInviteDelete(data.code, currentInstanceTarget());
}
