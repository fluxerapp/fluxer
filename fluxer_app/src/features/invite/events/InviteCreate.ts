// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import Invites from '@app/features/invite/state/Invites';
import {currentInstanceTarget} from '@app/features/platform/transport/InstanceHTTP';
import type {Invite} from '@fluxer/schema/src/domains/invite/InviteSchemas';

export function handleInviteCreate(data: Invite, _context: GatewayHandlerContext): void {
	Invites.handleInviteCreate(data, currentInstanceTarget());
}
