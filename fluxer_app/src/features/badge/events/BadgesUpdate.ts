// SPDX-License-Identifier: AGPL-3.0-or-later

import Badges from '@app/features/badge/state/Badges';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import type {
	BadgesUpdateDispatchData,
	UserBadgesUpdateDispatchData,
} from '@fluxer/schema/src/domains/badge/BadgeSchemas';

export function handleBadgesUpdate(data: BadgesUpdateDispatchData, _context: GatewayHandlerContext): void {
	Badges.handleBadgesUpdate(data);
}

export function handleUserBadgesUpdate(data: UserBadgesUpdateDispatchData, _context: GatewayHandlerContext): void {
	Badges.handleUserBadgesUpdate(data);
}
