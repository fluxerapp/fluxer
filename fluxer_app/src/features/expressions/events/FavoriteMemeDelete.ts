// SPDX-License-Identifier: AGPL-3.0-or-later

import FavoriteMemes from '@app/features/expressions/state/FavoriteMemes';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';

export interface FavoriteMemeDeletePayload {
	id: string;
}

export function handleFavoriteMemeDelete(data: FavoriteMemeDeletePayload, _context: GatewayHandlerContext): void {
	FavoriteMemes.deleteMeme(data.id);
}
