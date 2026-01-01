/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {RelationshipTypes} from '~/Constants';
import type {Relationship} from '~/records/RelationshipRecord';
import MemberSearchStore from '~/stores/MemberSearchStore';
import MessageStore from '~/stores/MessageStore';
import NotificationStore from '~/stores/NotificationStore';
import QuickSwitcherStore from '~/stores/QuickSwitcherStore';
import RelationshipStore from '~/stores/RelationshipStore';
import type {GatewayHandlerContext} from '../index';

interface RelationshipPayload {
	id: string;
	type: number;
}

export function handleRelationshipAdd(data: RelationshipPayload, _context: GatewayHandlerContext): void {
	RelationshipStore.updateRelationship(data as Relationship);
	MemberSearchStore.handleFriendshipChange(data.id, data.type === RelationshipTypes.FRIEND);
	MessageStore.handleRelationshipUpdate();
	QuickSwitcherStore.recomputeIfOpen();
	NotificationStore.handleRelationshipNotification(data as Relationship);
}
