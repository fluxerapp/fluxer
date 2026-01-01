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

export const DND_TYPES = {
	CHANNEL: 'channel',
	CATEGORY: 'category',
	VOICE_PARTICIPANT: 'voice-participant',
} as const;

export interface DragItem {
	type: string;
	id: string;
	channelType: number;
	parentId: string | null;
	guildId: string;
	userId?: string;
	currentChannelId?: string;
}

export interface DropResult {
	targetId: string;
	position: 'before' | 'after' | 'inside';
	targetParentId: string | null;
}
