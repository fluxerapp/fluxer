// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RelativePosition} from '@app/features/ui/RelativePosition';

export const DragItemType = Object.freeze({
	CHANNEL: 'channel',
	CATEGORY: 'category',
	VOICE_PARTICIPANT: 'voice-participant',
	GUILD_ITEM: 'guild-item',
	GUILD_FOLDER: 'guild-folder',
	ATTACHMENT: 'attachment',
	CONNECTION: 'connection',
	FAVORITES_CHANNEL: 'favorites-channel',
	FAVORITES_CATEGORY: 'favorites-category',
} as const);

export type DragItemType = (typeof DragItemType)[keyof typeof DragItemType];

export const DropPlacement = Object.freeze({
	INSIDE: 'inside',
	COMBINE: 'combine',
} as const);

export type DropPlacement = (typeof DropPlacement)[keyof typeof DropPlacement];

export type DropPosition = RelativePosition | typeof DropPlacement.INSIDE;

export const DNDReorderState = Object.freeze({
	IDLE: 'idle',
	RESOLVING: 'resolving',
	TARGETING: 'targeting',
	BLOCKED: 'blocked',
} as const);

export type DNDReorderState = (typeof DNDReorderState)[keyof typeof DNDReorderState];

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
	position: DropPosition;
	targetParentId: string | null;
}

export interface GuildDragItem {
	type: typeof DragItemType.GUILD_ITEM | typeof DragItemType.GUILD_FOLDER;
	id: string;
	isFolder: boolean;
	folderId: number | null;
}

export type GuildDropPosition = DropPosition | typeof DropPlacement.COMBINE;

export interface GuildDropResult {
	readonly targetId: string;
	readonly position: GuildDropPosition;
	readonly targetIsFolder: boolean;
	readonly targetFolderId: number | null;
}

export interface AttachmentDragItem {
	type: typeof DragItemType.ATTACHMENT;
	id: number;
	channelId: string;
}

export interface AttachmentDropResult {
	targetId: number;
	position: RelativePosition;
}

export interface ConnectionDragItem {
	type: typeof DragItemType.CONNECTION;
	id: string;
	index: number;
}
