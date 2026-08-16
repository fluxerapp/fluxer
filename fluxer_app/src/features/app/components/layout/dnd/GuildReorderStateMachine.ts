// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	DNDReorderState,
	GuildDragItem,
	GuildDropPosition,
	GuildDropResult,
} from '@app/features/app/components/layout/types/DndTypes';
import {DropPlacement} from '@app/features/app/components/layout/types/DndTypes';
import {Edge, type VerticalEdge} from '@app/features/ui/AxisOrientation';
import {
	getVerticalDropTargetHeight,
	resolveVerticalDropEdge,
	resolveVerticalDropZone,
	type VerticalDropPoint,
	type VerticalDropRect,
} from '@app/features/ui/dnd/DropGeometry';
import {RelativePosition} from '@app/features/ui/RelativePosition';
import {
	assign,
	getInitialSnapshot,
	type MachineSnapshot,
	type MetaObject,
	type NonReducibleUnknown,
	type StateSchema,
	setup,
	transition,
} from 'xstate';

export const GuildReorderTargetKind = Object.freeze({
	FOLDER: 'folder',
	TOP_LEVEL_GUILD: 'top-level-guild',
	FOLDER_GUILD: 'folder-guild',
} as const);

export type GuildReorderTargetKind = (typeof GuildReorderTargetKind)[keyof typeof GuildReorderTargetKind];

interface GuildReorderFolderTarget {
	readonly kind: typeof GuildReorderTargetKind.FOLDER;
	readonly id: string;
}

interface GuildReorderTopLevelGuildTarget {
	readonly kind: typeof GuildReorderTargetKind.TOP_LEVEL_GUILD;
	readonly id: string;
}

interface GuildReorderFolderGuildTarget {
	readonly kind: typeof GuildReorderTargetKind.FOLDER_GUILD;
	readonly id: string;
	readonly folderId: number;
	readonly isTerminal: boolean;
}

export type GuildReorderTarget =
	| GuildReorderFolderTarget
	| GuildReorderTopLevelGuildTarget
	| GuildReorderFolderGuildTarget;

export type GuildFolderReorderIndicator = VerticalEdge | typeof DropPlacement.INSIDE;
export type GuildItemReorderIndicator = VerticalEdge | typeof DropPlacement.COMBINE;
export type GuildReorderIndicator = GuildFolderReorderIndicator | GuildItemReorderIndicator;

export interface GuildReorderIntent {
	indicator: GuildReorderIndicator;
	combineSourceGuildId: string | null;
	result: GuildDropResult;
}

export const GuildReorderBlockedReason = Object.freeze({
	SAME_SOURCE_AND_TARGET: 'same-source-and-target',
	FOLDER_INTO_FOLDER_GUILD: 'folder-into-folder-guild',
	EMPTY_TARGET_RECT: 'empty-target-rect',
} as const);

export type GuildReorderBlockedReason = (typeof GuildReorderBlockedReason)[keyof typeof GuildReorderBlockedReason];

export interface GuildReorderMachineContext {
	intent: GuildReorderIntent | null;
	blockedReason: GuildReorderBlockedReason | null;
}

export type GuildReorderEvent =
	| {
			type: 'drag.hover';
			item: GuildDragItem;
			target: GuildReorderTarget;
			clientOffset: VerticalDropPoint;
			targetRect: VerticalDropRect;
	  }
	| {type: 'drag.leave'}
	| {type: 'drag.drop'};

export interface GuildReorderHoverResolution {
	intent: GuildReorderIntent | null;
	blockedReason: GuildReorderBlockedReason | null;
}

const initialGuildReorderMachineContext: GuildReorderMachineContext = {
	intent: null,
	blockedReason: null,
};

interface CreateGuildDropResultRequest {
	readonly target: GuildReorderTarget;
	readonly position: GuildDropPosition;
}

interface CreateGuildReorderIntentRequest extends CreateGuildDropResultRequest {
	readonly indicator: GuildReorderIndicator;
	readonly combineSourceGuildId: string | null;
}

interface GuildReorderDropQuery {
	readonly item: GuildDragItem;
	readonly target: GuildReorderTarget;
}

interface GuildReorderBlockQuery extends GuildReorderDropQuery {
	readonly targetRect: VerticalDropRect | null;
}

interface ResolveFolderTargetIntentRequest {
	readonly item: GuildDragItem;
	readonly target: GuildReorderFolderTarget;
	readonly clientOffset: VerticalDropPoint;
	readonly targetRect: VerticalDropRect;
}

interface ResolveFolderGuildTargetIntentRequest {
	readonly target: GuildReorderFolderGuildTarget;
	readonly clientOffset: VerticalDropPoint;
	readonly targetRect: VerticalDropRect;
}

interface ResolveTopLevelGuildTargetIntentRequest {
	readonly item: GuildDragItem;
	readonly target: GuildReorderTopLevelGuildTarget;
	readonly clientOffset: VerticalDropPoint;
	readonly targetRect: VerticalDropRect;
}

export interface GuildReorderHoverRequest extends GuildReorderDropQuery {
	readonly clientOffset: VerticalDropPoint;
	readonly targetRect: VerticalDropRect;
}

interface TransitionGuildReorderSnapshotRequest {
	readonly snapshot: GuildReorderSnapshot;
	readonly event: GuildReorderEvent;
}

function createDropResult({target, position}: CreateGuildDropResultRequest): GuildDropResult {
	switch (target.kind) {
		case GuildReorderTargetKind.FOLDER:
			return {
				targetId: target.id,
				position,
				targetIsFolder: true,
				targetFolderId: null,
			};
		case GuildReorderTargetKind.TOP_LEVEL_GUILD:
			return {
				targetId: target.id,
				position,
				targetIsFolder: false,
				targetFolderId: null,
			};
		case GuildReorderTargetKind.FOLDER_GUILD:
			return {
				targetId: target.id,
				position,
				targetIsFolder: false,
				targetFolderId: target.folderId,
			};
		default:
			throw new Error(`Unsupported guild reorder target: ${target satisfies never}`);
	}
}

function createIntent({
	target,
	indicator,
	position,
	combineSourceGuildId,
}: CreateGuildReorderIntentRequest): GuildReorderIntent {
	return {
		indicator,
		combineSourceGuildId,
		result: createDropResult({target, position}),
	};
}

function getGuildReorderBlockedReason({
	item,
	target,
	targetRect,
}: GuildReorderBlockQuery): GuildReorderBlockedReason | null {
	if (item.id === target.id) return GuildReorderBlockedReason.SAME_SOURCE_AND_TARGET;
	if (target.kind === GuildReorderTargetKind.FOLDER_GUILD && item.isFolder) {
		return GuildReorderBlockedReason.FOLDER_INTO_FOLDER_GUILD;
	}
	if (targetRect != null && getVerticalDropTargetHeight(targetRect) <= 0) {
		return GuildReorderBlockedReason.EMPTY_TARGET_RECT;
	}
	return null;
}

function canGuildDropOnTarget({item, target}: GuildReorderDropQuery): boolean {
	return getGuildReorderBlockedReason({item, target, targetRect: null}) == null;
}

function resolveFolderTargetIntent({
	item,
	target,
	clientOffset,
	targetRect,
}: ResolveFolderTargetIntentRequest): GuildReorderIntent {
	if (item.isFolder) {
		return createIntent({
			target,
			indicator: Edge.TOP,
			position: RelativePosition.BEFORE,
			combineSourceGuildId: null,
		});
	}
	const zone = resolveVerticalDropZone(clientOffset, targetRect, 0.25);
	if (zone === RelativePosition.BEFORE) {
		return createIntent({
			target,
			indicator: Edge.TOP,
			position: RelativePosition.BEFORE,
			combineSourceGuildId: null,
		});
	}
	return createIntent({
		target,
		indicator: DropPlacement.INSIDE,
		position: DropPlacement.INSIDE,
		combineSourceGuildId: null,
	});
}

function resolveFolderGuildTargetIntent({
	target,
	clientOffset,
	targetRect,
}: ResolveFolderGuildTargetIntentRequest): GuildReorderIntent {
	const zone = resolveVerticalDropEdge(clientOffset, targetRect);
	if (target.isTerminal && zone === RelativePosition.AFTER) {
		return createIntent({
			target,
			indicator: Edge.BOTTOM,
			position: RelativePosition.AFTER,
			combineSourceGuildId: null,
		});
	}
	return createIntent({
		target,
		indicator: Edge.TOP,
		position: RelativePosition.BEFORE,
		combineSourceGuildId: null,
	});
}

function resolveTopLevelGuildTargetIntent({
	item,
	target,
	clientOffset,
	targetRect,
}: ResolveTopLevelGuildTargetIntentRequest): GuildReorderIntent {
	if (item.isFolder) {
		return createIntent({
			target,
			indicator: Edge.TOP,
			position: RelativePosition.BEFORE,
			combineSourceGuildId: null,
		});
	}
	const zone = resolveVerticalDropZone(clientOffset, targetRect, 0.25);
	if (zone === RelativePosition.BEFORE) {
		return createIntent({
			target,
			indicator: Edge.TOP,
			position: RelativePosition.BEFORE,
			combineSourceGuildId: null,
		});
	}
	return createIntent({
		target,
		indicator: DropPlacement.COMBINE,
		position: DropPlacement.COMBINE,
		combineSourceGuildId: item.id,
	});
}

export function resolveGuildReorderHover({
	item,
	target,
	clientOffset,
	targetRect,
}: GuildReorderHoverRequest): GuildReorderHoverResolution {
	const blockedReason = getGuildReorderBlockedReason({item, target, targetRect});
	if (blockedReason != null) {
		return {intent: null, blockedReason};
	}
	switch (target.kind) {
		case GuildReorderTargetKind.FOLDER:
			return {intent: resolveFolderTargetIntent({item, target, clientOffset, targetRect}), blockedReason: null};
		case GuildReorderTargetKind.FOLDER_GUILD:
			return {intent: resolveFolderGuildTargetIntent({target, clientOffset, targetRect}), blockedReason: null};
		case GuildReorderTargetKind.TOP_LEVEL_GUILD:
			return {intent: resolveTopLevelGuildTargetIntent({item, target, clientOffset, targetRect}), blockedReason: null};
		default:
			throw new Error(`Unsupported guild reorder target: ${target satisfies never}`);
	}
}

const guildReorderStateMachine = setup({
	types: {} as {
		context: GuildReorderMachineContext;
		events: GuildReorderEvent;
	},
	guards: {
		hasIntent: ({context}) => context.intent != null,
	},
	actions: {
		resolveHover: assign(({event}) => {
			if (event.type !== 'drag.hover') return initialGuildReorderMachineContext;
			return resolveGuildReorderHover({
				item: event.item,
				target: event.target,
				clientOffset: event.clientOffset,
				targetRect: event.targetRect,
			});
		}),
		clear: assign(() => initialGuildReorderMachineContext),
	},
}).createMachine({
	id: 'guildReorder',
	initial: 'idle',
	context: initialGuildReorderMachineContext,
	states: {
		idle: {
			on: {
				'drag.hover': {target: 'resolving', actions: 'resolveHover'},
				'drag.leave': {actions: 'clear'},
				'drag.drop': {actions: 'clear'},
			},
		},
		resolving: {
			always: [{target: 'targeting', guard: 'hasIntent'}, {target: 'blocked'}],
		},
		targeting: {
			on: {
				'drag.hover': {target: 'resolving', actions: 'resolveHover'},
				'drag.leave': {target: 'idle', actions: 'clear'},
				'drag.drop': {target: 'idle', actions: 'clear'},
			},
		},
		blocked: {
			on: {
				'drag.hover': {target: 'resolving', actions: 'resolveHover'},
				'drag.leave': {target: 'idle', actions: 'clear'},
				'drag.drop': {target: 'idle', actions: 'clear'},
			},
		},
	},
});

export type GuildReorderSnapshot = MachineSnapshot<
	GuildReorderMachineContext,
	GuildReorderEvent,
	Record<string, never>,
	DNDReorderState,
	string,
	NonReducibleUnknown,
	MetaObject,
	StateSchema
>;

export function createGuildReorderSnapshot(): GuildReorderSnapshot {
	return getInitialSnapshot(guildReorderStateMachine);
}

export function transitionGuildReorderSnapshot({
	snapshot,
	event,
}: TransitionGuildReorderSnapshotRequest): GuildReorderSnapshot {
	return transition(guildReorderStateMachine, snapshot, event)[0];
}

function selectGuildReorderIntent(request: GuildReorderHoverRequest): GuildReorderIntent | null {
	const snapshot = transitionGuildReorderSnapshot({
		snapshot: createGuildReorderSnapshot(),
		event: {
			type: 'drag.hover',
			item: request.item,
			target: request.target,
			clientOffset: request.clientOffset,
			targetRect: request.targetRect,
		},
	});
	return snapshot.context.intent;
}

export const GuildReorderStateMachine = Object.freeze({
	canDrop: canGuildDropOnTarget,
	selectIntent: selectGuildReorderIntent,
});
