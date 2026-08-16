// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createGuildReorderSnapshot,
	GuildReorderBlockedReason,
	GuildReorderStateMachine,
	type GuildReorderTarget,
	GuildReorderTargetKind,
	resolveGuildReorderHover,
	transitionGuildReorderSnapshot,
} from '@app/features/app/components/layout/dnd/GuildReorderStateMachine';
import {
	DNDReorderState,
	DragItemType,
	DropPlacement,
	type GuildDragItem,
} from '@app/features/app/components/layout/types/DndTypes';
import {Edge} from '@app/features/ui/AxisOrientation';
import type {VerticalDropRect} from '@app/features/ui/dnd/DropGeometry';
import {RelativePosition} from '@app/features/ui/RelativePosition';
import {describe, expect, it} from 'vitest';

const rect: VerticalDropRect = {
	top: 100,
	bottom: 154,
};

function guildItem(id: string, folderId: number | null = null): GuildDragItem {
	return {
		type: DragItemType.GUILD_ITEM,
		id,
		isFolder: false,
		folderId,
	};
}

function folderItem(id = 'folder-1', folderId = 1): GuildDragItem {
	return {
		type: DragItemType.GUILD_FOLDER,
		id,
		isFolder: true,
		folderId,
	};
}

function rootGuildTarget(id = 'guild-target'): GuildReorderTarget {
	return {
		id,
		kind: GuildReorderTargetKind.TOP_LEVEL_GUILD,
	};
}

function folderGuildTarget(id = 'guild-target', isTerminal = false): GuildReorderTarget {
	return {
		id,
		kind: GuildReorderTargetKind.FOLDER_GUILD,
		folderId: 1,
		isTerminal,
	};
}

function folderTarget(id = 'folder-1'): GuildReorderTarget {
	return {
		id,
		kind: GuildReorderTargetKind.FOLDER,
	};
}

function point(y: number) {
	return {
		x: 36,
		y,
	};
}

describe('GuildReorderStateMachine', () => {
	it('starts idle and targets a valid top-level reorder hover', () => {
		let snapshot = createGuildReorderSnapshot();
		expect(snapshot.value).toBe(DNDReorderState.IDLE);
		snapshot = transitionGuildReorderSnapshot({
			snapshot,
			event: {
				type: 'drag.hover',
				item: guildItem('guild-source'),
				target: rootGuildTarget(),
				clientOffset: point(104),
				targetRect: rect,
			},
		});
		expect(snapshot.value).toBe(DNDReorderState.TARGETING);
		expect(snapshot.context.intent?.result).toMatchObject({
			targetId: 'guild-target',
			position: RelativePosition.BEFORE,
			targetIsFolder: false,
		});
		expect(snapshot.context.intent?.indicator).toBe(Edge.TOP);
	});

	it('moves to blocked state for invalid hovers and clears on leave', () => {
		let snapshot = createGuildReorderSnapshot();
		snapshot = transitionGuildReorderSnapshot({
			snapshot,
			event: {
				type: 'drag.hover',
				item: guildItem('guild-target'),
				target: rootGuildTarget('guild-target'),
				clientOffset: point(104),
				targetRect: rect,
			},
		});
		expect(snapshot.value).toBe(DNDReorderState.BLOCKED);
		expect(snapshot.context.blockedReason).toBe(GuildReorderBlockedReason.SAME_SOURCE_AND_TARGET);
		snapshot = transitionGuildReorderSnapshot({snapshot, event: {type: 'drag.leave'}});
		expect(snapshot.value).toBe(DNDReorderState.IDLE);
		expect(snapshot.context.intent).toBeNull();
		expect(snapshot.context.blockedReason).toBeNull();
	});

	it('clears targeting state after drop', () => {
		let snapshot = createGuildReorderSnapshot();
		snapshot = transitionGuildReorderSnapshot({
			snapshot,
			event: {
				type: 'drag.hover',
				item: guildItem('guild-source'),
				target: rootGuildTarget(),
				clientOffset: point(104),
				targetRect: rect,
			},
		});
		snapshot = transitionGuildReorderSnapshot({snapshot, event: {type: 'drag.drop'}});
		expect(snapshot.value).toBe(DNDReorderState.IDLE);
		expect(snapshot.context.intent).toBeNull();
	});

	it('covers every pixel of a top-level guild target with either reorder-before or combine', () => {
		const item = guildItem('guild-source');
		const target = rootGuildTarget();
		for (let y = rect.top; y <= rect.bottom; y++) {
			const intent = GuildReorderStateMachine.selectIntent({item, target, clientOffset: point(y), targetRect: rect});
			expect(intent, `expected intent for y=${y}`).not.toBeNull();
			expect([RelativePosition.BEFORE, DropPlacement.COMBINE]).toContain(intent?.result.position);
		}
	});

	it('uses one top-level insertion target between adjacent guilds by never returning after from an item hover', () => {
		const item = guildItem('guild-source');
		const target = rootGuildTarget('guild-next');
		const upperEdgeIntent = GuildReorderStateMachine.selectIntent({
			item,
			target,
			clientOffset: point(101),
			targetRect: rect,
		});
		const middleIntent = GuildReorderStateMachine.selectIntent({
			item,
			target,
			clientOffset: point(130),
			targetRect: rect,
		});
		const lowerEdgeIntent = GuildReorderStateMachine.selectIntent({
			item,
			target,
			clientOffset: point(153),
			targetRect: rect,
		});
		expect(upperEdgeIntent?.result.position).toBe(RelativePosition.BEFORE);
		expect(middleIntent?.result.position).toBe(DropPlacement.COMBINE);
		expect(lowerEdgeIntent?.result.position).toBe(DropPlacement.COMBINE);
		expect(middleIntent?.result.position).not.toBe(RelativePosition.AFTER);
		expect(lowerEdgeIntent?.result.position).not.toBe(RelativePosition.AFTER);
	});

	it('maps root guild top edge to reorder and center/bottom to folder combine for guild sources', () => {
		const item = guildItem('guild-source');
		const target = rootGuildTarget();
		expect(
			GuildReorderStateMachine.selectIntent({item, target, clientOffset: point(100), targetRect: rect}),
		).toMatchObject({
			indicator: Edge.TOP,
			result: {position: RelativePosition.BEFORE},
		});
		expect(
			GuildReorderStateMachine.selectIntent({item, target, clientOffset: point(114), targetRect: rect}),
		).toMatchObject({
			indicator: DropPlacement.COMBINE,
			combineSourceGuildId: 'guild-source',
			result: {position: DropPlacement.COMBINE},
		});
		expect(
			GuildReorderStateMachine.selectIntent({item, target, clientOffset: point(154), targetRect: rect}),
		).toMatchObject({
			indicator: DropPlacement.COMBINE,
			combineSourceGuildId: 'guild-source',
			result: {position: DropPlacement.COMBINE},
		});
	});

	it('does not combine folders with root guilds while dragging a folder', () => {
		const item = folderItem();
		const target = rootGuildTarget();
		for (const y of [rect.top, 127, rect.bottom]) {
			expect(
				GuildReorderStateMachine.selectIntent({item, target, clientOffset: point(y), targetRect: rect}),
			).toMatchObject({
				indicator: Edge.TOP,
				result: {
					position: RelativePosition.BEFORE,
					targetIsFolder: false,
				},
			});
		}
	});

	it('uses a single before-target slot for non-terminal guilds inside a folder', () => {
		const item = guildItem('guild-source', 1);
		const target = folderGuildTarget('guild-middle', false);
		for (const y of [rect.top, 127, rect.bottom]) {
			expect(
				GuildReorderStateMachine.selectIntent({item, target, clientOffset: point(y), targetRect: rect}),
			).toMatchObject({
				indicator: Edge.TOP,
				result: {
					targetId: 'guild-middle',
					position: RelativePosition.BEFORE,
					targetIsFolder: false,
					targetFolderId: 1,
				},
			});
		}
	});

	it('allows the terminal guild inside a folder to own the final after slot', () => {
		const item = guildItem('guild-source', 1);
		const target = folderGuildTarget('guild-last', true);
		expect(
			GuildReorderStateMachine.selectIntent({item, target, clientOffset: point(101), targetRect: rect}),
		).toMatchObject({
			indicator: Edge.TOP,
			result: {
				targetId: 'guild-last',
				position: RelativePosition.BEFORE,
				targetFolderId: 1,
			},
		});
		expect(
			GuildReorderStateMachine.selectIntent({item, target, clientOffset: point(153), targetRect: rect}),
		).toMatchObject({
			indicator: Edge.BOTTOM,
			result: {
				targetId: 'guild-last',
				position: RelativePosition.AFTER,
				targetFolderId: 1,
			},
		});
	});

	it('blocks dragging a folder onto a guild inside another folder', () => {
		const item = folderItem('folder-2', 2);
		const target = folderGuildTarget('guild-inside-folder', true);
		expect(GuildReorderStateMachine.canDrop({item, target})).toBe(false);
		expect(resolveGuildReorderHover({item, target, clientOffset: point(127), targetRect: rect})).toEqual({
			intent: null,
			blockedReason: GuildReorderBlockedReason.FOLDER_INTO_FOLDER_GUILD,
		});
	});

	it('uses top edge for reordering before a folder and the rest of a folder target for adding a guild inside', () => {
		const item = guildItem('guild-source');
		const target = folderTarget('folder-1');
		expect(
			GuildReorderStateMachine.selectIntent({item, target, clientOffset: point(101), targetRect: rect}),
		).toMatchObject({
			indicator: Edge.TOP,
			result: {
				targetId: 'folder-1',
				position: RelativePosition.BEFORE,
				targetIsFolder: true,
			},
		});
		expect(
			GuildReorderStateMachine.selectIntent({item, target, clientOffset: point(127), targetRect: rect}),
		).toMatchObject({
			indicator: DropPlacement.INSIDE,
			result: {
				targetId: 'folder-1',
				position: DropPlacement.INSIDE,
				targetIsFolder: true,
			},
		});
		expect(
			GuildReorderStateMachine.selectIntent({item, target, clientOffset: point(153), targetRect: rect}),
		).toMatchObject({
			indicator: DropPlacement.INSIDE,
			result: {
				targetId: 'folder-1',
				position: DropPlacement.INSIDE,
				targetIsFolder: true,
			},
		});
	});

	it('only reorders before a folder when dragging another folder', () => {
		const item = folderItem('folder-2', 2);
		const target = folderTarget('folder-1');
		for (const y of [rect.top, 127, rect.bottom]) {
			expect(
				GuildReorderStateMachine.selectIntent({item, target, clientOffset: point(y), targetRect: rect}),
			).toMatchObject({
				indicator: Edge.TOP,
				result: {
					targetId: 'folder-1',
					position: RelativePosition.BEFORE,
					targetIsFolder: true,
				},
			});
		}
	});

	it('blocks zero-height target rects instead of producing a void result', () => {
		const emptyRect = {...rect, bottom: rect.top};
		expect(
			resolveGuildReorderHover({
				item: guildItem('guild-source'),
				target: rootGuildTarget(),
				clientOffset: point(rect.top),
				targetRect: emptyRect,
			}),
		).toEqual({
			intent: null,
			blockedReason: GuildReorderBlockedReason.EMPTY_TARGET_RECT,
		});
	});
});
