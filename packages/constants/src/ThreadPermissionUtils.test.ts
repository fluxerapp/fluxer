// SPDX-License-Identifier: AGPL-3.0-or-later

import {ALL_PERMISSIONS, ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import cases from '@fluxer/constants/src/ThreadPermissionCases.json' with {type: 'json'};
import {
	canAddThreadMember,
	canCreateThread,
	canDeleteThread,
	canJoinThread,
	canLeave,
	canListArchivedThreads,
	canRemoveThreadMember,
	canSetTags,
	canUnarchive,
	canViewThread,
	DEFAULT_THREAD_PERMISSIONS,
	isThreadModerator,
	THREAD_AWARE_ALL_PERMISSIONS,
	THREAD_PERMISSIONS,
	type ThreadActorContext,
	type ThreadCreateKind,
	type ThreadPatch,
	ThreadPermissionFlags,
	type ThreadWriteAction,
	threadPatchRequirement,
	threadViewPermissions,
	threadWriteBlock,
	withImplicitThreadBits,
} from '@fluxer/constants/src/ThreadPermissionUtils';
import {describe, expect, it} from 'vitest';

interface CaseRow {
	name: string;
	fn: string;
	actor: {
		perms: Array<string>;
		isOwner?: boolean;
		timedOut?: boolean;
		isThreadOwner?: boolean;
		isMember?: boolean;
	};
	thread?: {type?: number; archived?: boolean; locked?: boolean; invitable?: boolean};
	args?: {
		action?: string;
		patch?: ThreadPatch;
		currentFlags?: number;
		touchesModeratedTag?: boolean;
		target?: {canViewParent: boolean; isModerator: boolean};
		kind?: string;
		privateThreads?: boolean;
	};
	expect: string | boolean | null;
}

const PERMISSION_BITS: Record<string, bigint> = {...Permissions, ...ThreadPermissionFlags};

function permissionsOf(names: Array<string>): bigint {
	return names.reduce((acc, name) => {
		const bit = PERMISSION_BITS[name];
		if (bit === undefined) throw new Error(`unknown permission ${name}`);
		return acc | bit;
	}, 0n);
}

function contextOf(row: CaseRow): ThreadActorContext {
	return {
		permissions: permissionsOf(row.actor.perms),
		isOwner: row.actor.isOwner ?? false,
		timedOut: row.actor.timedOut ?? false,
		isThreadOwner: row.actor.isThreadOwner ?? false,
		isMember: row.actor.isMember ?? false,
		thread: {
			type: row.thread?.type ?? ChannelTypes.PUBLIC_THREAD,
			archived: row.thread?.archived ?? false,
			locked: row.thread?.locked ?? false,
			invitable: row.thread?.invitable ?? true,
		},
	};
}

function evaluate(row: CaseRow): string | boolean | null {
	const ctx = contextOf(row);
	const args = row.args ?? {};
	switch (row.fn) {
		case 'isThreadModerator':
			return isThreadModerator(ctx.permissions, ctx);
		case 'canViewThread':
			return canViewThread(ctx);
		case 'threadWriteBlock':
			return threadWriteBlock(args.action as ThreadWriteAction, ctx);
		case 'canUnarchive':
			return canUnarchive(ctx);
		case 'threadPatchRequirement':
			return threadPatchRequirement(args.patch ?? {}, args.currentFlags ?? 0, ctx);
		case 'canSetTags':
			return canSetTags(ctx, {touchesModeratedTag: args.touchesModeratedTag ?? false});
		case 'canJoinThread':
			return canJoinThread(ctx);
		case 'canAddThreadMember':
			return canAddThreadMember(ctx, args.target ?? {canViewParent: true, isModerator: false});
		case 'canRemoveThreadMember':
			return canRemoveThreadMember(ctx);
		case 'canLeave':
			return canLeave(ctx);
		case 'canDeleteThread':
			return canDeleteThread(ctx);
		case 'canCreateThread':
			return canCreateThread(ctx, args.kind as ThreadCreateKind);
		case 'canListArchivedThreads':
			return canListArchivedThreads(ctx, {privateThreads: args.privateThreads ?? false});
		default:
			throw new Error(`unknown case function ${row.fn}`);
	}
}

describe('thread permission case table', () => {
	it.each((cases as Array<CaseRow>).map((row) => [row.name, row] as const))('%s', (_name, row) => {
		expect(evaluate(row)).toBe(row.expect);
	});

	it('has unique case names', () => {
		const names = (cases as Array<CaseRow>).map((row) => row.name);
		expect(new Set(names).size).toBe(names.length);
	});
});

describe('thread permission bits', () => {
	it('uses the expected bit positions', () => {
		expect(ThreadPermissionFlags.MANAGE_THREADS).toBe(1n << 34n);
		expect(ThreadPermissionFlags.CREATE_PUBLIC_THREADS).toBe(1n << 35n);
		expect(ThreadPermissionFlags.CREATE_PRIVATE_THREADS).toBe(1n << 36n);
		expect(ThreadPermissionFlags.SEND_MESSAGES_IN_THREADS).toBe(1n << 38n);
	});

	it('stays outside the control permission set', () => {
		expect(ALL_PERMISSIONS & THREAD_PERMISSIONS).toBe(0n);
		expect(THREAD_AWARE_ALL_PERMISSIONS).toBe(ALL_PERMISSIONS | THREAD_PERMISSIONS);
	});

	it('defaults to everything except manage threads', () => {
		expect(DEFAULT_THREAD_PERMISSIONS).toBe(THREAD_PERMISSIONS & ~ThreadPermissionFlags.MANAGE_THREADS);
	});

	it('grants every thread bit to administrators only', () => {
		expect(withImplicitThreadBits(Permissions.ADMINISTRATOR) & THREAD_PERMISSIONS).toBe(THREAD_PERMISSIONS);
		expect(withImplicitThreadBits(Permissions.MANAGE_CHANNELS)).toBe(Permissions.MANAGE_CHANNELS);
	});

	it('aliases send messages to send messages in threads', () => {
		const parent = Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES;
		expect(threadViewPermissions(parent) & Permissions.SEND_MESSAGES).toBe(0n);
		const threadSender = Permissions.VIEW_CHANNEL | ThreadPermissionFlags.SEND_MESSAGES_IN_THREADS;
		expect(threadViewPermissions(threadSender) & Permissions.SEND_MESSAGES).toBe(Permissions.SEND_MESSAGES);
		expect(threadViewPermissions(Permissions.ADMINISTRATOR) & Permissions.SEND_MESSAGES).toBe(
			Permissions.SEND_MESSAGES,
		);
	});
});
