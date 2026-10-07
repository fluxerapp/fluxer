// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ALL_PERMISSIONS, ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {THREAD_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';

export const ThreadPermissionFlags = {
	MANAGE_THREADS: 1n << 34n,
	CREATE_PUBLIC_THREADS: 1n << 35n,
	CREATE_PRIVATE_THREADS: 1n << 36n,
	SEND_MESSAGES_IN_THREADS: 1n << 38n,
} as const;

export const THREAD_PERMISSIONS = Object.values(ThreadPermissionFlags).reduce((acc, p) => acc | p, 0n);
export const DEFAULT_THREAD_PERMISSIONS =
	ThreadPermissionFlags.CREATE_PUBLIC_THREADS |
	ThreadPermissionFlags.CREATE_PRIVATE_THREADS |
	ThreadPermissionFlags.SEND_MESSAGES_IN_THREADS;
export const THREAD_AWARE_ALL_PERMISSIONS = ALL_PERMISSIONS | THREAD_PERMISSIONS;

export type ThreadDenial =
	| typeof APIErrorCodes.MISSING_ACCESS
	| typeof APIErrorCodes.MISSING_PERMISSIONS
	| typeof APIErrorCodes.COMMUNICATION_DISABLED
	| typeof APIErrorCodes.THREAD_ARCHIVED
	| typeof APIErrorCodes.THREAD_LOCKED
	| typeof APIErrorCodes.UNKNOWN_THREAD_MEMBER;

export interface ThreadActor {
	permissions: bigint;
	isOwner: boolean;
	timedOut: boolean;
}

export interface ThreadFacts {
	type: number;
	archived: boolean;
	locked: boolean;
	invitable: boolean;
}

export interface ThreadActorContext extends ThreadActor {
	thread: ThreadFacts;
	isThreadOwner: boolean;
	isMember: boolean;
}

export type ThreadWriteAction = 'send' | 'react' | 'pin' | 'edit' | 'delete';

export interface ThreadPatch {
	name?: unknown;
	archived?: boolean;
	auto_archive_duration?: unknown;
	locked?: boolean;
	invitable?: unknown;
	rate_limit_per_user?: unknown;
	flags?: number;
	applied_tags?: unknown;
}

export type ThreadCreateKind = 'from_message' | 'public' | 'private' | 'forum_post';

function has(perms: bigint, bit: bigint): boolean {
	return (perms & bit) === bit;
}

export function withImplicitThreadBits(perms: bigint): bigint {
	return has(perms, Permissions.ADMINISTRATOR) ? perms | THREAD_AWARE_ALL_PERMISSIONS : perms;
}

export function isThreadModerator(perms: bigint, {isOwner, timedOut}: {isOwner: boolean; timedOut: boolean}): boolean {
	if (isOwner || has(perms, Permissions.ADMINISTRATOR)) return true;
	return !timedOut && has(perms, ThreadPermissionFlags.MANAGE_THREADS);
}

function isCommunicationBlocked(actor: ThreadActor): boolean {
	return actor.timedOut && !actor.isOwner && !has(actor.permissions, Permissions.ADMINISTRATOR);
}

function isModerator(actor: ThreadActor): boolean {
	return isThreadModerator(withImplicitThreadBits(actor.permissions), actor);
}

export function threadViewPermissions(parentPerms: bigint): bigint {
	const perms = withImplicitThreadBits(parentPerms);
	const withoutSend = perms & ~Permissions.SEND_MESSAGES;
	return has(perms, ThreadPermissionFlags.SEND_MESSAGES_IN_THREADS)
		? withoutSend | Permissions.SEND_MESSAGES
		: withoutSend;
}

export function messageSendPermissionFor(type: number): bigint {
	return THREAD_CHANNEL_TYPES.has(type) ? ThreadPermissionFlags.SEND_MESSAGES_IN_THREADS : Permissions.SEND_MESSAGES;
}

export function canViewThread(ctx: ThreadActorContext): ThreadDenial | null {
	const perms = withImplicitThreadBits(ctx.permissions);
	if (!ctx.isOwner && !has(perms, Permissions.VIEW_CHANNEL)) return APIErrorCodes.MISSING_ACCESS;
	if (ctx.thread.type === ChannelTypes.PRIVATE_THREAD && !ctx.isMember && !isModerator(ctx)) {
		return APIErrorCodes.MISSING_ACCESS;
	}
	return null;
}

export function threadWriteBlock(action: ThreadWriteAction, ctx: ThreadActorContext): ThreadDenial | null {
	if (action === 'delete') return null;
	if (isCommunicationBlocked(ctx)) return APIErrorCodes.COMMUNICATION_DISABLED;
	const moderator = isModerator(ctx);
	if (action === 'send') {
		return ctx.thread.locked && !moderator ? APIErrorCodes.THREAD_LOCKED : null;
	}
	if (ctx.thread.archived) return APIErrorCodes.THREAD_ARCHIVED;
	if (ctx.thread.locked && !moderator) return APIErrorCodes.THREAD_LOCKED;
	return null;
}

export function canUnarchive(ctx: ThreadActorContext): ThreadDenial | null {
	if (isCommunicationBlocked(ctx)) return APIErrorCodes.COMMUNICATION_DISABLED;
	if (isModerator(ctx)) return null;
	if (ctx.thread.locked) return APIErrorCodes.THREAD_LOCKED;
	return ctx.isMember || ctx.isThreadOwner ? null : APIErrorCodes.MISSING_PERMISSIONS;
}

export function threadPatchRequirement(
	patch: ThreadPatch,
	currentFlags: number,
	ctx: ThreadActorContext,
): ThreadDenial | null {
	if (isCommunicationBlocked(ctx)) return APIErrorCodes.COMMUNICATION_DISABLED;
	const moderator = isModerator(ctx);
	const ownerOrModerator = moderator || ctx.isThreadOwner;
	if (patch.archived === false && ctx.thread.archived) {
		const unarchive = canUnarchive(ctx);
		if (unarchive !== null) return unarchive;
	}
	const staysArchived = ctx.thread.archived && patch.archived !== false;
	const touchesOtherFields =
		patch.name !== undefined ||
		patch.auto_archive_duration !== undefined ||
		patch.locked !== undefined ||
		patch.invitable !== undefined ||
		patch.rate_limit_per_user !== undefined ||
		(patch.flags !== undefined && patch.flags !== currentFlags) ||
		patch.applied_tags !== undefined;
	if (staysArchived && touchesOtherFields) return APIErrorCodes.THREAD_ARCHIVED;
	if (patch.locked !== undefined && patch.locked !== ctx.thread.locked && !moderator) {
		if (!(patch.locked && ctx.isThreadOwner)) return APIErrorCodes.MISSING_PERMISSIONS;
	}
	if (ctx.thread.locked && !moderator) return APIErrorCodes.THREAD_LOCKED;
	if ((patch.name !== undefined || patch.auto_archive_duration !== undefined) && !ownerOrModerator) {
		return APIErrorCodes.MISSING_PERMISSIONS;
	}
	if (patch.archived === true && !ctx.thread.archived && !ownerOrModerator) return APIErrorCodes.MISSING_PERMISSIONS;
	if (patch.invitable !== undefined && !ownerOrModerator) return APIErrorCodes.MISSING_PERMISSIONS;
	if (patch.rate_limit_per_user !== undefined && !moderator) return APIErrorCodes.MISSING_PERMISSIONS;
	if (patch.flags !== undefined && patch.flags !== currentFlags && !moderator) return APIErrorCodes.MISSING_PERMISSIONS;
	return null;
}

export function canSetTags(
	ctx: ThreadActorContext,
	{touchesModeratedTag}: {touchesModeratedTag: boolean},
): ThreadDenial | null {
	if (isCommunicationBlocked(ctx)) return APIErrorCodes.COMMUNICATION_DISABLED;
	const moderator = isModerator(ctx);
	if (!moderator && !ctx.isThreadOwner) return APIErrorCodes.MISSING_PERMISSIONS;
	if (touchesModeratedTag && !moderator) return APIErrorCodes.MISSING_PERMISSIONS;
	return null;
}

export function canJoinThread(ctx: ThreadActorContext): ThreadDenial | null {
	const view = canViewThread(ctx);
	if (view !== null) return view;
	if (ctx.thread.archived) return APIErrorCodes.THREAD_ARCHIVED;
	if (ctx.thread.locked && !isModerator(ctx)) return APIErrorCodes.THREAD_LOCKED;
	return null;
}

export function canAddThreadMember(
	ctx: ThreadActorContext,
	target: {canViewParent: boolean; isModerator: boolean},
): ThreadDenial | null {
	if (isCommunicationBlocked(ctx)) return APIErrorCodes.COMMUNICATION_DISABLED;
	if (ctx.thread.archived) return APIErrorCodes.THREAD_ARCHIVED;
	const moderator = isModerator(ctx);
	if (ctx.thread.locked && !moderator) return APIErrorCodes.THREAD_LOCKED;
	if (!has(threadViewPermissions(ctx.permissions), Permissions.SEND_MESSAGES)) return APIErrorCodes.MISSING_PERMISSIONS;
	if (ctx.thread.type === ChannelTypes.PRIVATE_THREAD && !ctx.thread.invitable && !moderator && !target.isModerator) {
		return APIErrorCodes.MISSING_PERMISSIONS;
	}
	if (!target.canViewParent) return APIErrorCodes.MISSING_ACCESS;
	return null;
}

export function canRemoveThreadMember(ctx: ThreadActorContext): ThreadDenial | null {
	if (isCommunicationBlocked(ctx)) return APIErrorCodes.COMMUNICATION_DISABLED;
	if (ctx.thread.archived) return APIErrorCodes.THREAD_ARCHIVED;
	if (isModerator(ctx)) return null;
	if (ctx.thread.type === ChannelTypes.PRIVATE_THREAD && ctx.isThreadOwner) return null;
	return APIErrorCodes.MISSING_PERMISSIONS;
}

export function canLeave(ctx: ThreadActorContext): ThreadDenial | null {
	if (!ctx.isMember) return APIErrorCodes.UNKNOWN_THREAD_MEMBER;
	if (ctx.thread.archived) return APIErrorCodes.THREAD_ARCHIVED;
	return null;
}

export function canDeleteThread(ctx: ThreadActor): ThreadDenial | null {
	if (isCommunicationBlocked(ctx)) return APIErrorCodes.COMMUNICATION_DISABLED;
	return isModerator(ctx) ? null : APIErrorCodes.MISSING_PERMISSIONS;
}

export function canCreateThread(actor: ThreadActor, kind: ThreadCreateKind): ThreadDenial | null {
	const perms = withImplicitThreadBits(actor.permissions);
	if (actor.isOwner) return null;
	if (!has(perms, Permissions.VIEW_CHANNEL)) return APIErrorCodes.MISSING_ACCESS;
	if (isCommunicationBlocked(actor)) return APIErrorCodes.COMMUNICATION_DISABLED;
	switch (kind) {
		case 'forum_post':
			return has(perms, Permissions.SEND_MESSAGES) ? null : APIErrorCodes.MISSING_PERMISSIONS;
		case 'from_message':
			return has(perms, ThreadPermissionFlags.CREATE_PUBLIC_THREADS) && has(perms, Permissions.READ_MESSAGE_HISTORY)
				? null
				: APIErrorCodes.MISSING_PERMISSIONS;
		case 'public':
			return has(perms, ThreadPermissionFlags.CREATE_PUBLIC_THREADS) ? null : APIErrorCodes.MISSING_PERMISSIONS;
		case 'private':
			return has(perms, ThreadPermissionFlags.CREATE_PRIVATE_THREADS) ? null : APIErrorCodes.MISSING_PERMISSIONS;
	}
}

export function canListArchivedThreads(
	actor: ThreadActor,
	{privateThreads}: {privateThreads: boolean},
): ThreadDenial | null {
	const perms = withImplicitThreadBits(actor.permissions);
	if (!actor.isOwner && !has(perms, Permissions.READ_MESSAGE_HISTORY)) return APIErrorCodes.MISSING_PERMISSIONS;
	if (privateThreads && !isModerator(actor)) return APIErrorCodes.MISSING_PERMISSIONS;
	return null;
}
