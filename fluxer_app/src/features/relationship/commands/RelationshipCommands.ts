// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';

const logger = new Logger('RelationshipCommands');

export interface SendFriendRequestOptions {
	staffForceAccept?: boolean;
}

type RelationshipCommand =
	| {kind: 'send'; userId: string; options: SendFriendRequestOptions}
	| {kind: 'send-by-tag'; username: string; discriminator: string}
	| {kind: 'accept'; userId: string}
	| {kind: 'remove'; userId: string}
	| {kind: 'block'; userId: string}
	| {kind: 'nickname'; userId: string; nickname: string | null};

function friendRequestBody(options: SendFriendRequestOptions): Record<string, boolean> {
	return options.staffForceAccept ? {staff_force_accept: true} : {};
}

async function dispatchRelationshipCommand(command: RelationshipCommand): Promise<void> {
	switch (command.kind) {
		case 'send':
			await http.post(Endpoints.USER_RELATIONSHIP(command.userId), {body: friendRequestBody(command.options)});
			return;
		case 'send-by-tag':
			await http.post(Endpoints.USER_RELATIONSHIPS, {
				body: {username: command.username, discriminator: command.discriminator},
			});
			return;
		case 'accept':
			await http.put(Endpoints.USER_RELATIONSHIP(command.userId));
			return;
		case 'remove':
			await http.delete(Endpoints.USER_RELATIONSHIP(command.userId));
			return;
		case 'block':
			await http.put(Endpoints.USER_RELATIONSHIP(command.userId), {body: {type: RelationshipTypes.BLOCKED}});
			return;
		case 'nickname':
			await http.patch(Endpoints.USER_RELATIONSHIP(command.userId), {body: {nickname: command.nickname}});
			return;
	}
}

function rethrowRelationshipFailure(message: string, error: unknown): never {
	logger.error(message, error);
	throw error;
}

export async function sendFriendRequest(userId: string, options: SendFriendRequestOptions = {}) {
	try {
		await dispatchRelationshipCommand({kind: 'send', userId, options});
	} catch (error) {
		rethrowRelationshipFailure('Failed to send friend request:', error);
	}
}

export async function sendFriendRequestByTag(username: string, discriminator: string) {
	try {
		await dispatchRelationshipCommand({kind: 'send-by-tag', username, discriminator});
	} catch (error) {
		rethrowRelationshipFailure('Failed to send friend request by tag:', error);
	}
}

export async function acceptFriendRequest(userId: string) {
	try {
		await dispatchRelationshipCommand({kind: 'accept', userId});
	} catch (error) {
		rethrowRelationshipFailure('Failed to accept friend request:', error);
	}
}

export async function removeRelationship(userId: string) {
	try {
		await dispatchRelationshipCommand({kind: 'remove', userId});
	} catch (error) {
		rethrowRelationshipFailure('Failed to remove relationship:', error);
	}
}

export async function blockUser(userId: string) {
	try {
		await dispatchRelationshipCommand({kind: 'block', userId});
	} catch (error) {
		rethrowRelationshipFailure('Failed to block user:', error);
	}
}

export async function updateFriendNickname(userId: string, nickname: string | null) {
	try {
		await dispatchRelationshipCommand({kind: 'nickname', userId, nickname});
	} catch (error) {
		rethrowRelationshipFailure('Failed to update friend nickname:', error);
	}
}
