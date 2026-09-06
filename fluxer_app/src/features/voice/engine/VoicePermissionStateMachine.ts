// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceChannelPermissions} from '@app/features/voice/utils/VoicePermissionUtils';
import {assign, getInitialSnapshot, type SnapshotFrom, setup, transition} from 'xstate';

export type VoicePermissionChangeTarget = 'speak' | 'stream' | 'video';
export type VoicePermissionWatchStateValue = 'inactive' | 'watching';

export interface VoiceRemoteMicrophonePublicationInput {
	publicationId: string;
	isDesired: boolean;
}

export type VoicePermissionCommand =
	| {type: 'syncSpeakPermission'; canSpeak: boolean; selfMute: boolean}
	| {type: 'revokeLocalAudio'}
	| {type: 'stopScreenShare'}
	| {type: 'disableCamera'}
	| {type: 'setRemoteMicrophoneSubscribed'; publicationId: string; subscribed: boolean}
	| {type: 'setRemoteMicrophoneEnabled'; publicationId: string; enabled: boolean}
	| {type: 'setRemoteVideoSubscribed'; publicationId: string; subscribed: boolean}
	| {type: 'stopPermissionWatch'};

export interface VoicePermissionMachineContext {
	permissions: VoiceChannelPermissions;
	deafened: boolean;
	guildId: string | null;
	channelId: string | null;
	roomPresent: boolean;
	watcherActive: boolean;
	commands: ReadonlyArray<VoicePermissionCommand>;
}

export type VoicePermissionEvent =
	| {type: 'permission.watch.start'; guildId: string | null; channelId: string; roomPresent: boolean}
	| {type: 'permission.watch.stop'}
	| {type: 'permission.room.set'; roomPresent: boolean}
	| {type: 'permission.update'; permissions: VoiceChannelPermissions; roomPresent?: boolean}
	| {
			type: 'permission.change';
			permission: VoicePermissionChangeTarget;
			allowed: boolean;
			roomPresent?: boolean;
	  }
	| {type: 'permission.set'; permissions: Partial<VoiceChannelPermissions>}
	| {
			type: 'subscription.initialize';
			microphonePublications: ReadonlyArray<VoiceRemoteMicrophonePublicationInput>;
			videoPublicationIds: ReadonlyArray<string>;
	  }
	| {
			type: 'deafen.apply';
			deafened: boolean;
			microphonePublications: ReadonlyArray<VoiceRemoteMicrophonePublicationInput>;
	  }
	| {type: 'permission.reset'}
	| {type: 'permission.clearCommands'};

export const DEFAULT_VOICE_PERMISSION_STATE: VoiceChannelPermissions = {
	canSpeak: true,
	canStream: true,
	canUseVideo: true,
	canConnect: true,
	canPrioritySpeaker: false,
	canUseVad: true,
};

const EMPTY_COMMANDS: ReadonlyArray<VoicePermissionCommand> = [];

function initialContext(
	commands: ReadonlyArray<VoicePermissionCommand> = EMPTY_COMMANDS,
): VoicePermissionMachineContext {
	return {
		permissions: {...DEFAULT_VOICE_PERMISSION_STATE},
		deafened: false,
		guildId: null,
		channelId: null,
		roomPresent: false,
		watcherActive: false,
		commands,
	};
}

function permissionsEqual(left: VoiceChannelPermissions, right: VoiceChannelPermissions): boolean {
	return (
		left.canSpeak === right.canSpeak &&
		left.canStream === right.canStream &&
		left.canUseVideo === right.canUseVideo &&
		left.canConnect === right.canConnect &&
		left.canPrioritySpeaker === right.canPrioritySpeaker &&
		left.canUseVad === right.canUseVad
	);
}

function appendCommands(
	context: VoicePermissionMachineContext,
	commands: ReadonlyArray<VoicePermissionCommand>,
): VoicePermissionMachineContext {
	if (commands.length === 0) return context;
	return {
		...context,
		commands: [...context.commands, ...commands],
	};
}

function permissionTransitionCommands(
	previous: VoiceChannelPermissions,
	next: VoiceChannelPermissions,
	roomPresent: boolean,
): Array<VoicePermissionCommand> {
	const commands: Array<VoicePermissionCommand> = [];
	if (previous.canSpeak !== next.canSpeak) {
		commands.push({type: 'syncSpeakPermission', canSpeak: next.canSpeak, selfMute: !next.canSpeak});
	}
	if (!roomPresent) return commands;
	if (previous.canSpeak && !next.canSpeak) commands.push({type: 'revokeLocalAudio'});
	if (previous.canStream && !next.canStream) commands.push({type: 'stopScreenShare'});
	if (previous.canUseVideo && !next.canUseVideo) commands.push({type: 'disableCamera'});
	return commands;
}

function updatePermissions(
	context: VoicePermissionMachineContext,
	permissions: VoiceChannelPermissions,
	roomPresent = context.roomPresent,
): VoicePermissionMachineContext {
	if (permissionsEqual(context.permissions, permissions)) {
		return context.roomPresent === roomPresent ? context : {...context, roomPresent};
	}
	const commands = permissionTransitionCommands(context.permissions, permissions, roomPresent);
	return appendCommands(
		{
			...context,
			permissions: {...permissions},
			roomPresent,
		},
		commands,
	);
}

function changePermission(
	context: VoicePermissionMachineContext,
	permission: VoicePermissionChangeTarget,
	allowed: boolean,
	roomPresent = context.roomPresent,
): VoicePermissionMachineContext {
	const nextPermissions = {...context.permissions};
	switch (permission) {
		case 'speak':
			nextPermissions.canSpeak = allowed;
			break;
		case 'stream':
			nextPermissions.canStream = allowed;
			nextPermissions.canUseVideo = allowed;
			break;
		case 'video':
			nextPermissions.canUseVideo = allowed;
			break;
	}
	return updatePermissions(context, nextPermissions, roomPresent);
}

function setPermissions(
	context: VoicePermissionMachineContext,
	permissions: Partial<VoiceChannelPermissions>,
): VoicePermissionMachineContext {
	const nextPermissions = {...context.permissions, ...permissions};
	return permissionsEqual(context.permissions, nextPermissions) ? context : {...context, permissions: nextPermissions};
}

function initializeSubscriptions(
	context: VoicePermissionMachineContext,
	microphonePublications: ReadonlyArray<VoiceRemoteMicrophonePublicationInput>,
	videoPublicationIds: ReadonlyArray<string>,
): VoicePermissionMachineContext {
	const commands: Array<VoicePermissionCommand> = [];
	for (const publication of microphonePublications) {
		commands.push({
			type: 'setRemoteMicrophoneSubscribed',
			publicationId: publication.publicationId,
			subscribed: !context.deafened,
		});
	}
	for (const publicationId of videoPublicationIds) {
		commands.push({type: 'setRemoteVideoSubscribed', publicationId, subscribed: false});
	}
	return appendCommands(context, commands);
}

function applyDeafen(
	context: VoicePermissionMachineContext,
	deafened: boolean,
	microphonePublications: ReadonlyArray<VoiceRemoteMicrophonePublicationInput>,
): VoicePermissionMachineContext {
	const commands: Array<VoicePermissionCommand> = [];
	for (const publication of microphonePublications) {
		if (deafened) {
			if (publication.isDesired) {
				commands.push({type: 'setRemoteMicrophoneEnabled', publicationId: publication.publicationId, enabled: false});
			}
			commands.push({
				type: 'setRemoteMicrophoneSubscribed',
				publicationId: publication.publicationId,
				subscribed: false,
			});
		} else {
			commands.push({
				type: 'setRemoteMicrophoneSubscribed',
				publicationId: publication.publicationId,
				subscribed: true,
			});
			commands.push({type: 'setRemoteMicrophoneEnabled', publicationId: publication.publicationId, enabled: true});
		}
	}
	return appendCommands({...context, deafened}, commands);
}

function stopWatch(context: VoicePermissionMachineContext): VoicePermissionMachineContext {
	if (!context.watcherActive) return context;
	return appendCommands(
		{
			...context,
			guildId: null,
			channelId: null,
			roomPresent: false,
			watcherActive: false,
		},
		[{type: 'stopPermissionWatch'}],
	);
}

function reset(context: VoicePermissionMachineContext): VoicePermissionMachineContext {
	return initialContext(context.watcherActive ? [{type: 'stopPermissionWatch'}] : EMPTY_COMMANDS);
}

export const voicePermissionStateMachine = setup({
	types: {} as {
		context: VoicePermissionMachineContext;
		events: VoicePermissionEvent;
	},
	actions: {
		startWatch: assign(({context, event}) =>
			event.type === 'permission.watch.start'
				? {
						...context,
						guildId: event.guildId,
						channelId: event.channelId,
						roomPresent: event.roomPresent,
						watcherActive: true,
					}
				: context,
		),
		stopWatch: assign(({context}) => stopWatch(context)),
		setRoomPresence: assign(({context, event}) =>
			event.type === 'permission.room.set' ? {...context, roomPresent: event.roomPresent} : context,
		),
		updatePermissions: assign(({context, event}) =>
			event.type === 'permission.update' ? updatePermissions(context, event.permissions, event.roomPresent) : context,
		),
		changePermission: assign(({context, event}) =>
			event.type === 'permission.change'
				? changePermission(context, event.permission, event.allowed, event.roomPresent)
				: context,
		),
		setPermissions: assign(({context, event}) =>
			event.type === 'permission.set' ? setPermissions(context, event.permissions) : context,
		),
		initializeSubscriptions: assign(({context, event}) =>
			event.type === 'subscription.initialize'
				? initializeSubscriptions(context, event.microphonePublications, event.videoPublicationIds)
				: context,
		),
		applyDeafen: assign(({context, event}) =>
			event.type === 'deafen.apply' ? applyDeafen(context, event.deafened, event.microphonePublications) : context,
		),
		reset: assign(({context}) => reset(context)),
		clearCommands: assign(({context}) =>
			context.commands.length === 0 ? context : {...context, commands: EMPTY_COMMANDS},
		),
	},
	guards: {
		watcherActive: ({context}) => context.watcherActive,
	},
}).createMachine({
	id: 'voicePermission',
	context: () => initialContext(),
	initial: 'routing',
	states: {
		routing: {
			always: [{guard: 'watcherActive', target: 'watching'}, {target: 'inactive'}],
		},
		inactive: {
			on: {
				'permission.watch.start': {target: 'routing', actions: 'startWatch'},
				'permission.watch.stop': {target: 'routing', actions: 'stopWatch'},
				'permission.room.set': {target: 'routing', actions: 'setRoomPresence'},
				'permission.update': {target: 'routing', actions: 'updatePermissions'},
				'permission.change': {target: 'routing', actions: 'changePermission'},
				'permission.set': {target: 'routing', actions: 'setPermissions'},
				'subscription.initialize': {target: 'routing', actions: 'initializeSubscriptions'},
				'deafen.apply': {target: 'routing', actions: 'applyDeafen'},
				'permission.reset': {target: 'routing', actions: 'reset'},
				'permission.clearCommands': {actions: 'clearCommands'},
			},
		},
		watching: {
			on: {
				'permission.watch.start': {target: 'routing', actions: 'startWatch'},
				'permission.watch.stop': {target: 'routing', actions: 'stopWatch'},
				'permission.room.set': {target: 'routing', actions: 'setRoomPresence'},
				'permission.update': {target: 'routing', actions: 'updatePermissions'},
				'permission.change': {target: 'routing', actions: 'changePermission'},
				'permission.set': {target: 'routing', actions: 'setPermissions'},
				'subscription.initialize': {target: 'routing', actions: 'initializeSubscriptions'},
				'deafen.apply': {target: 'routing', actions: 'applyDeafen'},
				'permission.reset': {target: 'routing', actions: 'reset'},
				'permission.clearCommands': {actions: 'clearCommands'},
			},
		},
	},
});

export type VoicePermissionSnapshot = SnapshotFrom<typeof voicePermissionStateMachine>;

export function createVoicePermissionSnapshot(): VoicePermissionSnapshot {
	return getInitialSnapshot(voicePermissionStateMachine);
}

export function transitionVoicePermissionSnapshot(
	snapshot: VoicePermissionSnapshot,
	event: VoicePermissionEvent,
): VoicePermissionSnapshot {
	return transition(voicePermissionStateMachine, snapshot, event)[0] as VoicePermissionSnapshot;
}

export function getVoicePermissionWatchStateValue(snapshot: VoicePermissionSnapshot): VoicePermissionWatchStateValue {
	return snapshot.context.watcherActive ? 'watching' : 'inactive';
}
