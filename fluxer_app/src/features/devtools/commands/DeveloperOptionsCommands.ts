// SPDX-License-Identifier: AGPL-3.0-or-later

import {Channel} from '@app/features/channel/models/Channel';
import DeveloperOptions, {type DeveloperOptionsState} from '@app/features/devtools/state/DeveloperOptions';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import MockIncomingCall from '@app/features/voice/state/MockIncomingCall';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {Channel as WireChannel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {generateSnowflake} from '@fluxer/snowflake/src/Snowflake';

const logger = new Logger('DeveloperOptions');

type AttachmentMock = DeveloperOptionsState['mockAttachmentStates'][string];
export type MockIncomingCallVariant = 'dm' | 'group-dm';

function publishLayoutResize(): void {
	ComponentBus.dispatch('LAYOUT_RESIZED');
}

function nextAttachmentMocks(
	attachmentId: string,
	mock: AttachmentMock | null,
): DeveloperOptionsState['mockAttachmentStates'] {
	const next = {...DeveloperOptions.mockAttachmentStates};
	if (mock === null) {
		delete next[attachmentId];
		return next;
	}
	next[attachmentId] = mock;
	return next;
}

function userPartialFromCurrentUser(currentUser: User): UserPartial {
	return {
		id: currentUser.id,
		username: currentUser.username,
		discriminator: currentUser.discriminator,
		global_name: currentUser.globalName,
		avatar: currentUser.avatar ?? null,
		avatar_color: currentUser.avatarColor ?? null,
		flags: currentUser.flags ?? 0,
	};
}

function mockUserPartial(id: string, globalName: string): UserPartial {
	return {
		id,
		username: globalName.toLowerCase(),
		discriminator: '0',
		global_name: globalName,
		avatar: null,
		avatar_color: null,
		flags: 0,
	};
}

function directMessageChannelWire(channelId: string, recipient: UserPartial): WireChannel {
	return {
		id: channelId,
		type: ChannelTypes.DM,
		recipients: [recipient],
	};
}

function groupDMChannelWire(channelId: string, recipients: ReadonlyArray<UserPartial>): WireChannel {
	return {
		id: channelId,
		type: ChannelTypes.GROUP_DM,
		recipients: [...recipients],
	};
}

function buildMockDMIncomingCall(currentUser: User): {channel: Channel; initiator: User} {
	const initiator = userPartialFromCurrentUser(currentUser);
	const channel = directMessageChannelWire(generateSnowflake().toString(), initiator);
	return {channel: new Channel(channel), initiator: new User(initiator)};
}

function buildMockGroupDMIncomingCall(currentUser: User): {channel: Channel; initiator: User} {
	const currentUserPartial = userPartialFromCurrentUser(currentUser);
	const others = [
		mockUserPartial('1100000000000000001', 'Riley Park'),
		mockUserPartial('1100000000000000002', 'Jordan Lee'),
		mockUserPartial('1100000000000000003', 'Sasha Mori'),
	];
	const channel = groupDMChannelWire(generateSnowflake().toString(), [currentUserPartial, ...others]);
	const initiatorPartial = others[0];
	return {channel: new Channel(channel), initiator: new User(initiatorPartial)};
}

function triggerVariant(variant: MockIncomingCallVariant): void {
	const currentUser = Users.getCurrentUser();
	if (!currentUser) {
		logger.warn('Cannot trigger mock incoming call: No current user');
		return;
	}
	const built =
		variant === 'group-dm' ? buildMockGroupDMIncomingCall(currentUser) : buildMockDMIncomingCall(currentUser);
	MockIncomingCall.setMockCall(built);
	logger.info(`Triggered mock ${variant} incoming call from user ${currentUser.username}`);
}

export function updateOption<K extends keyof DeveloperOptionsState>(key: K, value: DeveloperOptionsState[K]): void {
	logger.debug(`Updating developer option: ${String(key)} = ${value}`);
	DeveloperOptions.updateOption(key, value);
}

export function setAttachmentMock(attachmentId: string, mock: AttachmentMock | null): void {
	updateOption('mockAttachmentStates', nextAttachmentMocks(attachmentId, mock));
	publishLayoutResize();
}

export function clearAllAttachmentMocks(): void {
	updateOption('mockAttachmentStates', {});
	publishLayoutResize();
}

export function triggerMockIncomingCall(): void {
	triggerVariant('dm');
}

export function triggerMockIncomingCallDM(): void {
	triggerVariant('dm');
}

export function triggerMockIncomingCallGroupDM(): void {
	triggerVariant('group-dm');
}
