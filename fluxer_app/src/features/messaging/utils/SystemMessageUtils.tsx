// SPDX-License-Identifier: AGPL-3.0-or-later

import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import React from 'react';

const GLAD_YOU_RE_HERE_DESCRIPTOR = msg({
	message: "Glad you're here, {username}!",
	comment:
		'Randomly selected welcome message that appears as a system message when a user joins a community. Plural placeholder is the new member username. Tone is friendly and warm; keep variety across these strings.',
});
const WELCOME_MAKE_YOURSELF_AT_HOME_DESCRIPTOR = msg({
	message: 'Welcome, {username}! Make yourself at home.',
	comment:
		'Randomly selected welcome message that appears as a system message when a user joins a community. Plural placeholder is the new member username. Tone is friendly and warm; keep variety across these strings.',
});
const HELLO_NICE_TO_HAVE_YOU_HERE_DESCRIPTOR = msg({
	message: 'Hello, {username}! Nice to have you here.',
	comment:
		'Randomly selected welcome message that appears as a system message when a user joins a community. Plural placeholder is the new member username. Tone is friendly and warm; keep variety across these strings.',
});
const HELLO_JUMP_IN_WHENEVER_YOU_RE_READY_DESCRIPTOR = msg({
	message: "Hello, {username}! Jump in whenever you're ready.",
	comment:
		'Randomly selected welcome message that appears as a system message when a user joins a community. Plural placeholder is the new member username. Tone is friendly and warm; keep variety across these strings.',
});
const HEY_GREAT_TO_SEE_YOU_HERE_DESCRIPTOR = msg({
	message: 'Hey {username}, great to see you here!',
	comment:
		'Randomly selected welcome message that appears as a system message when a user joins a community. Plural placeholder is the new member username. Tone is friendly and warm; keep variety across these strings.',
});
const HEY_THERE_HOPE_YOU_ENJOY_YOUR_STAY_DESCRIPTOR = msg({
	message: 'Hey there, {username}! Hope you enjoy your stay.',
	comment:
		'Randomly selected welcome message that appears as a system message when a user joins a community. Plural placeholder is the new member username. Tone is friendly and warm; keep variety across these strings.',
});
const HEY_WELCOME_ABOARD_DESCRIPTOR = msg({
	message: 'Hey, {username}, welcome aboard!',
	comment:
		'Randomly selected welcome message that appears as a system message when a user joins a community. Plural placeholder is the new member username. Tone is friendly and warm; keep variety across these strings.',
});
const GLAD_YOU_MADE_IT_DESCRIPTOR = msg({
	message: 'Glad you made it, {username}!',
	comment:
		'Randomly selected welcome message that appears as a system message when a user joins a community. Plural placeholder is the new member username. Tone is friendly and warm; keep variety across these strings.',
});
const WELCOME_IN_DESCRIPTOR = msg({
	message: 'Welcome in, {username}!',
	comment:
		'Randomly selected welcome message that appears as a system message when a user joins a community. Plural placeholder is the new member username. Tone is friendly and warm; keep variety across these strings.',
});
const WELCOME_DESCRIPTOR = msg({
	message: 'Welcome, {username}!',
	comment:
		'Randomly selected welcome message that appears as a system message when a user joins a community. Plural placeholder is the new member username. Tone is friendly and warm; keep variety across these strings.',
});
const WELCOME_WE_RE_GLAD_YOU_RE_HERE_DESCRIPTOR = msg({
	message: "Welcome, {username}! We're glad you're here.",
	comment:
		'Randomly selected welcome message that appears as a system message when a user joins a community. Plural placeholder is the new member username. Tone is friendly and warm; keep variety across these strings.',
});
const WELCOME_HOPE_YOU_ENJOY_YOUR_TIME_HERE_DESCRIPTOR = msg({
	message: 'Welcome, {username}! Hope you enjoy your time here.',
	comment:
		'Randomly selected welcome message that appears as a system message when a user joins a community. Plural placeholder is the new member username. Tone is friendly and warm; keep variety across these strings.',
});
const WELCOME_YOUR_NEXT_CONVERSATION_STARTS_HERE_DESCRIPTOR = msg({
	message: 'Welcome, {username}! Your next conversation starts here.',
	comment:
		'Randomly selected welcome message that appears as a system message when a user joins a community. Plural placeholder is the new member username. Tone is friendly and warm; keep variety across these strings.',
});
const WELCOME_WE_RE_HAPPY_TO_HAVE_YOU_HERE_DESCRIPTOR = msg({
	message: "Welcome, {username}. We're happy to have you here.",
	comment:
		'Randomly selected welcome message that appears as a system message when a user joins a community. Plural placeholder is the new member username. Tone is friendly and warm; keep variety across these strings.',
});
const GREAT_TO_SEE_YOU_WELCOME_IN_DESCRIPTOR = msg({
	message: 'Great to see you, {username}! Welcome in.',
	comment:
		'Randomly selected welcome message that appears as a system message when a user joins a community. Plural placeholder is the new member username. Tone is friendly and warm; keep variety across these strings.',
});
const YOU_RE_HERE_GOOD_TO_HAVE_YOU_WITH_DESCRIPTOR = msg({
	message: "You're here, {username}! Good to have you with us.",
	comment:
		'Randomly selected welcome message that appears as a system message when a user joins a community. Plural placeholder is the new member username. Tone is friendly and warm; keep variety across these strings.',
});
const YOU_VE_ARRIVED_LET_S_GET_STARTED_DESCRIPTOR = msg({
	message: "You've arrived, {username}! Let's get started.",
	comment:
		'Randomly selected welcome message that appears as a system message when a user joins a community. Plural placeholder is the new member username. Tone is friendly and warm; keep variety across these strings.',
});
const PINNED_A_MESSAGE_TO_THIS_CHANNEL_DESCRIPTOR = msg({
	message: '{username} pinned a message to this channel.',
	comment: 'System message shown inline in a channel when a user pins a message. username is the actor.',
});
const ADDED_TO_THE_GROUP_DESCRIPTOR = msg({
	message: '{username} added {userName} to the group.',
	comment:
		'System message in a group DM when a member adds another user. username is the actor, userName is the added user.',
});
const ADDED_SOMEONE_TO_THE_GROUP_DESCRIPTOR = msg({
	message: '{username} added someone to the group.',
	comment: 'System message in a group DM when a member adds someone whose user record is not resolvable locally.',
});
const HAS_LEFT_THE_GROUP_DESCRIPTOR = msg({
	message: '{username} has left the group.',
	comment: 'System message in a group DM when a member removes themself from the group.',
});
const REMOVED_FROM_THE_GROUP_DESCRIPTOR = msg({
	message: '{username} removed {userName} from the group.',
	comment:
		'System message in a group DM when one member removes another. username is the actor, userName is the removed user.',
});
const REMOVED_SOMEONE_FROM_THE_GROUP_DESCRIPTOR = msg({
	message: '{username} removed someone from the group.',
	comment: 'System message in a group DM when a member removes someone whose user record is not resolvable locally.',
});
const ADDED_FOLLOWED_CHANNEL_DESCRIPTOR = msg({
	message: '{username} followed {name} into this channel. Messages published there will appear here.',
	comment:
		'Plain-text system message used in previews and screen readers when someone makes this channel follow an announcement channel. name is the follower webhook name, usually the source community and channel.',
});
const CHANGED_THE_CHANNEL_NAME_TO_DESCRIPTOR = msg({
	message: '{username} changed the channel name to {newName}.',
	comment: 'System message in a group DM when a member renames the channel. newName is the new channel name.',
});
const CHANGED_THE_CHANNEL_NAME_DESCRIPTOR = msg({
	message: '{username} changed the channel name.',
	comment: 'System message in a group DM when a member renames the channel and the new name is not available locally.',
});
const CHANGED_THE_CHANNEL_ICON_DESCRIPTOR = msg({
	message: '{username} changed the channel icon.',
	comment: 'System message in a group DM when a member changes the group icon.',
});
const STARTED_A_CALL_DESCRIPTOR = msg({
	message: '{username} started a call.',
	comment: 'System message shown inline when a user starts a voice or video call in a DM or group DM.',
});
const STARTED_A_THREAD_NAMED_DESCRIPTOR = msg({
	message: '{username} started a thread: {threadName}',
	comment:
		'Plain text summary of the thread created system message. username is the author, threadName the thread name.',
});

interface StringifyableMessage {
	id: string;
	type: number;
	content: string;
	author: {id: string};
	mentions?: ReadonlyArray<{id: string}>;
}

const getGuildJoinMessagesPlaintext = (i18n: I18n): Array<(username: string) => string> => [
	(username) => i18n._(GLAD_YOU_RE_HERE_DESCRIPTOR, {username}),
	(username) => i18n._(WELCOME_MAKE_YOURSELF_AT_HOME_DESCRIPTOR, {username}),
	(username) => i18n._(HELLO_NICE_TO_HAVE_YOU_HERE_DESCRIPTOR, {username}),
	(username) => i18n._(HELLO_JUMP_IN_WHENEVER_YOU_RE_READY_DESCRIPTOR, {username}),
	(username) => i18n._(HEY_GREAT_TO_SEE_YOU_HERE_DESCRIPTOR, {username}),
	(username) => i18n._(HEY_THERE_HOPE_YOU_ENJOY_YOUR_STAY_DESCRIPTOR, {username}),
	(username) => i18n._(HEY_WELCOME_ABOARD_DESCRIPTOR, {username}),
	(username) => i18n._(GLAD_YOU_MADE_IT_DESCRIPTOR, {username}),
	(username) => i18n._(WELCOME_IN_DESCRIPTOR, {username}),
	(username) => i18n._(WELCOME_DESCRIPTOR, {username}),
	(username) => i18n._(WELCOME_WE_RE_GLAD_YOU_RE_HERE_DESCRIPTOR, {username}),
	(username) => i18n._(WELCOME_HOPE_YOU_ENJOY_YOUR_TIME_HERE_DESCRIPTOR, {username}),
	(username) => i18n._(WELCOME_YOUR_NEXT_CONVERSATION_STARTS_HERE_DESCRIPTOR, {username}),
	(username) => i18n._(WELCOME_WE_RE_HAPPY_TO_HAVE_YOU_HERE_DESCRIPTOR, {username}),
	(username) => i18n._(GREAT_TO_SEE_YOU_WELCOME_IN_DESCRIPTOR, {username}),
	(username) => i18n._(YOU_RE_HERE_GOOD_TO_HAVE_YOU_WITH_DESCRIPTOR, {username}),
	(username) => i18n._(YOU_VE_ARRIVED_LET_S_GET_STARTED_DESCRIPTOR, {username}),
];
export const SystemMessageUtils = {
	getGuildJoinMessage(messageId: string, username: React.ReactNode, i18n: I18n): React.ReactElement {
		const messageList = getGuildJoinMessagesPlaintext(i18n);
		const messageIndex = SnowflakeUtils.extractTimestamp(messageId) % messageList.length;
		const messageGenerator = messageList[messageIndex];
		return (
			<>
				{messageGenerator('__USERNAME__')
					.split('__USERNAME__')
					.map((part, i, arr) => (
						<React.Fragment key={i}>
							{part}
							{i < arr.length - 1 && username}
						</React.Fragment>
					))}
			</>
		);
	},
	stringify(message: StringifyableMessage, i18n: I18n): string | null {
		const author = Users.getUser(message.author.id);
		if (!author) return null;
		const username = NicknameUtils.getDisplayName(author);
		switch (message.type) {
			case MessageTypes.USER_JOIN: {
				const messageList = getGuildJoinMessagesPlaintext(i18n);
				const messageIndex = SnowflakeUtils.extractTimestamp(message.id) % messageList.length;
				const messageGenerator = messageList[messageIndex];
				return messageGenerator(username);
			}
			case MessageTypes.CHANNEL_PINNED_MESSAGE:
				return i18n._(PINNED_A_MESSAGE_TO_THIS_CHANNEL_DESCRIPTOR, {username});
			case MessageTypes.RECIPIENT_ADD: {
				const mentionedUser =
					message.mentions && message.mentions.length > 0 ? Users.getUser(message.mentions[0].id) : null;
				if (mentionedUser) {
					return i18n._(ADDED_TO_THE_GROUP_DESCRIPTOR, {
						username,
						userName: NicknameUtils.getDisplayName(mentionedUser),
					});
				}
				return i18n._(ADDED_SOMEONE_TO_THE_GROUP_DESCRIPTOR, {username});
			}
			case MessageTypes.RECIPIENT_REMOVE: {
				const mentionedUserId = message.mentions && message.mentions.length > 0 ? message.mentions[0].id : null;
				const isSelfRemove = mentionedUserId === message.author.id;
				if (isSelfRemove) {
					return i18n._(HAS_LEFT_THE_GROUP_DESCRIPTOR, {username});
				}
				const mentionedUser = mentionedUserId ? Users.getUser(mentionedUserId) : null;
				if (mentionedUser) {
					return i18n._(REMOVED_FROM_THE_GROUP_DESCRIPTOR, {
						username,
						userName: NicknameUtils.getDisplayName(mentionedUser),
					});
				}
				return i18n._(REMOVED_SOMEONE_FROM_THE_GROUP_DESCRIPTOR, {username});
			}
			case MessageTypes.CHANNEL_NAME_CHANGE: {
				const newName = message.content;
				if (newName) {
					return i18n._(CHANGED_THE_CHANNEL_NAME_TO_DESCRIPTOR, {username, newName});
				}
				return i18n._(CHANGED_THE_CHANNEL_NAME_DESCRIPTOR, {username});
			}
			case MessageTypes.CHANNEL_ICON_CHANGE:
				return i18n._(CHANGED_THE_CHANNEL_ICON_DESCRIPTOR, {username});
			case MessageTypes.CALL:
				return i18n._(STARTED_A_CALL_DESCRIPTOR, {username});
			case MessageTypes.CHANNEL_FOLLOW_ADD:
				return i18n._(ADDED_FOLLOWED_CHANNEL_DESCRIPTOR, {username, name: message.content});
			case MessageTypes.THREAD_CREATED:
				return i18n._(STARTED_A_THREAD_NAMED_DESCRIPTOR, {username, threadName: message.content});
			default:
				return null;
		}
	},
};
