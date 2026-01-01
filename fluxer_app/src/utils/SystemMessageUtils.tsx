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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import React from 'react';
import {MessageTypes} from '~/Constants';
import UserStore from '~/stores/UserStore';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';

interface StringifyableMessage {
	id: string;
	type: number;
	content: string;
	author: {id: string};
	mentions?: ReadonlyArray<{id: string}>;
}

const getGuildJoinMessagesPlaintext = (i18n: I18n): Array<(username: string) => string> => [
	(username) => i18n._(msg`Glad you're here, ${username}! Watch out for Biff!`),
	(username) => i18n._(msg`Greetings, ${username}! More fun than a hoverboard!`),
	(username) => i18n._(msg`Hello, ${username}! Flux capacitor... fluxing. Welcome!`),
	(username) => i18n._(msg`Hello, ${username}! Make your history right here.`),
	(username) => i18n._(msg`Hey ${username}, great to see you! The flux capacitor is ready!`),
	(username) => i18n._(msg`Hey there, ${username}! No hoverboards required.`),
	(username) => i18n._(msg`Hey, ${username}, welcome! You can accomplish anything.`),
	(username) => i18n._(msg`Hey, ${username}! When it hits 88mph, you'll see some serious stuff!`),
	(username) => i18n._(msg`Hop in, ${username}! Mr Fusion is ready!`),
	(username) => i18n._(msg`Step in, ${username}! No paradoxes here; we're all friends.`),
	(username) => i18n._(msg`Welcome, ${username}! Don't forget to park your DeLorean.`),
	(username) => i18n._(msg`Welcome, ${username}! No life preserver needed.`),
	(username) => i18n._(msg`Welcome, ${username}! No need to hit 88mph; you're right on time.`),
	(username) => i18n._(msg`Welcome, ${username}! We've been expecting you since last week!`),
	(username) => i18n._(msg`Welcome, ${username}! Your future is whatever you make it!`),
	(username) => i18n._(msg`Welcome, ${username}. Where we're going, we don't need roads.`),
	(username) => i18n._(msg`Look at the time, ${username}! You're on schedule!`),
	(username) => i18n._(msg`You made it, ${username}! We're about to hit 88mph!`),
	(username) => i18n._(msg`You're here, ${username}! Just in time to rock 'n' roll!`),
	(username) => i18n._(msg`You've arrived, ${username}! Enjoy the Jigowatt Joyride!`),
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
		const author = UserStore.getUser(message.author.id);
		if (!author) return null;

		const username = author.username;

		switch (message.type) {
			case MessageTypes.USER_JOIN: {
				const messageList = getGuildJoinMessagesPlaintext(i18n);
				const messageIndex = SnowflakeUtils.extractTimestamp(message.id) % messageList.length;
				const messageGenerator = messageList[messageIndex];
				return messageGenerator(username);
			}
			case MessageTypes.CHANNEL_PINNED_MESSAGE:
				return i18n._(msg`${username} pinned a message to this channel.`);
			case MessageTypes.RECIPIENT_ADD: {
				const mentionedUser =
					message.mentions && message.mentions.length > 0 ? UserStore.getUser(message.mentions[0].id) : null;
				if (mentionedUser) {
					return i18n._(msg`${username} added ${mentionedUser.username} to the group.`);
				}
				return i18n._(msg`${username} added someone to the group.`);
			}
			case MessageTypes.RECIPIENT_REMOVE: {
				const mentionedUserId = message.mentions && message.mentions.length > 0 ? message.mentions[0].id : null;
				const isSelfRemove = mentionedUserId === message.author.id;
				if (isSelfRemove) {
					return i18n._(msg`${username} has left the group.`);
				}
				const mentionedUser = mentionedUserId ? UserStore.getUser(mentionedUserId) : null;
				if (mentionedUser) {
					return i18n._(msg`${username} removed ${mentionedUser.username} from the group.`);
				}
				return i18n._(msg`${username} removed someone from the group.`);
			}
			case MessageTypes.CHANNEL_NAME_CHANGE: {
				const newName = message.content;
				if (newName) {
					return i18n._(msg`${username} changed the channel name to ${newName}.`);
				}
				return i18n._(msg`${username} changed the channel name.`);
			}
			case MessageTypes.CHANNEL_ICON_CHANGE:
				return i18n._(msg`${username} changed the channel icon.`);
			case MessageTypes.CALL:
				return i18n._(msg`${username} started a call.`);
			default:
				return null;
		}
	},
};
