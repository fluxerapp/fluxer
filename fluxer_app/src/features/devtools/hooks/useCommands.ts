// SPDX-License-Identifier: AGPL-3.0-or-later

import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useMemo} from 'react';

const APPENDS_TO_YOUR_MESSAGE_DESCRIPTOR = msg({
	message: 'Appends ¯\\_(ツ)_/¯ to your message.',
	comment: "Slash-command description for /shrug. Appends a kaomoji to the user's message.",
});
const APPENDS_TO_YOUR_MESSAGE_2_DESCRIPTOR = msg({
	message: 'Appends (╯°□°)╯︵ ┻━┻ to your message.',
	comment: "Slash-command description for /tableflip. Appends a kaomoji to the user's message.",
});
const APPENDS_TO_YOUR_MESSAGE_3_DESCRIPTOR = msg({
	message: 'Appends ┬─┬ ノ( ゜-゜ノ) to your message.',
	comment: "Slash-command description for /unflip. Appends a kaomoji to the user's message.",
});
const SEND_AN_ACTION_MESSAGE_WRAPS_IN_ITALICS_DESCRIPTOR = msg({
	message: 'Send an action message (wraps in italics).',
	comment: 'Slash-command description for /me. Sends the message wrapped in italics, IRC-style.',
});
const SEND_A_SPOILER_MESSAGE_WRAPS_IN_SPOILER_TAGS_DESCRIPTOR = msg({
	message: 'Send a spoiler message (wraps in spoiler tags).',
	comment:
		'Slash-command description for /spoiler. Wraps the message in spoiler tags so recipients must click to reveal.',
});
const SEND_A_TEXT_TO_SPEECH_MESSAGE_DESCRIPTOR = msg({
	message: 'Send a text-to-speech message.',
	comment:
		'Slash-command description for /tts. Sends a text-to-speech message played aloud for users with TTS enabled.',
});
const CHANGE_YOUR_NICKNAME_IN_THIS_COMMUNITY_DESCRIPTOR = msg({
	message: 'Change your nickname in this community.',
	comment: "Slash-command description for /nick. Changes the user's nickname in the current community.",
});
const KICK_A_MEMBER_FROM_THIS_COMMUNITY_DESCRIPTOR = msg({
	message: 'Kick a member from this community.',
	comment: 'Slash-command description for /kick. Removes a member from the current community (moderation action).',
});
const BAN_A_MEMBER_FROM_THIS_COMMUNITY_DESCRIPTOR = msg({
	message: 'Ban a member from this community.',
	comment: 'Slash-command description for /ban. Bans a member from the current community (moderation action).',
});
const SEND_A_DIRECT_MESSAGE_TO_A_USER_DESCRIPTOR = msg({
	message: 'Send a direct message to a user.',
	comment: 'Slash-command description for /dm. Sends a direct message to a user.',
});
const SEND_A_SAVED_MEDIA_ITEM_DESCRIPTOR = msg({
	message: 'Send a saved media item.',
	comment: 'Slash-command description for /favorites. Sends a saved / favorited media item.',
});
const SEND_A_STICKER_DESCRIPTOR = msg({
	message: 'Send a sticker.',
	comment: 'Slash-command description for /sticker. Sends a sticker.',
});
const SEARCH_FOR_AND_SEND_A_GIF_DESCRIPTOR = msg({
	message: 'Search for and send a GIF.',
	comment: 'Slash-command description for /gif. Searches for and sends a GIF.',
});
const COMMAND_MEMBER_OPTION_DESCRIPTOR = msg({
	message: 'The member to target.',
	comment: 'Description for the member option of a moderation slash command.',
});
const COMMAND_REASON_OPTION_DESCRIPTOR = msg({
	message: 'Reason (optional).',
	comment: 'Description for an optional moderation slash-command reason.',
});
const COMMAND_MESSAGE_OPTION_DESCRIPTOR = msg({
	message: 'The message to send.',
	comment: 'Description for a required message option in a slash command.',
});
const COMMAND_QUERY_OPTION_DESCRIPTOR = msg({
	message: 'What to search for.',
	comment: 'Description for a media search slash-command query.',
});
const COMMAND_NICKNAME_OPTION_DESCRIPTOR = msg({
	message: 'Your new nickname, or leave blank to reset it.',
	comment: 'Description for the /nick nickname option.',
});
const COMMAND_DELETE_MESSAGES_OPTION_DESCRIPTOR = msg({
	message: "How much of the member's recent message history to delete.",
	comment: 'Description for the /ban delete_messages option.',
});
const DELETE_MESSAGES_NONE_DESCRIPTOR = msg({
	message: "Don't delete any",
	comment: 'Choice label for retaining all messages when banning a member.',
});
const DELETE_MESSAGES_ONE_DAY_DESCRIPTOR = msg({
	message: 'Previous 24 hours',
	comment: 'Choice label for deleting one day of messages when banning a member.',
});
const DELETE_MESSAGES_TWO_DAYS_DESCRIPTOR = msg({
	message: 'Previous 2 days',
	comment: 'Choice label for deleting two days of messages when banning a member.',
});
const DELETE_MESSAGES_THREE_DAYS_DESCRIPTOR = msg({
	message: 'Previous 3 days',
	comment: 'Choice label for deleting three days of messages when banning a member.',
});
const DELETE_MESSAGES_FOUR_DAYS_DESCRIPTOR = msg({
	message: 'Previous 4 days',
	comment: 'Choice label for deleting four days of messages when banning a member.',
});
const DELETE_MESSAGES_FIVE_DAYS_DESCRIPTOR = msg({
	message: 'Previous 5 days',
	comment: 'Choice label for deleting five days of messages when banning a member.',
});
const DELETE_MESSAGES_SIX_DAYS_DESCRIPTOR = msg({
	message: 'Previous 6 days',
	comment: 'Choice label for deleting six days of messages when banning a member.',
});
const DELETE_MESSAGES_SEVEN_DAYS_DESCRIPTOR = msg({
	message: 'Previous 7 days',
	comment: 'Choice label for deleting seven days of messages when banning a member.',
});

interface SimpleCommand {
	type: 'simple';
	name: string;
	content: string;
	description: string;
}

interface ActionCommand {
	type: 'action';
	name: string;
	description: string;
	permission?: bigint;
	requiresGuild?: boolean;
	options?: Array<CommandOption>;
}

export interface CommandOption {
	name: string;
	description: string;
	type: 'string' | 'user' | 'channel' | 'role' | 'integer' | 'number' | 'boolean' | 'choice';
	required: boolean;
	allowEmpty?: boolean;
	choices?: Array<{name: string; value: string}>;
}

const EMPTY_COMMAND_CHOICES: Array<{name: string; value: string}> = [];

export type Command = SimpleCommand | ActionCommand;

export function useCommands(): Array<Command> {
	const {i18n} = useLingui();
	return useMemo(
		(): Array<Command> => [
			{type: 'simple', name: '/shrug', content: '¯\\_(ツ)_/¯', description: i18n._(APPENDS_TO_YOUR_MESSAGE_DESCRIPTOR)},
			{
				type: 'simple',
				name: '/tableflip',
				content: '(╯°□°)╯︵ ┻━┻',
				description: i18n._(APPENDS_TO_YOUR_MESSAGE_2_DESCRIPTOR),
			},
			{
				type: 'simple',
				name: '/unflip',
				content: '┬─┬ ノ( ゜-゜ノ)',
				description: i18n._(APPENDS_TO_YOUR_MESSAGE_3_DESCRIPTOR),
			},
			{
				type: 'action',
				name: '/me',
				description: i18n._(SEND_AN_ACTION_MESSAGE_WRAPS_IN_ITALICS_DESCRIPTOR),
				options: [
					{
						name: 'message',
						description: i18n._(COMMAND_MESSAGE_OPTION_DESCRIPTOR),
						type: 'string',
						required: true,
						allowEmpty: false,
						choices: EMPTY_COMMAND_CHOICES,
					},
				],
			},
			{
				type: 'action',
				name: '/spoiler',
				description: i18n._(SEND_A_SPOILER_MESSAGE_WRAPS_IN_SPOILER_TAGS_DESCRIPTOR),
				options: [
					{
						name: 'message',
						description: i18n._(COMMAND_MESSAGE_OPTION_DESCRIPTOR),
						type: 'string',
						required: true,
						allowEmpty: false,
						choices: EMPTY_COMMAND_CHOICES,
					},
				],
			},
			{
				type: 'action',
				name: '/tts',
				description: i18n._(SEND_A_TEXT_TO_SPEECH_MESSAGE_DESCRIPTOR),
				permission: Permissions.SEND_TTS_MESSAGES,
				options: [
					{
						name: 'message',
						description: i18n._(COMMAND_MESSAGE_OPTION_DESCRIPTOR),
						type: 'string',
						required: true,
						allowEmpty: false,
						choices: EMPTY_COMMAND_CHOICES,
					},
				],
			},
			{
				type: 'action',
				name: '/nick',
				description: i18n._(CHANGE_YOUR_NICKNAME_IN_THIS_COMMUNITY_DESCRIPTOR),
				permission: Permissions.CHANGE_NICKNAME,
				requiresGuild: true,
				options: [
					{
						name: 'nickname',
						description: i18n._(COMMAND_NICKNAME_OPTION_DESCRIPTOR),
						type: 'string',
						required: true,
						allowEmpty: true,
						choices: EMPTY_COMMAND_CHOICES,
					},
				],
			},
			{
				type: 'action',
				name: '/kick',
				description: i18n._(KICK_A_MEMBER_FROM_THIS_COMMUNITY_DESCRIPTOR),
				permission: Permissions.KICK_MEMBERS,
				requiresGuild: true,
				options: [
					{
						name: 'user',
						description: i18n._(COMMAND_MEMBER_OPTION_DESCRIPTOR),
						type: 'user',
						required: true,
						allowEmpty: false,
						choices: EMPTY_COMMAND_CHOICES,
					},
					{
						name: 'reason',
						description: i18n._(COMMAND_REASON_OPTION_DESCRIPTOR),
						type: 'string',
						required: false,
						allowEmpty: false,
						choices: EMPTY_COMMAND_CHOICES,
					},
				],
			},
			{
				type: 'action',
				name: '/ban',
				description: i18n._(BAN_A_MEMBER_FROM_THIS_COMMUNITY_DESCRIPTOR),
				permission: Permissions.BAN_MEMBERS,
				requiresGuild: true,
				options: [
					{
						name: 'user',
						description: i18n._(COMMAND_MEMBER_OPTION_DESCRIPTOR),
						type: 'user',
						required: true,
						allowEmpty: false,
						choices: EMPTY_COMMAND_CHOICES,
					},
					{
						name: 'delete_messages',
						description: i18n._(COMMAND_DELETE_MESSAGES_OPTION_DESCRIPTOR),
						type: 'choice',
						required: true,
						allowEmpty: false,
						choices: [
							{name: i18n._(DELETE_MESSAGES_NONE_DESCRIPTOR), value: '0'},
							{name: i18n._(DELETE_MESSAGES_ONE_DAY_DESCRIPTOR), value: '1'},
							{name: i18n._(DELETE_MESSAGES_TWO_DAYS_DESCRIPTOR), value: '2'},
							{name: i18n._(DELETE_MESSAGES_THREE_DAYS_DESCRIPTOR), value: '3'},
							{name: i18n._(DELETE_MESSAGES_FOUR_DAYS_DESCRIPTOR), value: '4'},
							{name: i18n._(DELETE_MESSAGES_FIVE_DAYS_DESCRIPTOR), value: '5'},
							{name: i18n._(DELETE_MESSAGES_SIX_DAYS_DESCRIPTOR), value: '6'},
							{name: i18n._(DELETE_MESSAGES_SEVEN_DAYS_DESCRIPTOR), value: '7'},
						],
					},
					{
						name: 'reason',
						description: i18n._(COMMAND_REASON_OPTION_DESCRIPTOR),
						type: 'string',
						required: false,
						allowEmpty: false,
						choices: EMPTY_COMMAND_CHOICES,
					},
				],
			},
			{
				type: 'action',
				name: '/msg',
				description: i18n._(SEND_A_DIRECT_MESSAGE_TO_A_USER_DESCRIPTOR),
				options: [
					{
						name: 'user',
						description: i18n._(COMMAND_MEMBER_OPTION_DESCRIPTOR),
						type: 'user',
						required: true,
						allowEmpty: false,
						choices: EMPTY_COMMAND_CHOICES,
					},
					{
						name: 'message',
						description: i18n._(COMMAND_MESSAGE_OPTION_DESCRIPTOR),
						type: 'string',
						required: true,
						allowEmpty: false,
						choices: EMPTY_COMMAND_CHOICES,
					},
				],
			},
			{
				type: 'action',
				name: '/saved',
				description: i18n._(SEND_A_SAVED_MEDIA_ITEM_DESCRIPTOR),
				options: [
					{
						name: 'query',
						description: i18n._(COMMAND_QUERY_OPTION_DESCRIPTOR),
						type: 'string',
						required: true,
						allowEmpty: false,
						choices: EMPTY_COMMAND_CHOICES,
					},
				],
			},
			{
				type: 'action',
				name: '/sticker',
				description: i18n._(SEND_A_STICKER_DESCRIPTOR),
				options: [
					{
						name: 'query',
						description: i18n._(COMMAND_QUERY_OPTION_DESCRIPTOR),
						type: 'string',
						required: true,
						allowEmpty: false,
						choices: EMPTY_COMMAND_CHOICES,
					},
				],
			},
			{
				type: 'action',
				name: '/gif',
				description: i18n._(SEARCH_FOR_AND_SEND_A_GIF_DESCRIPTOR),
				options: [
					{
						name: 'query',
						description: i18n._(COMMAND_QUERY_OPTION_DESCRIPTOR),
						type: 'string',
						required: true,
						allowEmpty: false,
						choices: EMPTY_COMMAND_CHOICES,
					},
				],
			},
		],
		[i18n.locale],
	);
}
