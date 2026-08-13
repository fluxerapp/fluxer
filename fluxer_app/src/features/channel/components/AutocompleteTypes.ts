// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import type {Command} from '@app/features/devtools/hooks/useCommands';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import type {Gif} from '@app/features/expressions/commands/GifCommands';
import type {FavoriteMeme} from '@app/features/expressions/models/FavoriteMeme';
import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import type {User} from '@app/features/user/models/User';

export interface AutocompleteMentionMemberOption {
	type: 'mention';
	kind: 'member';
	member: GuildMember;
}

export interface AutocompleteMentionUserOption {
	type: 'mention';
	kind: 'user';
	user: User;
}

export interface AutocompleteMentionRoleOption {
	type: 'mention';
	kind: 'role';
	role: GuildRole;
}

export interface AutocompleteSpecialMentionOption {
	type: 'mention';
	kind: '@everyone' | '@here';
}

export type AutocompleteOption =
	| AutocompleteMentionMemberOption
	| AutocompleteMentionUserOption
	| AutocompleteMentionRoleOption
	| AutocompleteSpecialMentionOption
	| {type: 'channel'; channel: Channel}
	| {type: 'emoji'; emoji: FlatEmoji}
	| {type: 'command'; command: Command}
	| {type: 'meme'; meme: FavoriteMeme}
	| {type: 'gif'; gif: Gif}
	| {type: 'sticker'; sticker: GuildSticker}
	| {type: 'commandChoice'; choice: {name: string; value: string}; description: string}
	| {type: 'commandOptionalAdd'; name: string; description: string};

export type AutocompleteType =
	| 'mention'
	| 'channel'
	| 'emoji'
	| 'command'
	| 'meme'
	| 'gif'
	| 'sticker'
	| 'commandChoice'
	| 'commandOptionalAdd';

export function getAutocompleteOptionId(listboxId: string, index: number): string {
	return `${listboxId}-option-${index}`;
}

export const isMentionMember = (o: AutocompleteOption): o is AutocompleteMentionMemberOption =>
	o.type === 'mention' && o.kind === 'member';
export const isMentionUser = (o: AutocompleteOption): o is AutocompleteMentionUserOption =>
	o.type === 'mention' && o.kind === 'user';
export const isMentionRole = (o: AutocompleteOption): o is AutocompleteMentionRoleOption =>
	o.type === 'mention' && o.kind === 'role';
export const isSpecialMention = (o: AutocompleteOption): o is AutocompleteSpecialMentionOption =>
	o.type === 'mention' && (o.kind === '@everyone' || o.kind === '@here');
export const isChannel = (o: AutocompleteOption): o is {type: 'channel'; channel: Channel} => o.type === 'channel';
export const isEmoji = (o: AutocompleteOption): o is {type: 'emoji'; emoji: FlatEmoji} => o.type === 'emoji';
export const isCommand = (o: AutocompleteOption): o is {type: 'command'; command: Command} => o.type === 'command';
export const isMeme = (o: AutocompleteOption): o is {type: 'meme'; meme: FavoriteMeme} => o.type === 'meme';
export const isGif = (o: AutocompleteOption): o is {type: 'gif'; gif: Gif} => o.type === 'gif';
export const isSticker = (o: AutocompleteOption): o is {type: 'sticker'; sticker: GuildSticker} => o.type === 'sticker';
export const isCommandChoice = (
	o: AutocompleteOption,
): o is {type: 'commandChoice'; choice: {name: string; value: string}; description: string} =>
	o.type === 'commandChoice';
export const isCommandOptionalAdd = (
	o: AutocompleteOption,
): o is {type: 'commandOptionalAdd'; name: string; description: string} => o.type === 'commandOptionalAdd';
