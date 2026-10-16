// SPDX-License-Identifier: AGPL-3.0-or-later

import Emoji from '@app/features/emoji/state/Emoji';
import EmojiPicker, {getEmojiUsageKey} from '@app/features/emoji/state/EmojiPicker';
import Sticker from '@app/features/emoji/state/EmojiSticker';
import StickerPicker from '@app/features/emoji/state/StickerPicker';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import * as GuildEmojiCommands from '@app/features/expressions/commands/GuildEmojiCommands';
import * as GuildStickerCommands from '@app/features/expressions/commands/GuildStickerCommands';
import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import ExpressionMetadata from '@app/features/expressions/state/ExpressionMetadata';
import type {Guild} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const CLONED_TO_DESCRIPTOR = msg({
	message: 'Cloned :{sourceName}: to {targetName}',
	comment: 'Toast confirming an emoji was cloned to a target community.',
});
const CHECKING_ACCESS_DESCRIPTOR = msg({
	message: 'Checking access...',
	comment: 'Loading label shown while permissions are being verified.',
});
const CLONE_EMOJI_TO_DESCRIPTOR = msg({
	message: 'Clone emoji to...',
	comment: 'Submenu label listing communities the emoji can be cloned to.',
});
const EMOJI_SLOTS_FULL_DESCRIPTOR = msg({
	message: 'Emoji slots full',
	comment: 'Status label shown when the target community has no available emoji slots.',
});
const CLONED_STICKER_TO_DESCRIPTOR = msg({
	message: 'Cloned sticker {stickerName} to {targetName}',
	comment: 'Toast confirming a sticker was cloned to a target community.',
});
const CLONE_STICKER_TO_DESCRIPTOR = msg({
	message: 'Clone sticker to…',
	comment: 'Submenu label listing communities the sticker can be cloned to.',
});
const STICKER_SLOTS_FULL_DESCRIPTOR = msg({
	message: 'Sticker slots full',
	comment: 'Status label shown when the target community has no available sticker slots.',
});
const UNFAVORITE_EMOJI_DESCRIPTOR = msg({
	message: 'Unfavorite emoji',
	comment: 'Emoji context menu action that removes the emoji from favorites.',
});
const FAVORITE_EMOJI_DESCRIPTOR = msg({
	message: 'Favorite emoji',
	comment: 'Emoji context menu action that adds the emoji to favorites.',
});
const COPY_EMOJI_ID_DESCRIPTOR = msg({
	message: 'Copy emoji ID',
	comment: 'Developer-mode action that copies the emoji ID to the clipboard.',
});
const COPY_EMOJI_URL_DESCRIPTOR = msg({
	message: 'Copy emoji URL',
	comment: 'Action that copies the emoji URL to the clipboard.',
});
const OPEN_EMOJI_IN_BROWSER_DESCRIPTOR = msg({
	message: 'Open emoji in browser',
	comment: 'Action that opens the emoji URL in an external browser.',
});
const COPY_IMAGE_LINK_DESCRIPTOR = msg({
	message: 'Copy image link',
	comment: 'Image context menu action that copies the image URL to the clipboard.',
});
const OPEN_IMAGE_LINK_DESCRIPTOR = msg({
	message: 'Open image link',
	comment: 'Image context menu action that opens the image URL in an external browser.',
});
const MORE_EMOJI_ACTIONS_DESCRIPTOR = msg({
	message: 'More emoji actions',
	comment: 'Submenu label that contains additional emoji context menu actions.',
});
const UNFAVORITE_STICKER_DESCRIPTOR = msg({
	message: 'Unfavorite sticker',
	comment: 'Sticker context menu action that removes the sticker from favorites.',
});
const FAVORITE_STICKER_DESCRIPTOR = msg({
	message: 'Favorite sticker',
	comment: 'Sticker context menu action that adds the sticker to favorites.',
});
const COPY_STICKER_ID_DESCRIPTOR = msg({
	message: 'Copy sticker ID',
	comment: 'Developer-mode action that copies the sticker ID to the clipboard.',
});
const COPY_STICKER_URL_DESCRIPTOR = msg({
	message: 'Copy sticker URL',
	comment: 'Action that copies the sticker URL to the clipboard.',
});
const OPEN_STICKER_IN_BROWSER_DESCRIPTOR = msg({
	message: 'Open sticker in browser',
	comment: 'Action that opens the sticker URL in an external browser.',
});
const MORE_STICKER_ACTIONS_DESCRIPTOR = msg({
	message: 'More sticker actions',
	comment: 'Submenu label that contains additional sticker context menu actions.',
});
export type ExpressionItem = Readonly<{id?: string; guildId?: string; name: string; uniqueName?: string}>;
export type StickerContextMenuSticker = Readonly<
	Pick<GuildSticker, 'id' | 'guildId' | 'name' | 'description' | 'tags' | 'animated' | 'url' | 'user'>
>;
export interface ExpressionCloneConfig {
	kind: 'emoji' | 'sticker';
	getMetadata: (id: string) => ReturnType<typeof ExpressionMetadata.getEmojiMetadata>;
	fetchMetadata: (id: string) => Promise<unknown>;
	cloningAllowed: (guild: Guild) => boolean;
	slotsFull: (guild: Guild) => boolean;
	clone: (guildId: string, id: string) => Promise<unknown>;
	toast: (i18n: I18n, item: ExpressionItem, targetName: string) => string;
	label: MessageDescriptor;
	slotsFullLabel: MessageDescriptor;
}
export interface ExpressionMenuConfig<T extends ExpressionItem> {
	clone: ExpressionCloneConfig;
	canFavorite: (item: T) => boolean;
	isFavorite: (item: T) => boolean;
	toggleFavorite: (item: T) => void;
	getURL: (item: T) => string | null;
	favoriteLabel: MessageDescriptor;
	unfavoriteLabel: MessageDescriptor;
	copyIdLabel: MessageDescriptor;
	copyUrlLabel: MessageDescriptor;
	openLabel: MessageDescriptor;
	moreLabel: MessageDescriptor;
}
export const EMOJI_MENU_CONFIG: ExpressionMenuConfig<FlatEmoji> = {
	clone: {
		kind: 'emoji',
		getMetadata: (id) => ExpressionMetadata.getEmojiMetadata(id),
		fetchMetadata: (id) => ExpressionMetadata.fetchEmojiMetadata(id),
		cloningAllowed: (guild) => guild.cloneEmojiAllowed,
		slotsFull: (guild) => Emoji.getGuildEmoji(guild.id).length >= guild.maxEmojis,
		clone: (guildId, id) => GuildEmojiCommands.clone(guildId, {sourceEmojiId: id}),
		toast: (i18n, item, targetName) =>
			i18n._(CLONED_TO_DESCRIPTOR, {sourceName: item.uniqueName || item.name, targetName}),
		label: CLONE_EMOJI_TO_DESCRIPTOR,
		slotsFullLabel: EMOJI_SLOTS_FULL_DESCRIPTOR,
	},
	canFavorite: (emoji) => !emoji.id || Boolean(emoji.guildId && Guilds.getGuild(emoji.guildId)),
	isFavorite: (emoji) => EmojiPicker.isFavorite(emoji),
	toggleFavorite: (emoji) => EmojiPicker.toggleFavorite(getEmojiUsageKey(emoji)),
	getURL: (emoji) =>
		emoji.id ? AvatarUtils.getEmojiURL({id: emoji.id, animated: emoji.animated}) : (emoji.url ?? null),
	favoriteLabel: FAVORITE_EMOJI_DESCRIPTOR,
	unfavoriteLabel: UNFAVORITE_EMOJI_DESCRIPTOR,
	copyIdLabel: COPY_EMOJI_ID_DESCRIPTOR,
	copyUrlLabel: COPY_EMOJI_URL_DESCRIPTOR,
	openLabel: OPEN_EMOJI_IN_BROWSER_DESCRIPTOR,
	moreLabel: MORE_EMOJI_ACTIONS_DESCRIPTOR,
};
export const STICKER_MENU_CONFIG: ExpressionMenuConfig<StickerContextMenuSticker> = {
	clone: {
		kind: 'sticker',
		getMetadata: (id) => ExpressionMetadata.getStickerMetadata(id),
		fetchMetadata: (id) => ExpressionMetadata.fetchStickerMetadata(id),
		cloningAllowed: (guild) => guild.cloneStickerAllowed,
		slotsFull: (guild) => Sticker.getGuildStickers(guild.id).length >= guild.maxStickers,
		clone: (guildId, id) => GuildStickerCommands.clone(guildId, {sourceStickerId: id}),
		toast: (i18n, item, targetName) => i18n._(CLONED_STICKER_TO_DESCRIPTOR, {stickerName: item.name, targetName}),
		label: CLONE_STICKER_TO_DESCRIPTOR,
		slotsFullLabel: STICKER_SLOTS_FULL_DESCRIPTOR,
	},
	canFavorite: (sticker) => Boolean(sticker.guildId && Guilds.getGuild(sticker.guildId)),
	isFavorite: (sticker) => StickerPicker.isFavorite(sticker),
	toggleFavorite: (sticker) => StickerPicker.toggleFavorite(sticker),
	getURL: (sticker) => AvatarUtils.getStickerURL({id: sticker.id, animated: sticker.animated, size: 320}),
	favoriteLabel: FAVORITE_STICKER_DESCRIPTOR,
	unfavoriteLabel: UNFAVORITE_STICKER_DESCRIPTOR,
	copyIdLabel: COPY_STICKER_ID_DESCRIPTOR,
	copyUrlLabel: COPY_STICKER_URL_DESCRIPTOR,
	openLabel: OPEN_STICKER_IN_BROWSER_DESCRIPTOR,
	moreLabel: MORE_STICKER_ACTIONS_DESCRIPTOR,
};
export {CHECKING_ACCESS_DESCRIPTOR, COPY_IMAGE_LINK_DESCRIPTOR, OPEN_IMAGE_LINK_DESCRIPTOR};
