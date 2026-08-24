// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export interface GuildEmojiShape {
	id: string;
	guildId: string;
	name: string;
	uniqueName: string;
	allNamesString: string;
	url: string;
	animated: boolean;
	nsfw?: boolean;
	user?: UserPartial;
}

export interface UnicodeEmoji {
	id?: string;
	uniqueName: string;
	name: string;
	names: ReadonlyArray<string>;
	keywords?: ReadonlyArray<string>;
	allNamesString: string;
	url?: string;
	surrogates: string;
	hasSkinTones: boolean;
	managed: boolean;
	useSpriteSheet: boolean;
	index?: number;
	skinToneIndex?: number;
	guildId?: string;
}

export type FlatEmoji = Readonly<
	Partial<GuildEmojiShape> &
		Partial<UnicodeEmoji> & {
			name: string;
			allNamesString: string;
			uniqueName: string;
			useSpriteSheet?: boolean;
			index?: number;
			skinToneIndex?: number;
			hasSkinTones?: boolean;
		}
>;
