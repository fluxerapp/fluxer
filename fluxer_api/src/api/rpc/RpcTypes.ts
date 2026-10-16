// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID} from '@app/api/BrandedTypes';
import type {Channel} from '@app/api/models/Channel';
import type {FavoriteMeme} from '@app/api/models/FavoriteMeme';
import type {ReadState} from '@app/api/models/ReadState';
import type {Relationship} from '@app/api/models/Relationship';
import type {User} from '@app/api/models/User';
import type {UserGuildSettings} from '@app/api/models/UserGuildSettings';
import type {UserSettings} from '@app/api/models/UserSettings';
import type {WebAuthnCredential} from '@app/api/models/WebAuthnCredential';

export interface UserData {
	user: User;
	settings: UserSettings | null;
	guildSettings: Array<UserGuildSettings>;
	notes: Map<UserID, string>;
	readStates: Array<ReadState>;
	guildIds: Array<GuildID>;
	privateChannels: Array<Channel>;
	relationships: Array<Relationship>;
	favoriteMemes: Array<FavoriteMeme>;
	pinnedDMs: Array<ChannelID>;
	webAuthnCredentials: Array<WebAuthnCredential>;
}
