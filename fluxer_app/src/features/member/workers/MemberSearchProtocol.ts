// SPDX-License-Identifier: AGPL-3.0-or-later

export enum MessageTypes {
	INGEST_DIRECTORY = 'INGEST_DIRECTORY',
	MATCHES_READY = 'MATCHES_READY',
	SEARCH_BEGIN = 'SEARCH_BEGIN',
	SEARCH_CANCEL = 'SEARCH_CANCEL',
}

export interface MemberSearchFilters {
	friends?: boolean;
	guild?: string;
}

export interface TransformedMember {
	id: string;
	username: string;
	globalName?: string | null;
	guildNicknames?: Record<string, string | null>;
	isBot?: boolean;
	isFriend?: boolean;
	guildIds?: Array<string>;
	_delete?: boolean;
	_removeGuild?: string;
	[key: string]: string | boolean | null | undefined | Array<string> | Record<string, string | null>;
}
