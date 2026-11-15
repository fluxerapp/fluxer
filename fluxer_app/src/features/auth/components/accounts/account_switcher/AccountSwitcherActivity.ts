// SPDX-License-Identifier: AGPL-3.0-or-later

import AccountEngine from '@app/features/account_engine/state/AccountEngine';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import {AccountEngineConnectionState} from '@fluxer/desktop_ipc/src/AccountEngineContract';

export const AccountTileConnection = Object.freeze({
	LIVE: 'live',
	RECONNECTING: 'reconnecting',
	OFFLINE: 'offline',
} as const);

export type AccountTileConnection = (typeof AccountTileConnection)[keyof typeof AccountTileConnection];

export interface AccountTileActivity {
	readonly mentionCount: number;
	readonly hasUnread: boolean;
	readonly connection: AccountTileConnection;
}

function engineConnection(accountKey: string): AccountTileConnection {
	switch (AccountEngine.getSummary(accountKey)?.state) {
		case AccountEngineConnectionState.BACKOFF:
			return AccountTileConnection.RECONNECTING;
		case AccountEngineConnectionState.OFFLINE:
			return AccountTileConnection.OFFLINE;
		default:
			return AccountTileConnection.LIVE;
	}
}

export function getAccountTileActivity(accountKey: string, isCurrent: boolean): AccountTileActivity {
	if (isCurrent) {
		return {
			mentionCount: GuildReadState.mentionCountAcrossGuilds(),
			hasUnread: GuildReadState.anyGuildUnread,
			connection: AccountTileConnection.LIVE,
		};
	}
	return {
		mentionCount: AccountEngine.getMentionCount(accountKey),
		hasUnread: AccountEngine.hasUnread(accountKey),
		connection: engineConnection(accountKey),
	};
}
