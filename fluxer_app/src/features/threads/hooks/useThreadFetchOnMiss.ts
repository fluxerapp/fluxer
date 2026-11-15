// SPDX-License-Identifier: AGPL-3.0-or-later

import {ensureThreadLoaded} from '@app/features/threads/commands/ThreadCommands';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import {useEffect, useState} from 'react';

export function useThreadFetchOnMiss(guildId: string | undefined, channelId: string | undefined): boolean {
	const active = guildId != null && channelId != null && ThreadGuilds.isActive(guildId);
	const key = active ? `${guildId}:${channelId}` : null;
	const [settledKey, setSettledKey] = useState<string | null>(null);
	useEffect(() => {
		if (key == null || guildId == null || channelId == null) return;
		let cancelled = false;
		void ensureThreadLoaded(guildId, channelId).finally(() => {
			if (!cancelled) setSettledKey(key);
		});
		return () => {
			cancelled = true;
		};
	}, [key, guildId, channelId]);
	return key != null && settledKey !== key;
}
