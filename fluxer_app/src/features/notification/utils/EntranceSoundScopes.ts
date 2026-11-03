// SPDX-License-Identifier: AGPL-3.0-or-later

import {ME} from '@fluxer/constants/src/AppConstants';

export type EntranceSoundScope =
	| {
			kind: 'global';
	  }
	| {
			kind: 'guilds';
	  }
	| {
			kind: 'dms';
	  }
	| {
			kind: 'guild';
			guildId: string;
	  };

export const GLOBAL_ENTRANCE_SOUND_SCOPE: EntranceSoundScope = {kind: 'global'};
export const GUILDS_ENTRANCE_SOUND_SCOPE: EntranceSoundScope = {kind: 'guilds'};
export const DMS_ENTRANCE_SOUND_SCOPE: EntranceSoundScope = {kind: 'dms'};
export function getEntranceSoundScopeId(scope: EntranceSoundScope): string {
	switch (scope.kind) {
		case 'global':
			return 'global';
		case 'guilds':
			return 'guilds';
		case 'dms':
			return 'dms';
		case 'guild':
			return `guild:${scope.guildId}`;
	}
}

export function parseEntranceSoundScopeId(scopeId: string): EntranceSoundScope | null {
	if (scopeId === 'global') {
		return GLOBAL_ENTRANCE_SOUND_SCOPE;
	}
	if (scopeId === 'guilds') {
		return GUILDS_ENTRANCE_SOUND_SCOPE;
	}
	if (scopeId === 'dms') {
		return DMS_ENTRANCE_SOUND_SCOPE;
	}
	if (scopeId.startsWith('guild:')) {
		const guildId = scopeId.slice('guild:'.length);
		return guildId ? {kind: 'guild', guildId} : null;
	}
	return null;
}

export function getVoiceContextEntranceSoundScope(guildId: string | null): EntranceSoundScope {
	if (!guildId || guildId === ME) {
		return DMS_ENTRANCE_SOUND_SCOPE;
	}
	return {
		kind: 'guild',
		guildId,
	};
}

export function getEntranceSoundFallbackScopes(scope: EntranceSoundScope): Array<EntranceSoundScope> {
	switch (scope.kind) {
		case 'global':
			return [GLOBAL_ENTRANCE_SOUND_SCOPE];
		case 'guilds':
			return [GUILDS_ENTRANCE_SOUND_SCOPE, GLOBAL_ENTRANCE_SOUND_SCOPE];
		case 'dms':
			return [DMS_ENTRANCE_SOUND_SCOPE, GLOBAL_ENTRANCE_SOUND_SCOPE];
		case 'guild':
			return [scope, GUILDS_ENTRANCE_SOUND_SCOPE, GLOBAL_ENTRANCE_SOUND_SCOPE];
	}
}
