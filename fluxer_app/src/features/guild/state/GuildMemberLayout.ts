// SPDX-License-Identifier: AGPL-3.0-or-later

import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {
	GuildMemberLayoutSettingsSchema,
	GuildMemberViewMode as ProtoGuildMemberViewMode,
} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable} from 'mobx';

type GuildMemberViewMode = 'table' | 'grid';

const MODE_FROM_PROTO: Record<ProtoGuildMemberViewMode, GuildMemberViewMode | null> = {
	[ProtoGuildMemberViewMode.UNSPECIFIED]: null,
	[ProtoGuildMemberViewMode.TABLE]: 'table',
	[ProtoGuildMemberViewMode.GRID]: 'grid',
};
const MODE_TO_PROTO: Record<GuildMemberViewMode, ProtoGuildMemberViewMode> = {
	table: ProtoGuildMemberViewMode.TABLE,
	grid: ProtoGuildMemberViewMode.GRID,
};

class GuildMemberLayout {
	memberViewMode: GuildMemberViewMode = 'table';

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => this.initPersistence());
	}

	private async initPersistence(): Promise<void> {
		await makeSyncedField(this, {
			field: 'guildMemberLayout',
			schema: GuildMemberLayoutSettingsSchema,
			persist: ['memberViewMode'],
			toMessage: (s) => ({mode: MODE_TO_PROTO[s.memberViewMode]}),
			applyMessage: (s, m) => {
				const mode = MODE_FROM_PROTO[m.mode];
				if (mode !== null) s.memberViewMode = mode;
			},
		});
	}

	getViewMode(): GuildMemberViewMode {
		return this.memberViewMode;
	}
}

export default new GuildMemberLayout();
