// SPDX-License-Identifier: AGPL-3.0-or-later

import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {
	PermissionLayoutSettingsSchema,
	PermissionGridMode as ProtoPermissionGridMode,
	PermissionLayoutMode as ProtoPermissionLayoutMode,
} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable} from 'mobx';

enum PermissionLayoutMode {
	COMFY = 'comfy',
	DENSE = 'dense',
}

enum PermissionGridMode {
	SINGLE = 'single',
	GRID = 'grid',
}

const LAYOUT_FROM_PROTO: Record<ProtoPermissionLayoutMode, PermissionLayoutMode | null> = {
	[ProtoPermissionLayoutMode.UNSPECIFIED]: null,
	[ProtoPermissionLayoutMode.COMFY]: PermissionLayoutMode.COMFY,
	[ProtoPermissionLayoutMode.DENSE]: PermissionLayoutMode.DENSE,
};
const LAYOUT_TO_PROTO: Record<PermissionLayoutMode, ProtoPermissionLayoutMode> = {
	[PermissionLayoutMode.COMFY]: ProtoPermissionLayoutMode.COMFY,
	[PermissionLayoutMode.DENSE]: ProtoPermissionLayoutMode.DENSE,
};
const GRID_FROM_PROTO: Record<ProtoPermissionGridMode, PermissionGridMode | null> = {
	[ProtoPermissionGridMode.UNSPECIFIED]: null,
	[ProtoPermissionGridMode.SINGLE]: PermissionGridMode.SINGLE,
	[ProtoPermissionGridMode.GRID]: PermissionGridMode.GRID,
};
const GRID_TO_PROTO: Record<PermissionGridMode, ProtoPermissionGridMode> = {
	[PermissionGridMode.SINGLE]: ProtoPermissionGridMode.SINGLE,
	[PermissionGridMode.GRID]: ProtoPermissionGridMode.GRID,
};

class PermissionLayout {
	layoutMode: PermissionLayoutMode = PermissionLayoutMode.COMFY;
	gridMode: PermissionGridMode = PermissionGridMode.SINGLE;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => this.initPersistence());
	}

	private async initPersistence(): Promise<void> {
		await makeSyncedField(this, {
			field: 'permissionLayout',
			schema: PermissionLayoutSettingsSchema,
			persist: ['layoutMode', 'gridMode'],
			toMessage: (s) => ({
				layout: LAYOUT_TO_PROTO[s.layoutMode],
				grid: GRID_TO_PROTO[s.gridMode],
			}),
			applyMessage: (s, m) => {
				const layout = LAYOUT_FROM_PROTO[m.layout];
				if (layout !== null) s.layoutMode = layout;
				const grid = GRID_FROM_PROTO[m.grid];
				if (grid !== null) s.gridMode = grid;
			},
		});
	}

	get isComfy(): boolean {
		return this.layoutMode === PermissionLayoutMode.COMFY;
	}

	get isDense(): boolean {
		return this.layoutMode === PermissionLayoutMode.DENSE;
	}

	get isGrid(): boolean {
		return this.gridMode === PermissionGridMode.GRID;
	}

	toggleLayoutMode(): void {
		this.layoutMode = this.isComfy ? PermissionLayoutMode.DENSE : PermissionLayoutMode.COMFY;
	}

	toggleGridMode(): void {
		this.gridMode = this.isGrid ? PermissionGridMode.SINGLE : PermissionGridMode.GRID;
	}
}

export default new PermissionLayout();
