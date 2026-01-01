/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {makeAutoObservable} from 'mobx';
import {makePersistent} from '~/lib/MobXPersistence';

export enum PermissionLayoutMode {
	COMFY = 'comfy',
	DENSE = 'dense',
}

export enum PermissionGridMode {
	SINGLE = 'single',
	GRID = 'grid',
}

class PermissionLayoutStore {
	layoutMode: PermissionLayoutMode = PermissionLayoutMode.COMFY;
	gridMode: PermissionGridMode = PermissionGridMode.SINGLE;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'PermissionLayoutStore', ['layoutMode', 'gridMode']);
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

	setLayoutMode(mode: PermissionLayoutMode): void {
		this.layoutMode = mode;
	}

	setGridMode(mode: PermissionGridMode): void {
		this.gridMode = mode;
	}

	toggleLayoutMode(): void {
		this.layoutMode = this.isComfy ? PermissionLayoutMode.DENSE : PermissionLayoutMode.COMFY;
	}

	toggleGridMode(): void {
		this.gridMode = this.isGrid ? PermissionGridMode.SINGLE : PermissionGridMode.GRID;
	}
}

export default new PermissionLayoutStore();
