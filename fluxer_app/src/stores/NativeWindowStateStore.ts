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
import {buildStateFlags, saveCurrentWindowState} from '~/utils/WindowStateUtils';

class NativeWindowStateStore {
	rememberSizeAndPosition = true;
	rememberMaximized = true;
	rememberFullscreen = true;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void makePersistent(this, 'NativeWindowStateStore', [
			'rememberSizeAndPosition',
			'rememberMaximized',
			'rememberFullscreen',
		]);
	}

	setRememberSizeAndPosition(enabled: boolean): void {
		this.rememberSizeAndPosition = enabled;
		void this.saveWithCurrentFlags();
	}

	setRememberMaximized(enabled: boolean): void {
		this.rememberMaximized = enabled;
		void this.saveWithCurrentFlags();
	}

	setRememberFullscreen(enabled: boolean): void {
		this.rememberFullscreen = enabled;
		void this.saveWithCurrentFlags();
	}

	private async saveWithCurrentFlags(): Promise<void> {
		const flags = buildStateFlags({
			rememberSizeAndPosition: this.rememberSizeAndPosition,
			rememberMaximized: this.rememberMaximized,
			rememberFullscreen: this.rememberFullscreen,
		});
		await saveCurrentWindowState(flags);
	}
}

export default new NativeWindowStateStore();
