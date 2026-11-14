// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage from '@app/features/platform/state/PersistentStorage';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {makeAutoObservable} from 'mobx';

const THREAD_PANEL_MIN_WIDTH = 360;
const THREAD_PANEL_MAX_WIDTH = 900;
const THREAD_PANEL_DEFAULT_WIDTH = 450;
const STORAGE_KEY = 'ThreadPanelWidth';

let persisted = false;

class ThreadPanelWidth {
	width = THREAD_PANEL_DEFAULT_WIDTH;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		if (AppStorage.getItem(STORAGE_KEY) != null) this.persist();
	}

	setWidth(width: number): void {
		this.persist();
		this.width = Math.round(Math.min(THREAD_PANEL_MAX_WIDTH, Math.max(THREAD_PANEL_MIN_WIDTH, width)));
	}

	private persist(): void {
		if (persisted) return;
		persisted = true;
		void makePersistent(this, STORAGE_KEY, ['width']);
	}
}

export default new ThreadPanelWidth();
