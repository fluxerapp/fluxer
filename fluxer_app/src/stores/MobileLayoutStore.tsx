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

import {makeAutoObservable, reaction} from 'mobx';
import {makePersistent} from '~/lib/MobXPersistence';
import {Platform} from '~/lib/Platform';
import WindowStore from '~/stores/WindowStore';

const MOBILE_ENABLE_BREAKPOINT = 640;
const MOBILE_DISABLE_BREAKPOINT = 768;

const shouldForceMobileLayout = (): boolean => Platform.isMobileBrowser;

const getInitialMobileEnabled = (): boolean => {
	if (shouldForceMobileLayout()) {
		return true;
	}
	return window.innerWidth < MOBILE_ENABLE_BREAKPOINT;
};

class MobileLayoutStore {
	navExpanded = true;
	chatExpanded = false;
	enabled = getInitialMobileEnabled();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.initPersistence();
		this.initWindowSync();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'MobileLayoutStore', ['navExpanded', 'chatExpanded']);
	}

	private initWindowSync(): void {
		this.handleWindowSizeChange();

		reaction(
			() => WindowStore.windowSize,
			() => this.handleWindowSizeChange(),
			{fireImmediately: false},
		);
	}

	isEnabled() {
		return this.enabled;
	}

	private handleWindowSizeChange(): void {
		const windowSize = WindowStore.windowSize;
		const forceMobile = shouldForceMobileLayout();
		const threshold = this.enabled ? MOBILE_DISABLE_BREAKPOINT : MOBILE_ENABLE_BREAKPOINT;
		const widthBased = windowSize.width < threshold;
		const newEnabled = forceMobile || widthBased;

		if (newEnabled === this.enabled) {
			return;
		}

		this.enabled = newEnabled;
		if (newEnabled) {
			this.navExpanded = this.navExpanded && !this.chatExpanded;
		}
	}

	updateState(data: {navExpanded?: boolean; chatExpanded?: boolean}): void {
		const hasChanges =
			(data.navExpanded !== undefined && data.navExpanded !== this.navExpanded) ||
			(data.chatExpanded !== undefined && data.chatExpanded !== this.chatExpanded);

		if (!hasChanges) {
			return;
		}

		if (data.navExpanded !== undefined) {
			this.navExpanded = data.navExpanded;
			if (data.navExpanded && this.enabled && this.chatExpanded) {
				this.chatExpanded = false;
			}
		}

		if (data.chatExpanded !== undefined) {
			this.chatExpanded = data.chatExpanded;
			if (data.chatExpanded && this.enabled && this.navExpanded) {
				this.navExpanded = false;
			}
		}
	}

	isMobileLayout(): boolean {
		return this.enabled;
	}

	get platformMobileDetected(): boolean {
		return shouldForceMobileLayout();
	}

	isNavExpanded(): boolean {
		return this.navExpanded;
	}

	isChatExpanded(): boolean {
		return this.chatExpanded;
	}
}

export default new MobileLayoutStore();
