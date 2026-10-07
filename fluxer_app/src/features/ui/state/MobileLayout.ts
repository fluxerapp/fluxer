// SPDX-License-Identifier: AGPL-3.0-or-later

import {Platform} from '@app/features/platform/types/Platform';
import {deferUntilModulesLoaded} from '@app/features/platform/utils/DeferUntilModulesLoaded';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import Window from '@app/features/window/state/Window';
import {makeAutoObservable, reaction} from 'mobx';

const MOBILE_ENABLE_BREAKPOINT = 640;
const MOBILE_DISABLE_BREAKPOINT = 768;
const shouldForceMobileLayout = (): boolean => Platform.isMobileBrowser;
const getInitialMobileEnabled = (): boolean => {
	if (shouldForceMobileLayout()) {
		return true;
	}
	return window.innerWidth < MOBILE_ENABLE_BREAKPOINT;
};

class MobileLayout {
	navExpanded = true;
	chatExpanded = false;
	enabled = getInitialMobileEnabled();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => this.initPersistence());
		this.initWindowSync();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'MobileLayout', ['navExpanded', 'chatExpanded']);
	}

	private initWindowSync(): void {
		deferUntilModulesLoaded(() => {
			this.handleWindowSizeChange();
			reaction(
				() => Window.windowSize,
				() => this.handleWindowSizeChange(),
				{fireImmediately: false},
			);
		});
	}

	isEnabled() {
		return this.enabled;
	}

	private handleWindowSizeChange(): void {
		const windowSize = Window.windowSize;
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

export default new MobileLayout();
