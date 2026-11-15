// SPDX-License-Identifier: AGPL-3.0-or-later

import AccountThemeMode from '@app/features/theme/state/AccountThemeMode';
import {makeAutoObservable} from 'mobx';

export type ThemeTarget = 'desktop' | 'account';

class ThemeEditTarget {
	picked: ThemeTarget | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get target(): ThemeTarget {
		if (!AccountThemeMode.canUseDesktopTheme) {
			return 'account';
		}
		return this.picked ?? AccountThemeMode.activeTarget;
	}

	pick(target: ThemeTarget | null): void {
		this.picked = target;
	}

	reset(): void {
		this.picked = null;
	}
}

export default new ThemeEditTarget();
