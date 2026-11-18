// SPDX-License-Identifier: AGPL-3.0-or-later

import {IS_DEV} from '@app/features/platform/types/Env';
import Users from '@app/features/user/state/Users';
import {makeAutoObservable} from 'mobx';

class DeveloperMode {
	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get isDeveloper(): boolean {
		if (IS_DEV) return true;
		return Users.currentUser?.isStaff?.() ?? false;
	}
}

export default new DeveloperMode();
