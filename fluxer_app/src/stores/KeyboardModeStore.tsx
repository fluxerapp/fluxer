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

import {makeAutoObservable, runInAction} from 'mobx';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {KeyboardModeIntroModal} from '~/components/modals/KeyboardModeIntroModal';
import {Logger} from '~/lib/Logger';
import {makePersistent} from '~/lib/MobXPersistence';

const logger = new Logger('KeyboardModeStore');

class KeyboardModeStore {
	keyboardModeEnabled = false;
	introSeen = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void makePersistent(this, 'KeyboardModeStore', ['introSeen']);
	}

	enterKeyboardMode(showIntro = true): void {
		logger.debug(
			`Entering keyboard mode (showIntro=${showIntro}) previous=${this.keyboardModeEnabled ? 'true' : 'false'}`,
		);
		runInAction(() => {
			this.keyboardModeEnabled = true;
		});

		if (showIntro && !this.introSeen) {
			this.introSeen = true;
			ModalActionCreators.push(modal(() => <KeyboardModeIntroModal />));
		}
	}

	exitKeyboardMode(): void {
		if (!this.keyboardModeEnabled) {
			logger.debug('exitKeyboardMode ignored (already false)');
			return;
		}
		logger.debug('Exiting keyboard mode');
		runInAction(() => {
			this.keyboardModeEnabled = false;
		});
	}

	dismissIntro(): void {
		this.introSeen = true;
	}
}

export default new KeyboardModeStore();
