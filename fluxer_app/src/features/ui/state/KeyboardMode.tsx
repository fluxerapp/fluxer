// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {registerKeyboardModeRestoreCallback, registerKeyboardModeStateResolver} from '@app/features/ui/state/Modal';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {KeyboardModeIntroStateSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable, runInAction} from 'mobx';

const logger = new Logger('KeyboardMode');

class KeyboardMode {
	keyboardModeEnabled = false;
	introSeen = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void makeSyncedField(this, {
			field: 'keyboardModeIntro',
			schema: KeyboardModeIntroStateSchema,
			persist: ['introSeen'],
			toMessage: (s) => ({seen: s.introSeen}),
			applyMessage: (s, m) => {
				s.introSeen = m.seen;
			},
		});
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
			void Promise.all([
				import('@app/features/ui/commands/ModalCommands'),
				import('@app/features/input/components/modals/KeyboardModeIntroModal'),
			]).then(([{modal, push}, {KeyboardModeIntroModal}]) => {
				push(modal(() => <KeyboardModeIntroModal data-flx="ui.keyboard-mode.keyboard-mode-intro-modal" />));
			});
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

const keyboardModeState = new KeyboardMode();

registerKeyboardModeStateResolver(() => keyboardModeState.keyboardModeEnabled);

registerKeyboardModeRestoreCallback((showIntro) => keyboardModeState.enterKeyboardMode(showIntro));

export default keyboardModeState;
