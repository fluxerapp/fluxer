// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import styles from '@app/features/input/components/modals/KeyboardModeIntroModal.module.css';
import {SHIFT_KEY_DESCRIPTOR} from '@app/features/input/utils/KeyboardUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import {isNativeMacOS} from '@app/features/ui/utils/NativeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useCallback} from 'react';

const KEYBOARD_MODE_DESCRIPTOR = msg({
	message: 'Keyboard mode',
	comment: 'Modal title shown after the user first enters keyboard navigation mode.',
});
const CTRL_DESCRIPTOR = msg({
	message: 'Ctrl',
	comment: 'Keyboard keycap label for Control on non-Mac platforms. Keep it short.',
});
const TAB_DESCRIPTOR = msg({
	message: 'Tab',
	comment: 'Keyboard keycap label for Tab. Keep it short.',
});
const YOU_JUST_PRESSED_TAB_KEYBOARD_MODE_IS_NOW_DESCRIPTOR = msg({
	message: 'You just pressed Tab. Keyboard mode is now on so you can navigate {productName} without a mouse.',
	comment: 'Intro copy in the keyboard navigation modal. {productName} is the app name.',
});
const OR_DESCRIPTOR = msg({
	message: 'or',
	comment: 'Small separator between alternative shortcut keycaps in the keyboard mode intro modal.',
});
const MOVE_FOCUS_ACROSS_BUTTONS_INPUTS_AND_LINKS_DESCRIPTOR = msg({
	message: 'Navigate buttons, inputs, and links.',
	comment: 'Tip explaining Tab and Shift+Tab navigation in the keyboard mode intro modal.',
});
const STEP_THROUGH_MESSAGES_AND_ACTION_BARS_IN_CHAT_DESCRIPTOR = msg({
	message: 'Navigate messages and actions in chat.',
	comment: 'Tip explaining up/down arrow navigation in the keyboard mode intro modal.',
});
const OPEN_THE_SHORTCUTS_LIST_ANYTIME_FOR_QUICK_ACTIONS_DESCRIPTOR = msg({
	message: 'Open the shortcuts list for quick actions.',
	comment: 'Tip explaining the keyboard shortcut for opening the shortcuts list.',
});
export function KeyboardModeIntroModal() {
	const {i18n} = useLingui();
	const title = i18n._(KEYBOARD_MODE_DESCRIPTOR);
	const commandKeyLabel = isNativeMacOS() ? '⌘' : i18n._(CTRL_DESCRIPTOR);
	const tabKeyLabel = i18n._(TAB_DESCRIPTOR);
	const handleClose = useCallback(() => {
		KeyboardMode.dismissIntro();
		ModalCommands.pop();
	}, []);
	return (
		<Modal.Root size="small" centered onClose={handleClose} data-flx="input.keyboard-mode-intro-modal.modal-root">
			<Modal.Header title={title} onClose={handleClose} data-flx="input.keyboard-mode-intro-modal.modal-header" />
			<Modal.Content contentClassName={styles.content} data-flx="input.keyboard-mode-intro-modal.modal-content">
				<p className={styles.description} data-flx="input.keyboard-mode-intro-modal.description">
					{i18n._(YOU_JUST_PRESSED_TAB_KEYBOARD_MODE_IS_NOW_DESCRIPTOR, {productName: PRODUCT_NAME})}
				</p>
				<ul className={styles.tips} data-flx="input.keyboard-mode-intro-modal.tips">
					<li className={styles.tip} data-flx="input.keyboard-mode-intro-modal.tip">
						<div className={styles.keys} aria-hidden="true" data-flx="input.keyboard-mode-intro-modal.keys">
							<kbd className={styles.kbd} data-flx="input.keyboard-mode-intro-modal.kbd">
								{tabKeyLabel}
							</kbd>
							<span className={styles.separator} data-flx="input.keyboard-mode-intro-modal.separator">
								{i18n._(OR_DESCRIPTOR)}
							</span>
							<kbd className={styles.kbd} data-flx="input.keyboard-mode-intro-modal.kbd--2">
								{i18n._(SHIFT_KEY_DESCRIPTOR)}
							</kbd>
							<span className={styles.separator} data-flx="input.keyboard-mode-intro-modal.separator--2">
								+
							</span>
							<kbd className={styles.kbd} data-flx="input.keyboard-mode-intro-modal.kbd--3">
								{tabKeyLabel}
							</kbd>
						</div>
						<p className={styles.tipText} data-flx="input.keyboard-mode-intro-modal.tip-text">
							{i18n._(MOVE_FOCUS_ACROSS_BUTTONS_INPUTS_AND_LINKS_DESCRIPTOR)}
						</p>
					</li>
					<li className={styles.tip} data-flx="input.keyboard-mode-intro-modal.tip--2">
						<div className={styles.keys} aria-hidden="true" data-flx="input.keyboard-mode-intro-modal.keys--2">
							<kbd className={styles.kbd} data-flx="input.keyboard-mode-intro-modal.kbd--4">
								↑
							</kbd>
							<kbd className={styles.kbd} data-flx="input.keyboard-mode-intro-modal.kbd--5">
								↓
							</kbd>
						</div>
						<p className={styles.tipText} data-flx="input.keyboard-mode-intro-modal.tip-text--2">
							{i18n._(STEP_THROUGH_MESSAGES_AND_ACTION_BARS_IN_CHAT_DESCRIPTOR)}
						</p>
					</li>
					<li className={styles.tip} data-flx="input.keyboard-mode-intro-modal.tip--3">
						<div className={styles.keys} aria-hidden="true" data-flx="input.keyboard-mode-intro-modal.keys--3">
							<kbd className={styles.kbd} data-flx="input.keyboard-mode-intro-modal.kbd--6">
								{commandKeyLabel}
							</kbd>
							<kbd className={styles.kbd} data-flx="input.keyboard-mode-intro-modal.kbd--7">
								/
							</kbd>
						</div>
						<p className={styles.tipText} data-flx="input.keyboard-mode-intro-modal.tip-text--3">
							{i18n._(OPEN_THE_SHORTCUTS_LIST_ANYTIME_FOR_QUICK_ACTIONS_DESCRIPTOR)}
						</p>
					</li>
				</ul>
			</Modal.Content>
		</Modal.Root>
	);
}
