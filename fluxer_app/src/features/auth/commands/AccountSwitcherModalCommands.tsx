// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	AccountSwitcherInitialView,
	AccountSwitcherModalProps,
} from '@app/features/auth/components/accounts/AccountSwitcherModal';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {loadLazyModule} from '@app/features/platform/utils/LazyModuleLoader';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import Modal from '@app/features/ui/state/Modal';

const logger = new Logger('AccountSwitcherModalCommands');
const ACCOUNT_SWITCHER_MODAL_KEY = 'account-switcher';
let openSerial = 0;

interface AccountSwitcherOpenOptions extends Partial<Omit<AccountSwitcherModalProps, 'initialView'>> {
	readonly view?: AccountSwitcherInitialView;
}

function resolveProps(options: AccountSwitcherOpenOptions): AccountSwitcherModalProps {
	const {view, ...props} = options;
	return {
		closeCurrentAccountOnSelect: props.closeCurrentAccountOnSelect ?? true,
		initialReloginAccount: props.initialReloginAccount ?? null,
		redirectAfterLogin: props.redirectAfterLogin ?? null,
		switchAccount: props.switchAccount ?? null,
		initialView: view ?? 'accounts',
		focusAccountKey: props.focusAccountKey ?? null,
		'data-flx': props['data-flx'],
	};
}

export function openAccountSwitcherModal(options: AccountSwitcherOpenOptions = {}, key: string | null = null): void {
	const props = resolveProps(options);
	openSerial += 1;
	const instanceKey = openSerial;
	void loadLazyModule(() => import('@app/features/auth/components/accounts/AccountSwitcherModal'))
		.then(({default: AccountSwitcherModal}) => {
			const render = modal(() => (
				<AccountSwitcherModal
					key={instanceKey}
					data-flx="auth.account-switcher-modal-commands.open-account-switcher-modal.account-switcher-modal"
					{...props}
				/>
			));
			ModalCommands.pushWithKey(render, key ?? ACCOUNT_SWITCHER_MODAL_KEY);
		})
		.catch((error: unknown) => logger.error('Failed to load the account switcher', error));
}

export function isAccountSwitcherModalOpen(): boolean {
	return Modal.hasModal(ACCOUNT_SWITCHER_MODAL_KEY);
}

export function toggleAccountSwitcherModal(options: AccountSwitcherOpenOptions = {}): void {
	if (ModalCommands.getTopModalKey() === ACCOUNT_SWITCHER_MODAL_KEY) {
		ModalCommands.popWithKey(ACCOUNT_SWITCHER_MODAL_KEY);
		return;
	}
	openAccountSwitcherModal(options);
}
