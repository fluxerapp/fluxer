// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AccountSwitcherModalProps} from '@app/features/auth/components/accounts/AccountSwitcherModal';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {loadLazyModule} from '@app/features/platform/utils/LazyModuleLoader';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';

const logger = new Logger('AccountSwitcherModalCommands');

export function openAccountSwitcherModal(props: AccountSwitcherModalProps, key: string | null): void {
	void loadLazyModule(() => import('@app/features/auth/components/accounts/AccountSwitcherModal'))
		.then(({default: AccountSwitcherModal}) => {
			const render = modal(() => <AccountSwitcherModal {...props} />);
			if (key === null) {
				ModalCommands.push(render);
				return;
			}
			ModalCommands.pushWithKey(render, key);
		})
		.catch((error: unknown) => logger.error('Failed to load the account switcher', error));
}
