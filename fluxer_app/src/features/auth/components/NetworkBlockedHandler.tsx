// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {NETWORK_BLOCKED_DESCRIPTOR} from '@app/features/auth/AuthMessageDescriptors';
import {NetworkBlockDetails} from '@app/features/auth/flow/AccountRestrictionScreen';
import styles from '@app/features/auth/flow/AccountRestrictionScreen.module.css';
import {TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {getLocaleDirection} from '@app/features/i18n/utils/LocaleDirection';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import ModalState from '@app/features/ui/state/Modal';
import type {NetworkBlock} from '@fluxer/schema/src/domains/auth/EnforcementNoticeSchemas';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const NETWORK_BLOCKED_MODAL_KEY = 'network-blocked';

const NetworkBlockedModal = observer(function NetworkBlockedModal({block}: {block: NetworkBlock}) {
	const {i18n} = useLingui();
	const dir = getLocaleDirection(i18n.locale);
	return (
		<Modal.Root size="small" centered className={styles.selectable} data-flx="auth.network-blocked-handler.modal-root">
			<div className={styles.directionScope} dir={dir} data-flx="auth.network-blocked-handler.direction-scope">
				<Modal.Header title={i18n._(NETWORK_BLOCKED_DESCRIPTOR)} data-flx="auth.network-blocked-handler.modal-header" />
				<Modal.Content dir={dir} data-flx="auth.network-blocked-handler.modal-content">
					<div className={styles.dialogBody} data-flx="auth.network-blocked-handler.dialog-body">
						<NetworkBlockDetails block={block} data-flx="auth.network-blocked-handler.network-block-details" />
					</div>
				</Modal.Content>
				<Modal.Footer data-flx="auth.network-blocked-handler.footer">
					<Button onClick={ModalCommands.pop} data-flx="auth.network-blocked-handler.button.try-again">
						{i18n._(TRY_AGAIN_DESCRIPTOR)}
					</Button>
				</Modal.Footer>
			</div>
		</Modal.Root>
	);
});

export function showNetworkBlockedModal(block: NetworkBlock): void {
	if (ModalState.hasModal(NETWORK_BLOCKED_MODAL_KEY)) {
		return;
	}
	ModalCommands.pushWithKey(
		modal(() => <NetworkBlockedModal block={block} data-flx="auth.network-blocked-handler.network-blocked-modal" />),
		NETWORK_BLOCKED_MODAL_KEY,
	);
}
