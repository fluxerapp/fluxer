// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const FAILED_TO_DELETE_ROLE_DESCRIPTOR = msg({
	message: "Couldn't delete role",
	comment: 'Error modal title shown when role deletion fails.',
});

interface RoleDeleteFailedModalProps {
	roleName: string;
}

export const RoleDeleteFailedModal = observer(({roleName}: RoleDeleteFailedModalProps) => {
	const {i18n} = useLingui();
	return (
		<GenericErrorModal
			title={i18n._(FAILED_TO_DELETE_ROLE_DESCRIPTOR)}
			message={
				<Trans>
					We couldn't delete <strong data-flx="app.role-delete-failed-modal.strong">"{roleName}"</strong>. Try again.
				</Trans>
			}
			data-flx="app.role-delete-failed-modal.generic-error-modal"
		/>
	);
});
