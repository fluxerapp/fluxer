// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const CONTENT_BLOCKED_DESCRIPTOR = msg({
	message: 'Content blocked',
	comment: 'Short label in the authentication content blocked handler. Keep the tone plain and specific.',
});
const YOUR_ACTION_COULD_NOT_BE_COMPLETED_BECAUSE_IT_DESCRIPTOR = msg({
	message:
		'Your action could not be completed because it was flagged by our safety systems. If you believe this is a mistake, please contact support.',
	comment:
		'Modal body shown when the safety system blocks the requested action. Keep plain and calm; offer support path.',
});
const ContentBlockedModal = observer(() => {
	const {i18n} = useLingui();
	return (
		<GenericErrorModal
			title={i18n._(CONTENT_BLOCKED_DESCRIPTOR)}
			message={i18n._(YOUR_ACTION_COULD_NOT_BE_COMPLETED_BECAUSE_IT_DESCRIPTOR)}
			data-flx="auth.content-blocked-handler.content-blocked-modal.confirm-modal"
		/>
	);
});

export function showContentBlockedModal(): void {
	ModalCommands.push(
		modal(() => (
			<ContentBlockedModal data-flx="auth.content-blocked-handler.show-content-blocked-modal.content-blocked-modal" />
		)),
	);
}
