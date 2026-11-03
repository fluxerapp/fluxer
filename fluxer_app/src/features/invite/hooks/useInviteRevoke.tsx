// SPDX-License-Identifier: AGPL-3.0-or-later

import modalI18n from '@app/app/I18n';
import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import * as InviteCommands from '@app/features/invite/commands/InviteCommands';
import {msg} from '@lingui/core/macro';
import {useCallback} from 'react';

const InviteRevokeFailedModal_FAILED_TO_REVOKE_INVITE_DESCRIPTOR = msg({
	message: "Couldn't revoke invite",
	comment: 'Error modal title shown when revoking an invite fails.',
});

const InviteRevokeFailedModal_WE_COULDN_T_REVOKE_THE_INVITE_THE_LINK_DESCRIPTOR = msg({
	message: 'The link may still work. Try again in a moment.',
	comment: 'Error modal body shown when revoking an invite fails; warns the user that the link may still be live.',
});

export function useInviteRevoke() {
	return useCallback(async (code: string) => {
		try {
			await InviteCommands.remove(code);
		} catch (_error) {
			showGenericErrorModal({
				title: () => modalI18n._(InviteRevokeFailedModal_FAILED_TO_REVOKE_INVITE_DESCRIPTOR),
				message: () => modalI18n._(InviteRevokeFailedModal_WE_COULDN_T_REVOKE_THE_INVITE_THE_LINK_DESCRIPTOR),
				dataFlx: 'invite.invite-revoke-failed-modal.generic-error-modal',
			});
		}
	}, []);
}
