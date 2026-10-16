// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import * as ChannelCommands from '@app/features/channel/commands/ChannelCommands';
import {LEAVE_GROUP_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {LeaveConfirmModal} from '@app/features/guild/components/modals/LeaveConfirmModal';
import {TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {ME} from '@fluxer/constants/src/AppConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useCallback, useState} from 'react';

const LEFT_GROUP_DESCRIPTOR = msg({
	message: 'Left group',
	comment: 'Success toast title shown after leaving a group DM.',
});
const ARE_YOU_SURE_YOU_WANT_TO_LEAVE_THIS_DESCRIPTOR = msg({
	message: "Leave this group? You won't see its messages anymore.",
	comment: 'Body text in the leave-group confirmation modal.',
});
const LEAVE_SILENTLY_DESCRIPTOR = msg({
	message: 'Leave silently',
	comment:
		'Switch label in the leave-group confirmation modal. Suppresses the system leave notice so other members are not pinged.',
});
const LEAVE_SILENTLY_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Skip the system leave notice — nobody gets pinged.',
	comment:
		'Switch description in the leave-group confirmation modal. Explains that silently leaving avoids posting a system leave notice.',
});

const LeaveGroupModal = ({channelId}: {channelId: string}) => {
	const {i18n} = useLingui();
	const [silent, setSilent] = useState(false);
	return (
		<LeaveConfirmModal
			title={i18n._(LEAVE_GROUP_DESCRIPTOR)}
			body={i18n._(ARE_YOU_SURE_YOU_WANT_TO_LEAVE_THIS_DESCRIPTOR)}
			extraSwitches={
				<Switch
					label={i18n._(LEAVE_SILENTLY_DESCRIPTOR)}
					description={i18n._(LEAVE_SILENTLY_DESCRIPTION_DESCRIPTOR)}
					value={silent}
					onChange={setSilent}
					data-flx="guild.use-leave-group.leave-group-modal.switch.set-silent"
				/>
			}
			onLeave={async (deleteMessages) => {
				await ChannelCommands.remove(channelId, silent, {deleteMessages});
				const selectedChannel = SelectedChannel.selectedChannelIds.get(ME);
				if (selectedChannel === channelId) {
					RouterUtils.transitionTo(Routes.ME);
				}
			}}
			errorModal={() => (
				<GenericErrorModal
					title={i18n._(FAILED_TO_LEAVE_GROUP_DESCRIPTOR)}
					message={i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR)}
					data-flx="guild.group-leave-failed-modal.generic-error-modal"
				/>
			)}
			toastText={i18n._(LEFT_GROUP_DESCRIPTOR)}
			dataFlx="guild.use-leave-group.leave-group-modal.modal-root"
			data-flx="guild.use-leave-group.leave-group-modal.leave-confirm-modal"
		/>
	);
};
export const useLeaveGroup = () => {
	return useCallback((channelId: string) => {
		ModalCommands.push(
			modal(() => <LeaveGroupModal channelId={channelId} data-flx="guild.use-leave-group.leave-group-modal" />),
		);
	}, []);
};

const FAILED_TO_LEAVE_GROUP_DESCRIPTOR = msg({
	message: "Couldn't leave group",
	comment: 'Error modal title shown when leaving a group DM fails.',
});
