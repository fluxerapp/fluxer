// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import {LeaveConfirmModal} from '@app/features/guild/components/modals/LeaveConfirmModal';
import {isStockCommunityGuild} from '@app/features/guild/utils/GuildCommunityUtils';
import {LEAVE_COMMUNITY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useCallback} from 'react';

const ARE_YOU_SURE_YOU_WANT_TO_LEAVE_THIS_DESCRIPTOR = msg({
	message: "Leave this community? You won't see its messages anymore.",
	comment: 'Body text in the leave-community confirmation modal.',
});

const LEFT_COMMUNITY_DESCRIPTOR = msg({
	message: 'Left community',
	comment: 'Success toast title shown after leaving a community.',
});
const FAILED_TO_LEAVE_COMMUNITY_DESCRIPTOR = msg({
	message: 'Failed to leave community',
	comment: 'Error modal title shown when leaving a community fails.',
});
const WE_COULDN_T_REMOVE_YOU_FROM_THE_COMMUNITY_DESCRIPTOR = msg({
	message: "Couldn't leave right now. Try again in a moment.",
	comment: 'Error modal body shown when leaving a community fails. Keep the tone plain and specific.',
});

const LeaveGuildModal = ({guildId}: {guildId: string}) => {
	const {i18n} = useLingui();
	return (
		<LeaveConfirmModal
			title={i18n._(LEAVE_COMMUNITY_DESCRIPTOR)}
			body={i18n._(ARE_YOU_SURE_YOU_WANT_TO_LEAVE_THIS_DESCRIPTOR)}
			onLeave={async (deleteMessages) => {
				const wasSelected = SelectedGuild.selectedGuildId === guildId;
				await GuildCommands.leave(guildId, {deleteMessages});
				if (wasSelected) {
					RouterUtils.transitionTo(Routes.ME);
				}
			}}
			errorModal={() => (
				<GenericErrorModal
					title={i18n._(FAILED_TO_LEAVE_COMMUNITY_DESCRIPTOR)}
					message={i18n._(WE_COULDN_T_REMOVE_YOU_FROM_THE_COMMUNITY_DESCRIPTOR)}
					data-flx="guild.use-leave-guild.handle-confirm.generic-error-modal"
				/>
			)}
			toastText={i18n._(LEFT_COMMUNITY_DESCRIPTOR)}
			dataFlx="guild.use-leave-guild.leave-guild-modal.modal-root"
			data-flx="guild.use-leave-guild.leave-guild-modal.leave-confirm-modal"
		/>
	);
};
export const useLeaveGuild = () => {
	return useCallback((guildId: string) => {
		if (isStockCommunityGuild(guildId)) {
			return;
		}
		ModalCommands.push(
			modal(() => <LeaveGuildModal guildId={guildId} data-flx="guild.use-leave-guild.leave-guild-modal" />),
		);
	}, []);
};
