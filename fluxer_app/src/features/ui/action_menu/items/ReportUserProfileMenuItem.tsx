// SPDX-License-Identifier: AGPL-3.0-or-later

import {REPORT_USER_PROFILE_DESCRIPTOR} from '@app/features/moderation/utils/ModerationMessageDescriptors';
import {openReportUserProfileModal} from '@app/features/moderation/utils/ReportActionUtils';
import {ReportMessageIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import type {User} from '@app/features/user/models/User';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

interface ReportUserProfileMenuItemProps {
	user: User;
	guildId?: string;
	onClose: () => void;
}

export const ReportUserProfileMenuItem: React.FC<ReportUserProfileMenuItemProps> = observer(
	({user, guildId, onClose}) => {
		const {i18n} = useLingui();
		const handleReportProfile = useCallback(() => {
			ModalCommands.runAfterBottomSheetClose(onClose, () => openReportUserProfileModal({user, guildId}));
		}, [guildId, onClose, user]);
		if (user.system) {
			return null;
		}
		return (
			<MenuItem
				icon={
					<ReportMessageIcon
						size={16}
						data-flx="ui.action-menu.items.report-user-profile-menu-item.report-message-icon"
					/>
				}
				onClick={handleReportProfile}
				danger
				data-flx="ui.action-menu.items.report-user-profile-menu-item.menu-item.report-profile"
			>
				{i18n._(REPORT_USER_PROFILE_DESCRIPTOR)}
			</MenuItem>
		);
	},
);
