// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import type {User} from '@app/features/user/models/User';
import UserSettings from '@app/features/user/state/UserSettings';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const STAFF_DM_ACCESS_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't update direct message permission",
	comment: 'Title of the error modal shown when toggling staff DM access fails.',
});
const STAFF_DM_ACCESS_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: 'Something went wrong while saving this setting. Please try again in a moment.',
	comment: 'Body of the error modal shown when toggling staff DM access fails.',
});
const MENTION_BYPASS_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't update mention suppression bypass",
	comment: 'Title of the error modal shown when toggling mention suppression bypass fails.',
});
const MENTION_BYPASS_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: 'Something went wrong while saving this setting. Please try again in a moment.',
	comment: 'Body of the error modal shown when toggling mention suppression bypass fails.',
});

function showStaffControlErrorModal(title: string, message: string, flxKey: string): void {
	ModalCommands.push(modal(() => <GenericErrorModal title={title} message={message} data-flx={flxKey} />));
}
const ALLOW_DIRECT_MESSAGES_WITH_THIS_USER_DESCRIPTOR = msg({
	message: 'Allow direct messages with this user',
	comment: 'Staff-only setting that allows the current staff member and selected user to exchange direct messages.',
});
const BYPASS_MENTION_SUPPRESSION_DESCRIPTOR = msg({
	message: 'Bypass mention suppression',
	comment: 'Setting label that bypasses mention suppression for the current user.',
});

interface StaffUserControlsMenuItemsProps {
	user: User;
}

export function shouldShowStaffUserControlsMenuItems(params: {
	currentUser: User | null | undefined;
	user: User;
	isCurrentUser: boolean;
	restrictUserActions: boolean;
}): boolean {
	return (
		!params.isCurrentUser && !params.restrictUserActions && !params.user.bot && (params.currentUser?.isStaff() ?? false)
	);
}

function toggleUserIdList(userIds: ReadonlyArray<string>, userId: string, enabled: boolean): Array<string> {
	const nextUserIds = new Set(userIds);
	if (enabled) {
		nextUserIds.add(userId);
	} else {
		nextUserIds.delete(userId);
	}
	return [...nextUserIds];
}

export const StaffUserControlsMenuItems: React.FC<StaffUserControlsMenuItemsProps> = observer(({user}) => {
	const {i18n} = useLingui();
	const staffDmAccessUserIds = UserSettings.getStaffDmAccessUserIds();
	const mentionBypassUserIds = UserSettings.getSuppressUnprivilegedSelfMentionsBypassUserIds();
	const handleStaffDmAccessChange = useCallback(
		(checked: boolean) => {
			void UserSettingsCommands.update({
				staffDmAccessUserIds: toggleUserIdList(staffDmAccessUserIds, user.id, checked),
			}).catch(() => {
				showStaffControlErrorModal(
					i18n._(STAFF_DM_ACCESS_FAILED_TITLE_DESCRIPTOR),
					i18n._(STAFF_DM_ACCESS_FAILED_MESSAGE_DESCRIPTOR),
					'ui.action-menu.items.staff-user-controls-menu-items.staff-dm-access.generic-error-modal',
				);
			});
		},
		[staffDmAccessUserIds, user.id, i18n],
	);
	const handleMentionBypassChange = useCallback(
		(checked: boolean) => {
			void UserSettingsCommands.update({
				suppressUnprivilegedSelfMentionsBypassUserIds: toggleUserIdList(mentionBypassUserIds, user.id, checked),
			}).catch(() => {
				showStaffControlErrorModal(
					i18n._(MENTION_BYPASS_FAILED_TITLE_DESCRIPTOR),
					i18n._(MENTION_BYPASS_FAILED_MESSAGE_DESCRIPTOR),
					'ui.action-menu.items.staff-user-controls-menu-items.mention-bypass.generic-error-modal',
				);
			});
		},
		[mentionBypassUserIds, user.id, i18n],
	);
	return (
		<>
			<CheckboxItem
				checked={staffDmAccessUserIds.includes(user.id)}
				onCheckedChange={handleStaffDmAccessChange}
				data-flx="ui.action-menu.items.staff-user-controls-menu-items.checkbox-item"
			>
				{i18n._(ALLOW_DIRECT_MESSAGES_WITH_THIS_USER_DESCRIPTOR)}
			</CheckboxItem>
			<CheckboxItem
				checked={mentionBypassUserIds.includes(user.id)}
				onCheckedChange={handleMentionBypassChange}
				data-flx="ui.action-menu.items.staff-user-controls-menu-items.checkbox-item--2"
			>
				{i18n._(BYPASS_MENTION_SUPPRESSION_DESCRIPTOR)}
			</CheckboxItem>
		</>
	);
});
