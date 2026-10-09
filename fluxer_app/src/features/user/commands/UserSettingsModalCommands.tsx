// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserSettingsModal} from '@app/features/app/components/dialogs/LoadableSettingsModals';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import UserSettings from '@app/features/user/state/UserSettings';

export function openUserSettingsModal(): void {
	const privacyReviewPending = UserSettings.isPrivacySetupPending();
	ModalCommands.push(
		modal(
			() =>
				privacyReviewPending ? (
					<UserSettingsModal
						initialTab="privacy_safety"
						initialSubtab="connections"
						data-flx="user.user-settings-modal-commands.open-user-settings-modal.user-settings-modal--privacy-review"
					/>
				) : (
					<UserSettingsModal data-flx="user.user-settings-modal-commands.open-user-settings-modal.user-settings-modal" />
				),
			'user-settings',
		),
	);
}
