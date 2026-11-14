// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {SelfAssignableRolesView} from '@app/features/guild/components/self_assignable_roles/SelfAssignableRolesView';
import {ROLES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

export const SelfAssignableRolesModal = observer(function SelfAssignableRolesModal({guildId}: {guildId: string}) {
	const {i18n} = useLingui();
	return (
		<Modal.Root size="medium" data-flx="guild.self-assignable-roles.self-assignable-roles-modal.modal-root">
			<Modal.Header
				title={i18n._(ROLES_DESCRIPTOR)}
				data-flx="guild.self-assignable-roles.self-assignable-roles-modal.modal-header"
			/>
			<Modal.Content data-flx="guild.self-assignable-roles.self-assignable-roles-modal.modal-content">
				<Modal.ContentLayout data-flx="guild.self-assignable-roles.self-assignable-roles-modal.modal-content-layout">
					<SelfAssignableRolesView
						guildId={guildId}
						variant="modal"
						data-flx="guild.self-assignable-roles.self-assignable-roles-modal.self-assignable-roles-view"
					/>
				</Modal.ContentLayout>
			</Modal.Content>
		</Modal.Root>
	);
});

export function openSelfAssignableRolesModal(guildId: string): void {
	ModalCommands.push(
		modal(() => (
			<SelfAssignableRolesModal
				guildId={guildId}
				data-flx="guild.self-assignable-roles.self-assignable-roles-modal.open-self-assignable-roles-modal.self-assignable-roles-modal"
			/>
		)),
	);
}
