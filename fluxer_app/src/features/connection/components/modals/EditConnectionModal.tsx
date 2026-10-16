// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import * as ConnectionCommands from '@app/features/connection/commands/ConnectionCommands';
import type {Connection} from '@app/features/connection/models/Connection';
import {COMMUNITY_MEMBERS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {WarningAlert} from '@app/features/ui/warning_alert/WarningAlert';
import UserSettings from '@app/features/user/state/UserSettings';
import {ConnectionVisibilityFlags} from '@fluxer/constants/src/ConnectionConstants';
import {ProfilePrivacyLevels} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useState} from 'react';

const EDIT_CONNECTION_DESCRIPTOR = msg({
	message: 'Edit connection',
	comment: 'Short label in the connection edit connection modal.',
});

interface Props {
	connection: Connection;
}

export const EditConnectionModal = observer(({connection}: Props) => {
	const {i18n} = useLingui();
	const [visibilityFlags, setVisibilityFlags] = useState(connection.visibilityFlags);
	const [submitting, setSubmitting] = useState(false);
	const hasFlag = useCallback((flag: number) => (visibilityFlags & flag) === flag, [visibilityFlags]);
	const everyoneEnabled = (visibilityFlags & ConnectionVisibilityFlags.EVERYONE) === ConnectionVisibilityFlags.EVERYONE;
	const profilePrivacy = UserSettings.profilePrivacy;
	const profilePrivacyLimited =
		profilePrivacy === ProfilePrivacyLevels.SMALL_GUILDS_ONLY || profilePrivacy === ProfilePrivacyLevels.FRIENDS_ONLY;
	const handleToggle = useCallback(
		(flag: number, value: boolean) => {
			setVisibilityFlags((prev) => {
				if (value) {
					return prev | flag;
				}
				return prev & ~flag;
			});
		},
		[setVisibilityFlags],
	);
	const handleSave = useCallback(async () => {
		setSubmitting(true);
		try {
			await ConnectionCommands.updateConnection(i18n, connection.type, connection.id, {
				visibility_flags: visibilityFlags,
			});
			ModalCommands.pop();
		} finally {
			setSubmitting(false);
		}
	}, [i18n, connection.type, connection.id, visibilityFlags]);
	return (
		<Modal.Root size="small" centered data-flx="connection.edit-connection-modal.modal-root">
			<Modal.Header
				title={i18n._(EDIT_CONNECTION_DESCRIPTOR)}
				data-flx="connection.edit-connection-modal.modal-header"
			/>
			<Modal.Content data-flx="connection.edit-connection-modal.modal-content">
				<Modal.ContentLayout data-flx="connection.edit-connection-modal.modal-content-layout">
					<Modal.Description data-flx="connection.edit-connection-modal.modal-description">
						<Trans>Choose who can see this connection on your profile.</Trans>
					</Modal.Description>
					{profilePrivacyLimited && (
						<WarningAlert data-flx="connection.edit-connection-modal.limited-profile-warning-alert">
							<Trans>
								Your profile privacy is limited, so some people still won't see this connection even when Everyone is
								enabled.
							</Trans>
						</WarningAlert>
					)}
					<Switch
						label={<Trans>Everyone</Trans>}
						description={<Trans>Allow anyone to see this connection on your profile</Trans>}
						value={hasFlag(ConnectionVisibilityFlags.EVERYONE)}
						onChange={(value) => handleToggle(ConnectionVisibilityFlags.EVERYONE, value)}
						data-flx="connection.edit-connection-modal.switch.toggle"
					/>
					<Switch
						label={<Trans>Friends</Trans>}
						description={<Trans>Allow your friends to see this connection</Trans>}
						value={everyoneEnabled || hasFlag(ConnectionVisibilityFlags.FRIENDS)}
						onChange={(value) => handleToggle(ConnectionVisibilityFlags.FRIENDS, value)}
						disabled={everyoneEnabled}
						data-flx="connection.edit-connection-modal.switch.toggle--2"
					/>
					<Switch
						label={i18n._(COMMUNITY_MEMBERS_DESCRIPTOR)}
						description={<Trans>Allow members from communities you're in to see this connection</Trans>}
						value={everyoneEnabled || hasFlag(ConnectionVisibilityFlags.MUTUAL_GUILDS)}
						onChange={(value) => handleToggle(ConnectionVisibilityFlags.MUTUAL_GUILDS, value)}
						disabled={everyoneEnabled}
						data-flx="connection.edit-connection-modal.switch.toggle--3"
					/>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="connection.edit-connection-modal.modal-footer">
				<Button onClick={ModalCommands.pop} variant="secondary" data-flx="connection.edit-connection-modal.button.pop">
					<Trans>Cancel</Trans>
				</Button>
				<Button onClick={handleSave} submitting={submitting} data-flx="connection.edit-connection-modal.button.save">
					<Trans>Save</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
