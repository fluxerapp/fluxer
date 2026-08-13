// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import * as GuildMemberCommands from '@app/features/member/commands/GuildMemberCommands';
import {showModerationErrorModal} from '@app/features/moderation/components/alerts/ModerationErrorModalUtils';
import {REMOVE_TIMEOUT_DESCRIPTOR} from '@app/features/moderation/utils/ModerationMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import type {User} from '@app/features/user/models/User';
import * as DisplayNameUtils from '@app/features/user/utils/DisplayNameUtils';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useState} from 'react';

const logger = new Logger('RemoveTimeoutModal');

interface RemoveTimeoutModalProps {
	guildId: string;
	targetUser: User;
}

export const RemoveTimeoutModal: React.FC<RemoveTimeoutModalProps> = observer(({guildId, targetUser}) => {
	const {i18n} = useLingui();
	const [isSubmitting, setIsSubmitting] = useState(false);
	const targetUserTag = DisplayNameUtils.formatTagForStreamerMode(targetUser.tag);
	const handleRemove = async () => {
		setIsSubmitting(true);
		try {
			await GuildMemberCommands.timeout(guildId, targetUser.id, null);
			ToastCommands.createToast({
				type: 'success',
				children: <Trans>Removed timeout from {targetUserTag}</Trans>,
			});
			ModalCommands.pop();
		} catch (error) {
			logger.error('Failed to remove timeout:', error);
			showModerationErrorModal(
				i18n,
				<Trans>Failed to remove timeout. Try again.</Trans>,
				'moderation.remove-timeout-modal.remove-error-modal',
			);
		} finally {
			setIsSubmitting(false);
		}
	};
	return (
		<Modal.Root size="small" centered data-flx="moderation.remove-timeout-modal.modal-root">
			<Modal.Header title={i18n._(REMOVE_TIMEOUT_DESCRIPTOR)} data-flx="moderation.remove-timeout-modal.modal-header" />
			<Modal.Content data-flx="moderation.remove-timeout-modal.modal-content">
				<Modal.ContentLayout data-flx="moderation.remove-timeout-modal.modal-content-layout">
					<Modal.Description data-flx="moderation.remove-timeout-modal.description">
						<Trans>
							Removing the timeout will allow{' '}
							<strong data-flx="moderation.remove-timeout-modal.strong">{targetUserTag}</strong> to send messages,
							react, and join voice channels again.
						</Trans>
					</Modal.Description>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="moderation.remove-timeout-modal.modal-footer">
				<Button
					variant="secondary"
					onClick={() => ModalCommands.pop()}
					disabled={isSubmitting}
					data-flx="moderation.remove-timeout-modal.button.pop"
				>
					<Trans>Cancel</Trans>
				</Button>
				<Button
					variant="danger"
					onClick={handleRemove}
					disabled={isSubmitting}
					data-flx="moderation.remove-timeout-modal.button.remove"
				>
					<Trans>Remove timeout</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
