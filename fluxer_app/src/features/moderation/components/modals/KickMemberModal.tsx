// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {CANCEL_DESCRIPTOR, KICK_MEMBER_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as GuildMemberCommands from '@app/features/member/commands/GuildMemberCommands';
import {showModerationErrorModal} from '@app/features/moderation/components/alerts/ModerationErrorModalUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import type {User} from '@app/features/user/models/User';
import * as DisplayNameUtils from '@app/features/user/utils/DisplayNameUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const KICK_DESCRIPTOR = msg({
	message: 'Kick',
	comment: 'Button or menu action label in the kick member modal. Keep it concise. Keep the tone plain and specific.',
});
const logger = new Logger('KickMemberModal');
export const KickMemberModal: React.FC<{guildId: string; targetUser: User}> = observer(({guildId, targetUser}) => {
	const {i18n} = useLingui();
	const targetUserTag = DisplayNameUtils.formatTagForStreamerMode(targetUser.tag);
	const handleKick = async () => {
		try {
			await GuildMemberCommands.kick(guildId, targetUser.id);
			ToastCommands.createToast({
				type: 'success',
				children: <Trans>Kicked {targetUserTag} from the community</Trans>,
			});
		} catch (error) {
			logger.error('Failed to kick member:', error);
			showModerationErrorModal(
				i18n,
				<Trans>Failed to kick member. Try again.</Trans>,
				'moderation.kick-member-modal.kick-error-modal',
				true,
			);
		}
	};
	return (
		<ConfirmModal
			title={i18n._(KICK_MEMBER_DESCRIPTOR)}
			description={
				<div data-flx="moderation.kick-member-modal.div">
					<Trans>
						Are you sure you want to kick{' '}
						<strong data-flx="moderation.kick-member-modal.strong">{targetUserTag}</strong> from the community? They
						will be able to rejoin with a new invite.
					</Trans>
				</div>
			}
			primaryText={i18n._(KICK_DESCRIPTOR)}
			primaryVariant="danger"
			secondaryText={i18n._(CANCEL_DESCRIPTOR)}
			onPrimary={handleKick}
			data-flx="moderation.kick-member-modal.confirm-modal"
		/>
	);
});
