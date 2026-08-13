// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import * as GuildMemberCommands from '@app/features/member/commands/GuildMemberCommands';
import {showModerationErrorModal} from '@app/features/moderation/components/alerts/ModerationErrorModalUtils';
import styles from '@app/features/moderation/components/modals/TimeoutMemberModal.module.css';
import {getTimeoutDurationOptions} from '@app/features/moderation/components/modals/TimeoutMemberOptions';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Combobox as FormCombobox} from '@app/features/ui/components/form/FormCombobox';
import {Textarea} from '@app/features/ui/components/form/FormInput';
import {formatGuildSettingsPath} from '@app/features/user/components/settings_utils/GuildSettingsConstants';
import type {User} from '@app/features/user/models/User';
import * as DisplayNameUtils from '@app/features/user/utils/DisplayNameUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo, useState} from 'react';

const TIMEOUT_DESCRIPTOR = msg({
	message: 'Timeout {tag}',
	comment:
		'Title of the timeout-member modal. {tag} is the target user tag (username#tag). Moderation action; keep tone direct.',
});
const TIMEOUT_DURATION_DESCRIPTOR = msg({
	message: 'Timeout duration',
	comment: 'Label above the timeout duration dropdown in the timeout-member modal.',
});
const HOW_LONG_THIS_USER_SHOULD_BE_TIMED_OUT_DESCRIPTOR = msg({
	message: 'How long they stay timed out.',
	comment: 'Helper text under the timeout duration dropdown in the timeout-member modal.',
});
const REASON_OPTIONAL_DESCRIPTOR = msg({
	message: 'Reason (optional)',
	comment:
		'Label of the optional reason textarea in the timeout-member modal. The reason is recorded in the activity log.',
});
const logger = new Logger('TimeoutMemberModal');

interface TimeoutMemberModalProps {
	guildId: string;
	targetUser: User;
}

export const TimeoutMemberModal: React.FC<TimeoutMemberModalProps> = observer(({guildId, targetUser}) => {
	const {i18n} = useLingui();
	const activityLogSettingsPath = formatGuildSettingsPath(i18n, 'audit_log');
	const durationOptions = useMemo(() => getTimeoutDurationOptions(i18n), [i18n.locale]);
	const [selectedDuration, setSelectedDuration] = useState<number>(durationOptions[3].value);
	const [reason, setReason] = useState('');
	const [isSubmitting, setIsSubmitting] = useState(false);
	const targetUserTag = DisplayNameUtils.formatTagForStreamerMode(targetUser.tag);
	const handleTimeout = async () => {
		setIsSubmitting(true);
		try {
			const timeoutUntil = new Date(Date.now() + selectedDuration * 1000).toISOString();
			const trimmedReason = reason.trim();
			let auditReason: string | null = trimmedReason;
			if (trimmedReason.length === 0) {
				auditReason = null;
			}
			await GuildMemberCommands.timeout(guildId, targetUser.id, timeoutUntil, auditReason);
			ToastCommands.createToast({
				type: 'success',
				children: <Trans>Timed out {targetUserTag}</Trans>,
			});
			ModalCommands.pop();
		} catch (error) {
			logger.error('Failed to time out member:', error);
			showModerationErrorModal(
				i18n,
				<Trans>Failed to time out member. Try again.</Trans>,
				'moderation.timeout-member-modal.timeout-error-modal',
			);
		} finally {
			setIsSubmitting(false);
		}
	};
	return (
		<Modal.Root size="small" centered data-flx="moderation.timeout-member-modal.modal-root">
			<Modal.Header
				title={i18n._(TIMEOUT_DESCRIPTOR, {tag: targetUserTag})}
				data-flx="moderation.timeout-member-modal.modal-header"
			/>
			<Modal.Content data-flx="moderation.timeout-member-modal.modal-content">
				<Modal.ContentLayout data-flx="moderation.timeout-member-modal.modal-content-layout">
					<Modal.Description data-flx="moderation.timeout-member-modal.helper-text">
						<Trans>
							Prevent <strong data-flx="moderation.timeout-member-modal.strong">{targetUserTag}</strong> from sending
							messages, reacting, and joining voice channels for the specified duration.
						</Trans>
					</Modal.Description>
					<FormCombobox<number>
						label={i18n._(TIMEOUT_DURATION_DESCRIPTOR)}
						description={i18n._(HOW_LONG_THIS_USER_SHOULD_BE_TIMED_OUT_DESCRIPTOR)}
						value={selectedDuration}
						onChange={setSelectedDuration}
						options={durationOptions}
						disabled={isSubmitting}
						data-flx="moderation.timeout-member-modal.form-select.set-selected-duration"
					/>
					<Textarea
						label={i18n._(REASON_OPTIONAL_DESCRIPTOR)}
						value={reason}
						onChange={(event) => setReason(event.target.value)}
						maxLength={512}
						minRows={3}
						disabled={isSubmitting}
						data-flx="moderation.timeout-member-modal.textarea.set-reason"
					/>
					<Modal.Description className={styles.hint} data-flx="moderation.timeout-member-modal.hint">
						<Trans>This reason will be displayed in the activity log in {activityLogSettingsPath}.</Trans>
					</Modal.Description>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="moderation.timeout-member-modal.modal-footer">
				<Button
					variant="secondary"
					onClick={() => ModalCommands.pop()}
					disabled={isSubmitting}
					data-flx="moderation.timeout-member-modal.button.pop"
				>
					<Trans>Cancel</Trans>
				</Button>
				<Button
					variant="danger"
					onClick={handleTimeout}
					disabled={isSubmitting}
					data-flx="moderation.timeout-member-modal.button.timeout"
				>
					<Trans>Timeout</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
