// SPDX-License-Identifier: AGPL-3.0-or-later

import * as GuildMemberCommands from '@app/features/member/commands/GuildMemberCommands';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import {showModerationErrorModal} from '@app/features/moderation/components/alerts/ModerationErrorModalUtils';
import {RemoveTimeoutModal} from '@app/features/moderation/components/modals/RemoveTimeoutModal';
import {
	getTimeoutDurationOptions,
	type TimeoutDurationOption,
} from '@app/features/moderation/components/modals/TimeoutMemberOptions';
import styles from '@app/features/moderation/components/modals/TimeoutMemberSheet.module.css';
import {
	REMOVE_TIMEOUT_DESCRIPTOR,
	TIMEOUT_DESCRIPTOR,
} from '@app/features/moderation/utils/ModerationMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {
	MenuBottomSheet,
	type MenuGroupType,
	type MenuItemType,
	type MenuRadioType,
} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import type {User} from '@app/features/user/models/User';
import * as DisplayNameUtils from '@app/features/user/utils/DisplayNameUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ClockIcon, XCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useState} from 'react';

const UPDATE_TIMEOUT_DESCRIPTOR = msg({
	message: 'Update timeout',
	comment: 'Short label in the timeout member sheet. Keep it concise.',
});
const TIMEOUT_2_DESCRIPTOR = msg({
	message: 'Timeout {tag}',
	comment: 'Short label in the timeout member sheet. Keep it concise. Preserve {tag}; it is inserted by code.',
});
const logger = new Logger('TimeoutMemberSheet');

interface TimeoutMemberSheetProps {
	isOpen: boolean;
	onClose: () => void;
	guildId: string;
	targetUser: User;
	targetMember: GuildMember;
}

export const TimeoutMemberSheet: React.FC<TimeoutMemberSheetProps> = observer(
	({isOpen, onClose, guildId, targetUser, targetMember}) => {
		const {i18n} = useLingui();
		const isCurrentlyTimedOut = targetMember.isTimedOut();
		const timeoutOptions = useMemo(() => getTimeoutDurationOptions(i18n), [i18n.locale]);
		const [timeoutDuration, setTimeoutDuration] = useState<number>(timeoutOptions[3].value);
		const [isSubmitting, setIsSubmitting] = useState(false);
		const targetUserTag = DisplayNameUtils.formatTagForStreamerMode(targetUser.tag);
		const handleTimeout = useCallback(async () => {
			setIsSubmitting(true);
			try {
				const timeoutUntil = new Date(Date.now() + timeoutDuration * 1000).toISOString();
				await GuildMemberCommands.timeout(guildId, targetUser.id, timeoutUntil);
				ToastCommands.createToast({
					type: 'success',
					children: <Trans>Timed out {targetUserTag}</Trans>,
				});
				onClose();
			} catch (error) {
				logger.error('Failed to time out member:', error);
				showModerationErrorModal(
					i18n,
					<Trans>Failed to time out member. Try again.</Trans>,
					'moderation.timeout-member-sheet.timeout-error-modal',
				);
			} finally {
				setIsSubmitting(false);
			}
		}, [guildId, onClose, targetUser.id, targetUserTag, timeoutDuration]);
		const durationItems: Array<MenuRadioType> = useMemo(() => {
			return timeoutOptions.map((option: TimeoutDurationOption) => ({
				label: option.label,
				selected: option.value === timeoutDuration,
				onSelect: () => setTimeoutDuration(option.value),
			}));
		}, [timeoutDuration, timeoutOptions]);
		const actionItems: Array<MenuItemType> = useMemo(() => {
			const items: Array<MenuItemType> = [
				{
					id: 'timeout',
					icon: <ClockIcon size={remFromPx(20)} data-flx="moderation.timeout-member-sheet.action-items.clock-icon" />,
					label: i18n._(TIMEOUT_DESCRIPTOR),
					onClick: handleTimeout,
					danger: true,
					disabled: isSubmitting,
				},
			];
			if (isCurrentlyTimedOut) {
				items.push({
					id: 'remove-timeout',
					icon: (
						<XCircleIcon size={remFromPx(20)} data-flx="moderation.timeout-member-sheet.action-items.x-circle-icon" />
					),
					label: i18n._(REMOVE_TIMEOUT_DESCRIPTOR),
					onClick: () => {
						ModalCommands.pushAfterBottomSheetClose(
							onClose,
							modal(() => (
								<RemoveTimeoutModal
									guildId={guildId}
									targetUser={targetUser}
									data-flx="moderation.timeout-member-sheet.on-click.remove-timeout-modal"
								/>
							)),
						);
					},
					danger: true,
					disabled: isSubmitting,
				});
			}
			return items;
		}, [guildId, handleTimeout, isCurrentlyTimedOut, isSubmitting, onClose, targetUser]);
		const headerContent = (
			<div className={styles.header} data-flx="moderation.timeout-member-sheet.header">
				<p className={styles.description} data-flx="moderation.timeout-member-sheet.description">
					{isCurrentlyTimedOut ? (
						<Trans>
							<strong data-flx="moderation.timeout-member-sheet.strong">{targetUserTag}</strong> is currently timed out.
							You can update their timeout duration or remove the timeout.
						</Trans>
					) : (
						<Trans>
							Prevent <strong data-flx="moderation.timeout-member-sheet.strong--2">{targetUserTag}</strong> from sending
							messages, reacting, and connecting to voice channels for the specified duration.
						</Trans>
					)}
				</p>
			</div>
		);
		const groups: Array<MenuGroupType> = [{items: durationItems}, {items: actionItems}];
		return (
			<MenuBottomSheet
				isOpen={isOpen}
				onClose={onClose}
				title={
					isCurrentlyTimedOut ? i18n._(UPDATE_TIMEOUT_DESCRIPTOR) : i18n._(TIMEOUT_2_DESCRIPTOR, {tag: targetUserTag})
				}
				showCloseButton={true}
				headerContent={headerContent}
				groups={groups}
				data-flx="moderation.timeout-member-sheet.menu-bottom-sheet"
			/>
		);
	},
);
