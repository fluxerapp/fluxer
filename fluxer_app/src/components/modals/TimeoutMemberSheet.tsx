/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Trans, useLingui} from '@lingui/react/macro';
import {ClockIcon, XCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as GuildMemberActionCreators from '~/actions/GuildMemberActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {
	MenuBottomSheet,
	type MenuGroupType,
	type MenuItemType,
	type MenuRadioType,
} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import type {GuildMemberRecord} from '~/records/GuildMemberRecord';
import type {UserRecord} from '~/records/UserRecord';
import {RemoveTimeoutModal} from './RemoveTimeoutModal';
import {getTimeoutDurationOptions, type TimeoutDurationOption} from './TimeoutMemberOptions';
import styles from './TimeoutMemberSheet.module.css';

interface TimeoutMemberSheetProps {
	isOpen: boolean;
	onClose: () => void;
	guildId: string;
	targetUser: UserRecord;
	targetMember: GuildMemberRecord;
}

export const TimeoutMemberSheet: React.FC<TimeoutMemberSheetProps> = observer(
	({isOpen, onClose, guildId, targetUser, targetMember}) => {
		const {t} = useLingui();
		const isCurrentlyTimedOut = targetMember.isTimedOut();
		const timeoutOptions = React.useMemo(() => getTimeoutDurationOptions(t), [t]);
		const [timeoutDuration, setTimeoutDuration] = React.useState<number>(timeoutOptions[3].value);
		const [isSubmitting, setIsSubmitting] = React.useState(false);

		const handleTimeout = React.useCallback(async () => {
			setIsSubmitting(true);
			try {
				const timeoutUntil = new Date(Date.now() + timeoutDuration * 1000).toISOString();
				await GuildMemberActionCreators.timeout(guildId, targetUser.id, timeoutUntil);
				ToastActionCreators.createToast({
					type: 'success',
					children: <Trans>Successfully timed out {targetUser.tag}</Trans>,
				});
				onClose();
			} catch (error) {
				console.error('Failed to timeout member:', error);
				ToastActionCreators.createToast({
					type: 'error',
					children: <Trans>Failed to timeout member. Please try again.</Trans>,
				});
			} finally {
				setIsSubmitting(false);
			}
		}, [guildId, onClose, targetUser.id, timeoutDuration, targetUser.tag]);

		const durationItems: Array<MenuRadioType> = React.useMemo(() => {
			return timeoutOptions.map((option: TimeoutDurationOption) => ({
				label: option.label,
				selected: option.value === timeoutDuration,
				onSelect: () => setTimeoutDuration(option.value),
			}));
		}, [timeoutDuration, timeoutOptions]);

		const actionItems: Array<MenuItemType> = React.useMemo(() => {
			const items: Array<MenuItemType> = [
				{
					id: 'timeout',
					icon: <ClockIcon size={20} />,
					label: t`Timeout`,
					onClick: handleTimeout,
					danger: true,
					disabled: isSubmitting,
				},
			];

			if (isCurrentlyTimedOut) {
				items.push({
					id: 'remove-timeout',
					icon: <XCircleIcon size={20} />,
					label: t`Remove Timeout`,
					onClick: () => {
						onClose();
						ModalActionCreators.push(modal(() => <RemoveTimeoutModal guildId={guildId} targetUser={targetUser} />));
					},
					danger: true,
					disabled: isSubmitting,
				});
			}

			return items;
		}, [guildId, handleTimeout, isCurrentlyTimedOut, isSubmitting, onClose, targetUser]);

		const headerContent = (
			<div className={styles.header}>
				<p className={styles.description}>
					{isCurrentlyTimedOut ? (
						<Trans>
							<strong>{targetUser.tag}</strong> is currently timed out. You can update their timeout duration or remove
							the timeout.
						</Trans>
					) : (
						<Trans>
							Prevent <strong>{targetUser.tag}</strong> from sending messages, reacting, and connecting to voice
							channels for the specified duration.
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
				title={isCurrentlyTimedOut ? t`Update Timeout` : t`Timeout ${targetUser.tag}`}
				showCloseButton={true}
				headerContent={headerContent}
				groups={groups}
			/>
		);
	},
);
