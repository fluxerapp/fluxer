// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {useAnimatedMediaVideoPlayback} from '@app/features/app/hooks/useAnimatedMediaPlayback';
import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import {
	CUSTOM_ELLIPSIS_DESCRIPTOR,
	ONE_DAY_DURATION_DESCRIPTOR,
	ONE_HOUR_DURATION_DESCRIPTOR,
	ONE_MONTH_DURATION_DESCRIPTOR,
	ONE_WEEK_DURATION_DESCRIPTOR,
	TWELVE_HOURS_DURATION_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {showModerationErrorModal} from '@app/features/moderation/components/alerts/ModerationErrorModalUtils';
import styles from '@app/features/moderation/components/modals/BanMemberModal.module.css';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Combobox as FormCombobox} from '@app/features/ui/components/form/FormCombobox';
import {Input} from '@app/features/ui/components/form/FormInput';
import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import type {User} from '@app/features/user/models/User';
import * as DisplayNameUtils from '@app/features/user/utils/DisplayNameUtils';
import bannedMp4 from '@app/media/videos/banned.mp4';
import bannedWebm from '@app/media/videos/banned.webm';
import bannedPoster from '@app/media/videos/banned.webp';
import {MAX_TEMP_BAN_DURATION_SECONDS, MIN_TEMP_BAN_DURATION_SECONDS} from '@fluxer/constants/src/LimitConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useRef, useState} from 'react';

const PERMANENT_DESCRIPTOR = msg({
	message: 'Permanent',
	comment:
		'Ban duration option in the destructive ban-member modal meaning the ban never expires. Short standalone label.',
});
const MESSAGE_3_DAYS_DESCRIPTOR = msg({
	message: '3 days',
	comment: 'Ban duration option in the destructive ban-member modal for a three-day temporary ban.',
});
const MESSAGE_5_DAYS_DESCRIPTOR = msg({
	message: '5 days',
	comment: 'Ban duration option in the destructive ban-member modal for a five-day temporary ban.',
});
const MESSAGE_2_WEEKS_DESCRIPTOR = msg({
	message: '2 weeks',
	comment: 'Ban duration option in the destructive ban-member modal for a two-week temporary ban.',
});
const BAN_DESCRIPTOR = msg({
	message: 'Ban {tag}',
	comment:
		'Title of the destructive ban-member modal. {tag} is the target user tag (username#tag). Destructive moderation action; keep tone direct.',
});
const BAN_DURATION_DESCRIPTOR = msg({
	message: 'Ban duration',
	comment: 'Label above the ban duration dropdown in the destructive ban-member modal.',
});
const HOW_LONG_THIS_USER_SHOULD_BE_BANNED_FOR_DESCRIPTOR = msg({
	message: 'How long they stay banned.',
	comment: 'Helper text under the ban duration dropdown in the destructive ban-member modal.',
});
const CUSTOM_BAN_DURATION_SECONDS_DESCRIPTOR = msg({
	message: 'Custom ban duration (seconds)',
	comment:
		'Label of the numeric input shown after choosing "Custom..." in the ban duration dropdown. The unit is seconds.',
});
const ANY_VALUE_FROM_TO_SECONDS_DESCRIPTOR = msg({
	message: 'Any value from {minTempBanDurationSeconds} to {maxTempBanDurationSeconds} seconds.',
	comment:
		'Helper text under the custom ban duration input. Both placeholders are integer numbers of seconds bounding the allowed range.',
});
const DELETE_MESSAGE_HISTORY_DESCRIPTOR = msg({
	message: 'Delete message history',
	comment:
		'Section heading and accessible label for the message-history-deletion radio group in the destructive ban-member modal. Destructive option group.',
});
const DON_T_DELETE_ANY_DESCRIPTOR = msg({
	message: "Don't delete any",
	comment:
		'Message-history-deletion option in the destructive ban-member modal. Keeps all messages from the banned member. Short standalone label.',
});
const KEEP_ALL_MESSAGES_DESCRIPTOR = msg({
	message: 'Keep all messages',
	comment: 'Helper text under the "Don\'t delete any" message-history option in the destructive ban-member modal.',
});
const PREVIOUS_24_HOURS_DESCRIPTOR = msg({
	message: 'Previous 24 hours',
	comment:
		"Message-history-deletion option in the destructive ban-member modal. Deletes the banned member's messages from the last 24 hours.",
});
const DELETE_MESSAGES_FROM_THE_LAST_DAY_DESCRIPTOR = msg({
	message: 'Delete their messages from the last 24 hours',
	comment: 'Helper text under the "Previous 24 hours" option in the destructive ban-member modal.',
});
const PREVIOUS_7_DAYS_DESCRIPTOR = msg({
	message: 'Previous 7 days',
	comment:
		"Message-history-deletion option in the destructive ban-member modal. Deletes the banned member's messages from the last 7 days.",
});
const DELETE_MESSAGES_FROM_THE_LAST_WEEK_DESCRIPTOR = msg({
	message: 'Delete their messages from the last 7 days',
	comment: 'Helper text under the "Previous 7 days" option in the destructive ban-member modal.',
});
const REASON_OPTIONAL_DESCRIPTOR = msg({
	message: 'Reason (optional)',
	comment:
		'Label of the optional reason input in the destructive ban-member modal. Reason is recorded in the activity log.',
});
const ENTER_A_REASON_FOR_THE_BAN_DESCRIPTOR = msg({
	message: 'Enter a reason for the ban',
	comment: 'Placeholder in the optional reason input in the destructive ban-member modal.',
});
const logger = new Logger('BanMemberModal');
const BAN_DURATION_CUSTOM_SENTINEL = -1;

interface ComboboxOption {
	value: number;
	label: string;
}

export const BanMemberModal: React.FC<{guildId: string; targetUser: User}> = observer(({guildId, targetUser}) => {
	const {i18n} = useLingui();
	const videoRef = useRef<HTMLVideoElement>(null);
	const videoPlaybackAllowed = useAnimatedMediaVideoPlayback(videoRef);
	const [reason, setReason] = useState('');
	const [deleteMessageDays, setDeleteMessageDays] = useState<number>(1);
	const [banDuration, setBanDuration] = useState<number>(0);
	const [isBanDurationCustom, setIsBanDurationCustom] = useState(false);
	const [isBanning, setIsBanning] = useState(false);
	const targetUserTag = DisplayNameUtils.formatTagForStreamerMode(targetUser.tag);
	const getBanDurationOptions = useCallback(
		(): ReadonlyArray<ComboboxOption> => [
			{value: 0, label: i18n._(PERMANENT_DESCRIPTOR)},
			{value: 60 * 60, label: i18n._(ONE_HOUR_DURATION_DESCRIPTOR)},
			{value: 60 * 60 * 12, label: i18n._(TWELVE_HOURS_DURATION_DESCRIPTOR)},
			{value: 60 * 60 * 24, label: i18n._(ONE_DAY_DURATION_DESCRIPTOR)},
			{value: 60 * 60 * 24 * 3, label: i18n._(MESSAGE_3_DAYS_DESCRIPTOR)},
			{value: 60 * 60 * 24 * 5, label: i18n._(MESSAGE_5_DAYS_DESCRIPTOR)},
			{value: 60 * 60 * 24 * 7, label: i18n._(ONE_WEEK_DURATION_DESCRIPTOR)},
			{value: 60 * 60 * 24 * 14, label: i18n._(MESSAGE_2_WEEKS_DESCRIPTOR)},
			{value: 60 * 60 * 24 * 30, label: i18n._(ONE_MONTH_DURATION_DESCRIPTOR)},
			{value: BAN_DURATION_CUSTOM_SENTINEL, label: i18n._(CUSTOM_ELLIPSIS_DESCRIPTOR)},
		],
		[i18n],
	);
	const BAN_DURATION_OPTIONS = getBanDurationOptions();
	const handleBan = async () => {
		setIsBanning(true);
		try {
			await GuildCommands.banMember(guildId, targetUser.id, deleteMessageDays, reason || undefined, banDuration);
			ToastCommands.createToast({
				type: 'success',
				children: <Trans>Banned {targetUserTag} from the community</Trans>,
			});
			ModalCommands.pop();
		} catch (error) {
			logger.error('Failed to ban member:', error);
			showModerationErrorModal(
				i18n,
				<Trans>Failed to ban member. Try again.</Trans>,
				'moderation.ban-member-modal.ban-error-modal',
			);
		} finally {
			setIsBanning(false);
		}
	};
	return (
		<Modal.Root size="small" centered data-flx="moderation.ban-member-modal.modal-root">
			<Modal.Header
				title={i18n._(BAN_DESCRIPTOR, {tag: targetUserTag})}
				data-flx="moderation.ban-member-modal.modal-header"
			/>
			<Modal.Content data-flx="moderation.ban-member-modal.modal-content">
				<div className={styles.content} data-flx="moderation.ban-member-modal.content">
					<video
						ref={videoRef}
						autoPlay={videoPlaybackAllowed}
						loop
						muted
						playsInline
						poster={bannedPoster}
						className={styles.video}
						data-flx="moderation.ban-member-modal.video"
					>
						<source src={bannedWebm} type="video/webm" data-flx="moderation.ban-member-modal.source.video-webm" />
						<source src={bannedMp4} type="video/mp4" data-flx="moderation.ban-member-modal.source.video-mp4" />
					</video>
					<div data-flx="moderation.ban-member-modal.div">
						<FormCombobox<number>
							label={i18n._(BAN_DURATION_DESCRIPTOR)}
							description={i18n._(HOW_LONG_THIS_USER_SHOULD_BE_BANNED_FOR_DESCRIPTOR)}
							value={isBanDurationCustom ? BAN_DURATION_CUSTOM_SENTINEL : banDuration}
							onChange={(v) => {
								if (v === BAN_DURATION_CUSTOM_SENTINEL) {
									setIsBanDurationCustom(true);
									if (banDuration === 0) {
										setBanDuration(MIN_TEMP_BAN_DURATION_SECONDS);
									}
									return;
								}
								setIsBanDurationCustom(false);
								setBanDuration(v);
							}}
							options={BAN_DURATION_OPTIONS}
							disabled={isBanning}
							data-flx="moderation.ban-member-modal.form-select.set-is-ban-duration-custom"
						/>
						{isBanDurationCustom && (
							<Input
								type="number"
								label={i18n._(CUSTOM_BAN_DURATION_SECONDS_DESCRIPTOR)}
								footer={i18n._(ANY_VALUE_FROM_TO_SECONDS_DESCRIPTOR, {
									minTempBanDurationSeconds: MIN_TEMP_BAN_DURATION_SECONDS,
									maxTempBanDurationSeconds: MAX_TEMP_BAN_DURATION_SECONDS,
								})}
								min={MIN_TEMP_BAN_DURATION_SECONDS}
								max={MAX_TEMP_BAN_DURATION_SECONDS}
								step={1}
								value={String(banDuration)}
								onChange={(event) => {
									const raw = event.target.value;
									if (raw === '') return;
									const parsed = Number.parseInt(raw, 10);
									if (Number.isNaN(parsed)) return;
									const clamped = Math.max(
										MIN_TEMP_BAN_DURATION_SECONDS,
										Math.min(MAX_TEMP_BAN_DURATION_SECONDS, parsed),
									);
									setBanDuration(clamped);
								}}
								disabled={isBanning}
								data-flx="moderation.ban-member-modal.input.set-ban-duration.number"
							/>
						)}
					</div>
					<div data-flx="moderation.ban-member-modal.div--2">
						<div className={styles.sectionTitle} data-flx="moderation.ban-member-modal.section-title">
							<Trans>Delete message history</Trans>
						</div>
						<RadioGroup
							aria-label={i18n._(DELETE_MESSAGE_HISTORY_DESCRIPTOR)}
							options={[
								{value: 0, name: i18n._(DON_T_DELETE_ANY_DESCRIPTOR), desc: i18n._(KEEP_ALL_MESSAGES_DESCRIPTOR)},
								{
									value: 1,
									name: i18n._(PREVIOUS_24_HOURS_DESCRIPTOR),
									desc: i18n._(DELETE_MESSAGES_FROM_THE_LAST_DAY_DESCRIPTOR),
								},
								{
									value: 7,
									name: i18n._(PREVIOUS_7_DAYS_DESCRIPTOR),
									desc: i18n._(DELETE_MESSAGES_FROM_THE_LAST_WEEK_DESCRIPTOR),
								},
							]}
							value={deleteMessageDays}
							onChange={setDeleteMessageDays}
							disabled={isBanning}
							data-flx="moderation.ban-member-modal.radio-group.set-delete-message-days"
						/>
					</div>
					<Input
						type="text"
						label={i18n._(REASON_OPTIONAL_DESCRIPTOR)}
						value={reason}
						onChange={(e) => setReason(e.target.value)}
						placeholder={i18n._(ENTER_A_REASON_FOR_THE_BAN_DESCRIPTOR)}
						maxLength={512}
						disabled={isBanning}
						data-flx="moderation.ban-member-modal.input.set-reason.text"
					/>
				</div>
			</Modal.Content>
			<Modal.Footer data-flx="moderation.ban-member-modal.modal-footer">
				<Button
					variant="secondary"
					onClick={() => ModalCommands.pop()}
					disabled={isBanning}
					data-flx="moderation.ban-member-modal.button.pop"
				>
					<Trans>Cancel</Trans>
				</Button>
				<Button
					variant="danger"
					onClick={handleBan}
					disabled={isBanning}
					data-flx="moderation.ban-member-modal.button.ban"
				>
					<Trans>Ban member</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
