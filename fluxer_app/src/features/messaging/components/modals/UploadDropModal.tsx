// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import {UNKNOWN_CHANNEL_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import styles from '@app/features/messaging/components/modals/UploadDropModal.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowFatUpIcon, FileIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';

const UPLOAD_DIRECTLY_TO_DESCRIPTOR = msg({
	message: 'Upload directly to {displayName}',
	comment: 'Title of the upload drop modal when shift is held. Files will be sent immediately to displayName.',
});
const UPLOAD_TO_DESCRIPTOR = msg({
	message: 'Upload to {displayName}',
	comment: 'Title of the upload drop modal in the default state. Files will go to displayName, with a preview step.',
});
const FILES_WILL_BE_SENT_IMMEDIATELY_WITHOUT_PREVIEW_DESCRIPTOR = msg({
	message: 'Files will be sent immediately without preview.',
	comment: 'Helper text in the upload drop modal when shift-drop is active.',
});
const YOU_CAN_ADD_COMMENTS_BEFORE_UPLOADING_DIRECT_UPLOAD_DESCRIPTOR = msg({
	message: 'You can add comments before uploading. Direct upload is disabled during slowmode.',
	comment: 'Helper text in the upload drop modal during slowmode, when direct upload is disabled.',
});
const YOU_CAN_ADD_COMMENTS_BEFORE_UPLOADING_HOLD_SHIFT_DESCRIPTOR = msg({
	message: 'You can add comments before uploading. Hold Shift to upload directly.',
	comment: 'Helper text in the upload drop modal explaining the shift-to-direct-upload behavior.',
});
const DIRECT_UPLOAD_ACTIVE_DESCRIPTOR = msg({
	message: 'Direct upload active',
	comment: 'Status pill shown in the upload drop modal when shift is currently held.',
});
const HOLD_FOR_INSTANT_UPLOAD_DESCRIPTOR = msg({
	message: 'Hold for instant upload',
	comment: 'Hint chip shown in the upload drop modal explaining how to enable direct upload.',
});

interface UploadDropModalProps {
	channel: Channel;
	isShiftHeld: boolean;
	isSlowmodeActive: boolean;
}

export const UploadDropModal = ({channel, isShiftHeld, isSlowmodeActive}: UploadDropModalProps) => {
	const {i18n} = useLingui();
	const displayName = channel.isPrivate()
		? ChannelUtils.getDMDisplayName(channel)
		: channel.name
			? `#${channel.name}`
			: i18n._(UNKNOWN_CHANNEL_DESCRIPTOR);
	return (
		<div className={styles.overlay} data-flx="messaging.upload-drop-modal.overlay">
			<div className={styles.dialog} data-flx="messaging.upload-drop-modal.dialog">
				<div className={styles.dialogIconCircle} data-flx="messaging.upload-drop-modal.dialog-icon-circle">
					<FileIcon className={styles.dialogIcon} weight="fill" data-flx="messaging.upload-drop-modal.dialog-icon" />
				</div>
				<div className={styles.dialogTextBlock} data-flx="messaging.upload-drop-modal.dialog-text-block">
					<h2 className={styles.dialogTitle} data-flx="messaging.upload-drop-modal.dialog-title">
						{isShiftHeld
							? i18n._(UPLOAD_DIRECTLY_TO_DESCRIPTOR, {displayName})
							: i18n._(UPLOAD_TO_DESCRIPTOR, {displayName})}
					</h2>
					<p className={styles.dialogDescription} data-flx="messaging.upload-drop-modal.dialog-description">
						{isShiftHeld
							? i18n._(FILES_WILL_BE_SENT_IMMEDIATELY_WITHOUT_PREVIEW_DESCRIPTOR)
							: isSlowmodeActive
								? i18n._(YOU_CAN_ADD_COMMENTS_BEFORE_UPLOADING_DIRECT_UPLOAD_DESCRIPTOR)
								: i18n._(YOU_CAN_ADD_COMMENTS_BEFORE_UPLOADING_HOLD_SHIFT_DESCRIPTOR)}
					</p>
				</div>
				<div
					className={clsx(styles.statusBanner, isShiftHeld ? styles.statusBannerActive : styles.statusBannerDefault)}
					data-flx="messaging.upload-drop-modal.status-banner"
				>
					<div
						className={clsx(styles.statusIndicator, isShiftHeld && styles.statusIndicatorActive)}
						data-flx="messaging.upload-drop-modal.status-indicator"
					>
						<ArrowFatUpIcon
							className={styles.statusIcon}
							weight="fill"
							data-flx="messaging.upload-drop-modal.status-icon"
						/>
					</div>
					<span data-flx="messaging.upload-drop-modal.span">
						{isShiftHeld ? i18n._(DIRECT_UPLOAD_ACTIVE_DESCRIPTOR) : i18n._(HOLD_FOR_INSTANT_UPLOAD_DESCRIPTOR)}
					</span>
				</div>
			</div>
		</div>
	);
};
